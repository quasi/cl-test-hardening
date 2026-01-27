;;; ABOUTME: Generator combinators
;;; Provides one-of, frequency, such-that, tuple, fmap, constant, elements

(in-package #:th.gen)

;;;; Constant Generator

(defclass constant-generator (generator)
  ((value :initarg :value :reader gen-value))
  (:documentation "Generator that always returns the same value."))

(defun const (value)
  "Create a generator that always returns VALUE."
  (make-instance 'constant-generator :name 'const :value value))

(defmethod generate ((gen constant-generator) random-state size)
  (declare (ignore random-state size))
  (gen-value gen))

(defmethod shrink ((gen constant-generator) value)
  (declare (ignore value))
  '())

;;;; Elements Generator

(defclass elements-generator (generator)
  ((choices :initarg :choices :reader gen-choices))
  (:documentation "Generator that picks uniformly from a list of values."))

(defun elements (&rest values)
  "Create a generator that picks uniformly from VALUES."
  (when (null values)
    (error "elements requires at least one value"))
  (make-instance 'elements-generator :name 'elements :choices values))

(defmethod generate ((gen elements-generator) random-state size)
  (declare (ignore size))
  (th.property::next-random-element (gen-choices gen) random-state))

(defmethod shrink ((gen elements-generator) value)
  ;; Shrink toward first element in choices
  (let ((choices (gen-choices gen)))
    (if (equal value (first choices))
        '()
        (cl:list (first choices)))))

;;;; One-Of Generator

(defclass one-of-generator (generator)
  ((generators :initarg :generators :reader gen-generators))
  (:documentation "Generator that picks uniformly from several generators."))

(defun one-of (&rest generators)
  "Create a generator that picks uniformly from GENERATORS."
  (when (null generators)
    (error "one-of requires at least one generator"))
  (make-instance 'one-of-generator :name 'one-of :generators generators))

(defmethod generate ((gen one-of-generator) random-state size)
  (let ((chosen (th.property::next-random-element (gen-generators gen) random-state)))
    (generate chosen random-state size)))

(defmethod shrink ((gen one-of-generator) value)
  ;; Try shrinking with each sub-generator
  (cl:loop for sub-gen in (gen-generators gen)
           append (shrink sub-gen value)))

;;;; Frequency Generator

(defclass frequency-generator (generator)
  ((weighted-generators :initarg :weighted :reader gen-weighted)
   (total-weight :initarg :total :reader gen-total-weight))
  (:documentation "Generator with weighted random choice between generators."))

(defun frequency (&rest weight-generator-pairs)
  "Create a generator with weighted choice.
Each argument is (weight generator) where weight is a positive integer.
Example: (frequency (3 (th.gen:integers)) (1 (th.gen:strings)))"
  (when (null weight-generator-pairs)
    (error "frequency requires at least one (weight generator) pair"))
  (let ((total (reduce #'+ weight-generator-pairs :key #'first)))
    (make-instance 'frequency-generator
                   :name 'frequency
                   :weighted weight-generator-pairs
                   :total total)))

(defmethod generate ((gen frequency-generator) random-state size)
  (let ((roll (random (gen-total-weight gen) random-state))
        (cumulative 0))
    (dolist (pair (gen-weighted gen))
      (incf cumulative (first pair))
      (when (< roll cumulative)
        (return-from generate (generate (second pair) random-state size))))
    ;; Fallback to last generator
    (generate (second (car (last (gen-weighted gen)))) random-state size)))

(defmethod shrink ((gen frequency-generator) value)
  (loop for pair in (gen-weighted gen)
        append (shrink (second pair) value)))

;;;; Such-That Generator

(defclass such-that-generator (generator)
  ((predicate :initarg :predicate :reader gen-predicate)
   (base-generator :initarg :base :reader gen-base)
   (max-tries :initarg :max-tries :reader gen-max-tries :initform 100))
  (:documentation "Generator that filters values by a predicate."))

(defun such-that (predicate generator &key (max-tries 100))
  "Create a generator that only produces values satisfying PREDICATE.
MAX-TRIES: Maximum attempts before giving up (signals error)."
  (make-instance 'such-that-generator
                 :name 'such-that
                 :predicate predicate
                 :base generator
                 :max-tries max-tries))

(defmethod generate ((gen such-that-generator) random-state size)
  (dotimes (i (gen-max-tries gen))
    (let ((value (generate (gen-base gen) random-state size)))
      (when (funcall (gen-predicate gen) value)
        (return-from generate value))))
  (error "such-that: Could not generate value satisfying predicate after ~D tries"
         (gen-max-tries gen)))

(defmethod shrink ((gen such-that-generator) value)
  ;; Only keep shrinks that satisfy the predicate
  (remove-if-not (gen-predicate gen)
                 (shrink (gen-base gen) value)))

;;;; Tuple Generator

(defclass tuple-generator (generator)
  ((generators :initarg :generators :reader gen-generators))
  (:documentation "Generator that produces a list of values from multiple generators."))

(defun tuple (&rest generators)
  "Create a generator that produces a list of values, one from each generator.
Example: (th.gen:tuple (th.gen:integers) (th.gen:strings)) => (42 \"foo\")"
  (make-instance 'tuple-generator :name 'tuple :generators generators))

(defmethod generate ((gen tuple-generator) random-state size)
  (mapcar (lambda (g) (generate g random-state size))
          (gen-generators gen)))

(defmethod shrink ((gen tuple-generator) value)
  (when (and (listp value) (= (length value) (length (gen-generators gen))))
    (let ((shrinks '()))
      ;; Try shrinking each position
      (cl:loop for i from 0
               for elem in value
               for sub-gen in (gen-generators gen)
               do (dolist (shrunk-elem (shrink sub-gen elem))
                    (let ((new-tuple (copy-list value)))
                      (setf (nth i new-tuple) shrunk-elem)
                      (push new-tuple shrinks))))
      (nreverse shrinks))))

;;;; Fmap Generator

(defclass fmap-generator (generator)
  ((function :initarg :function :reader gen-function)
   (generators :initarg :generators :reader gen-generators))
  (:documentation "Generator that transforms values through a function."))

(defun fmap (function &rest generators)
  "Create a generator that applies FUNCTION to generated values.
FUNCTION receives one value from each generator.
Example: (th.gen:fmap #'cons (th.gen:integers) (th.gen:strings)) => (42 . \"foo\")"
  (make-instance 'fmap-generator
                 :name 'fmap
                 :function function
                 :generators generators))

(defmethod generate ((gen fmap-generator) random-state size)
  (let ((values (mapcar (lambda (g) (generate g random-state size))
                        (gen-generators gen))))
    (apply (gen-function gen) values)))

(defmethod shrink ((gen fmap-generator) value)
  ;; Can't automatically shrink transformed values
  ;; Would need inverse function
  (declare (ignore value))
  '())

;;;; Resize Generator

(defclass resize-generator (generator)
  ((base-generator :initarg :base :reader gen-base)
   (size-fn :initarg :size-fn :reader gen-size-fn))
  (:documentation "Generator that modifies the size parameter."))

(defun resize (size-or-fn generator)
  "Create a generator with modified size.
SIZE-OR-FN: Either a fixed size integer, or a function (lambda (size) new-size)."
  (let ((size-fn (etypecase size-or-fn
                   (cl:integer (constantly size-or-fn))
                   (function size-or-fn))))
    (make-instance 'resize-generator
                   :name 'resize
                   :base generator
                   :size-fn size-fn)))

(defun sized (size generator)
  "Shorthand for (resize size generator) with fixed size."
  (resize size generator))

(defmethod generate ((gen resize-generator) random-state size)
  (let ((new-size (funcall (gen-size-fn gen) size)))
    (generate (gen-base gen) random-state new-size)))

(defmethod shrink ((gen resize-generator) value)
  (shrink (gen-base gen) value))
