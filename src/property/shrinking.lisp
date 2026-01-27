;;; ABOUTME: Shrinking strategies for finding minimal counterexamples
;;; Implements binary search shrinking, structural shrinking, and custom shrink registry

(in-package #:th.property)

;;;; Shrinking Protocol

(defvar *shrink-registry* (make-hash-table :test 'eq)
  "Registry of custom shrink functions keyed by type name.")

(defun register-shrink (type-name shrink-fn)
  "Register a custom shrink function for TYPE-NAME."
  (setf (gethash type-name *shrink-registry*) shrink-fn))

(defun find-shrink (type-name)
  "Find registered shrink function for TYPE-NAME."
  (gethash type-name *shrink-registry*))

;;;; Integer Shrinking

(defun shrink-integer (n)
  "Shrink integer N toward zero.
Returns list of smaller integers in preference order."
  (cond
    ((zerop n) '())
    ((plusp n)
     ;; For positive: try 0, half, half+1, n-1
     (remove-duplicates
      (remove-if-not #'integerp
                     (list 0
                           (floor n 2)
                           (1+ (floor n 2))
                           (1- n)))
      :from-end t))
    (t
     ;; For negative: try 0, negate, half toward zero, n+1
     (remove-duplicates
      (remove-if-not #'integerp
                     (list 0
                           (- n)
                           (ceiling n 2)
                           (1+ n)))
      :from-end t))))

;;;; Float Shrinking

(defun shrink-float (x)
  "Shrink float X toward zero."
  (cond
    ((zerop x) '())
    ((< (abs x) 0.0001) (list 0.0))
    (t (list 0.0
             (/ x 2.0)
             (float (truncate x))))))

;;;; String Shrinking

(defun shrink-string (s)
  "Shrink string S by removing characters and simplifying.
Order: empty string, single chars removed, simplified chars."
  (when (plusp (length s))
    (let ((shrinks '()))
      ;; Try empty string
      (push "" shrinks)
      ;; Try removing each character
      (dotimes (i (length s))
        (push (concatenate 'string
                           (subseq s 0 i)
                           (subseq s (1+ i)))
              shrinks))
      ;; Try shrinking each character to 'a'
      (dotimes (i (length s))
        (unless (char= (char s i) #\a)
          (let ((copy (copy-seq s)))
            (setf (char copy i) #\a)
            (push copy shrinks))))
      (remove-duplicates (nreverse shrinks) :test #'string=))))

;;;; List Shrinking

(defun shrink-list (lst element-shrinker)
  "Shrink list LST using ELEMENT-SHRINKER for elements.
Strategy: try empty, remove elements, shrink elements."
  (when lst
    (let ((shrinks '()))
      ;; Try empty list
      (push '() shrinks)
      ;; Try removing each element
      (loop for i from 0 below (length lst)
            collect (append (subseq lst 0 i)
                            (subseq lst (1+ i)))
            into removals
            finally (setf shrinks (nconc removals shrinks)))
      ;; Try shrinking first element (limited for performance)
      (when (and element-shrinker (first lst))
        (dolist (shrunk-elem (funcall element-shrinker (first lst)))
          (push (cons shrunk-elem (rest lst)) shrinks)))
      (remove-duplicates (nreverse shrinks) :test #'equal))))

;;;; Vector Shrinking

(defun shrink-vector (vec element-shrinker)
  "Shrink vector VEC using ELEMENT-SHRINKER."
  (mapcar (lambda (lst) (coerce lst 'vector))
          (shrink-list (coerce vec 'list) element-shrinker)))

;;;; Generic Shrinking Wrapper

(defun shrink-value (value generator)
  "Shrink VALUE according to its GENERATOR's shrink method.
Falls back to type-based shrinking if generator doesn't specialize."
  (let ((from-generator (shrink generator value)))
    (if from-generator
        from-generator
        ;; Type-based fallback
        (typecase value
          (integer (shrink-integer value))
          (float (shrink-float value))
          (string (shrink-string value))
          (list (shrink-list value nil))
          (vector (shrink-vector value nil))
          (t '())))))

;;;; Shrinking Loop

(defvar *max-shrink-iterations* 100
  "Maximum number of shrinking iterations to prevent infinite loops.")

(defun shrink-counterexample (property inputs generators)
  "Shrink INPUTS (alist of (var . value)) to find minimal failing case.
GENERATORS is alist of (var . generator).
Returns shrunk inputs alist."
  (let ((current-inputs inputs)
        (iterations 0))
    (loop while (< iterations *max-shrink-iterations*)
          do (let ((improved nil))
               ;; Try shrinking each variable in turn
               (dolist (binding current-inputs)
                 (let* ((var (car binding))
                        (value (cdr binding))
                        (gen (cdr (assoc var generators)))
                        (shrink-candidates (shrink-value value gen)))
                   (dolist (candidate shrink-candidates)
                     (let ((test-inputs (substitute-binding current-inputs var candidate)))
                       (when (property-fails-with-inputs-p property test-inputs)
                         (setf current-inputs test-inputs
                               improved t)
                         (return))))))
               (unless improved
                 (return))
               (incf iterations)))
    current-inputs))

(defun substitute-binding (inputs var new-value)
  "Return copy of INPUTS alist with VAR's value replaced by NEW-VALUE."
  (mapcar (lambda (binding)
            (if (eq (car binding) var)
                (cons var new-value)
                binding))
          inputs))

(defun property-fails-with-inputs-p (property inputs)
  "Test if PROPERTY fails with given INPUTS alist.
Returns T if property fails, NIL if passes or precondition not met."
  (let ((precond (property-precondition property)))
    ;; Check precondition first
    (when (and precond (not (apply-with-bindings precond inputs)))
      (return-from property-fails-with-inputs-p nil))
    ;; Check if property fails
    (handler-case
        (not (apply-with-bindings (property-holds-predicate property) inputs))
      (error () t))))  ; Errors count as failures

(defun apply-with-bindings (fn inputs)
  "Call FN with variables bound from INPUTS alist."
  (let ((values (mapcar #'cdr inputs)))
    (apply fn values)))
