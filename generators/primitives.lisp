;;; ABOUTME: Primitive value generators
;;; Provides generators for integers, floats, booleans, characters, strings, symbols

(in-package #:th.gen)

;;;; Integer Generator

(defclass integer-generator (generator)
  ((min :initarg :min :reader gen-min :initform nil)
   (max :initarg :max :reader gen-max :initform nil))
  (:documentation "Generator for integers in a specified range."))

(defun integers (&key min max)
  "Create an integer generator.
MIN: Minimum value (default: size-dependent negative).
MAX: Maximum value (default: size-dependent positive)."
  (make-instance 'integer-generator :name 'integers :min min :max max))

(defmethod generate ((gen integer-generator) random-state size)
  (let ((min (or (gen-min gen) (- size)))
        (max (or (gen-max gen) size)))
    (th.property::next-random-integer min max random-state)))

(defmethod shrink ((gen integer-generator) value)
  (th.property::shrink-integer value))

;;;; Natural Number Generator

(defclass natural-generator (generator)
  ((max :initarg :max :reader gen-max :initform nil))
  (:documentation "Generator for non-negative integers."))

(defun naturals (&key max)
  "Create a natural number (non-negative integer) generator."
  (make-instance 'natural-generator :name 'naturals :max max))

(defmethod generate ((gen natural-generator) random-state size)
  (let ((max (or (gen-max gen) size)))
    (th.property::next-random-integer 0 max random-state)))

(defmethod shrink ((gen natural-generator) value)
  (remove-if #'minusp (th.property::shrink-integer value)))

;;;; Float Generator

(defclass float-generator (generator)
  ((min :initarg :min :reader gen-min :initform 0.0)
   (max :initarg :max :reader gen-max :initform 1.0))
  (:documentation "Generator for floating-point numbers."))

(defun floats (&key (min 0.0) (max 1.0))
  "Create a float generator in range [MIN, MAX)."
  (make-instance 'float-generator :name 'floats :min min :max max))

(defmethod generate ((gen float-generator) random-state size)
  (declare (ignore size))
  (th.property::next-random-float (gen-min gen) (gen-max gen) random-state))

(defmethod shrink ((gen float-generator) value)
  (th.property::shrink-float value))

;;;; Boolean Generator

(defclass boolean-generator (generator)
  ()
  (:documentation "Generator for boolean values."))

(defun booleans ()
  "Create a boolean generator."
  (make-instance 'boolean-generator :name 'booleans))

(defmethod generate ((gen boolean-generator) random-state size)
  (declare (ignore size))
  (th.property::next-random-boolean random-state))

(defmethod shrink ((gen boolean-generator) value)
  ;; Shrink true to false
  (if value '(nil) '()))

;;;; Character Generator

(defclass character-generator (generator)
  ((alphabet :initarg :alphabet :reader gen-alphabet
             :initform "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (:documentation "Generator for characters from an alphabet."))

(defun characters (&key alphabet)
  "Create a character generator.
ALPHABET: String of characters to choose from."
  (make-instance 'character-generator
                 :name 'characters
                 :alphabet (or alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))

(defmethod generate ((gen character-generator) random-state size)
  (declare (ignore size))
  (let ((alphabet (gen-alphabet gen)))
    (char alphabet (random (length alphabet) random-state))))

(defmethod shrink ((gen character-generator) value)
  ;; Shrink toward 'a'
  (if (char= value #\a) '() (list #\a)))

;;;; String Generator

(defclass string-generator (generator)
  ((min-length :initarg :min-length :reader gen-min-length :initform 0)
   (max-length :initarg :max-length :reader gen-max-length :initform nil)
   (alphabet :initarg :alphabet :reader gen-alphabet
             :initform "abcdefghijklmnopqrstuvwxyz"))
  (:documentation "Generator for strings."))

(defun strings (&key (min-length 0) max-length alphabet)
  "Create a string generator.
MIN-LENGTH: Minimum string length.
MAX-LENGTH: Maximum string length (default: size-dependent).
ALPHABET: String of characters to use."
  (make-instance 'string-generator
                 :name 'strings
                 :min-length min-length
                 :max-length max-length
                 :alphabet (or alphabet "abcdefghijklmnopqrstuvwxyz")))

(defmethod generate ((gen string-generator) random-state size)
  (let* ((max-len (or (gen-max-length gen) size))
         (min-len (gen-min-length gen))
         (length (th.property::next-random-integer min-len max-len random-state))
         (alphabet (gen-alphabet gen)))
    (with-output-to-string (s)
      (dotimes (i length)
        (write-char (char alphabet (random (length alphabet) random-state)) s)))))

(defmethod shrink ((gen string-generator) value)
  (th.property::shrink-string value))

;;;; Symbol Generator

(defclass symbol-generator (generator)
  ((package :initarg :package :reader gen-package :initform *package*))
  (:documentation "Generator for symbols."))

(defun symbols (&key package)
  "Create a symbol generator.
PACKAGE: Package for generated symbols (default: *package*)."
  (make-instance 'symbol-generator :name 'symbols :package package))

(defmethod generate ((gen symbol-generator) random-state size)
  (let ((name (generate (strings :min-length 1 :max-length (max 1 (floor size 3)))
                        random-state
                        size)))
    (intern (string-upcase name) (gen-package gen))))

(defmethod shrink ((gen symbol-generator) value)
  ;; Shrink symbol name
  (let ((name (symbol-name value)))
    (mapcar (lambda (s) (intern s (symbol-package value)))
            (th.property::shrink-string name))))

;;;; Keyword Generator

(defclass keyword-generator (generator)
  ()
  (:documentation "Generator for keywords."))

(defun keywords ()
  "Create a keyword generator."
  (make-instance 'keyword-generator :name 'keywords))

(defmethod generate ((gen keyword-generator) random-state size)
  (let ((name (generate (strings :min-length 1 :max-length (max 1 (floor size 3)))
                        random-state
                        size)))
    (intern (string-upcase name) :keyword)))

(defmethod shrink ((gen keyword-generator) value)
  (let ((name (symbol-name value)))
    (mapcar (lambda (s) (intern s :keyword))
            (th.property::shrink-string name))))
