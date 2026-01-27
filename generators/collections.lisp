;;; ABOUTME: Collection generators
;;; Provides generators for lists, vectors, and hash-tables

(in-package #:th.gen)

;;;; List Generator

(defclass list-generator (generator)
  ((element-generator :initarg :element :reader gen-element)
   (min-length :initarg :min-length :reader gen-min-length :initform 0)
   (max-length :initarg :max-length :reader gen-max-length :initform nil))
  (:documentation "Generator for lists with generated elements."))

(defun lists (element-generator &key (min-length 0) max-length)
  "Create a list generator.
ELEMENT-GENERATOR: Generator for list elements.
MIN-LENGTH: Minimum list length.
MAX-LENGTH: Maximum list length (default: size-dependent)."
  (make-instance 'list-generator
                 :name 'lists
                 :element element-generator
                 :min-length min-length
                 :max-length max-length))

(defmethod generate ((gen list-generator) random-state size)
  (let* ((max-len (or (gen-max-length gen) size))
         (min-len (gen-min-length gen))
         (length (th.property::next-random-integer min-len max-len random-state))
         (elem-gen (gen-element gen))
         ;; Reduce size for elements to prevent explosion
         (elem-size (max 1 (floor size 2))))
    (loop repeat length
          collect (generate elem-gen random-state elem-size))))

(defmethod shrink ((gen list-generator) value)
  (th.property::shrink-list value
                             (lambda (elem)
                               (shrink (gen-element gen) elem))))

;;;; Vector Generator

(defclass vector-generator (generator)
  ((element-generator :initarg :element :reader gen-element)
   (min-length :initarg :min-length :reader gen-min-length :initform 0)
   (max-length :initarg :max-length :reader gen-max-length :initform nil))
  (:documentation "Generator for vectors with generated elements."))

(defun vectors (element-generator &key (min-length 0) max-length)
  "Create a vector generator.
ELEMENT-GENERATOR: Generator for vector elements.
MIN-LENGTH: Minimum vector length.
MAX-LENGTH: Maximum vector length (default: size-dependent)."
  (make-instance 'vector-generator
                 :name 'vectors
                 :element element-generator
                 :min-length min-length
                 :max-length max-length))

(defmethod generate ((gen vector-generator) random-state size)
  (let* ((max-len (or (gen-max-length gen) size))
         (min-len (gen-min-length gen))
         (length (th.property::next-random-integer min-len max-len random-state))
         (elem-gen (gen-element gen))
         (elem-size (max 1 (floor size 2)))
         (vec (make-array length)))
    (dotimes (i length vec)
      (setf (aref vec i) (generate elem-gen random-state elem-size)))))

(defmethod shrink ((gen vector-generator) value)
  (th.property::shrink-vector value
                               (lambda (elem)
                                 (shrink (gen-element gen) elem))))

;;;; Hash-Table Generator

(defclass hash-table-generator (generator)
  ((key-generator :initarg :key :reader gen-key)
   (value-generator :initarg :value :reader gen-value)
   (min-size :initarg :min-size :reader gen-min-size :initform 0)
   (max-size :initarg :max-size :reader gen-max-size :initform nil)
   (test :initarg :test :reader gen-test :initform 'equal))
  (:documentation "Generator for hash-tables with generated keys and values."))

(defun hash-tables (key-generator value-generator &key (min-size 0) max-size (test 'equal))
  "Create a hash-table generator.
KEY-GENERATOR: Generator for hash-table keys.
VALUE-GENERATOR: Generator for hash-table values.
MIN-SIZE: Minimum number of entries.
MAX-SIZE: Maximum number of entries (default: size-dependent).
TEST: Hash-table test function."
  (make-instance 'hash-table-generator
                 :name 'hash-tables
                 :key key-generator
                 :value value-generator
                 :min-size min-size
                 :max-size max-size
                 :test test))

(defmethod generate ((gen hash-table-generator) random-state size)
  (let* ((max-sz (or (gen-max-size gen) (floor size 2)))
         (min-sz (gen-min-size gen))
         (num-entries (th.property::next-random-integer min-sz max-sz random-state))
         (key-gen (gen-key gen))
         (val-gen (gen-value gen))
         (elem-size (max 1 (floor size 3)))
         (ht (make-hash-table :test (gen-test gen))))
    (dotimes (i num-entries ht)
      (let ((key (generate key-gen random-state elem-size))
            (value (generate val-gen random-state elem-size)))
        (setf (gethash key ht) value)))))

(defmethod shrink ((gen hash-table-generator) value)
  "Shrink hash-table by removing entries."
  (when (plusp (hash-table-count value))
    (let ((shrinks '()))
      ;; Try empty hash-table
      (push (make-hash-table :test (hash-table-test value)) shrinks)
      ;; Try removing each key
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (let ((smaller (make-hash-table :test (hash-table-test value))))
                   (maphash (lambda (k2 v2)
                              (unless (funcall (hash-table-test value) k k2)
                                (setf (gethash k2 smaller) v2)))
                            value)
                   (push smaller shrinks)))
               value)
      (nreverse shrinks))))
