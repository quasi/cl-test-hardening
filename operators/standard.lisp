;;; ABOUTME: Standard mutation operators
;;; Provides arithmetic, conditional, logical, and boundary mutation operators

(in-package #:th.mutation)

;;;; Arithmetic Operators

(define-mutation-operators standard-arithmetic
  (mutate-pattern add-to-sub
    :pattern (+ ?x ?y)
    :mutations ((- ?x ?y))
    :description "Replace addition with subtraction")

  (mutate-pattern sub-to-add
    :pattern (- ?x ?y)
    :mutations ((+ ?x ?y))
    :description "Replace subtraction with addition")

  (mutate-pattern mul-to-div
    :pattern (* ?x ?y)
    :mutations ((/ ?x ?y))
    :description "Replace multiplication with division")

  (mutate-pattern div-to-mul
    :pattern (/ ?x ?y)
    :mutations ((* ?x ?y))
    :description "Replace division with multiplication")

  (mutate-pattern incf-to-decf
    :pattern (incf ?x)
    :mutations ((decf ?x))
    :description "Replace increment with decrement")

  (mutate-pattern decf-to-incf
    :pattern (decf ?x)
    :mutations ((incf ?x))
    :description "Replace decrement with increment")

  (mutate-pattern 1+-to-1-
    :pattern (1+ ?x)
    :mutations ((1- ?x))
    :description "Replace 1+ with 1-")

  (mutate-pattern 1--to-1+
    :pattern (1- ?x)
    :mutations ((1+ ?x))
    :description "Replace 1- with 1+"))

;;;; Conditional Operators

(define-mutation-operators standard-conditional
  (mutate-pattern less-than
    :pattern (< ?x ?y)
    :mutations ((<= ?x ?y) (> ?x ?y) (>= ?x ?y) (= ?x ?y))
    :description "Mutate < comparison")

  (mutate-pattern less-equal
    :pattern (<= ?x ?y)
    :mutations ((< ?x ?y) (> ?x ?y) (>= ?x ?y) (= ?x ?y))
    :description "Mutate <= comparison")

  (mutate-pattern greater-than
    :pattern (> ?x ?y)
    :mutations ((>= ?x ?y) (< ?x ?y) (<= ?x ?y) (= ?x ?y))
    :description "Mutate > comparison")

  (mutate-pattern greater-equal
    :pattern (>= ?x ?y)
    :mutations ((> ?x ?y) (< ?x ?y) (<= ?x ?y) (= ?x ?y))
    :description "Mutate >= comparison")

  (mutate-pattern equal-to-notequal
    :pattern (= ?x ?y)
    :mutations ((/= ?x ?y))
    :description "Replace = with /=")

  (mutate-pattern notequal-to-equal
    :pattern (/= ?x ?y)
    :mutations ((= ?x ?y))
    :description "Replace /= with =")

  (mutate-pattern eq-to-not-eq
    :pattern (eq ?x ?y)
    :mutations ((not (eq ?x ?y)))
    :description "Negate eq")

  (mutate-pattern equal-to-not-equal
    :pattern (equal ?x ?y)
    :mutations ((not (equal ?x ?y)))
    :description "Negate equal")

  (mutate-pattern eql-to-not-eql
    :pattern (eql ?x ?y)
    :mutations ((not (eql ?x ?y)))
    :description "Negate eql"))

;;;; Logical Operators

(define-mutation-operators standard-logical
  (mutate-pattern and-to-or
    :pattern (and ?x ?y)
    :mutations ((or ?x ?y))
    :description "Replace AND with OR")

  (mutate-pattern or-to-and
    :pattern (or ?x ?y)
    :mutations ((and ?x ?y))
    :description "Replace OR with AND")

  (mutate-pattern not-removal
    :pattern (not ?x)
    :mutations (?x)
    :description "Remove NOT")

  (mutate-pattern add-not
    :pattern (if ?cond ?then ?else)
    :mutations ((if (not ?cond) ?then ?else))
    :description "Negate if condition")

  (mutate-pattern when-to-unless
    :pattern (when ?cond ?body)
    :mutations ((unless ?cond ?body))
    :description "Replace WHEN with UNLESS")

  (mutate-pattern unless-to-when
    :pattern (unless ?cond ?body)
    :mutations ((when ?cond ?body))
    :description "Replace UNLESS with WHEN"))

;;;; Boundary Operators

(define-mutation-operators standard-boundary
  (mutate-boundary off-by-one-less
    :pattern (< ?x ?n)
    :mutations ((<= ?x ?n) (< ?x (1- ?n)) (< ?x (1+ ?n)))
    :description "Off-by-one for < boundary")

  (mutate-boundary off-by-one-greater
    :pattern (> ?x ?n)
    :mutations ((>= ?x ?n) (> ?x (1- ?n)) (> ?x (1+ ?n)))
    :description "Off-by-one for > boundary")

  (mutate-boundary zerop-to-plusp
    :pattern (zerop ?x)
    :mutations ((plusp ?x) (minusp ?x))
    :description "Replace zerop with plusp/minusp")

  (mutate-boundary plusp-to-zerop
    :pattern (plusp ?x)
    :mutations ((zerop ?x) (minusp ?x))
    :description "Replace plusp with zerop/minusp")

  (mutate-boundary null-to-not-null
    :pattern (null ?x)
    :mutations ((not (null ?x)))
    :description "Negate null check")

  (mutate-boundary endp-mutation
    :pattern (endp ?x)
    :mutations ((not (endp ?x)))
    :description "Negate endp"))

;;;; Return Value Operators

(define-mutation-operators standard-return
  (mutate-pattern return-nil
    :pattern (return ?x)
    :mutations ((return nil) (return t))
    :description "Mutate return value")

  (mutate-pattern return-from-nil
    :pattern (return-from ?block ?x)
    :mutations ((return-from ?block nil) (return-from ?block t))
    :description "Mutate return-from value"))

;;;; Convenience Sets

(defvar *all-standard-operators*
  '(standard-arithmetic standard-conditional standard-logical standard-boundary standard-return)
  "List of all standard operator sets.")

(defun standard-operators ()
  "Return list of all standard mutation operators."
  (loop for set-name in *all-standard-operators*
        append (operator-set-operators (find-operator-set set-name))))
