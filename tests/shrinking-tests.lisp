;;; ABOUTME: Tests for shrinking functionality
;;; Verifies shrinking converges to minimal counterexamples

(defpackage #:th.property-tests/shrinking
  (:use #:cl #:fiveam #:th.property))

(in-package #:th.property-tests/shrinking)

(def-suite shrinking-tests
  :description "Tests for th.property shrinking"
  :in :th.property-tests)

(in-suite shrinking-tests)

;;;; Integer Shrinking

(test integer-shrinks-toward-zero
  (let ((shrinks (th.property::shrink-integer 100)))
    (is (member 0 shrinks))
    (is (member 50 shrinks))
    (is (every (lambda (s) (< (abs s) (abs 100))) shrinks))))

(test negative-integer-shrinks-toward-zero
  (let ((shrinks (th.property::shrink-integer -100)))
    (is (member 0 shrinks))
    (is (member 100 shrinks))  ; Negation
    (is (member -50 shrinks))))

(test zero-has-no-shrinks
  (is (null (th.property::shrink-integer 0))))

;;;; String Shrinking

(test string-shrinks-toward-empty
  (let ((shrinks (th.property::shrink-string "hello")))
    (is (member "" shrinks :test #'string=))
    (is (member "ello" shrinks :test #'string=))
    (is (member "hllo" shrinks :test #'string=))))

(test empty-string-has-no-shrinks
  (is (null (th.property::shrink-string ""))))

;;;; List Shrinking

(test list-shrinks-toward-empty
  (let ((shrinks (th.property::shrink-list '(1 2 3) nil)))
    (is (member '() shrinks :test #'equal))
    (is (member '(2 3) shrinks :test #'equal))
    (is (member '(1 3) shrinks :test #'equal))
    (is (member '(1 2) shrinks :test #'equal))))

(test empty-list-has-no-shrinks
  (is (null (th.property::shrink-list '() nil))))

;;;; Shrinking with Element Shrinker

(test list-shrinking-uses-element-shrinker
  (let ((shrinks (th.property::shrink-list '(10 20)
                                             #'th.property::shrink-integer)))
    ;; Should include shrunk first element
    (is (some (lambda (s) (and (= 2 (length s))
                               (< (first s) 10)))
              shrinks))))
