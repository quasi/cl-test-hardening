;;; ABOUTME: Tests for th.mutation
;;; Verifies pattern matching, mutant generation, and mutation score calculation

(defpackage #:th.mutation-tests
  (:use #:cl #:fiveam #:th.mutation))

(in-package #:th.mutation-tests)

(def-suite th.mutation-tests
  :description "Tests for th.mutation"
  :in :th.tests)

(in-suite th.mutation-tests)

;;;; Pattern Matching Tests

(test pattern-variable-detection
  (is (th.mutation::pattern-variable-p '?x))
  (is (th.mutation::pattern-variable-p '?foo))
  (is (not (th.mutation::pattern-variable-p 'x)))
  (is (not (th.mutation::pattern-variable-p 'foo))))

(test simple-pattern-match
  (multiple-value-bind (match bindings)
      (pattern-matches-p '(+ ?x ?y) '(+ 1 2))
    (is match)
    (is (equal 1 (cdr (assoc 'x bindings))))
    (is (equal 2 (cdr (assoc 'y bindings))))))

(test nested-pattern-match
  (multiple-value-bind (match bindings)
      (pattern-matches-p '(if ?cond ?then ?else) '(if (> x 0) "pos" "neg"))
    (is match)
    (is (equal '(> x 0) (cdr (assoc 'cond bindings))))
    (is (equal "pos" (cdr (assoc 'then bindings))))
    (is (equal "neg" (cdr (assoc 'else bindings))))))

(test pattern-no-match
  (multiple-value-bind (match bindings)
      (pattern-matches-p '(+ ?x ?y) '(- 1 2))
    (declare (ignore bindings))
    (is (not match))))

;;;; Pattern Substitution Tests

(test pattern-substitution
  (let ((bindings '((x . 10) (y . 20))))
    (is (equal '(- 10 20)
               (th.mutation::substitute-pattern '(- ?x ?y) bindings)))))

;;;; Mutation Site Finding Tests

(test find-mutation-sites-simple
  (let* ((op (make-mutation-operator :name 'test-op
                                     :pattern '(+ ?x ?y)
                                     :mutations '((- ?x ?y))))
         (form '(+ 1 2))
         (sites (th.mutation::find-mutation-sites form (list op))))
    (is (= 1 (length sites)))
    (is (equal form (th.mutation::mutation-site-form (first sites))))))

(test find-mutation-sites-nested
  (let* ((op (make-mutation-operator :name 'test-op
                                     :pattern '(+ ?x ?y)
                                     :mutations '((- ?x ?y))))
         (form '(* (+ 1 2) (+ 3 4)))
         (sites (th.mutation::find-mutation-sites form (list op))))
    (is (= 2 (length sites)))))

;;;; Mutant Generation Tests

(test generate-mutants-simple
  (let* ((op (make-mutation-operator :name 'test-op
                                     :pattern '(+ ?x ?y)
                                     :mutations '((- ?x ?y) (* ?x ?y))))
         (form '(+ 1 2))
         (mutants (generate-mutants (list form) (list op))))
    (is (= 2 (length mutants)))
    (is (member '(- 1 2) mutants :key #'mutant-replacement :test #'equal))
    (is (member '(* 1 2) mutants :key #'mutant-replacement :test #'equal))))

;;;; Form Mutation Tests

(test mutate-form
  (let* ((op (make-mutation-operator :name 'test-op
                                     :pattern '(+ ?x ?y)
                                     :mutations '((- ?x ?y))))
         (mutations (mutate-form '(+ 1 2) (list op))))
    (is (= 1 (length mutations)))
    (is (equal '(- 1 2) (getf (first mutations) :mutated)))))

;;;; Mutation Score Calculation Tests

(test mutation-score-calculation
  (is (= 1.0 (calculate-mutation-score 10 10)))
  (is (= 0.5 (calculate-mutation-score 5 10)))
  (is (= 0.0 (calculate-mutation-score 0 10)))
  (is (= 1.0 (calculate-mutation-score 0 0))))  ; No mutants = perfect

;;;; Standard Operators Tests

(test standard-arithmetic-operators-loaded
  (is (find-operator-set 'standard-arithmetic))
  (let ((ops (th.mutation::operator-set-operators (find-operator-set 'standard-arithmetic))))
    (is (plusp (length ops)))))

(test standard-conditional-operators-loaded
  (is (find-operator-set 'standard-conditional))
  (let ((ops (th.mutation::operator-set-operators (find-operator-set 'standard-conditional))))
    (is (plusp (length ops)))))

;;;; Integration Test

(test mutation-testing-integration
  (let* ((form '(defun add-positive (x y)
                  (if (and (> x 0) (> y 0))
                      (+ x y)
                      0)))
         (test-fn (lambda (f)
                    (declare (ignore f))
                    t))  ; Weak test - passes everything
         (operators (standard-operators)))
    (multiple-value-bind (score passed total killed)
        (mutate-and-test (list form) test-fn operators)
      (declare (ignore passed killed))
      ;; With a weak test, many mutants should survive
      (is (numberp score))
      (is (plusp total)))))
