;;; ABOUTME: Tests for property checking
;;; Verifies property checking, shrinking integration, and reporting

(defpackage #:th.property-tests/properties
  (:use #:cl #:fiveam #:th.property)
  (:local-nicknames (#:gen #:th.gen)))

(in-package #:th.property-tests/properties)

(def-suite property-tests
  :description "Tests for th.property property checking"
  :in :th.property-tests)

(in-suite property-tests)

;;;; Setup - clear properties before tests

(defun clear-test-properties ()
  (clear-properties (find-package :th.property-tests/properties)))

;;;; Basic Property Definition

(test define-property-creates-property
  (clear-test-properties)
  (define-property test-property-1
    :for-all ((x (gen:integers :min 0 :max 100)))
    :holds (>= x 0))
  (is (find-property 'test-property-1))
  (is (eq 'test-property-1 (property-name (find-property 'test-property-1)))))

;;;; Passing Properties

(test passing-property-returns-success
  (clear-test-properties)
  (define-property reverse-involution
    :for-all ((lst (gen:lists (gen:integers))))
    :holds (equal lst (reverse (reverse lst))))
  (let ((result (check-property 'reverse-involution :iterations 50 :seed 12345)))
    (is (property-result-passed-p result))
    (is (= 50 (property-result-iterations result)))))

;;;; Failing Properties

(test failing-property-returns-counterexample
  (clear-test-properties)
  (define-property always-false
    :for-all ((x (gen:integers :min 1 :max 100)))
    :holds nil)
  (let ((result (check-property 'always-false :iterations 10 :seed 12345)))
    (is (not (property-result-passed-p result)))
    (is (property-result-counterexample result))))

(test failing-property-shrinks-counterexample
  (clear-test-properties)
  (define-property fails-over-ten
    :for-all ((x (gen:integers :min 0 :max 100)))
    :holds (< x 10))
  (let ((result (check-property 'fails-over-ten :iterations 100 :seed 12345)))
    (is (not (property-result-passed-p result)))
    ;; Shrunk counterexample should be minimal (10)
    (let ((shrunk-x (cdr (assoc 'x (property-result-shrunk-counterexample result)))))
      (is (= 10 shrunk-x)))))

;;;; Preconditions

(test precondition-filters-inputs
  (clear-test-properties)
  (define-property division-safe
    :for-all ((x (gen:integers :min 1 :max 100))
              (y (gen:integers :min 0 :max 100)))
    :when (not (zerop y))
    :holds (integerp (floor x y)))
  (let ((result (check-property 'division-safe :iterations 100 :seed 12345)))
    (is (property-result-passed-p result))))

;;;; Tags

(test property-tags-work
  (clear-test-properties)
  (define-property tagged-prop
    :for-all ((x (gen:integers)))
    :holds t
    :tags (:math :invariant))
  (is (equal '(:math :invariant) (property-tags (find-property 'tagged-prop))))
  (is (member (find-property 'tagged-prop)
              (list-properties :tags '(:math)))))

;;;; Classification

(test classification-collects-stats
  (clear-test-properties)
  (define-property classified-prop
    :for-all ((x (gen:integers :min 0 :max 100)))
    :holds t
    :classify (cond ((< x 25) :small)
                    ((< x 75) :medium)
                    (t :large)))
  (let ((result (check-property 'classified-prop :iterations 100 :seed 12345)))
    (is (property-result-passed-p result))
    (is (property-result-classifications result))))

;;;; Seed Reproducibility

(test same-seed-produces-same-result
  (clear-test-properties)
  (define-property deterministic-test
    :for-all ((x (gen:integers)))
    :holds (evenp x))  ; Will fail on odd numbers
  (let ((result1 (check-property 'deterministic-test :iterations 100 :seed 99999))
        (result2 (check-property 'deterministic-test :iterations 100 :seed 99999)))
    (is (equal (property-result-counterexample result1)
               (property-result-counterexample result2)))))

;;;; Integration Suite

(def-suite* all-th-property-tests
  :description "All th.property tests")

;; Import the other test suites
(in-suite all-th-property-tests)

(test run-generator-tests
  (is (fiveam:run! 'th.property-tests/generators::generator-tests)))

(test run-shrinking-tests
  (is (fiveam:run! 'th.property-tests/shrinking::shrinking-tests)))
