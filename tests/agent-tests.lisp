;;;; Tests for th.agent verification system
;;;; Tests policy definition, verification dimensions, and reporting

(defpackage #:th.agent-tests
  (:use #:cl #:th.agent #:fiveam)
  (:export #:run-tests))

(in-package #:th.agent-tests)

;;;; Test Suite Definition

(def-suite th.agent-tests
  :description "Tests for Agent Verification DSL")

(in-suite th.agent-tests)

;;;; Policy Definition Tests

(test policy-definition
  "Test that policies can be defined and registered"
  (define-agent-verification test-policy
    :agent-type :code-generation

    (scope
     :allowed-paths ("src/**/*.lisp")
     :forbidden-paths ("vendor/**")
     :max-files-changed 5)

    (hallucination
     :check-functions t
     :check-imports t)

    (complexity
     :max-function-length 50
     :max-cyclomatic-complexity 10))

  (let ((policy (get-policy 'test-policy)))
    (is (not (null policy)))
    (is (eq (verification-policy-name policy) 'test-policy))
    (is (eq (verification-policy-agent-type policy) :code-generation))
    (is (not (null (verification-policy-scope-rules policy))))
    (is (not (null (verification-policy-hallucination-rules policy))))
    (is (not (null (verification-policy-complexity-rules policy))))))

(test minimal-policy
  "Test minimal policy with no rules"
  (define-agent-verification minimal-policy
    :agent-type :refactoring)

  (let ((policy (get-policy 'minimal-policy)))
    (is (not (null policy)))
    (is (eq (verification-policy-agent-type policy) :refactoring))
    (is (null (verification-policy-scope-rules policy)))
    (is (null (verification-policy-hallucination-rules policy)))))

;;;; Scope Verification Tests

(test path-matching
  "Test path pattern matching"
  (is (path-matches-pattern "src/foo.lisp" "src/*.lisp"))
  (is (path-matches-pattern "src/sub/foo.lisp" "src/**/*.lisp"))
  (is (not (path-matches-pattern "vendor/foo.lisp" "src/**/*.lisp")))
  (is (path-matches-pattern "test.md" "*.md"))
  (is (path-matches-pattern "docs/guide.md" "docs/**")))

(test scope-verification-pass
  "Test scope verification with allowed files"
  (define-agent-verification scope-test-pass
    (scope
     :allowed-paths ("src/**/*.lisp")
     :max-files-changed 10))

  (let* ((policy (get-policy 'scope-test-pass))
         (result (run-scope-verification policy
                                         '("src/foo.lisp" "src/bar.lisp")
                                         nil)))
    (is (eq (dimension-result-status result) :passed))))

(test scope-verification-fail
  "Test scope verification with forbidden files"
  (define-agent-verification scope-test-fail
    (scope
     :forbidden-paths ("vendor/**")
     :max-files-changed 2))

  (let* ((policy (get-policy 'scope-test-fail))
         (result (run-scope-verification policy
                                         '("src/foo.lisp" "vendor/bad.lisp")
                                         nil)))
    (is (eq (dimension-result-status result) :failed))
    (is (> (length (dimension-result-violations result)) 0))))

(test scope-too-many-files
  "Test scope verification with too many files"
  (define-agent-verification scope-max-files
    (scope :max-files-changed 2))

  (let* ((policy (get-policy 'scope-max-files))
         (result (run-scope-verification policy
                                         '("a.lisp" "b.lisp" "c.lisp" "d.lisp")
                                         nil)))
    (is (member (dimension-result-status result) '(:failed :warning)))))

;;;; Complexity Analysis Tests

(test count-lines
  "Test line counting for forms"
  (is (= (count-lines nil) 0))
  (is (= (count-lines 'x) 1))
  (is (> (count-lines '(defun foo (x) (+ x 1))) 1)))

(test measure-nesting
  "Test nesting depth measurement"
  (is (= (measure-nesting-depth 'x) 0))
  (is (= (measure-nesting-depth '(if t 1 2)) 1))
  (is (= (measure-nesting-depth '(if t (if t 1 2) 3)) 2))
  (is (= (measure-nesting-depth '(when t (let ((x 1)) (if x 2 3)))) 3)))

(test cyclomatic-complexity
  "Test cyclomatic complexity calculation"
  ;; Simple form: CC = 1
  (is (= (cyclomatic-complexity '(+ 1 2)) 1))
  ;; One if: CC = 2
  (is (= (cyclomatic-complexity '(if t 1 2)) 2))
  ;; Nested ifs: CC = 3
  (is (= (cyclomatic-complexity '(if t (if x 1 2) 3)) 3))
  ;; Cond with 3 clauses: CC = 3
  (is (= (cyclomatic-complexity '(cond (a 1) (b 2) (t 3))) 3)))

;;;; Style Detection Tests

(test naming-convention-detection
  "Test detection of naming conventions"
  (is (eq (detect-naming-convention "foo-bar") :kebab-case))
  (is (eq (detect-naming-convention "FOO-BAR") :screaming-kebab))
  (is (eq (detect-naming-convention "FooBar") :pascal-case))
  (is (eq (detect-naming-convention "fooBar") :camel-case))
  (is (eq (detect-naming-convention "foo_bar") :snake-case))
  (is (eq (detect-naming-convention "*foo*") :stars))
  (is (eq (detect-naming-convention "+foo+") :earmuffs)))

(test docstring-detection
  "Test docstring presence detection"
  (is (has-docstring-p '(defun foo (x) "Doc" x)))
  (is (not (has-docstring-p '(defun foo (x) x))))
  (is (has-docstring-p '(defmacro bar (x) "Doc" `(,x))))
  (is (not (has-docstring-p '(defmacro bar (x) `(,x))))))

;;;; Hallucination Detection Tests

(test cl-symbol-detection
  "Test detection of CL standard symbols"
  (ensure-cl-symbols)
  (is (cl-symbol-p "CAR"))
  (is (cl-symbol-p "car"))
  (is (cl-symbol-p "DEFUN"))
  (is (not (cl-symbol-p "MY-CUSTOM-FUNCTION"))))

;;;; Semantic Verification Tests

(test lambda-list-info
  "Test lambda list parsing"
  (let ((info (lambda-list-info '(a b &optional c &key d))))
    (is (equal (getf info :required) '(a b)))
    (is (equal (getf info :optional) '(c)))
    (is (equal (getf info :keyword) '(d)))))

(test lambda-list-arity
  "Test arity calculation"
  (let ((arity1 (lambda-list-arity '(a b)))
        (arity2 (lambda-list-arity '(a &optional b c)))
        (arity3 (lambda-list-arity '(a &rest args))))
    (is (= (getf arity1 :min) 2))
    (is (= (getf arity1 :max) 2))
    (is (= (getf arity2 :min) 1))
    (is (= (getf arity2 :max) 3))
    (is (= (getf arity3 :min) 1))
    (is (eq (getf arity3 :max) :unlimited))))

(test signature-compatibility
  "Test signature compatibility checking"
  (let ((old-sig (list :arity (list :min 2 :max 3)))
        (new-sig-compat (list :arity (list :min 1 :max 4)))
        (new-sig-incompat (list :arity (list :min 3 :max 3))))
    (is (signatures-compatible-p old-sig new-sig-compat))
    (is (not (signatures-compatible-p old-sig new-sig-incompat)))))

;;;; Verification Result Tests

(test verification-result-construction
  "Test verification result creation"
  (let ((result (make-agent-verification-result
                 :policy-name 'test
                 :status :passed
                 :violations nil
                 :warnings nil
                 :duration-ms 100)))
    (is (verification-passed-p result))
    (is (eq (agent-verification-result-status result) :passed))
    (is (= (agent-verification-result-duration-ms result) 100))))

;;;; Integration Tests

(test full-verification
  "Test full verification workflow"
  (define-agent-verification integration-test
    :agent-type :code-generation

    (complexity
     :max-function-length 100
     :max-cyclomatic-complexity 15))

  (let ((result (verify-agent-work 'integration-test
                                   :changed-files '()
                                   :task-description "Test task")))
    (is (not (null result)))
    (is (stringp (agent-verification-result-task-description result)))))

(test quick-verify-function
  "Test quick verification helper"
  (define-agent-verification quick-test
    :agent-type :code-generation)

  (is (quick-verify 'quick-test '() :task "Quick test")))

;;;; Reporting Tests

(test json-conversion
  "Test verification result to JSON conversion"
  (let* ((result (make-agent-verification-result
                  :policy-name 'test-policy
                  :status :passed
                  :violations nil
                  :warnings nil
                  :duration-ms 50))
         (json (verification-to-json result)))
    (is (hash-table-p json))
    (is (string= (gethash "status" json) "passed"))
    (is (gethash "passed" json))
    (is (= (gethash "durationMs" json) 50))))

(test violation-json
  "Test violation to JSON conversion"
  (let* ((v (make-violation :type :test-error
                            :severity :error
                            :dimension :scope
                            :message "Test message"
                            :file "test.lisp"
                            :line 42))
         (json (violation-to-json v)))
    (is (string= (gethash "type" json) "test-error"))
    (is (string= (gethash "severity" json) "error"))
    (is (string= (gethash "file" json) "test.lisp"))
    (is (= (gethash "line" json) 42))))

;;;; Test Runner

(defun run-tests ()
  "Run all th.agent tests"
  (run! 'th.agent-tests))
