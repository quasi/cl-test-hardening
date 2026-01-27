;;;; Main DSL macros for th.agent
;;;; Provides define-agent-verification and verify-agent-work

(in-package #:th.agent)

;;;; Policy Definition Macro

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun quote-clause-values (clause)
    "Quote list values in a clause that should be data, not code."
    (loop for (key val) on clause by #'cddr
          collect key
          collect (if (and (listp val) (not (keywordp (car val))))
                      `(quote ,val)
                      val))))

(defmacro define-agent-verification (name &body clauses)
  "Define an agent verification policy.

   Example:
   (define-agent-verification code-review-policy
     :agent-type :code-generation

     (scope
       :allowed-paths (\"src/**/*.lisp\" \"tests/**/*.lisp\")
       :forbidden-paths (\"vendor/**\" \".git/**\")
       :max-files-changed 10)

     (hallucination
       :check-functions t
       :check-imports t
       :external-whitelist (\"alexandria\" \"cl-ppcre\"))

     (style
       :naming-conventions (:functions :kebab-case)
       :max-line-length 100
       :documentation-required (defun defgeneric))

     (semantic
       :no-removed-exports t
       :no-changed-signatures-without-flag t
       :existing-tests-pass t)

     (complexity
       :max-function-length 50
       :max-cyclomatic-complexity 10
       :max-nesting-depth 4))"
  (let ((agent-type :code-generation)
        (scope-clause nil)
        (hallucination-clause nil)
        (style-clause nil)
        (semantic-clause nil)
        (complexity-clause nil))

    ;; Parse clauses - handle both keyword args and dimension forms
    ;; Note: Use string= for symbols to handle cross-package forms
    (loop with remaining = clauses
          while remaining
          do (let ((item (pop remaining)))
               (cond
                 ;; :agent-type keyword followed by value
                 ((eq item :agent-type)
                  (setf agent-type (pop remaining)))
                 ;; Dimension clause lists (compare symbol names for package independence)
                 ((and (listp item) (string= (symbol-name (car item)) "SCOPE"))
                  (setf scope-clause (cdr item)))
                 ((and (listp item) (string= (symbol-name (car item)) "HALLUCINATION"))
                  (setf hallucination-clause (cdr item)))
                 ((and (listp item) (string= (symbol-name (car item)) "STYLE"))
                  (setf style-clause (cdr item)))
                 ((and (listp item) (string= (symbol-name (car item)) "SEMANTIC"))
                  (setf semantic-clause (cdr item)))
                 ((and (listp item) (string= (symbol-name (car item)) "COMPLEXITY"))
                  (setf complexity-clause (cdr item))))))

    `(progn
       (register-policy
        (%make-policy
         :name ',name
         :agent-type ,agent-type
         :scope-rules ,(when scope-clause
                         `(make-scope-rules ,@(quote-clause-values scope-clause)))
         :hallucination-rules ,(when hallucination-clause
                                 `(make-hallucination-rules ,@(quote-clause-values hallucination-clause)))
         :style-rules ,(when style-clause
                         `(make-style-rules ,@(quote-clause-values style-clause)))
         :semantic-rules ,(when semantic-clause
                            `(make-semantic-rules ,@(quote-clause-values semantic-clause)))
         :complexity-rules ,(when complexity-clause
                              `(make-complexity-rules ,@(quote-clause-values complexity-clause)))))
       ',name)))

;;;; Main Verification Function

(defun verify-agent-work (policy-name &key
                                        changed-files
                                        existing-files
                                        before-files
                                        after-files
                                        task-description
                                        test-command
                                        git-diff)
  "Run verification against agent-generated changes.

   Arguments:
   - POLICY-NAME: Symbol naming a registered policy
   - CHANGED-FILES: List of file paths that were modified
   - EXISTING-FILES: List of existing project files (for style extraction)
   - BEFORE-FILES: Files before changes (for semantic comparison)
   - AFTER-FILES: Files after changes (defaults to CHANGED-FILES)
   - TASK-DESCRIPTION: Description of what agent was asked to do
   - TEST-COMMAND: Command to run existing tests
   - GIT-DIFF: Git diff output (for scope verification)

   Returns an agent-verification-result struct."
  (let* ((policy (get-policy policy-name))
         (start-time (get-internal-real-time))
         (files-to-check (or changed-files after-files))
         (after (or after-files changed-files))
         (before (or before-files '())))

    (unless policy
      (error "Unknown policy: ~A" policy-name))

    ;; Run all dimension verifications
    (let ((scope-result (run-scope-verification policy files-to-check git-diff))
          (hallucination-result (run-hallucination-verification policy files-to-check))
          (style-result (run-style-verification policy files-to-check
                                                (or existing-files '())))
          (semantic-result (run-semantic-verification policy before after
                                                      :test-command test-command))
          (complexity-result (run-complexity-verification policy files-to-check)))

      ;; Combine results
      (let* ((all-violations (append
                              (when scope-result
                                (dimension-result-violations scope-result))
                              (when hallucination-result
                                (dimension-result-violations hallucination-result))
                              (when style-result
                                (dimension-result-violations style-result))
                              (when semantic-result
                                (dimension-result-violations semantic-result))
                              (when complexity-result
                                (dimension-result-violations complexity-result))))
             (errors (remove-if-not (lambda (v) (eq (violation-severity v) :error))
                                    all-violations))
             (warnings (remove-if-not (lambda (v) (eq (violation-severity v) :warning))
                                      all-violations))
             (end-time (get-internal-real-time))
             (duration-ms (round (* 1000 (/ (- end-time start-time)
                                            internal-time-units-per-second)))))

        (make-agent-verification-result
         :policy-name policy-name
         :task-description task-description
         :status (cond (errors :failed)
                       (warnings :warning)
                       (t :passed))
         :violations errors
         :warnings warnings
         :scope-result scope-result
         :hallucination-result hallucination-result
         :style-result style-result
         :semantic-result semantic-result
         :complexity-result complexity-result
         :duration-ms duration-ms)))))

;;;; Quick Verification Function

(defun quick-verify (policy-name files &key task)
  "Quick verification of files against a policy.
   Returns T if passed, NIL if failed."
  (let ((result (verify-agent-work policy-name
                                   :changed-files files
                                   :task-description task)))
    (verification-passed-p result)))

;;;; Verification with Report

(defun verify-and-report (policy-name &rest args
                          &key changed-files existing-files before-files
                               after-files task-description test-command git-diff
                               (stream *standard-output*))
  "Run verification and print a formatted report."
  (declare (ignore changed-files existing-files before-files after-files
                   task-description test-command git-diff))
  (let ((result (apply #'verify-agent-work policy-name
                       (remove-from-plist args :stream))))
    (format-verification-report result stream)
    result))

;;;; Git Integration

(defun verify-git-changes (policy-name &key
                                         (base-ref "HEAD~1")
                                         task-description
                                         test-command)
  "Verify changes between git refs.
   Automatically extracts changed files and git diff."
  (let* ((diff-output (uiop:run-program
                       (format nil "git diff ~A" base-ref)
                       :output :string
                       :ignore-error-status t))
         (changed-files (uiop:run-program
                         (format nil "git diff --name-only ~A" base-ref)
                         :output :string
                         :ignore-error-status t))
         (file-list (remove-if #'(lambda (s) (string= s ""))
                               (uiop:split-string changed-files :separator '(#\Newline)))))
    (verify-agent-work policy-name
                       :changed-files file-list
                       :git-diff diff-output
                       :task-description task-description
                       :test-command test-command)))

;;;; Predefined Policies

(defun define-standard-policies ()
  "Define commonly used verification policies."

  ;; Strict code generation policy
  (define-agent-verification strict-code-generation
    :agent-type :code-generation

    (scope
     :max-files-changed 5
     :forbidden-paths ("*.md" "*.txt" "docs/**"))

    (hallucination
     :check-functions t
     :check-imports t)

    (style
     :matches-existing-patterns t
     :max-line-length 100
     :documentation-required (defun defgeneric))

    (semantic
     :no-removed-exports t
     :no-changed-signatures-without-flag t
     :existing-tests-pass t)

    (complexity
     :max-function-length 50
     :max-cyclomatic-complexity 10
     :max-nesting-depth 4))

  ;; Lenient refactoring policy
  (define-agent-verification lenient-refactoring
    :agent-type :refactoring

    (scope
     :max-files-changed 20)

    (hallucination
     :check-functions t
     :check-imports t)

    (semantic
     :no-removed-exports t
     :existing-tests-pass t)

    (complexity
     :max-function-length 75
     :max-cyclomatic-complexity 15))

  ;; Documentation-only policy
  (define-agent-verification documentation-only
    :agent-type :documentation

    (scope
     :allowed-paths ("*.md" "docs/**" "README*")
     :forbidden-paths ("src/**/*.lisp" "tests/**")))

  t)
