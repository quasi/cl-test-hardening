;;;; Package definitions for agent verification module
;;;; Verifies agent-generated code changes against defined policies

(defpackage #:th.agent
  (:use #:cl #:alexandria #:th.core)
  (:export
   ;; Policy definition
   #:define-agent-verification
   #:define-standard-policies

   ;; Running verification
   #:verify-agent-work
   #:quick-verify
   #:verify-and-report
   #:verify-git-changes

   ;; Dimension verifications
   #:run-scope-verification
   #:run-hallucination-verification
   #:run-style-verification
   #:run-semantic-verification
   #:run-complexity-verification

   ;; Policy structures
   #:verification-policy
   #:verification-policy-name
   #:verification-policy-agent-type
   #:verification-policy-scope-rules
   #:verification-policy-hallucination-rules
   #:verification-policy-style-rules
   #:verification-policy-semantic-rules
   #:verification-policy-complexity-rules

   ;; Rule structures
   #:make-scope-rules
   #:make-hallucination-rules
   #:make-style-rules
   #:make-semantic-rules
   #:make-complexity-rules

   ;; Violations
   #:make-violation
   #:violation
   #:violation-type
   #:violation-severity
   #:violation-dimension
   #:violation-file
   #:violation-line
   #:violation-message
   #:violation-suggestion

   ;; Results
   #:agent-verification-result
   #:make-agent-verification-result
   #:agent-verification-result-policy-name
   #:agent-verification-result-task-description
   #:agent-verification-result-status
   #:agent-verification-result-violations
   #:agent-verification-result-warnings
   #:agent-verification-result-duration-ms
   #:verification-passed-p
   #:format-verification-report
   #:verification-to-json
   #:violation-to-json

   ;; Dimension results
   #:dimension-result
   #:make-dimension-result
   #:dimension-result-name
   #:dimension-result-status
   #:dimension-result-violations

   ;; Registry
   #:find-policy
   #:get-policy
   #:list-policies
   #:register-policy
   #:clear-policies

   ;; Complexity analysis
   #:count-lines
   #:measure-nesting-depth
   #:cyclomatic-complexity

   ;; Style analysis
   #:detect-naming-convention
   #:has-docstring-p
   #:extract-project-style

   ;; Hallucination detection
   #:ensure-cl-symbols
   #:cl-symbol-p

   ;; Semantic preservation
   #:lambda-list-info
   #:lambda-list-arity
   #:signatures-compatible-p

   ;; Utilities
   #:path-matches-pattern))
