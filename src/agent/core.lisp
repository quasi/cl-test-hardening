;;;; Core data structures for th.agent
;;;; Defines policies, rules, and registries

(in-package #:th.agent)

;;;; Verification Policy

(defstruct (verification-policy (:constructor %make-policy))
  "A verification policy for agent work."
  (name nil :type symbol :read-only t)
  (agent-type :code-generation :type keyword)
  (scope-rules nil)
  (hallucination-rules nil)
  (style-rules nil)
  (semantic-rules nil)
  (complexity-rules nil))

(defun make-verification-policy (&key name agent-type scope-rules hallucination-rules
                                   style-rules semantic-rules complexity-rules)
  "Create a verification policy."
  (check-type name symbol)
  (%make-policy :name name
                :agent-type (or agent-type :code-generation)
                :scope-rules scope-rules
                :hallucination-rules hallucination-rules
                :style-rules style-rules
                :semantic-rules semantic-rules
                :complexity-rules complexity-rules))

;;;; Rule Structures

(defstruct scope-rules
  "Rules for scope verification."
  (allowed-paths nil :type list)
  (forbidden-paths nil :type list)
  (allowed-operations '(:create :modify) :type list)
  (forbidden-operations nil :type list)
  (max-files-changed nil :type (or null integer)))

(defstruct hallucination-rules
  "Rules for hallucination detection."
  (check-imports t :type boolean)
  (check-functions t :type boolean)
  (check-classes t :type boolean)
  (external-whitelist nil :type list))

(defstruct style-rules
  "Rules for style conformance."
  (matches-existing-patterns t :type boolean)
  (naming-conventions nil :type list)  ; Plist of :functions :classes :constants
  (indentation-style :standard :type keyword)
  (max-line-length 100 :type integer)
  (documentation-required nil :type list))

(defstruct semantic-rules
  "Rules for semantic preservation."
  (existing-tests-pass t :type boolean)
  (no-removed-exports t :type boolean)
  (no-changed-signatures-without-flag t :type boolean))

(defstruct complexity-rules
  "Rules for complexity bounds."
  (max-function-length 50 :type integer)
  (max-cyclomatic-complexity 10 :type integer)
  (max-nesting-depth 4 :type integer))

;;;; Verification Result
;;;; Note: renamed to agent-verification-result to avoid conflict with th.core

(defstruct agent-verification-result
  "Result of agent verification."
  (policy-name nil :type symbol)
  (task-description nil :type (or null string))
  (status :pending :type keyword)  ; :passed :failed :warning
  (scope-result nil)
  (hallucination-result nil)
  (style-result nil)
  (semantic-result nil)
  (complexity-result nil)
  (violations nil :type list)
  (warnings nil :type list)
  (duration-ms 0 :type integer))

(defstruct dimension-result
  "Result for a single verification dimension."
  (name nil :type keyword)
  (status :pending :type keyword)
  (violations nil :type list)
  (warnings nil :type list)
  (details nil :type list))

;;;; Policy Registry

(defvar *policies* (make-hash-table :test 'eq)
  "Registry of defined verification policies.")

(defun register-policy (policy)
  "Register a verification policy."
  (setf (gethash (verification-policy-name policy) *policies*) policy))

(defun find-policy (name)
  "Find a policy by name."
  (gethash name *policies*))

(defun get-policy (name)
  "Get a policy by name (alias for find-policy)."
  (find-policy name))

(defun list-policies ()
  "List all registered policy names."
  (hash-table-keys *policies*))

(defun clear-policies ()
  "Clear all registered policies."
  (clrhash *policies*))

;;;; Verification Predicates

(defun verification-passed-p (result)
  "Check if verification passed (no failures)."
  (null (agent-verification-result-violations result)))

(defun verification-failed-p (result)
  "Check if verification failed."
  (not (verification-passed-p result)))

(defun dimension-passed-p (dim-result)
  "Check if a dimension passed."
  (eq (dimension-result-status dim-result) :passed))

;;;; File Change Tracking

(defstruct file-change
  "A change to a single file."
  (path nil :type string)
  (operation :modify :type keyword)  ; :create :modify :delete
  (added-lines 0 :type integer)
  (removed-lines 0 :type integer)
  (hunks nil :type list))  ; List of diff hunks

(defstruct diff-hunk
  "A single hunk from a diff."
  (old-start 0 :type integer)
  (old-count 0 :type integer)
  (new-start 0 :type integer)
  (new-count 0 :type integer)
  (lines nil :type list))  ; List of (operation . line-content)
