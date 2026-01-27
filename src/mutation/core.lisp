;;; ABOUTME: Core data structures for th.mutation
;;; Defines mutation operators, policies, mutants, and results

(in-package #:th.mutation)

;;;; Mutation Operator

(defstruct (mutation-operator (:constructor %make-mutation-operator))
  "A rule for generating mutations from source patterns."
  (name nil :type symbol :read-only t)
  (pattern nil :read-only t)              ; S-expression pattern with ?variables
  (mutations nil :type list :read-only t) ; List of replacement patterns
  (description nil :type (or null string) :read-only t))

(defun make-mutation-operator (&key name pattern mutations description)
  "Create a mutation operator."
  (check-type name symbol)
  (check-type mutations list)
  (%make-mutation-operator :name name
                           :pattern pattern
                           :mutations mutations
                           :description description))

;;;; Mutation Operator Set

(defstruct (operator-set (:constructor %make-operator-set))
  "A named collection of mutation operators."
  (name nil :type symbol :read-only t)
  (operators nil :type list :read-only t))

(defun make-operator-set (name operators)
  "Create an operator set."
  (check-type name symbol)
  (%make-operator-set :name name :operators operators))

;;;; Mutation Policy

(defstruct (mutation-policy (:constructor %make-mutation-policy))
  "Configuration for mutation testing."
  (name nil :type symbol :read-only t)
  (operator-sets nil :type list)          ; List of operator-set names
  (threshold 0.8 :type (real 0 1))        ; Required mutation score
  (exclude-patterns nil :type list)        ; Patterns to skip
  (timeout-per-mutant 5 :type (integer 1)) ; Seconds
  (parallel nil :type boolean))            ; Run mutants in parallel

(defun make-mutation-policy (&key name operator-sets (threshold 0.8)
                               exclude-patterns (timeout-per-mutant 5) parallel)
  "Create a mutation policy."
  (check-type name symbol)
  (check-type threshold (real 0 1))
  (%make-mutation-policy :name name
                         :operator-sets operator-sets
                         :threshold threshold
                         :exclude-patterns exclude-patterns
                         :timeout-per-mutant timeout-per-mutant
                         :parallel parallel))

;;;; Mutant

(defstruct mutant
  "A single mutation instance in source code."
  (id 0 :type (integer 0))
  (file nil :type (or null pathname string))
  (line nil :type (or null (integer 1)))
  (original nil)                          ; Original form
  (replacement nil)                       ; Mutated form
  (operator nil :type symbol)             ; Which operator generated this
  (status :pending :type (member :pending :killed :survived :timeout :error)))

;;;; Mutation Result

(defstruct mutation-result
  "Result of running mutation testing."
  (policy-name nil :type symbol)
  (total 0 :type (integer 0))
  (killed 0 :type (integer 0))
  (survived 0 :type (integer 0))
  (timeout 0 :type (integer 0))
  (error 0 :type (integer 0))
  (score 0.0 :type (real 0 1))
  (threshold-met nil :type boolean)
  (mutants nil :type list)                ; All mutant structs
  (duration-ms 0 :type (integer 0))
  (by-operator nil :type list))           ; Alist of (operator . (killed . total))

(defun calculate-mutation-score (killed total)
  "Calculate mutation score as ratio of killed to total."
  (if (zerop total)
      1.0  ; No mutants = perfect score
      (float (/ killed total))))

;;;; Operator Registry

(defvar *operator-sets* (make-hash-table :test 'eq)
  "Registry of defined operator sets.")

(defun register-operator-set (set)
  "Register an operator set."
  (setf (gethash (operator-set-name set) *operator-sets*) set))

(defun find-operator-set (name)
  "Find an operator set by name."
  (or (gethash name *operator-sets*)
      (error "Unknown operator set: ~S" name)))

(defun list-operator-sets ()
  "List all registered operator sets."
  (hash-table-keys *operator-sets*))

;;;; Policy Registry

(defvar *policies* (make-hash-table :test 'eq)
  "Registry of defined mutation policies.")

(defun register-policy (policy)
  "Register a mutation policy."
  (setf (gethash (mutation-policy-name policy) *policies*) policy))

(defun find-policy (name)
  "Find a policy by name."
  (or (gethash name *policies*)
      (error "Unknown mutation policy: ~S" name)))

(defun list-policies ()
  "List all registered policies."
  (hash-table-keys *policies*))

(defun clear-policies ()
  "Clear all registered policies."
  (clrhash *policies*))

;;;; Operator Resolution

(defun resolve-operators (policy)
  "Resolve all operators for a policy."
  (loop for set-name in (mutation-policy-operator-sets policy)
        for set = (find-operator-set set-name)
        append (operator-set-operators set)))
