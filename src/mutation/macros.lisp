;;; ABOUTME: DSL macros for mutation testing
;;; Provides define-mutation-operators and define-mutation-policy

(in-package #:th.mutation)

;;;; define-mutation-operators

(defmacro define-mutation-operators (name &body operator-forms)
  "Define a set of mutation operators.

NAME: Symbol naming this operator set.
OPERATOR-FORMS: List of (mutate-pattern ...) forms.

Example:
  (define-mutation-operators arithmetic-operators
    (mutate-pattern negate-addition
      :pattern (+ ?x ?y)
      :mutations ((- ?x ?y)))
    (mutate-pattern swap-comparison
      :pattern (< ?x ?y)
      :mutations ((> ?x ?y) (<= ?x ?y) (>= ?x ?y))))"
  (let ((operators (mapcar #'parse-operator-form operator-forms)))
    `(progn
       (register-operator-set
        (make-operator-set ',name
                           (list ,@operators)))
       ',name)))

(defun parse-operator-form (form)
  "Parse a mutate-pattern form into a make-mutation-operator call."
  (destructuring-bind (mutate-kw name &key pattern mutations description) form
    (unless (member mutate-kw '(mutate-pattern mutate-boundary))
      (error "Expected (mutate-pattern ...) or (mutate-boundary ...), got ~S" form))
    (unless pattern
      (error "Mutation operator ~S missing :pattern" name))
    (unless mutations
      (error "Mutation operator ~S missing :mutations" name))
    `(make-mutation-operator :name ',name
                             :pattern ',pattern
                             :mutations ',mutations
                             :description ,description)))

;;;; define-mutation-policy

(defmacro define-mutation-policy (name &key operators (threshold 0.8)
                                         exclude-patterns (timeout-per-mutant 5)
                                         parallel)
  "Define a mutation testing policy.

NAME: Symbol naming this policy.
OPERATORS: List of operator set names to use.
THRESHOLD: Required mutation score (0.0 to 1.0).
EXCLUDE-PATTERNS: List of patterns to skip.
TIMEOUT-PER-MUTANT: Seconds before killing a mutant test.
PARALLEL: Run mutants in parallel.

Example:
  (define-mutation-policy order-system-policy
    :operators (arithmetic-operators conditional-operators)
    :threshold 0.85
    :exclude-patterns ((log-*) (debug-*))
    :timeout-per-mutant 10)"
  (unless operators
    (error "define-mutation-policy requires :operators"))
  `(progn
     (register-policy
      (make-mutation-policy :name ',name
                            :operator-sets ',operators
                            :threshold ,threshold
                            :exclude-patterns ',exclude-patterns
                            :timeout-per-mutant ,timeout-per-mutant
                            :parallel ,parallel))
     ',name))

;;;; mutate-pattern helper (for documentation)

(defmacro mutate-pattern (name &key pattern mutations description)
  "Define a mutation pattern (only valid inside define-mutation-operators).
NAME: Symbol naming this operator.
PATTERN: S-expression with ?variables for matching.
MUTATIONS: List of replacement patterns.
DESCRIPTION: Optional description."
  (declare (ignore name pattern mutations description))
  (error "mutate-pattern is only valid inside define-mutation-operators"))

(defmacro mutate-boundary (name &key pattern mutations description)
  "Define a boundary mutation (only valid inside define-mutation-operators).
Similar to mutate-pattern but semantically indicates boundary condition testing."
  (declare (ignore name pattern mutations description))
  (error "mutate-boundary is only valid inside define-mutation-operators"))

;;;; Quick Testing Macro

(defmacro with-mutation-operators ((&rest operator-sets) &body body)
  "Execute BODY with specified operator sets resolved to operators list."
  `(let ((operators (loop for set-name in ',operator-sets
                          append (operator-set-operators (find-operator-set set-name)))))
     ,@body))

;;;; Convenience for Testing

(defmacro check-mutation-score (forms test-fn operators &key (threshold 0.8))
  "Check if FORMS achieve required mutation score.
Returns (values score passed-p total-mutants killed-count)."
  `(mutate-and-test ,forms ,test-fn ,operators :threshold ,threshold))
