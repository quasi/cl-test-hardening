;;;; Package definitions for mutation testing module
;;;; Migrated from mutation-dsl

(defpackage #:th.mutation
  (:use #:cl #:alexandria #:th.core)
  (:nicknames #:th.mut)
  (:export
   ;; Mutation operator definition
   #:define-mutation-operators
   #:mutate-pattern
   #:mutate-boundary

   ;; Policy definition
   #:define-mutation-policy

   ;; Running mutation tests
   #:mutate-and-test
   #:run-mutation-analysis

   ;; Core types
   #:mutation-operator
   #:mutation-policy
   #:mutant
   #:mutation-result

   ;; Accessors
   #:operator-name
   #:operator-pattern
   #:operator-mutations
   #:policy-name
   #:policy-operators
   #:policy-threshold
   #:mutant-location
   #:mutant-original
   #:mutant-replacement
   #:mutant-operator
   #:result-score
   #:result-killed
   #:result-survived
   #:result-total

   ;; Standard operator set names (for find-operator-set)
   #:standard-arithmetic
   #:standard-conditional
   #:standard-logical
   #:standard-boundary
   #:standard-return

   ;; Reporting
   #:format-mutation-report
   #:format-survived-mutants

   ;; Pattern utilities
   #:pattern-matches-p
   #:apply-mutation

   ;; Registry
   #:find-policy
   #:list-policies
   #:clear-policies
   #:find-operator-set
   #:list-operator-sets

   ;; Operator creation
   #:make-mutation-operator

   ;; Mutant creation and accessors
   #:make-mutant
   #:mutant-id
   #:mutant-file
   #:mutant-line
   #:mutant-status

   ;; Result calculation
   #:calculate-mutation-score

   ;; Mutant generation
   #:generate-mutants
   #:mutate-form

   ;; Standard operators
   #:standard-operators
   #:*all-standard-operators*))

(defpackage #:th.mutation/internals
  (:use #:cl #:alexandria)
  (:export
   #:walk-forms
   #:find-mutation-sites
   #:generate-mutants
   #:compile-with-mutation
   #:run-tests-against-mutant))
