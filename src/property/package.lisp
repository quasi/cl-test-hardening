;;;; Package definitions for property-based testing module
;;;; Migrated from property-dsl with package renaming

(defpackage #:th.property
  (:use #:cl #:alexandria #:th.core)
  (:nicknames #:th.prop)
  (:export
   ;; Property definition
   #:define-property
   #:defgenerator
   #:defshrink
   ;; Property struct accessors
   #:property
   #:property-name
   #:property-forall-bindings
   #:property-holds-predicate
   #:property-precondition
   #:property-tags
   #:property-timeout
   ;; Generator protocol
   #:generator
   #:generate
   #:shrink
   #:generator-name
   ;; Running properties
   #:check-property
   #:check-all-properties
   #:*default-iterations*
   #:*default-seed*
   ;; Results
   #:property-result
   #:result-passed-p
   #:result-counterexample
   #:result-shrunk-counterexample
   #:result-iterations
   #:result-seed
   #:result-error
   ;; Registry
   #:find-property
   #:list-properties
   #:clear-properties
   ;; Context
   #:with-property-context
   #:*test-random-state*
   #:*current-size*))

(defpackage #:th.gen
  (:use #:cl #:alexandria #:th.property)
  (:export
   ;; Primitive generators (plural names to avoid CL conflicts)
   #:integers
   #:naturals
   #:floats
   #:booleans
   #:characters
   #:strings
   #:symbols
   #:keywords
   ;; Collection generators
   #:lists
   #:vectors
   #:hash-tables
   ;; Combinators
   #:one-of
   #:frequency
   #:such-that
   #:tuple
   #:fmap
   #:const
   #:elements
   ;; Size control
   #:resize
   #:sized))

(defpackage #:th.property/internals
  (:use #:cl #:alexandria)
  (:export
   #:make-random-state-from-seed
   #:next-random-integer
   #:next-random-float
   #:current-size))
