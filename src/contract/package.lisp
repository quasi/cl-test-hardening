;;;; Package definitions for contract testing module
;;;; ABOUTME: Defines th.contract package for consumer-driven contract testing

(defpackage #:th.contract
  (:use #:cl #:alexandria #:th.core)
  (:export
   ;; Contract definition
   #:define-contract
   #:defschema
   #:interaction

   ;; Schema matcher names (used as quoted symbols in patterns)
   ;; Users write: '(type-of string), '(string-matching "pat"), etc.
   ;; These are exported for documentation, not as macros/functions

   ;; Contract operations
   #:generate-pact
   #:verify-provider
   #:mock-provider
   #:can-i-deploy

   ;; Core structures
   #:contract
   #:contract-name
   #:contract-consumer
   #:contract-provider
   #:contract-version
   #:contract-interactions

   #:interaction-struct
   #:interaction-name
   #:interaction-description
   #:interaction-given
   #:interaction-request
   #:interaction-response

   #:schema
   #:schema-name
   #:schema-fields

   ;; Verification results
   #:verification-result
   #:verification-result-passed
   #:verification-result-failed
   #:verification-result-errors
   #:format-verification-report

   ;; Registry access
   #:find-contract
   #:find-schema
   #:list-contracts
   #:list-schemas
   #:clear-contracts
   #:clear-schemas

   ;; Mock provider
   #:with-mock-provider
   #:create-mock-provider
   #:mock-request
   #:verify-mock-interactions

   ;; Matchers
   #:string-matching
   #:integer-matching
   #:any-string
   #:any-integer
   #:any-boolean

   ;; Validation
   #:validate-against-schema))
