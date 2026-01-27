;;; ABOUTME: Core data structures for th.contract
;;; Defines contracts, interactions, schemas, and registries

(in-package #:th.contract)

;;;; Contract Structure

(defstruct (contract (:constructor %make-contract))
  "A consumer-driven contract between two services."
  (name nil :type symbol :read-only t)
  (consumer nil :type (or symbol string) :read-only t)
  (provider nil :type (or symbol string) :read-only t)
  (version "1.0.0" :type string)
  (interactions nil :type list)  ; List of interaction-struct
  (metadata nil :type list))     ; Alist of additional metadata

(defun make-contract (&key name consumer provider (version "1.0.0") interactions metadata)
  "Create a contract."
  (check-type name symbol)
  (%make-contract :name name
                  :consumer consumer
                  :provider provider
                  :version version
                  :interactions interactions
                  :metadata metadata))

;;;; Interaction Structure

(defstruct (interaction-struct (:constructor %make-interaction))
  "A single interaction (request/response pair) in a contract."
  (name nil :type symbol :read-only t)
  (description nil :type (or null string))
  (given nil :type (or null string))           ; Provider state
  (request nil)                                 ; http-request struct
  (response nil))                               ; http-response struct

(defun make-interaction (&key name description given request response)
  "Create an interaction."
  (check-type name symbol)
  (%make-interaction :name name
                     :description description
                     :given given
                     :request request
                     :response response))

;;;; Schema Structure

(defstruct (schema (:constructor %make-schema))
  "A reusable schema definition."
  (name nil :type symbol :read-only t)
  (fields nil :type list))  ; List of field definitions

(defun make-schema (name fields)
  "Create a schema."
  (check-type name symbol)
  (%make-schema :name name :fields fields))

;;;; Request Structure (for internal use)

(defstruct http-request
  "An HTTP request for verification."
  (method :get :type keyword)
  (path "/" :type string)
  (headers nil :type list)
  (body nil))

;;;; Response Structure (for internal use)

(defstruct http-response
  "An HTTP response for verification."
  (status 200 :type integer)
  (headers nil :type list)
  (body nil))

;;;; Contract Registry

(defvar *contracts* (make-hash-table :test 'eq)
  "Registry of defined contracts.")

(defun register-contract (contract)
  "Register a contract."
  (setf (gethash (contract-name contract) *contracts*) contract))

(defun find-contract (name)
  "Find a contract by name."
  (gethash name *contracts*))

(defun list-contracts ()
  "List all registered contract names."
  (alexandria:hash-table-keys *contracts*))

(defun clear-contracts ()
  "Clear all registered contracts."
  (clrhash *contracts*))

;;;; Schema Registry

(defvar *schemas* (make-hash-table :test 'eq)
  "Registry of defined schemas.")

(defun register-schema (schema)
  "Register a schema."
  (setf (gethash (schema-name schema) *schemas*) schema))

(defun find-schema (name)
  "Find a schema by name."
  (gethash name *schemas*))

(defun list-schemas ()
  "List all registered schema names."
  (alexandria:hash-table-keys *schemas*))

(defun clear-schemas ()
  "Clear all registered schemas."
  (clrhash *schemas*))

;;;; Verification Result

(defstruct verification-result
  "Result of provider verification."
  (contract-name nil :type symbol)
  (provider-url nil :type (or null string))
  (passed 0 :type (integer 0))
  (failed 0 :type (integer 0))
  (errors nil :type list)       ; List of error details
  (duration-ms 0 :type (integer 0))
  (interaction-results nil :type list))  ; Per-interaction results

(defstruct interaction-result
  "Result of verifying a single interaction."
  (interaction-name nil :type symbol)
  (status :pending :type keyword)  ; :passed :failed :error
  (expected nil)
  (actual nil)
  (mismatches nil :type list))     ; List of mismatch descriptions
