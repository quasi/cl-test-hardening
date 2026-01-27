;;; ABOUTME: Pact file generation for th.contract
;;; Generates Pact-compatible JSON files from contract definitions

(in-package #:th.contract)

;;;; Pact File Structure
;;;
;;; Standard Pact v3 format:
;;; {
;;;   "consumer": {"name": "..."},
;;;   "provider": {"name": "..."},
;;;   "interactions": [...],
;;;   "metadata": {"pactSpecification": {"version": "3.0.0"}}
;;; }

;;;; Main Generation Function

(defun generate-pact (contract-name &key output)
  "Generate a Pact JSON file for CONTRACT-NAME.
   If OUTPUT is provided, write to that file path.
   Returns the Pact structure as a hash table."
  (let ((contract (find-contract contract-name)))
    (unless contract
      (error "Contract not found: ~A" contract-name))
    (let ((pact (contract-to-pact contract)))
      (when output
        (write-pact-file pact output))
      pact)))

;;;; Contract to Pact Conversion

(defun contract-to-pact (contract)
  "Convert a contract to Pact format (hash table)."
  (let ((pact (make-hash-table :test 'equal)))
    (setf (gethash "consumer" pact)
          (make-participant (contract-consumer contract)))
    (setf (gethash "provider" pact)
          (make-participant (contract-provider contract)))
    (setf (gethash "interactions" pact)
          (mapcar #'interaction-to-pact (contract-interactions contract)))
    (setf (gethash "metadata" pact)
          (make-pact-metadata (contract-version contract)))
    pact))

(defun make-participant (name)
  "Create a participant hash table."
  (let ((p (make-hash-table :test 'equal)))
    (setf (gethash "name" p)
          (etypecase name
            (symbol (string-downcase (symbol-name name)))
            (string name)))
    p))

(defun make-pact-metadata (version)
  "Create Pact metadata hash table."
  (let ((meta (make-hash-table :test 'equal))
        (spec (make-hash-table :test 'equal)))
    (setf (gethash "version" spec) "3.0.0")
    (setf (gethash "pactSpecification" meta) spec)
    (when version
      (setf (gethash "contractVersion" meta) version))
    meta))

;;;; Interaction to Pact Conversion

(defun interaction-to-pact (interaction)
  "Convert an interaction to Pact format."
  (let ((pact (make-hash-table :test 'equal)))
    (setf (gethash "description" pact)
          (or (interaction-struct-description interaction)
              (string-downcase (symbol-name (interaction-struct-name interaction)))))
    (when (interaction-struct-given interaction)
      (setf (gethash "providerState" pact)
            (interaction-struct-given interaction)))
    (setf (gethash "request" pact)
          (request-to-pact (interaction-struct-request interaction)))
    (setf (gethash "response" pact)
          (response-to-pact (interaction-struct-response interaction)))
    pact))

;;;; Request to Pact Conversion

(defun request-to-pact (request)
  "Convert an http-request to Pact format."
  (let ((pact (make-hash-table :test 'equal)))
    (setf (gethash "method" pact)
          (string-upcase (symbol-name (http-request-method request))))
    (setf (gethash "path" pact)
          (http-request-path request))
    (when (http-request-headers request)
      (setf (gethash "headers" pact)
            (headers-to-pact (http-request-headers request))))
    (when (http-request-body request)
      (setf (gethash "body" pact)
            (body-to-pact (http-request-body request)))
      (setf (gethash "matchingRules" pact)
            (body-to-matching-rules (http-request-body request) "$.body")))
    pact))

;;;; Response to Pact Conversion

(defun response-to-pact (response)
  "Convert an http-response to Pact format."
  (let ((pact (make-hash-table :test 'equal)))
    (setf (gethash "status" pact)
          (http-response-status response))
    (when (http-response-headers response)
      (setf (gethash "headers" pact)
            (headers-to-pact (http-response-headers response))))
    (when (http-response-body response)
      (setf (gethash "body" pact)
            (body-to-pact (http-response-body response)))
      (let ((rules (body-to-matching-rules (http-response-body response) "$.body")))
        (when (plusp (hash-table-count rules))
          (setf (gethash "matchingRules" pact) rules))))
    pact))

;;;; Headers Conversion

(defun headers-to-pact (headers)
  "Convert headers alist to Pact format."
  (let ((pact (make-hash-table :test 'equal)))
    (loop for (name . value) in headers
          do (setf (gethash name pact) value))
    pact))

;;;; Body Conversion

(defun body-to-pact (body)
  "Convert body specification to Pact format.
   Generates example values for matchers."
  (cond
    ;; Literal values pass through
    ((or (stringp body) (numberp body) (eq body t) (null body))
     body)

    ;; Keywords become strings
    ((keywordp body)
     (string-downcase (symbol-name body)))

    ;; Matcher expressions generate examples
    ((and (listp body) (symbolp (first body)) (gethash (first body) *matchers*))
     (matcher-to-example (first body) (rest body)))

    ;; Schema reference
    ((and (symbolp body) (find-schema body))
     (schema-to-example body))

    ;; Plist structure
    ((and (listp body) (keywordp (first body)))
     (plist-to-pact body))

    ;; List of items
    ((listp body)
     (mapcar #'body-to-pact body))

    ;; Unknown - pass through
    (t body)))

(defun plist-to-pact (plist)
  "Convert a plist body to Pact hash table."
  (let ((pact (make-hash-table :test 'equal)))
    (loop for (key value) on plist by #'cddr
          for key-name = (string-downcase (symbol-name key))
          do (setf (gethash key-name pact) (body-to-pact value)))
    pact))

;;;; Matcher Examples

(defun matcher-to-example (matcher-name args)
  "Generate an example value for a matcher."
  (case matcher-name
    (type-of
     (case (first args)
       ((string) "string")
       ((integer) 1)
       ((number) 1.0)
       ((boolean) t)
       (t "example")))
    (string-matching
     (or (generate-from-pattern (first args)) "example-string"))
    (integer-in-range
     (floor (+ (first args) (second args)) 2))
    (number-in-range
     (/ (+ (first args) (second args)) 2))
    (one-of
     (first args))
    (array-of
     (list (body-to-pact (first args))))
    (any-value
     "any")
    (object-with
     (plist-to-pact args))
    (optional-field
     (body-to-pact (first args)))
    (t "example")))

(defun generate-from-pattern (pattern)
  "Generate a simple example from a regex pattern."
  (cond
    ;; UUID pattern
    ((search "[0-9a-f]{8}-[0-9a-f]{4}" pattern)
     "12345678-1234-1234-1234-123456789012")
    ;; Prefixed ID pattern like "ord_xxx" or "cust_xxx"
    ((cl-ppcre:scan "^([a-z]+)_" pattern)
     (cl-ppcre:register-groups-bind (prefix) ("^([a-z]+)_" pattern)
       (concatenate 'string prefix "_example123")))
    ;; ISO datetime
    ((search "\\d{4}-\\d{2}-\\d{2}" pattern)
     "2024-01-15T10:30:00Z")
    ;; Email-like
    ((search "@" pattern)
     "example@example.com")
    (t nil)))

(defun schema-to-example (schema-name)
  "Generate an example value from a schema."
  (let ((schema (find-schema schema-name)))
    (when schema
      (let ((pact (make-hash-table :test 'equal)))
        (loop for (key matcher) in (schema-fields schema)
              for key-name = (string-downcase (symbol-name key))
              do (setf (gethash key-name pact) (body-to-pact matcher)))
        pact))))

;;;; Matching Rules Generation

(defun body-to-matching-rules (body base-path)
  "Generate Pact matching rules from body matchers."
  (let ((rules (make-hash-table :test 'equal)))
    (collect-matching-rules body base-path rules)
    rules))

(defun collect-matching-rules (body path rules)
  "Recursively collect matching rules from body specification."
  (cond
    ;; Matcher expression
    ((and (listp body) (symbolp (first body)) (gethash (first body) *matchers*))
     (let ((rule (matcher-to-rule (first body) (rest body))))
       (when rule
         (setf (gethash path rules) rule))))

    ;; Schema reference
    ((and (symbolp body) (find-schema body))
     (let ((schema (find-schema body)))
       (loop for (key matcher) in (schema-fields schema)
             for key-path = (format nil "~A.~A" path (string-downcase (symbol-name key)))
             do (collect-matching-rules matcher key-path rules))))

    ;; Plist structure
    ((and (listp body) (keywordp (first body)))
     (loop for (key value) on body by #'cddr
           for key-path = (format nil "~A.~A" path (string-downcase (symbol-name key)))
           do (collect-matching-rules value key-path rules)))

    ;; Array
    ((and (listp body) (listp (first body)) (eq 'array-of (caar body)))
     (collect-matching-rules (cadar body)
                             (format nil "~A[*]" path)
                             rules))))

(defun matcher-to-rule (matcher-name args)
  "Convert a matcher to a Pact matching rule."
  (let ((rule (make-hash-table :test 'equal)))
    (case matcher-name
      (type-of
       (setf (gethash "match" rule) "type")
       rule)
      (string-matching
       (setf (gethash "match" rule) "regex")
       (setf (gethash "regex" rule) (first args))
       rule)
      (integer-in-range
       (setf (gethash "match" rule) "integer")
       (setf (gethash "min" rule) (first args))
       (setf (gethash "max" rule) (second args))
       rule)
      (one-of
       (setf (gethash "match" rule) "type")
       rule)
      (array-of
       (setf (gethash "match" rule) "type")
       ;; args is (element-matcher [:min n] [:max n])
       ;; Only try getf if there's a keyword in the rest
       (let ((min-val (when (and (cdr args) (keywordp (second args)))
                        (getf (cdr args) :min))))
         (setf (gethash "min" rule) (or min-val 0)))
       rule)
      (t nil))))

;;;; File Writing

(defun write-pact-file (pact filepath)
  "Write a Pact structure to a JSON file."
  (ensure-directories-exist filepath)
  (with-open-file (out filepath
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (yason:encode pact out)))

;;;; Utility Functions

(defun pact-filename (consumer provider)
  "Generate standard Pact filename."
  (format nil "~A-~A.json"
          (string-downcase (etypecase consumer
                             (symbol (symbol-name consumer))
                             (string consumer)))
          (string-downcase (etypecase provider
                             (symbol (symbol-name provider))
                             (string provider)))))
