;;; ABOUTME: Schema definition for th.contract
;;; Provides schema creation and management utilities

(in-package #:th.contract)

;;;; Schema Definition Helpers

(defun parse-schema-field (field-spec)
  "Parse a field specification into (name matcher) form.
   Accepts:
   - keyword alone: (:field-name) -> (:field-name (any-value))
   - keyword with matcher: (:field-name matcher)
   - nested plist for objects"
  (cond
    ((keywordp field-spec)
     (list field-spec '(any-value)))
    ((and (listp field-spec) (= 1 (length field-spec)))
     (list (first field-spec) '(any-value)))
    ((and (listp field-spec) (keywordp (first field-spec)))
     (list (first field-spec) (second field-spec)))
    (t
     (error "Invalid field specification: ~S" field-spec))))

(defun parse-schema-fields (field-specs)
  "Parse a list of field specifications."
  (mapcar #'parse-schema-field field-specs))

;;;; Schema Composition

(defun extend-schema (base-schema-name new-fields)
  "Create field list that extends an existing schema."
  (let ((base (find-schema base-schema-name)))
    (unless base
      (error "Unknown base schema: ~S" base-schema-name))
    (append (schema-fields base) (parse-schema-fields new-fields))))

;;;; Schema to Matcher Conversion

(defun schema-to-matcher (schema)
  "Convert a schema to an object-with matcher."
  `(object-with ,@(loop for (key matcher) in (schema-fields schema)
                        collect key
                        collect matcher)))

;;;; Schema Validation

(defun validate-against-schema (schema-name value)
  "Validate VALUE against a named schema. Returns match-result."
  (let ((schema (find-schema schema-name)))
    (unless schema
      (return-from validate-against-schema
        (match-failure schema-name value
                       :message (format nil "Unknown schema: ~A" schema-name))))
    (matches-p (schema-to-matcher schema) value nil)))

;;;; Common Schema Patterns

(defun iso-datetime-matcher ()
  "Matcher for ISO 8601 datetime strings."
  '(string-matching "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}"))

(defun uuid-matcher ()
  "Matcher for UUID strings."
  '(string-matching "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"))

(defun email-matcher ()
  "Matcher for email addresses (simplified)."
  '(string-matching "^[^@]+@[^@]+\\.[^@]+$"))

(defun url-matcher ()
  "Matcher for URLs (simplified)."
  '(string-matching "^https?://"))

;;;; Schema Registry Utilities

(defun schema-exists-p (name)
  "Check if a schema with NAME exists."
  (not (null (find-schema name))))

(defun describe-schema (name &optional (stream *standard-output*))
  "Print a human-readable description of a schema."
  (let ((schema (find-schema name)))
    (unless schema
      (format stream "~&Schema ~A not found.~%" name)
      (return-from describe-schema nil))
    (format stream "~&Schema: ~A~%" (schema-name schema))
    (format stream "Fields:~%")
    (loop for (field-name matcher) in (schema-fields schema)
          do (format stream "  ~A: ~S~%" field-name matcher))
    schema))
