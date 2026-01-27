;;; ABOUTME: Schema matchers for th.contract
;;; Provides type, pattern, and structure matching for contract validation

(in-package #:th.contract)

;;;; Matcher Protocol
;;;
;;; A matcher is either:
;;; - A literal value (exact match)
;;; - A symbol naming a schema (resolve and match)
;;; - A list starting with a matcher function symbol

;;;; Match Result

(defstruct match-result
  "Result of matching a value against a matcher."
  (matched nil :type boolean)
  (path nil :type list)         ; Path to the value being matched
  (expected nil)                 ; What was expected
  (actual nil)                   ; What was found
  (message nil :type (or null string)))

(defun match-success ()
  "Create a successful match result."
  (make-match-result :matched t))

(defun match-failure (expected actual &key path message)
  "Create a failed match result."
  (make-match-result :matched nil
                     :expected expected
                     :actual actual
                     :path path
                     :message message))

;;;; Core Matching Function

(defun matches-p (matcher value &optional path)
  "Check if VALUE matches MATCHER. Returns match-result."
  (cond
    ;; Nil matcher matches anything
    ((null matcher)
     (match-success))

    ;; Literal match
    ((or (stringp matcher) (numberp matcher) (eq matcher t))
     (if (equal matcher value)
         (match-success)
         (match-failure matcher value :path path)))

    ;; Keyword literal
    ((keywordp matcher)
     (if (eq matcher value)
         (match-success)
         (match-failure matcher value :path path)))

    ;; Symbol - could be a schema reference
    ((and (symbolp matcher) (not (keywordp matcher)))
     (let ((schema (find-schema matcher)))
       (if schema
           (match-schema schema value path)
           (match-failure matcher value
                          :path path
                          :message (format nil "Unknown schema: ~A" matcher)))))

    ;; Matcher expression
    ((and (listp matcher) (symbolp (first matcher)))
     (apply-matcher (first matcher) (rest matcher) value path))

    ;; List literal - match structure
    ((listp matcher)
     (match-list-structure matcher value path))

    ;; Unknown matcher type
    (t
     (match-failure matcher value
                    :path path
                    :message "Unknown matcher type"))))

;;;; Matcher Dispatch

(defvar *matchers* (make-hash-table :test 'eq)
  "Registry of matcher functions.")

(defun register-matcher (name fn)
  "Register a matcher function."
  (setf (gethash name *matchers*) fn))

(defun apply-matcher (name args value path)
  "Apply a named matcher."
  (let ((fn (gethash name *matchers*)))
    (if fn
        (funcall fn args value path)
        (match-failure (cons name args) value
                       :path path
                       :message (format nil "Unknown matcher: ~A" name)))))

;;;; Type Matchers

(register-matcher 'type-of
  (lambda (args value path)
    (let ((expected-type (first args)))
      (case expected-type
        ((string) (if (stringp value)
                      (match-success)
                      (match-failure 'string (type-of value) :path path)))
        ((integer) (if (integerp value)
                       (match-success)
                       (match-failure 'integer (type-of value) :path path)))
        ((number) (if (numberp value)
                      (match-success)
                      (match-failure 'number (type-of value) :path path)))
        ((boolean) (if (or (eq value t) (eq value nil) (eq value :true) (eq value :false))
                       (match-success)
                       (match-failure 'boolean (type-of value) :path path)))
        ((list array) (if (listp value)
                          (match-success)
                          (match-failure 'list (type-of value) :path path)))
        ((object hash-table) (if (hash-table-p value)
                                 (match-success)
                                 (match-failure 'object (type-of value) :path path)))
        (otherwise
         (if (typep value expected-type)
             (match-success)
             (match-failure expected-type (type-of value) :path path)))))))

;;;; Pattern Matchers

(register-matcher 'string-matching
  (lambda (args value path)
    (let ((pattern (first args)))
      (if (not (stringp value))
          (match-failure `(string-matching ,pattern) value
                         :path path
                         :message "Expected string")
          (if (cl-ppcre:scan pattern value)
              (match-success)
              (match-failure `(string-matching ,pattern) value
                             :path path
                             :message (format nil "String does not match pattern ~S" pattern)))))))

(register-matcher 'integer-in-range
  (lambda (args value path)
    (destructuring-bind (min max) args
      (if (not (integerp value))
          (match-failure `(integer-in-range ,min ,max) value
                         :path path
                         :message "Expected integer")
          (if (and (>= value min) (<= value max))
              (match-success)
              (match-failure `(integer-in-range ,min ,max) value
                             :path path
                             :message (format nil "Integer ~A not in range [~A, ~A]"
                                              value min max)))))))

(register-matcher 'number-in-range
  (lambda (args value path)
    (destructuring-bind (min max) args
      (if (not (numberp value))
          (match-failure `(number-in-range ,min ,max) value
                         :path path
                         :message "Expected number")
          (if (and (>= value min) (<= value max))
              (match-success)
              (match-failure `(number-in-range ,min ,max) value
                             :path path
                             :message (format nil "Number ~A not in range [~A, ~A]"
                                              value min max)))))))

;;;; Structure Matchers

(register-matcher 'object-with
  (lambda (args value path)
    (if (not (hash-table-p value))
        (match-failure (list* 'object-with args) value
                       :path path
                       :message "Expected object/hash-table")
        (loop for (key matcher) on args by #'cddr
              for key-name = (if (keywordp key)
                                 (string-downcase (symbol-name key))
                                 (string key))
              for actual = (gethash key-name value)
              for result = (matches-p matcher actual (append path (list key-name)))
              unless (match-result-matched result)
                return result
              finally (return (match-success))))))

(register-matcher 'array-of
  (lambda (args value path)
    (destructuring-bind (element-matcher &key min max) args
      (cond
        ((not (or (listp value) (vectorp value)))
         (match-failure `(array-of ,element-matcher) value
                        :path path
                        :message "Expected array/list"))
        ((and min (< (length value) min))
         (match-failure `(array-of ,element-matcher :min ,min) value
                        :path path
                        :message (format nil "Array too short: ~A < ~A" (length value) min)))
        ((and max (> (length value) max))
         (match-failure `(array-of ,element-matcher :max ,max) value
                        :path path
                        :message (format nil "Array too long: ~A > ~A" (length value) max)))
        (t
         (loop for item in (coerce value 'list)
               for i from 0
               for result = (matches-p element-matcher item (append path (list i)))
               unless (match-result-matched result)
                 return result
               finally (return (match-success))))))))

(register-matcher 'one-of
  (lambda (args value path)
    (if (member value args :test #'equal)
        (match-success)
        (match-failure `(one-of ,@args) value
                       :path path
                       :message (format nil "Value ~S not in allowed set" value)))))

;;;; Flexible Matchers

(register-matcher 'any-value
  (lambda (args value path)
    (declare (ignore args path))
    (if (null value)
        ;; Even any-value requires presence
        (match-failure '(any-value) value :message "Field is missing")
        (match-success))))

(register-matcher 'optional-field
  (lambda (args value path)
    (let ((inner-matcher (first args)))
      (if (null value)
          (match-success)  ; Optional and missing is OK
          (matches-p inner-matcher value path)))))

;;;; Schema Matching

(defun match-schema (schema value path)
  "Match VALUE against SCHEMA."
  (if (not (hash-table-p value))
      (match-failure (schema-name schema) value
                     :path path
                     :message "Expected object for schema match")
      (loop for field in (schema-fields schema)
            for (key matcher) = (if (listp field)
                                    (list (first field) (second field))
                                    (list field '(any-value)))
            for key-name = (string-downcase (symbol-name key))
            for actual = (gethash key-name value)
            for result = (matches-p matcher actual (append path (list key-name)))
            unless (match-result-matched result)
              return result
            finally (return (match-success)))))

;;;; List Structure Matching

(defun match-list-structure (matcher value path)
  "Match a list structure (plist-like)."
  (cond
    ((null matcher)
     (match-success))
    ((not (or (listp value) (hash-table-p value)))
     (match-failure matcher value
                    :path path
                    :message "Expected list or object"))
    ((hash-table-p value)
     ;; Convert plist matcher to object-with
     (apply-matcher 'object-with matcher value path))
    (t
     ;; Match as plist
     (loop for (key val-matcher) on matcher by #'cddr
           for key-name = (string-downcase (symbol-name key))
           for actual = (getf value key)
           for result = (matches-p val-matcher actual (append path (list key-name)))
           unless (match-result-matched result)
             return result
           finally (return (match-success))))))
