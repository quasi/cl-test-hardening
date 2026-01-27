;;; ABOUTME: Interaction handling for th.contract
;;; Manages request/response pair definitions and matching

(in-package #:th.contract)

;;;; Request Parsing

(defun parse-request-spec (spec)
  "Parse a request specification plist into http-request."
  (make-http-request
   :method (or (getf spec :method) :get)
   :path (or (getf spec :path) "/")
   :headers (getf spec :headers)
   :body (getf spec :body)))

;;;; Response Parsing

(defun parse-response-spec (spec)
  "Parse a response specification plist into expected response."
  (make-http-response
   :status (or (getf spec :status) 200)
   :headers (getf spec :headers)
   :body (getf spec :body)))

;;;; Interaction Parsing

(defun parse-interaction (name &rest args)
  "Parse interaction arguments into interaction-struct."
  (make-interaction
   :name name
   :description (getf args :description)
   :given (getf args :given)
   :request (parse-request-spec (getf args :request))
   :response (parse-response-spec (getf args :response))))

;;;; Request Matching

(defun request-matches-p (expected actual)
  "Check if an actual request matches the expected request pattern."
  (and (eq (http-request-method expected) (http-request-method actual))
       (string= (http-request-path expected) (http-request-path actual))
       (headers-match-p (http-request-headers expected)
                        (http-request-headers actual))
       (body-matches-p (http-request-body expected)
                       (http-request-body actual))))

;;;; Response Matching

(defun response-matches-p (expected actual &optional path)
  "Check if an actual response matches the expected response pattern.
   Returns a list of match-result objects."
  (let ((results nil))
    ;; Status check
    (let ((expected-status (http-response-status expected))
          (actual-status (http-response-status actual)))
      (unless (= expected-status actual-status)
        (push (match-failure expected-status actual-status
                             :path (append path '("status"))
                             :message (format nil "Status mismatch: expected ~A, got ~A"
                                              expected-status actual-status))
              results)))

    ;; Headers check
    (let ((header-results (match-headers (http-response-headers expected)
                                         (http-response-headers actual)
                                         (append path '("headers")))))
      (setf results (append results header-results)))

    ;; Body check
    (let ((body-result (body-matches-p (http-response-body expected)
                                       (http-response-body actual)
                                       (append path '("body")))))
      (unless (match-result-matched body-result)
        (push body-result results)))

    (if results
        results
        (list (match-success)))))

;;;; Header Matching

(defun headers-match-p (expected actual)
  "Check if expected headers are present in actual headers."
  (loop for (name . value) in expected
        for actual-value = (cdr (assoc name actual :test #'string-equal))
        always (or (null value)  ; Just checking presence
                   (equal value actual-value))))

(defun match-headers (expected actual path)
  "Match expected headers against actual, returning list of failures."
  (loop for (name . value) in expected
        for actual-value = (cdr (assoc name actual :test #'string-equal))
        unless (or (null value) (equal value actual-value))
          collect (match-failure value actual-value
                                 :path (append path (list name))
                                 :message (format nil "Header ~A mismatch" name))))

;;;; Body Matching

(defun body-matches-p (expected actual &optional path)
  "Match expected body against actual body using matchers."
  (cond
    ;; No expected body - anything matches
    ((null expected)
     (match-success))

    ;; Expected is a matcher expression (list starting with non-keyword symbol)
    ;; or a schema reference (symbol, non-keyword)
    ((or (and (listp expected)
              (symbolp (first expected))
              (not (keywordp (first expected))))
         (and (symbolp expected) (not (keywordp expected))))
     (matches-p expected actual path))

    ;; Expected is a literal value
    ((or (stringp expected) (numberp expected))
     (if (equal expected actual)
         (match-success)
         (match-failure expected actual :path path)))

    ;; Expected is a plist structure
    ((and (listp expected) (keywordp (first expected)))
     (match-plist-body expected actual path))

    ;; Unknown
    (t
     (match-failure expected actual
                    :path path
                    :message "Unknown body matcher type"))))

(defun match-plist-body (expected actual path)
  "Match a plist body specification against actual (hash-table or plist)."
  (cond
    ;; Actual is a hash table (from JSON parsing)
    ((hash-table-p actual)
     (loop for (key val-matcher) on expected by #'cddr
           for key-name = (string-downcase (symbol-name key))
           for actual-value = (gethash key-name actual)
           for result = (matches-p val-matcher actual-value
                                   (append path (list key-name)))
           unless (match-result-matched result)
             return result
           finally (return (match-success))))

    ;; Actual is a plist
    ((and (listp actual) (or (null actual) (keywordp (first actual))))
     (loop for (key val-matcher) on expected by #'cddr
           for actual-value = (getf actual key)
           for result = (matches-p val-matcher actual-value
                                   (append path (list (string-downcase (symbol-name key)))))
           unless (match-result-matched result)
             return result
           finally (return (match-success))))

    ;; Type mismatch
    (t
     (match-failure expected actual
                    :path path
                    :message "Expected object/plist for body"))))

;;;; Interaction Utilities

(defun interaction-request-path (interaction)
  "Get the request path from an interaction."
  (http-request-path (interaction-struct-request interaction)))

(defun interaction-request-method (interaction)
  "Get the request method from an interaction."
  (http-request-method (interaction-struct-request interaction)))

(defun find-interaction (contract interaction-name)
  "Find an interaction by name in a contract."
  (find interaction-name (contract-interactions contract)
        :key #'interaction-struct-name))
