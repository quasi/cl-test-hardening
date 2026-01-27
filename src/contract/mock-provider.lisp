;;; ABOUTME: Mock provider for th.contract consumer testing
;;; Provides a simulated provider that responds according to contract

(in-package #:th.contract)

;;;; Mock Provider State

(defstruct mock-provider
  "A mock provider for consumer testing."
  (contract nil :type (or null contract))
  (recorded-requests nil :type list)
  (expected-interactions nil :type list))

;;;; Mock Provider Operations

(defun create-mock-provider (contract-name)
  "Create a mock provider from a contract."
  (let ((contract (find-contract contract-name)))
    (unless contract
      (error "Contract not found: ~A" contract-name))
    (make-mock-provider
     :contract contract
     :expected-interactions (mapcar #'interaction-struct-name
                                    (contract-interactions contract)))))

(defun mock-request (mock-provider method path &key headers body)
  "Send a mock request to the provider.
   Returns (values response matched-interaction)."
  (let* ((request (make-http-request :method method
                                     :path path
                                     :headers headers
                                     :body body))
         (interaction (find-matching-interaction mock-provider request)))
    (if interaction
        (progn
          (push (list :request request :interaction (interaction-struct-name interaction))
                (mock-provider-recorded-requests mock-provider))
          (values (generate-mock-response interaction) interaction))
        (values (make-http-response :status 404
                                    :body "No matching interaction found")
                nil))))

(defun find-matching-interaction (mock-provider request)
  "Find an interaction that matches the request."
  (let ((contract (mock-provider-contract mock-provider)))
    (find-if (lambda (interaction)
               (and (eq (http-request-method request)
                        (http-request-method (interaction-struct-request interaction)))
                    (path-matches-p (http-request-path (interaction-struct-request interaction))
                                    (http-request-path request))))
             (contract-interactions contract))))

(defun path-matches-p (pattern path)
  "Check if a path matches a pattern.
   Supports :param placeholders."
  (let ((pattern-parts (split-path pattern))
        (path-parts (split-path path)))
    (and (= (length pattern-parts) (length path-parts))
         (every (lambda (pat part)
                  (or (string= pat part)
                      (and (> (length pat) 0)
                           (char= (char pat 0) #\:))))  ; :param placeholder
                pattern-parts path-parts))))

(defun split-path (path)
  "Split a path into parts."
  (remove "" (cl-ppcre:split "/" path) :test #'string=))

;;;; Response Generation

(defun generate-mock-response (interaction)
  "Generate a mock response from an interaction's expected response."
  (let ((expected (interaction-struct-response interaction)))
    (make-http-response
     :status (http-response-status expected)
     :headers (http-response-headers expected)
     :body (body-to-pact (http-response-body expected)))))

;;;; Verification

(defun verify-mock-interactions (mock-provider)
  "Verify all expected interactions were exercised.
   Returns list of missing interaction names."
  (let ((recorded-names (mapcar (lambda (r) (getf r :interaction))
                                (mock-provider-recorded-requests mock-provider)))
        (expected (mock-provider-expected-interactions mock-provider)))
    (set-difference expected recorded-names)))

(defun mock-provider-summary (mock-provider)
  "Return a summary of mock provider usage."
  (let* ((recorded (mock-provider-recorded-requests mock-provider))
         (expected (mock-provider-expected-interactions mock-provider))
         (missing (verify-mock-interactions mock-provider)))
    (list :total-expected (length expected)
          :total-recorded (length recorded)
          :missing-interactions missing
          :all-exercised (null missing))))

;;;; Convenience Macros

(defmacro with-mock-provider ((var contract-name) &body body)
  "Execute BODY with VAR bound to a mock provider for CONTRACT-NAME."
  `(let ((,var (create-mock-provider ,contract-name)))
     (unwind-protect
          (progn ,@body)
       ;; Could add cleanup here if needed
       )))

;;;; Mock Provider Testing Utilities

(defun assert-mock-response (mock-provider method path expected-status &key body-matcher)
  "Assert that a mock request returns expected response.
   Returns the response on success, signals error on failure."
  (multiple-value-bind (response interaction)
      (mock-request mock-provider method path)
    (unless interaction
      (error "No matching interaction for ~A ~A" method path))
    (unless (= (http-response-status response) expected-status)
      (error "Expected status ~A, got ~A" expected-status (http-response-status response)))
    (when body-matcher
      (let ((result (matches-p body-matcher (http-response-body response) nil)))
        (unless (match-result-matched result)
          (error "Body mismatch: ~A" (match-result-message result)))))
    response))
