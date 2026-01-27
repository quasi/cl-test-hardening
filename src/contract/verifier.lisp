;;; ABOUTME: Provider verification for th.contract
;;; Verifies that providers meet contract expectations

(in-package #:th.contract)

;;;; Main Verification Function

(defun verify-provider (contract-name &key against setup-fn teardown-fn timeout)
  "Verify a provider meets the contract.
   AGAINST: Base URL of the provider
   SETUP-FN: Function called with provider state before each interaction
   TEARDOWN-FN: Function called after each interaction
   TIMEOUT: Request timeout in seconds (default 30)
   Returns verification-result."
  (let* ((contract (find-contract contract-name))
         (start-time (get-internal-real-time))
         (result (make-verification-result
                  :contract-name contract-name
                  :provider-url against)))
    (unless contract
      (error "Contract not found: ~A" contract-name))

    ;; Verify each interaction
    (dolist (interaction (contract-interactions contract))
      (let ((int-result (verify-interaction interaction
                                            :base-url against
                                            :setup-fn setup-fn
                                            :teardown-fn teardown-fn
                                            :timeout (or timeout 30))))
        (push int-result (verification-result-interaction-results result))
        (case (interaction-result-status int-result)
          (:passed (incf (verification-result-passed result)))
          (:failed (incf (verification-result-failed result))
                   (push int-result (verification-result-errors result)))
          (:error (incf (verification-result-failed result))
                  (push int-result (verification-result-errors result))))))

    ;; Calculate duration
    (setf (verification-result-duration-ms result)
          (round (* 1000 (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second))))
    result))

;;;; Interaction Verification

(defun verify-interaction (interaction &key base-url setup-fn teardown-fn timeout)
  "Verify a single interaction against the provider."
  (let ((result (make-interaction-result
                 :interaction-name (interaction-struct-name interaction)
                 :status :pending
                 :expected (interaction-struct-response interaction))))
    (handler-case
        (progn
          ;; Setup provider state if needed
          (when (and setup-fn (interaction-struct-given interaction))
            (funcall setup-fn (interaction-struct-given interaction)))

          ;; Make the request
          (let* ((response (make-provider-request interaction base-url timeout))
                 (expected (interaction-struct-response interaction))
                 (mismatches (compare-responses expected response)))
            (setf (interaction-result-actual result) response)
            (if mismatches
                (progn
                  (setf (interaction-result-status result) :failed)
                  (setf (interaction-result-mismatches result) mismatches))
                (setf (interaction-result-status result) :passed)))

          ;; Teardown
          (when teardown-fn
            (funcall teardown-fn)))

      ;; Handle errors
      (error (e)
        (setf (interaction-result-status result) :error)
        (push (format nil "Error: ~A" e)
              (interaction-result-mismatches result))))

    result))

;;;; Provider Request

(defun make-provider-request (interaction base-url timeout)
  "Make an HTTP request to the provider."
  (let* ((request (interaction-struct-request interaction))
         (url (concatenate 'string base-url (http-request-path request)))
         (method (http-request-method request))
         (headers (http-request-headers request))
         (body (http-request-body request)))
    (multiple-value-bind (response-body status response-headers)
        (dex:request url
                     :method method
                     :headers (append headers
                                      (when body
                                        '(("Content-Type" . "application/json"))))
                     :content (when body
                                (with-output-to-string (s)
                                  (yason:encode (body-to-pact body) s)))
                     :read-timeout timeout
                     :connect-timeout timeout)
      (make-http-response
       :status status
       :headers (loop for (k . v) in response-headers
                      collect (cons (string k) v))
       :body (when (and response-body (plusp (length response-body)))
               (handler-case
                   (yason:parse response-body)
                 (error () response-body)))))))

;;;; Response Comparison

(defun compare-responses (expected actual)
  "Compare expected response with actual. Returns list of mismatches or nil."
  (let ((mismatches nil))
    ;; Status comparison
    (unless (= (http-response-status expected)
               (http-response-status actual))
      (push (format nil "Status: expected ~A, got ~A"
                    (http-response-status expected)
                    (http-response-status actual))
            mismatches))

    ;; Headers comparison (only check expected headers exist)
    (dolist (header (http-response-headers expected))
      (let ((expected-val (cdr header))
            (actual-val (cdr (assoc (car header) (http-response-headers actual)
                                    :test #'string-equal))))
        (when (and expected-val (not (equal expected-val actual-val)))
          (push (format nil "Header ~A: expected ~S, got ~S"
                        (car header) expected-val actual-val)
                mismatches))))

    ;; Body comparison using matchers
    (when (http-response-body expected)
      (let ((body-result (body-matches-p (http-response-body expected)
                                         (http-response-body actual)
                                         '("body"))))
        (unless (match-result-matched body-result)
          (push (format nil "Body at ~{~A~^.~}: expected ~S, got ~S~@[ (~A)~]"
                        (or (match-result-path body-result) '("body"))
                        (match-result-expected body-result)
                        (match-result-actual body-result)
                        (match-result-message body-result))
                mismatches))))

    (nreverse mismatches)))

;;;; Verification Predicates

(defun verification-passed-p (result)
  "Check if verification passed (no failures)."
  (zerop (verification-result-failed result)))

(defun verification-score (result)
  "Calculate verification score as ratio of passed to total."
  (let ((total (+ (verification-result-passed result)
                  (verification-result-failed result))))
    (if (zerop total)
        1.0
        (float (/ (verification-result-passed result) total)))))

;;;; Can-I-Deploy Check (Stub for broker integration)

(defun can-i-deploy (service-name version &key pact-broker)
  "Check if a service version can be deployed.
   This is a stub for Pact Broker integration.
   Returns (values can-deploy-p reasons)."
  (declare (ignore service-name version pact-broker))
  ;; Stub implementation - would call Pact Broker API
  (values t (list "Broker integration not implemented - assuming deployable")))
