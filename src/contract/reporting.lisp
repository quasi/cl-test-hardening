;;; ABOUTME: Reporting for th.contract verification results
;;; Formats and displays verification outcomes

(in-package #:th.contract)

;;;; Main Report Formatter

(defun format-verification-report (result &optional (stream *standard-output*))
  "Format a verification result as a human-readable report."
  (format stream "~&~%")
  (format stream "===================================================================~%")
  (format stream "  Contract Verification Report~%")
  (format stream "===================================================================~%")
  (format stream "~%")

  ;; Summary
  (format stream "Contract: ~A~%" (contract-verification-result-contract-name result))
  (format stream "Provider: ~A~%" (or (contract-verification-result-provider-url result) "N/A"))
  (format stream "Duration: ~Ams~%" (contract-verification-result-duration-ms result))
  (format stream "~%")

  ;; Results
  (let ((passed (contract-verification-result-passed result))
        (failed (contract-verification-result-failed result)))
    (format stream "Results: ~A passed, ~A failed (~,1F%)~%"
            passed failed
            (* 100 (verification-score result)))
    (format stream "~%")

    ;; Status indicator
    (if (verification-passed-p result)
        (format stream "[PASS] VERIFICATION PASSED~%")
        (format stream "[FAIL] VERIFICATION FAILED~%")))

  (format stream "~%")

  ;; Interaction details
  (format stream "-------------------------------------------------------------------~%")
  (format stream "Interaction Details:~%")
  (format stream "-------------------------------------------------------------------~%")

  (dolist (int-result (reverse (contract-verification-result-interaction-results result)))
    (format-interaction-result int-result stream))

  ;; Failures
  (when (contract-verification-result-errors result)
    (format stream "~%")
    (format stream "-------------------------------------------------------------------~%")
    (format stream "Failure Details:~%")
    (format stream "-------------------------------------------------------------------~%")
    (dolist (error (reverse (contract-verification-result-errors result)))
      (format-failure-detail error stream)))

  (format stream "~%")
  (format stream "===================================================================~%"))

;;;; Interaction Result Formatting

(defun format-interaction-result (result &optional (stream *standard-output*))
  "Format a single interaction result."
  (format stream "~%")
  (format stream "  ~A ~A~%"
          (case (interaction-result-status result)
            (:passed "[PASS]")
            (:failed "[FAIL]")
            (:error "[ERROR]")
            (t "[?]"))
          (interaction-result-interaction-name result))

  (when (eq (interaction-result-status result) :failed)
    (dolist (mismatch (interaction-result-mismatches result))
      (format stream "    -> ~A~%" mismatch))))

;;;; Failure Detail Formatting

(defun format-failure-detail (result &optional (stream *standard-output*))
  "Format detailed failure information."
  (format stream "~%")
  (format stream "  Interaction: ~A~%" (interaction-result-interaction-name result))
  (format stream "  Status: ~A~%" (interaction-result-status result))
  (format stream "~%")
  (format stream "  Mismatches:~%")
  (dolist (mismatch (interaction-result-mismatches result))
    (format stream "    * ~A~%" mismatch))
  (format stream "~%"))

;;;; JSON Report

(defun verification-to-json (result)
  "Convert verification result to JSON-compatible hash table."
  (let ((json (make-hash-table :test 'equal)))
    (setf (gethash "contractName" json)
          (symbol-name (contract-verification-result-contract-name result)))
    (setf (gethash "providerUrl" json)
          (contract-verification-result-provider-url result))
    (setf (gethash "passed" json) (contract-verification-result-passed result))
    (setf (gethash "failed" json) (contract-verification-result-failed result))
    (setf (gethash "score" json) (verification-score result))
    (setf (gethash "durationMs" json) (contract-verification-result-duration-ms result))
    (setf (gethash "success" json) (verification-passed-p result))
    (setf (gethash "interactions" json)
          (mapcar #'interaction-result-to-json
                  (contract-verification-result-interaction-results result)))
    json))

(defun interaction-result-to-json (result)
  "Convert interaction result to JSON-compatible hash table."
  (let ((json (make-hash-table :test 'equal)))
    (setf (gethash "name" json)
          (symbol-name (interaction-result-interaction-name result)))
    (setf (gethash "status" json)
          (string-downcase (symbol-name (interaction-result-status result))))
    (when (interaction-result-mismatches result)
      (setf (gethash "mismatches" json)
            (interaction-result-mismatches result)))
    json))

(defun write-verification-report (result filepath)
  "Write verification result as JSON to file."
  (ensure-directories-exist filepath)
  (with-open-file (out filepath
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (yason:encode (verification-to-json result) out)))

;;;; Contract Summary

(defun format-contract-summary (contract-name &optional (stream *standard-output*))
  "Print a summary of a contract's interactions."
  (let ((contract (find-contract contract-name)))
    (unless contract
      (format stream "~&Contract not found: ~A~%" contract-name)
      (return-from format-contract-summary nil))

    (format stream "~&~%")
    (format stream "Contract: ~A~%" (contract-name contract))
    (format stream "Consumer: ~A~%" (contract-consumer contract))
    (format stream "Provider: ~A~%" (contract-provider contract))
    (format stream "Version:  ~A~%" (contract-version contract))
    (format stream "~%")
    (format stream "Interactions (~A):~%" (length (contract-interactions contract)))

    (dolist (interaction (contract-interactions contract))
      (format stream "~%")
      (format stream "  ~A~%" (interaction-struct-name interaction))
      (when (interaction-struct-description interaction)
        (format stream "    ~A~%" (interaction-struct-description interaction)))
      (when (interaction-struct-given interaction)
        (format stream "    Given: ~A~%" (interaction-struct-given interaction)))
      (let ((req (interaction-struct-request interaction))
            (resp (interaction-struct-response interaction)))
        (format stream "    Request:  ~A ~A~%"
                (http-request-method req)
                (http-request-path req))
        (format stream "    Response: ~A~%"
                (http-response-status resp))))
    contract))

;;;; Mismatch Suggestions

(defun suggest-fixes (result &optional (stream *standard-output*))
  "Suggest fixes for verification failures."
  (format stream "~&~%")
  (format stream "Suggested Fixes:~%")
  (format stream "-------------------------------------------------------------------~%")

  (dolist (error (contract-verification-result-errors result))
    (format stream "~%")
    (format stream "~A:~%" (interaction-result-interaction-name error))
    (dolist (mismatch (interaction-result-mismatches error))
      (cond
        ((search "Status:" mismatch)
         (format stream "  * Check that the endpoint returns the correct status code~%")
         (format stream "    Verify routing and controller logic~%"))
        ((search "Header" mismatch)
         (format stream "  * Check response header configuration~%")
         (format stream "    Verify middleware/filter settings~%"))
        ((search "Body" mismatch)
         (format stream "  * Check response body structure and field types~%")
         (format stream "    Verify serialization format matches contract~%"))
        ((search "Error:" mismatch)
         (format stream "  * Check provider is running and accessible~%")
         (format stream "    Verify network/firewall settings~%"))
        (t
         (format stream "  * Review: ~A~%" mismatch))))))
