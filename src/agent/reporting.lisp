;;;; Reporting for th.agent verification results
;;;; Formats and displays verification outcomes

(in-package #:th.agent)

;;;; Main Report Formatter

(defun format-verification-report (result &optional (stream *standard-output*))
  "Format a verification result as a human-readable report."
  (format stream "~&~%")
  (format stream "Agent Verification Report~%")
  (format stream "=================================================================~%")
  (format stream "~%")

  ;; Task info
  (format stream "Policy: ~A~%" (agent-verification-result-policy-name result))
  (when (agent-verification-result-task-description result)
    (format stream "Task: ~S~%" (agent-verification-result-task-description result)))
  (format stream "Duration: ~Ams~%" (agent-verification-result-duration-ms result))
  (format stream "~%")

  ;; Overall status
  (let ((violations (agent-verification-result-violations result))
        (warnings (agent-verification-result-warnings result)))
    (format stream "Status: ~A~%"
            (cond
              ((null violations) "[PASSED]")
              (t (format nil "[FAILED] (~A violation~:P)"
                         (length violations)))))
    (when warnings
      (format stream "Warnings: ~A~%" (length warnings))))

  (format stream "~%")

  ;; Dimension summaries
  (format stream "-----------------------------------------------------------------~%")
  (format stream "Verification Dimensions:~%")
  (format stream "-----------------------------------------------------------------~%")

  (format-dimension-summary (agent-verification-result-scope-result result) "Scope" stream)
  (format-dimension-summary (agent-verification-result-hallucination-result result)
                            "Hallucination Detection" stream)
  (format-dimension-summary (agent-verification-result-style-result result) "Style" stream)
  (format-dimension-summary (agent-verification-result-semantic-result result)
                            "Semantic Preservation" stream)
  (format-dimension-summary (agent-verification-result-complexity-result result)
                            "Complexity" stream)

  ;; Detailed violations
  (let ((violations (agent-verification-result-violations result)))
    (when violations
      (format stream "~%")
      (format stream "-----------------------------------------------------------------~%")
      (format stream "Violations:~%")
      (format stream "-----------------------------------------------------------------~%")

      (dolist (v violations)
        (format-violation v stream))))

  ;; Warnings
  (let ((warnings (agent-verification-result-warnings result)))
    (when warnings
      (format stream "~%")
      (format stream "-----------------------------------------------------------------~%")
      (format stream "Warnings:~%")
      (format stream "-----------------------------------------------------------------~%")

      (dolist (w warnings)
        (format-violation w stream))))

  ;; Recommendations
  (format-recommendations result stream)

  (format stream "~%")
  (format stream "=================================================================~%"))

;;;; Dimension Summary

(defun format-dimension-summary (dim-result name stream)
  "Format a dimension result summary."
  (when dim-result
    (let ((status (dimension-result-status dim-result))
          (violations (dimension-result-violations dim-result)))
      (format stream "~%~A: ~A~%"
              name
              (case status
                (:passed "[PASSED]")
                (:failed (format nil "[FAILED] (~A)" (length violations)))
                (:warning (format nil "[WARNING] (~A)" (length violations)))
                (:skipped "[SKIPPED]")
                (t "[UNKNOWN]")))

      ;; Brief details for passed
      (when (and (eq status :passed) (dimension-result-details dim-result))
        (let ((details (dimension-result-details dim-result)))
          (when (getf details :files-checked)
            (format stream "  Files checked: ~A~%" (getf details :files-checked)))))

      ;; Show violations inline for failed
      (when (and (member status '(:failed :warning)) violations)
        (dolist (v (subseq violations 0 (min 3 (length violations))))
          (format stream "  - ~A~%" (violation-message v)))
        (when (> (length violations) 3)
          (format stream "  ... and ~A more~%" (- (length violations) 3)))))))

;;;; Violation Formatting

(defun format-violation (v &optional (stream *standard-output*))
  "Format a single violation."
  (format stream "~%~A [~A] ~A~%"
          (case (violation-severity v)
            (:error "[ERROR]")
            (:warning "[WARN]")
            (:info "[INFO]"))
          (violation-dimension v)
          (violation-type v))

  (when (violation-file v)
    (format stream "  File: ~A" (violation-file v))
    (when (violation-line v)
      (format stream ":~A" (violation-line v)))
    (format stream "~%"))

  (format stream "  ~A~%" (violation-message v))

  (when (violation-suggestion v)
    (format stream "  Suggestion: ~A~%" (violation-suggestion v))))

;;;; Recommendations

(defun format-recommendations (result &optional (stream *standard-output*))
  "Generate actionable recommendations from violations."
  (let ((violations (agent-verification-result-violations result))
        (recommendations nil))

    ;; Collect unique recommendations
    (dolist (v violations)
      (when (violation-suggestion v)
        (pushnew (violation-suggestion v) recommendations :test #'equal)))

    (when recommendations
      (format stream "~%")
      (format stream "-----------------------------------------------------------------~%")
      (format stream "Recommendations:~%")
      (format stream "-----------------------------------------------------------------~%")

      (loop for rec in (nreverse recommendations)
            for i from 1
            do (format stream "~A. ~A~%" i rec)))))

;;;; JSON Report

(defun verification-to-json (result)
  "Convert verification result to JSON-compatible hash table."
  (let ((json (make-hash-table :test 'equal)))
    (setf (gethash "policyName" json)
          (symbol-name (agent-verification-result-policy-name result)))
    (setf (gethash "task" json)
          (agent-verification-result-task-description result))
    (setf (gethash "status" json)
          (string-downcase (symbol-name (agent-verification-result-status result))))
    (setf (gethash "passed" json)
          (verification-passed-p result))
    (setf (gethash "durationMs" json)
          (agent-verification-result-duration-ms result))
    (setf (gethash "violationCount" json)
          (length (agent-verification-result-violations result)))
    (setf (gethash "warningCount" json)
          (length (agent-verification-result-warnings result)))

    ;; Dimensions
    (setf (gethash "dimensions" json)
          (list (dimension-to-json (agent-verification-result-scope-result result))
                (dimension-to-json (agent-verification-result-hallucination-result result))
                (dimension-to-json (agent-verification-result-style-result result))
                (dimension-to-json (agent-verification-result-semantic-result result))
                (dimension-to-json (agent-verification-result-complexity-result result))))

    ;; Violations
    (setf (gethash "violations" json)
          (mapcar #'violation-to-json (agent-verification-result-violations result)))

    json))

(defun dimension-to-json (dim-result)
  "Convert dimension result to JSON-compatible hash table."
  (when dim-result
    (let ((json (make-hash-table :test 'equal)))
      (setf (gethash "name" json)
            (string-downcase (symbol-name (dimension-result-name dim-result))))
      (setf (gethash "status" json)
            (string-downcase (symbol-name (dimension-result-status dim-result))))
      (setf (gethash "violationCount" json)
            (length (dimension-result-violations dim-result)))
      json)))

(defun violation-to-json (v)
  "Convert violation to JSON-compatible hash table."
  (let ((json (make-hash-table :test 'equal)))
    (setf (gethash "type" json)
          (string-downcase (symbol-name (violation-type v))))
    (setf (gethash "severity" json)
          (string-downcase (symbol-name (violation-severity v))))
    (setf (gethash "dimension" json)
          (string-downcase (symbol-name (violation-dimension v))))
    (when (violation-file v)
      (setf (gethash "file" json) (violation-file v)))
    (when (violation-line v)
      (setf (gethash "line" json) (violation-line v)))
    (setf (gethash "message" json) (violation-message v))
    (when (violation-suggestion v)
      (setf (gethash "suggestion" json) (violation-suggestion v)))
    json))

;;;; Summary Statistics

(defun verification-summary (result)
  "Generate summary statistics for a verification result."
  (let ((all-violations (agent-verification-result-violations result)))
    (list :passed (verification-passed-p result)
          :total-violations (length all-violations)
          :errors (count-if (lambda (v) (eq (violation-severity v) :error))
                            all-violations)
          :warnings (count-if (lambda (v) (eq (violation-severity v) :warning))
                              all-violations)
          :by-dimension (group-violations-by-dimension all-violations)
          :duration-ms (agent-verification-result-duration-ms result))))
