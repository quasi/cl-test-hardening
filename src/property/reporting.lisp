;;; ABOUTME: Result formatting and reporting for property tests
;;; Provides human-readable failure reports with reproduction info

(in-package #:th.property)

;;;; Result Formatting

(defun format-result (result &optional (stream *standard-output*))
  "Format RESULT for human consumption."
  (if (property-result-passed-p result)
      (format-passed result stream)
      (format-failed result stream)))

(defun format-passed (result stream)
  "Format a passing property result."
  (format stream "~&[PASS] ~A passed (~D iterations, ~Dms)~%"
          (property-result-property-name result)
          (property-result-iterations result)
          (property-result-duration-ms result))
  (when (property-result-classifications result)
    (format-classifications (property-result-classifications result) stream)))

(defun format-failed (result stream)
  "Format a failing property result with counterexample."
  (format stream "~&[FAIL] ~A FAILED~%~%" (property-result-property-name result))

  ;; Error if any
  (when (property-result-error result)
    (format stream "Error: ~A~%~%" (property-result-error result)))

  ;; Original counterexample
  (when (property-result-counterexample result)
    (format stream "Counterexample (after ~D iterations):~%"
            (property-result-iterations result))
    (format-bindings (property-result-counterexample result) stream))

  ;; Shrunk counterexample
  (when (and (property-result-shrunk-counterexample result)
             (not (equal (property-result-counterexample result)
                         (property-result-shrunk-counterexample result))))
    (format stream "~%Shrunk to:~%")
    (format-bindings (property-result-shrunk-counterexample result) stream))

  ;; Reproduction info
  (format stream "~%Reproduce with seed: ~D~%"
          (property-result-seed result))

  ;; Classifications if any
  (when (property-result-classifications result)
    (format stream "~%")
    (format-classifications (property-result-classifications result) stream)))

(defun format-bindings (bindings stream)
  "Format variable bindings alist."
  (dolist (binding bindings)
    (format stream "  ~A = ~S~%" (car binding) (cdr binding))))

(defun format-classifications (classifications stream)
  "Format classification statistics."
  (let ((total (reduce #'+ classifications :key #'cdr)))
    (format stream "Classifications:~%")
    (dolist (class (sort (copy-list classifications) #'> :key #'cdr))
      (format stream "  ~A: ~D (~,1F%)~%"
              (car class)
              (cdr class)
              (* 100.0 (/ (cdr class) total))))))

;;;; Summary Reporting

(defun format-summary (results &optional (stream *standard-output*))
  "Format summary of multiple property results."
  (let ((passed (count-if #'property-result-passed-p results))
        (failed (count-if-not #'property-result-passed-p results))
        (total-time (reduce #'+ results :key #'property-result-duration-ms)))
    (format stream "~&~%========================================~%")
    (format stream "Property Test Summary~%")
    (format stream "========================================~%")
    (format stream "Passed: ~D~%" passed)
    (format stream "Failed: ~D~%" failed)
    (format stream "Total:  ~D~%" (length results))
    (format stream "Time:   ~Dms~%" total-time)
    (format stream "========================================~%")

    ;; List failures
    (when (plusp failed)
      (format stream "~%Failed properties:~%")
      (dolist (result results)
        (unless (property-result-passed-p result)
          (format stream "  - ~A (seed: ~D)~%"
                  (property-result-property-name result)
                  (property-result-seed result)))))

    (zerop failed)))

;;;; Machine-Readable Output

(defun result-to-plist (result)
  "Convert RESULT to a plist for serialization."
  (list :passed (property-result-passed-p result)
        :property (property-result-property-name result)
        :iterations (property-result-iterations result)
        :seed (property-result-seed result)
        :duration-ms (property-result-duration-ms result)
        :counterexample (when (property-result-counterexample result)
                          (mapcar (lambda (b) (list (car b) (cdr b)))
                                  (property-result-counterexample result)))
        :shrunk (when (property-result-shrunk-counterexample result)
                  (mapcar (lambda (b) (list (car b) (cdr b)))
                          (property-result-shrunk-counterexample result)))
        :error (when (property-result-error result)
                 (princ-to-string (property-result-error result)))))
