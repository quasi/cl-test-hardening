;;; ABOUTME: Mutation testing reports
;;; Formats mutation scores, survived mutants, and improvement suggestions

(in-package #:th.mutation)

;;;; Result Formatting

(defun format-mutation-report (result &optional (stream *standard-output*))
  "Format a complete mutation testing report."
  (let ((passed (mutation-result-threshold-met result)))
    (format stream "~&===================================~%")
    (format stream "Mutation Testing Report~%")
    (format stream "===================================~%~%")

    ;; Overall score
    (format stream "Mutation Score: ~,1F% (target: ~,1F%) ~A~%~%"
            (* 100 (mutation-result-score result))
            (* 100 (find-threshold-for-policy (mutation-result-policy-name result)))
            (if passed "[PASS]" "[FAIL]"))

    ;; Summary counts
    (format stream "Summary:~%")
    (format stream "  Total mutants:  ~D~%" (mutation-result-total result))
    (format stream "  Killed:         ~D~%" (mutation-result-killed result))
    (format stream "  Survived:       ~D~%" (mutation-result-survived result))
    (when (plusp (mutation-result-timeout result))
      (format stream "  Timeout:        ~D~%" (mutation-result-timeout result)))
    (when (plusp (mutation-result-error result))
      (format stream "  Errors:         ~D~%" (mutation-result-error result)))
    (format stream "  Duration:       ~Dms~%~%" (mutation-result-duration-ms result))

    ;; By operator
    (when (mutation-result-by-operator result)
      (format stream "By Operator:~%")
      (dolist (entry (sort (copy-list (mutation-result-by-operator result))
                           #'> :key (lambda (e) (cdr (cdr e)))))
        (let* ((op-name (car entry))
               (killed (car (cdr entry)))
               (total (cdr (cdr entry)))
               (pct (if (zerop total) 100.0 (* 100.0 (/ killed total)))))
          (format stream "  ~20A ~D/~D (~,1F%)~%"
                  op-name killed total pct)))
      (terpri stream))

    ;; Survived mutants
    (let ((survived (remove-if-not (lambda (m) (eq (mutant-status m) :survived))
                                   (mutation-result-mutants result))))
      (when survived
        (format stream "Survived Mutants (~D):~%" (length survived))
        (format-survived-mutants survived stream)))

    passed))

(defun find-threshold-for-policy (policy-name)
  "Find the threshold for a policy, defaulting to 0.8."
  (let ((policy (gethash policy-name *policies*)))
    (if policy
        (mutation-policy-threshold policy)
        0.8)))

;;;; Survived Mutant Details

(defun format-survived-mutants (mutants &optional (stream *standard-output*) (limit 10))
  "Format details of survived mutants."
  (loop for mutant in mutants
        for i from 1 to limit
        do (format stream "~%  [~D] ~A~%" i (or (mutant-file mutant) "<in-memory>"))
           (when (mutant-line mutant)
             (format stream "      Line: ~D~%" (mutant-line mutant)))
           (format stream "      Operator: ~A~%" (mutant-operator mutant))
           (format stream "      Original:  ~S~%" (mutant-original mutant))
           (format stream "      Mutant:    ~S~%" (mutant-replacement mutant))
           (format stream "      Suggested: Add test for this specific case~%"))
  (when (> (length mutants) limit)
    (format stream "~%  ... and ~D more survived mutants~%"
            (- (length mutants) limit))))

;;;; Machine-Readable Output

(defun result-to-plist (result)
  "Convert mutation result to plist for serialization."
  (list :policy (mutation-result-policy-name result)
        :score (mutation-result-score result)
        :threshold-met (mutation-result-threshold-met result)
        :total (mutation-result-total result)
        :killed (mutation-result-killed result)
        :survived (mutation-result-survived result)
        :timeout (mutation-result-timeout result)
        :error (mutation-result-error result)
        :duration-ms (mutation-result-duration-ms result)
        :by-operator (mapcar (lambda (e)
                               (list (car e) :killed (cadr e) :total (cddr e)))
                             (mutation-result-by-operator result))))

;;;; Summary for Multiple Files

(defun format-multi-file-summary (results &optional (stream *standard-output*))
  "Format summary for mutation testing across multiple files."
  (let ((total-mutants 0)
        (total-killed 0)
        (files-passed 0)
        (files-failed 0))
    (dolist (result results)
      (incf total-mutants (mutation-result-total result))
      (incf total-killed (mutation-result-killed result))
      (if (mutation-result-threshold-met result)
          (incf files-passed)
          (incf files-failed)))
    (format stream "~&===================================~%")
    (format stream "Multi-File Mutation Summary~%")
    (format stream "===================================~%~%")
    (format stream "Files tested: ~D~%" (length results))
    (format stream "Files passed: ~D~%" files-passed)
    (format stream "Files failed: ~D~%" files-failed)
    (format stream "~%Overall:~%")
    (format stream "  Total mutants: ~D~%" total-mutants)
    (format stream "  Total killed:  ~D~%" total-killed)
    (format stream "  Overall score: ~,1F%~%"
            (if (zerop total-mutants)
                100.0
                (* 100.0 (/ total-killed total-mutants))))
    (zerop files-failed)))

;;;; Suggested Tests

(defun suggest-test-for-mutant (mutant)
  "Generate a suggestion for what test would kill this mutant."
  (let ((original (mutant-original mutant))
        (replacement (mutant-replacement mutant))
        (op (mutant-operator mutant)))
    (format nil "Test that distinguishes ~S from ~S (operator: ~A)"
            original replacement op)))

(defun generate-test-suggestions (result &optional (stream *standard-output*))
  "Generate test suggestions for all survived mutants."
  (let ((survived (remove-if-not (lambda (m) (eq (mutant-status m) :survived))
                                 (mutation-result-mutants result))))
    (format stream "~&Test Suggestions:~%")
    (format stream "-----------------~%")
    (dolist (mutant survived)
      (format stream "~%* ~A~%" (suggest-test-for-mutant mutant)))))
