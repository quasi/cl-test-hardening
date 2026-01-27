;;; ABOUTME: Mutation test runner
;;; Executes tests against mutants and tracks killed/survived status

(in-package #:th.mutation)

;;;; Test Runner Protocol

(defgeneric run-test-suite (test-suite)
  (:documentation "Run a test suite and return (values passed-p failure-info)."))

;; Default: assume test-suite is a function that returns T on success
(defmethod run-test-suite ((test-suite function))
  (handler-case
      (values (funcall test-suite) nil)
    (error (e)
      (values nil e))))

;; Symbol: call the function named by the symbol
(defmethod run-test-suite ((test-suite symbol))
  (run-test-suite (symbol-function test-suite)))

;;;; Mutant Execution

(defvar *current-mutant* nil
  "The mutant currently being tested.")

(defvar *original-definitions* (make-hash-table :test 'equal)
  "Storage for original function definitions during mutation.")

(defun save-original-definition (name)
  "Save original definition of NAME for later restoration."
  (when (fboundp name)
    (setf (gethash name *original-definitions*)
          (fdefinition name))))

(defun restore-original-definition (name)
  "Restore original definition of NAME."
  (let ((original (gethash name *original-definitions*)))
    (when original
      (setf (fdefinition name) original)
      (remhash name *original-definitions*))))

(defun restore-all-definitions ()
  "Restore all saved original definitions."
  (maphash (lambda (name def)
             (when (and name def)
               (setf (fdefinition name) def)))
           *original-definitions*)
  (clrhash *original-definitions*))

;;;; Timeout Handling

(defmacro with-timeout ((seconds &key on-timeout) &body body)
  "Execute BODY with a timeout. On timeout, execute ON-TIMEOUT form."
  ;; Portable version using handler-case
  ;; Note: True timeout requires implementation-specific support
  (declare (ignorable seconds))
  `(handler-case
       (progn ,@body)
     (error (e)
       (declare (ignore e))
       ,on-timeout)))

;;;; Running Tests Against a Mutant

(defun test-mutant (mutant test-suite policy)
  "Run TEST-SUITE against MUTANT, updating mutant status.
Returns the mutant with updated status."
  (let ((*current-mutant* mutant))
    (handler-case
        (with-timeout ((mutation-policy-timeout-per-mutant policy)
                       :on-timeout (progn
                                     (setf (mutant-status mutant) :timeout)
                                     mutant))
          (multiple-value-bind (passed failure-info)
              (run-test-suite test-suite)
            (declare (ignore failure-info))
            (setf (mutant-status mutant)
                  (if passed :survived :killed))
            mutant))
      (error (e)
        (declare (ignore e))
        (setf (mutant-status mutant) :killed)  ; Errors count as killed
        mutant))))

;;;; Form-Based Mutation Testing

(defun test-mutant-form (original-form mutant-form test-fn)
  "Test if TEST-FN catches the mutation from ORIGINAL-FORM to MUTANT-FORM.
TEST-FN should be (lambda (form) ...) that returns T if form is correct.
Returns :killed if test fails on mutant, :survived if test passes."
  (handler-case
      (if (funcall test-fn mutant-form)
          :survived  ; Test passed on mutant = bad test
          :killed)   ; Test failed on mutant = good test
    (error ()
      :killed)))  ; Errors count as killed

;;;; Main Mutation Testing Entry Point

(defun run-mutation-analysis (forms operators test-suite
                              &key (policy (make-mutation-policy :name 'default
                                                                  :operator-sets nil
                                                                  :threshold 0.8))
                                exclude-patterns)
  "Run mutation analysis on FORMS using OPERATORS.
FORMS: List of source forms to mutate
OPERATORS: List of mutation operators to apply
TEST-SUITE: Test suite to run (function or symbol)
POLICY: Mutation policy for thresholds and settings
Returns a MUTATION-RESULT struct."
  (reset-mutant-counter)
  (let* ((start-time (get-internal-real-time))
         (mutants (generate-mutants forms operators :exclude-patterns exclude-patterns))
         (killed 0)
         (survived 0)
         (timeout 0)
         (errors 0)
         (by-operator (make-hash-table :test 'eq)))

    ;; Initialize by-operator counts
    (dolist (op operators)
      (setf (gethash (mutation-operator-name op) by-operator) (cons 0 0)))

    ;; Test each mutant
    (dolist (mutant mutants)
      (let* ((op-name (mutant-operator mutant))
             (current (gethash op-name by-operator (cons 0 0))))
        ;; Increment total for this operator
        (setf (gethash op-name by-operator)
              (cons (car current) (1+ (cdr current))))

        ;; Run test
        (test-mutant mutant test-suite policy)

        ;; Update counts
        (case (mutant-status mutant)
          (:killed
           (incf killed)
           (setf (gethash op-name by-operator)
                 (cons (1+ (car (gethash op-name by-operator)))
                       (cdr (gethash op-name by-operator)))))
          (:survived (incf survived))
          (:timeout (incf timeout))
          (:error (incf errors)))))

    (let* ((total (length mutants))
           (score (calculate-mutation-score killed total)))
      (make-mutation-result
       :policy-name (mutation-policy-name policy)
       :total total
       :killed killed
       :survived survived
       :timeout timeout
       :error errors
       :score score
       :threshold-met (>= score (mutation-policy-threshold policy))
       :mutants mutants
       :duration-ms (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                      internal-time-units-per-second)))
       :by-operator (hash-table-alist by-operator)))))

;;;; Simplified API for Testing

(defun mutate-and-test (forms test-fn operators &key (threshold 0.8))
  "Simple mutation testing API.
FORMS: Source forms to mutate
TEST-FN: Function that returns T if code is correct
OPERATORS: List of mutation operators
Returns mutation score (0.0 to 1.0)."
  (let ((mutants (generate-mutants forms operators))
        (killed 0))
    (dolist (mutant mutants)
      (let ((status (test-mutant-form (mutant-original mutant)
                                      (mutant-replacement mutant)
                                      test-fn)))
        (setf (mutant-status mutant) status)
        (when (eq status :killed)
          (incf killed))))
    (let ((score (calculate-mutation-score killed (length mutants))))
      (values score
              (>= score threshold)
              (length mutants)
              killed))))
