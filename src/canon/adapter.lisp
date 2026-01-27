;;;; Canon adapter - JSON protocol and verification runner
;;;; Integrates cl-test-hardening with Canon specification system

(in-package #:th.canon)

;;;; Configuration

(defvar *request-path* "verification-request.json"
  "Default path for verification request file.")

(defvar *result-path* "verification-result.json"
  "Default path for verification result file.")

;;;; Verification Request Structure

(defstruct verification-request
  "Parsed verification request from Canon."
  (system nil :type (or string null))
  (test-suite nil :type (or string null))
  (properties nil :type list)
  (mutation nil :type list)
  (options nil :type list))

;;;; JSON Protocol

(defun read-verification-request (&optional (path *request-path*))
  "Read verification request from JSON file at PATH."
  (let ((json (yason:parse (read-file-into-string path)
                           :object-as :alist)))
    (make-verification-request
     :system (cdr (assoc "system" json :test #'string=))
     :test-suite (cdr (assoc "test_suite" json :test #'string=))
     :properties (cdr (assoc "properties" json :test #'string=))
     :mutation (cdr (assoc "mutation" json :test #'string=))
     :options (cdr (assoc "options" json :test #'string=)))))

(defun write-verification-result (result &optional (path *result-path*))
  "Write verification result to JSON file at PATH."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (yason:encode result out)))

(defun result-to-json (unit-tests properties mutations duration-ms)
  "Convert verification results to JSON-compatible alist.
UNIT-TESTS, PROPERTIES, MUTATIONS are plists with :passed, :failed, etc."
  (let ((passed (and (zerop (or (getf unit-tests :failed) 0))
                     (zerop (or (getf properties :failed) 0)))))
    `(("timestamp" . ,(current-timestamp))
      ("duration_ms" . ,duration-ms)
      ("platform" . "common-lisp")
      ("framework" . "fiveam")
      ("summary" . (("passed" . ,passed)
                    ("score" . ,(or (getf mutations :score) 1.0))))
      ("unit_tests" . ,unit-tests)
      ("properties" . ,properties)
      ("mutations" . ,mutations))))

;;;; Main Entry Point

(defun run-verification (&key (request-path *request-path*)
                              (result-path *result-path*))
  "Run verification based on Canon request at REQUEST-PATH.
Write result to RESULT-PATH."
  (let ((request (read-verification-request request-path)))
    (multiple-value-bind (result duration-ms)
        (measure-time
          (execute-verification request))
      (write-verification-result
       (result-to-json (getf result :unit-tests)
                       (getf result :properties)
                       (getf result :mutations)
                       duration-ms)
       result-path))))

;;;; Verification Execution

(defun execute-verification (request)
  "Execute all verification steps based on REQUEST.
Returns plist with :unit-tests, :properties, :mutations."
  (let ((system (verification-request-system request))
        (test-suite (verification-request-test-suite request))
        (properties (verification-request-properties request))
        (mutation-config (verification-request-mutation request))
        (options (verification-request-options request)))

    ;; Load the system under test
    (when system
      (asdf:load-system system))

    ;; Run unit tests
    (let ((unit-results (run-unit-tests test-suite)))
      ;; Run property tests
      (let ((property-results (run-properties properties options)))
        ;; Run mutation analysis if enabled
        (let ((mutation-results
                (when (and mutation-config
                           (cdr (assoc "enabled" mutation-config :test #'string=)))
                  (run-mutations mutation-config))))
          (list :unit-tests unit-results
                :properties property-results
                :mutations mutation-results))))))

;;;; Unit Test Runner

(defun run-unit-tests (test-suite)
  "Run FiveAM test suite, return results plist.
TEST-SUITE is a string naming the test suite."
  (if test-suite
      (let* ((suite-sym (intern (string-upcase test-suite) :keyword))
             (results (fiveam:run suite-sym)))
        (list :passed (count-if #'fiveam::test-passed-p results)
              :failed (count-if #'fiveam::test-failure-p results)
              :skipped 0
              :failures (mapcar #'format-test-failure
                                (remove-if-not #'fiveam::test-failure-p results))))
      (list :passed 0 :failed 0 :skipped 0 :failures nil)))

(defun format-test-failure (failure)
  "Format a FiveAM test failure for JSON output."
  (list :name (princ-to-string (fiveam::name failure))
        :message (princ-to-string failure)))

;;;; Property Test Runner

(defun run-properties (property-names options)
  "Run named properties, return results plist.
PROPERTY-NAMES is a list of property name strings.
OPTIONS is an alist of runner options."
  (if (null property-names)
      (list :checked 0 :passed 0 :failed 0 :failures nil)
      (let ((iterations (or (cdr (assoc "property_iterations" options :test #'string=)) 100))
            (checked 0)
            (passed 0)
            (failures nil))
        (dolist (name property-names)
          (let* ((prop-sym (intern (string-upcase name) :th.property))
                 (result (th.property:check-property prop-sym :iterations iterations)))
            (incf checked)
            (if (th.property:property-result-passed-p result)
                (incf passed)
                (push (format-property-failure name result) failures))))
        (list :checked checked
              :passed passed
              :failed (- checked passed)
              :failures (nreverse failures)))))

(defun format-property-failure (name result)
  "Format a property failure for JSON output."
  (list :name name
        :counterexample (th.property:property-result-counterexample result)
        :shrunk-to (th.property:property-result-shrunk-counterexample result)
        :seed (th.property:property-result-seed result)))

;;;; Mutation Test Runner

(defun run-mutations (config)
  "Run mutation analysis based on CONFIG.
Returns results plist with :score, :killed, :survived, :equivalent.
Currently a placeholder - full mutation testing requires more setup."
  (declare (ignore config))
  ;; TODO: Integrate with th.mutation module
  (list :score 1.0 :killed 0 :survived 0 :equivalent 0))
