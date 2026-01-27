;;; ABOUTME: Property execution engine
;;; Runs properties with configurable iterations, handles timeouts, collects statistics

(in-package #:th.property)

;;;; Configuration

(defvar *default-iterations* 100
  "Default number of iterations when checking a property.")

(defvar *default-seed* nil
  "Default seed for random generation. NIL means generate fresh seed.")

(defvar *verbose* nil
  "When true, print progress during property checking.")

;;;; Input Generation

(defun generate-inputs (property random-state size)
  "Generate inputs for PROPERTY as alist ((var . value) ...).
Uses generators from forall-bindings."
  (loop for (var generator) in (property-forall-bindings property)
        collect (cons var (generate generator random-state size))))

(defun generators-alist (property)
  "Extract generators from property as alist ((var . generator) ...)."
  (loop for (var generator) in (property-forall-bindings property)
        collect (cons var generator)))

;;;; Single Property Check

(defun check-property (property-or-name &key
                                          (iterations *default-iterations*)
                                          (seed *default-seed*)
                                          (verbose *verbose*))
  "Check a property by running ITERATIONS random test cases.
Returns a PROPERTY-RESULT struct.

PROPERTY-OR-NAME: Symbol naming a property, or a property struct.
ITERATIONS: Number of random test cases to generate.
SEED: Random seed for reproducibility. NIL generates a new seed.
VERBOSE: Print progress information."
  (let* ((property (etypecase property-or-name
                     (symbol (or (find-property property-or-name)
                                 (error "Property not found: ~S" property-or-name)))
                     (property property-or-name)))
         (actual-seed (or seed (generate-seed)))
         (*test-random-state* (make-random-state-from-seed actual-seed))
         (start-time (get-internal-real-time))
         (classifications (make-hash-table :test 'equal))
         (precondition-failures 0))

    (when verbose
      (format t "~&Checking ~A (seed: ~D)...~%" (property-name property) actual-seed))

    (block test-loop
      (dotimes (i iterations)
        (let* ((size (compute-size-for-iteration i iterations))
               (*current-size* size)
               (inputs (generate-inputs property *test-random-state* size)))

          ;; Check precondition
          (when (property-precondition property)
            (unless (apply-with-bindings (property-precondition property) inputs)
              (incf precondition-failures)
              (when (> precondition-failures (* iterations 10))
                ;; Too many precondition failures - likely a bad generator/precondition combo
                (return-from test-loop
                  (make-property-result
                   :passed-p nil
                   :property-name (property-name property)
                   :iterations i
                   :seed actual-seed
                   :error (make-condition 'simple-error
                                          :format-control "Too many precondition failures (~D). Check generator or precondition."
                                          :format-arguments (list precondition-failures))
                   :duration-ms (elapsed-ms start-time))))
              (go :next-iteration)))

          ;; Classify input if classifier provided
          (when (property-classifier property)
            (let ((class (apply-with-bindings (property-classifier property) inputs)))
              (incf (gethash class classifications 0))))

          ;; Test property
          (handler-case
              (unless (apply-with-bindings (property-holds-predicate property) inputs)
                ;; Property failed - shrink and report
                (let ((shrunk (shrink-counterexample property inputs (generators-alist property))))
                  (return-from test-loop
                    (make-property-result
                     :passed-p nil
                     :property-name (property-name property)
                     :iterations (1+ i)
                     :counterexample inputs
                     :shrunk-counterexample shrunk
                     :seed actual-seed
                     :classifications (hash-table-alist classifications)
                     :duration-ms (elapsed-ms start-time)))))
            (error (e)
              ;; Error during property evaluation
              (return-from test-loop
                (make-property-result
                 :passed-p nil
                 :property-name (property-name property)
                 :iterations (1+ i)
                 :counterexample inputs
                 :seed actual-seed
                 :error e
                 :duration-ms (elapsed-ms start-time))))))
        :next-iteration)

      ;; All iterations passed
      (make-property-result
       :passed-p t
       :property-name (property-name property)
       :iterations iterations
       :seed actual-seed
       :classifications (hash-table-alist classifications)
       :duration-ms (elapsed-ms start-time)))))

(defun compute-size-for-iteration (iteration total-iterations)
  "Compute size parameter for ITERATION of TOTAL-ITERATIONS.
Starts small and grows to *current-size* or 100."
  (let ((max-size (or *current-size* 100)))
    (if (zerop total-iterations)
        max-size
        (min max-size
             (max 1 (ceiling (* max-size (/ (1+ iteration) total-iterations))))))))

(defun elapsed-ms (start-time)
  "Return milliseconds elapsed since START-TIME."
  (round (* 1000 (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second))))

;;;; Multiple Property Check

(defun check-all-properties (&key package tags (iterations *default-iterations*) (verbose *verbose*))
  "Check all registered properties matching PACKAGE and/or TAGS.
Returns list of PROPERTY-RESULT structs."
  (let ((properties (list-properties :package package :tags tags)))
    (when verbose
      (format t "~&Checking ~D properties...~%" (length properties)))
    (loop for prop in properties
          collect (check-property prop :iterations iterations :verbose verbose))))

;;;; Quick Check

(defun quickcheck (property-or-name &key (iterations 100))
  "Quick helper to check a property and print result summary.
Returns T if property passes, NIL otherwise."
  (let ((result (check-property property-or-name :iterations iterations)))
    (format-result result *standard-output*)
    (property-result-passed-p result)))
