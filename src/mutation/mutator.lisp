;;; ABOUTME: Source code mutator
;;; Applies mutations to source files and manages mutant generation

(in-package #:th.mutation)

;;;; Reading Source Files

(defun read-source-file (pathname)
  "Read all forms from a source file."
  (with-open-file (stream pathname :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun read-source-string (string)
  "Read all forms from a string."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

;;;; Mutant Generation

(defvar *mutant-counter* 0
  "Counter for generating unique mutant IDs.")

(defun reset-mutant-counter ()
  "Reset the mutant counter."
  (setf *mutant-counter* 0))

(defun next-mutant-id ()
  "Generate next unique mutant ID."
  (incf *mutant-counter*))

(defun generate-mutants (forms operators &key exclude-patterns file)
  "Generate all possible mutants for FORMS using OPERATORS.
Returns list of mutant structs."
  (let ((all-mutants '()))
    (loop for form in forms
          for form-index from 0
          do (let* ((sites (find-mutation-sites form operators))
                    (filtered-sites (filter-excluded-sites sites exclude-patterns)))
               (dolist (site filtered-sites)
                 (dolist (mutant (generate-mutants-for-site site))
                   (setf (mutant-id mutant) (next-mutant-id))
                   (setf (mutant-file mutant) file)
                   ;; Store form index in path for reconstruction
                   (push mutant all-mutants)))))
    (nreverse all-mutants)))

;;;; Applying Mutations

(defun apply-mutant-to-forms (forms mutant form-index)
  "Apply MUTANT to FORMS at FORM-INDEX, returning modified forms list."
  (loop for form in forms
        for i from 0
        if (= i form-index)
          collect (mutant-replacement mutant)
        else
          collect form))

(defun mutant-source-string (forms mutant form-index)
  "Generate source string with MUTANT applied."
  (let ((mutated-forms (apply-mutant-to-forms forms mutant form-index)))
    (with-output-to-string (out)
      (dolist (form mutated-forms)
        (write form :stream out :pretty t :readably t)
        (terpri out)))))

;;;; Compilation with Mutation

(defvar *mutation-package* nil
  "Package to use for mutation testing compilation.")

(defun compile-mutated-source (source-string &optional (package *package*))
  "Compile SOURCE-STRING and return success status.
Returns (values success-p condition-if-error)."
  (handler-case
      (let ((*package* package)
            (*mutation-package* package))
        (with-input-from-string (stream source-string)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (compile nil `(lambda () ,form))))
        (values t nil))
    (error (e)
      (values nil e))))

;;;; In-Memory Mutation Testing

(defun make-mutated-function (original-function mutant)
  "Create a version of ORIGINAL-FUNCTION with MUTANT applied.
This works by source transformation if source is available."
  ;; Note: This is a simplified version. Full implementation would need
  ;; access to function source code.
  (declare (ignore original-function mutant))
  (error "In-memory mutation requires source access - use file-based mutation"))

;;;; Form-Based Mutation for Testing

(defun mutate-form (form operators)
  "Generate all mutants for a single FORM.
Useful for testing mutation operators."
  (let ((sites (find-mutation-sites form operators)))
    (loop for site in sites
          append (loop for mutant in (generate-mutants-for-site site)
                       collect (list :original (mutation-site-form site)
                                     :mutated (mutant-replacement mutant)
                                     :operator (mutant-operator mutant)
                                     :path (mutation-site-path site))))))

;;;; Mutant Statistics

(defun count-mutants-by-operator (mutants)
  "Count mutants grouped by operator."
  (let ((counts (make-hash-table :test 'eq)))
    (dolist (mutant mutants)
      (incf (gethash (mutant-operator mutant) counts 0)))
    (hash-table-alist counts)))

(defun estimate-mutant-count (forms operators &key exclude-patterns)
  "Estimate number of mutants without generating them.
Useful for progress reporting."
  (let ((count 0))
    (loop for form in forms
          do (let* ((sites (find-mutation-sites form operators))
                    (filtered-sites (filter-excluded-sites sites exclude-patterns)))
               (dolist (site filtered-sites)
                 (incf count (length (mutation-operator-mutations
                                      (mutation-site-operator site)))))))
    count))
