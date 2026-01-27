;;;; Hallucination detection for th.agent
;;;; Detects references to non-existent functions, packages, and symbols

(in-package #:th.agent)

;;;; Known Symbol Sets

(defvar *cl-symbols* nil
  "Set of Common Lisp standard symbols.")

(defun ensure-cl-symbols ()
  "Ensure CL symbols are loaded."
  (unless *cl-symbols*
    (setf *cl-symbols* (make-hash-table :test 'equal))
    (do-external-symbols (sym :common-lisp)
      (setf (gethash (symbol-name sym) *cl-symbols*) t))))

(defun cl-symbol-p (name)
  "Check if NAME is a standard CL symbol."
  (ensure-cl-symbols)
  (gethash (string-upcase name) *cl-symbols*))

;;;; Code Parsing (Simple S-expression based)

(defun extract-references-from-file (filepath)
  "Extract symbol references from a Lisp file.
   Returns plist with :functions :packages :variables"
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((functions nil)
              (packages nil)
              (variables nil))
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (extract-references-from-form form
                                                 functions packages variables))
          (list :functions (remove-duplicates functions :test #'equal)
                :packages (remove-duplicates packages :test #'equal)
                :variables (remove-duplicates variables :test #'equal))))
    (error (e)
      (declare (ignore e))
      (list :functions nil :packages nil :variables nil :error t))))

(defun extract-references-from-form (form functions packages variables)
  "Extract references from a single form."
  (when (consp form)
    (let ((head (car form)))
      (cond
        ;; Package operations
        ((member head '(in-package use-package require asdf:load-system ql:quickload))
         (when (second form)
           (push (normalize-package-ref (second form)) packages)))

        ;; Function definitions (don't count as references)
        ((member head '(defun defmacro defgeneric defmethod))
         ;; Skip the name, process body
         (dolist (subform (cdddr form))
           (extract-references-from-form subform functions packages variables)))

        ;; Variable definitions
        ((member head '(defvar defparameter defconstant))
         ;; Process init form if present
         (when (third form)
           (extract-references-from-form (third form) functions packages variables)))

        ;; Function calls
        ((symbolp head)
         (unless (special-operator-p head)
           (push (symbol-name head) functions))
         (dolist (subform (cdr form))
           (extract-references-from-form subform functions packages variables)))

        ;; Nested forms
        (t
         (dolist (subform form)
           (extract-references-from-form subform functions packages variables)))))))

(defun normalize-package-ref (ref)
  "Normalize a package reference to a string."
  (typecase ref
    (string ref)
    (symbol (symbol-name ref))
    (keyword (symbol-name ref))
    (t (format nil "~A" ref))))

;;;; Definition Extraction

(defun extract-definitions-from-file (filepath)
  "Extract definitions from a Lisp file.
   Returns plist with :functions :macros :classes :variables"
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((functions nil)
              (macros nil)
              (classes nil)
              (variables nil))
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (consp form)
                     (case (car form)
                       ((defun) (push (symbol-name (second form)) functions))
                       ((defmacro) (push (symbol-name (second form)) macros))
                       ((defclass defstruct) (push (symbol-name (second form)) classes))
                       ((defvar defparameter defconstant)
                        (push (symbol-name (second form)) variables))
                       ((defgeneric defmethod)
                        (push (symbol-name (second form)) functions)))))
          (list :functions functions
                :macros macros
                :classes classes
                :variables variables)))
    (error ()
      (list :functions nil :macros nil :classes nil :variables nil))))

;;;; Hallucination Checking

(defun check-function-hallucinations (references definitions external-whitelist)
  "Check for undefined function references."
  (let ((defined (make-hash-table :test 'equal))
        (violations nil))
    ;; Add all defined functions
    (dolist (def (getf definitions :functions))
      (setf (gethash (string-upcase def) defined) t))
    (dolist (def (getf definitions :macros))
      (setf (gethash (string-upcase def) defined) t))
    ;; Add whitelist
    (dolist (name external-whitelist)
      (setf (gethash (string-upcase name) defined) t))
    ;; Check references
    (dolist (ref (getf references :functions))
      (let ((uref (string-upcase ref)))
        (unless (or (gethash uref defined)
                    (cl-symbol-p ref)
                    (fboundp (intern uref)))  ; Check if globally defined
          (push (list :name ref :confidence 0.9) violations))))
    violations))

(defun check-package-hallucinations (references)
  "Check for undefined package references."
  (let ((violations nil))
    (dolist (pkg (getf references :packages))
      (unless (or (find-package pkg)
                  (find-package (string-upcase pkg))
                  (asdf:find-system pkg nil))
        (push (list :name pkg :confidence 0.95) violations)))
    violations))

;;;; Hallucination Verification

(defun verify-hallucinations (rules files)
  "Verify files for hallucinations.
   FILES is a list of file paths."
  (let ((violations nil)
        (all-definitions (list :functions nil :macros nil
                               :classes nil :variables nil)))

    ;; First pass: collect all definitions
    (dolist (file files)
      (let ((defs (extract-definitions-from-file file)))
        (setf (getf all-definitions :functions)
              (append (getf defs :functions)
                      (getf all-definitions :functions)))
        (setf (getf all-definitions :macros)
              (append (getf defs :macros)
                      (getf all-definitions :macros)))
        (setf (getf all-definitions :classes)
              (append (getf defs :classes)
                      (getf all-definitions :classes)))
        (setf (getf all-definitions :variables)
              (append (getf defs :variables)
                      (getf all-definitions :variables)))))

    ;; Second pass: check references
    (dolist (file files)
      (let* ((refs (extract-references-from-file file))
             (whitelist (hallucination-rules-external-whitelist rules)))

        ;; Check functions
        (when (hallucination-rules-check-functions rules)
          (dolist (h (check-function-hallucinations refs all-definitions whitelist))
            (push (make-hallucination-violation
                   :undefined-function file nil (getf h :name)
                   :confidence (getf h :confidence)
                   :suggestion "Define this function or import from a package")
                  violations)))

        ;; Check packages
        (when (hallucination-rules-check-imports rules)
          (dolist (h (check-package-hallucinations refs))
            (push (make-hallucination-violation
                   :undefined-package file nil (getf h :name)
                   :confidence (getf h :confidence)
                   :suggestion "This package/system does not exist")
                  violations)))))

    ;; Create dimension result
    (make-dimension-result
     :name :hallucination
     :status (if violations :failed :passed)
     :violations (nreverse violations)
     :details (list :files-checked (length files)
                    :definitions-found (+ (length (getf all-definitions :functions))
                                          (length (getf all-definitions :macros)))))))

;;;; High-Level Hallucination Verification

(defun run-hallucination-verification (policy files)
  "Run complete hallucination verification for a policy."
  (let ((rules (verification-policy-hallucination-rules policy)))
    (if rules
        (verify-hallucinations rules files)
        (make-dimension-result :name :hallucination
                               :status :skipped
                               :details '(:reason "No hallucination rules defined")))))
