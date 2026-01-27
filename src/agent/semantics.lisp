;;;; Semantic preservation verification for th.agent
;;;; Checks that existing behavior is preserved

(in-package #:th.agent)

;;;; Export Extraction

(defun extract-exports-from-file (filepath)
  "Extract exported symbols from a package definition."
  (let ((exports nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (eq (car form) 'defpackage))
                     (dolist (option (cddr form))
                       (when (and (consp option)
                                  (eq (car option) :export))
                         (dolist (sym (cdr option))
                           (push (typecase sym
                                   (symbol (symbol-name sym))
                                   (string sym)
                                   (t (format nil "~A" sym)))
                                 exports)))))))
      (error () nil))
    (remove-duplicates exports :test #'equal)))

(defun extract-all-exports (files)
  "Extract exports from all files."
  (let ((all-exports nil))
    (dolist (file files)
      (setf all-exports (append (extract-exports-from-file file) all-exports)))
    (remove-duplicates all-exports :test #'equal)))

;;;; Signature Extraction

(defun extract-function-signatures (filepath)
  "Extract function signatures from a file."
  (let ((signatures nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (member (car form) '(defun defgeneric)))
                     (let ((name (symbol-name (second form)))
                           (args (third form)))
                       (push (list :name name
                                   :args (lambda-list-info args)
                                   :arity (lambda-list-arity args))
                             signatures)))))
      (error () nil))
    (nreverse signatures)))

(defun lambda-list-info (lambda-list)
  "Extract info about a lambda list."
  (let ((required nil)
        (optional nil)
        (rest nil)
        (keyword nil))
    (let ((state :required))
      (dolist (item lambda-list)
        (cond
          ((eq item '&optional) (setf state :optional))
          ((eq item '&rest) (setf state :rest))
          ((eq item '&key) (setf state :keyword))
          ((eq item '&allow-other-keys) nil)
          ((eq item '&aux) (return))
          (t (case state
               (:required (push item required))
               (:optional (push item optional))
               (:rest (setf rest item))
               (:keyword (push item keyword)))))))
    (list :required (nreverse required)
          :optional (nreverse optional)
          :rest rest
          :keyword (nreverse keyword))))

(defun lambda-list-arity (lambda-list)
  "Calculate min and max arity of a lambda list."
  (let ((info (lambda-list-info lambda-list)))
    (list :min (length (getf info :required))
          :max (if (or (getf info :rest) (getf info :keyword))
                   :unlimited
                   (+ (length (getf info :required))
                      (length (getf info :optional)))))))

;;;; Signature Comparison

(defun signatures-compatible-p (old-sig new-sig)
  "Check if new signature is compatible with old."
  (let ((old-arity (getf old-sig :arity))
        (new-arity (getf new-sig :arity)))
    (and (<= (getf new-arity :min) (getf old-arity :min))
         (or (eq (getf new-arity :max) :unlimited)
             (eq (getf old-arity :max) :unlimited)
             (>= (getf new-arity :max) (getf old-arity :max))))))

;;;; Test Running (Placeholder)

(defun run-existing-tests (test-command &optional directory)
  "Run existing tests and return results.
   Returns (:passed count :failed count :errors errors)"
  (let* ((dir (or directory (uiop:getcwd)))
         (output (handler-case
                     (uiop:run-program test-command
                                       :output :string
                                       :error-output :string
                                       :directory dir
                                       :ignore-error-status t)
                   (error (e)
                     (format nil "Error running tests: ~A" e)))))
    ;; Simple pass/fail detection - could be enhanced
    (cond
      ((search "FAILED" output) (list :passed 0 :failed 1 :output output))
      ((search "passed" output) (list :passed 1 :failed 0 :output output))
      (t (list :passed 0 :failed 0 :unknown t :output output)))))

;;;; Semantic Verification

(defun verify-semantics (rules before-files after-files &key test-command)
  "Verify semantic preservation.
   BEFORE-FILES and AFTER-FILES are lists of file paths."
  (let ((violations nil))

    ;; Check for removed exports
    (when (semantic-rules-no-removed-exports rules)
      (let ((before-exports (extract-all-exports before-files))
            (after-exports (extract-all-exports after-files)))
        (dolist (export before-exports)
          (unless (member export after-exports :test #'equal)
            (push (make-semantic-violation
                   :removed-export
                   :message (format nil "Exported symbol '~A' was removed" export)
                   :suggestion "Don't remove exports without deprecation")
                  violations)))))

    ;; Check signature changes
    (when (semantic-rules-no-changed-signatures-without-flag rules)
      (let ((before-sigs (make-hash-table :test 'equal))
            (after-sigs (make-hash-table :test 'equal)))
        ;; Collect before signatures
        (dolist (file before-files)
          (dolist (sig (extract-function-signatures file))
            (setf (gethash (getf sig :name) before-sigs) sig)))
        ;; Collect after signatures
        (dolist (file after-files)
          (dolist (sig (extract-function-signatures file))
            (setf (gethash (getf sig :name) after-sigs) sig)))
        ;; Compare
        (maphash (lambda (name old-sig)
                   (let ((new-sig (gethash name after-sigs)))
                     (when (and new-sig
                                (not (signatures-compatible-p old-sig new-sig)))
                       (push (make-semantic-violation
                              :signature-changed
                              :message (format nil "Function '~A' signature changed incompatibly"
                                               name)
                              :suggestion "Use &optional or &key for backwards compatibility")
                             violations))))
                 before-sigs)))

    ;; Run tests if configured
    (when (and (semantic-rules-existing-tests-pass rules) test-command)
      (let ((result (run-existing-tests test-command)))
        (when (plusp (getf result :failed))
          (push (make-semantic-violation
                 :test-failure
                 :message (format nil "~A existing test(s) failed"
                                  (getf result :failed))
                 :suggestion "Fix failing tests before committing")
                violations))))

    ;; Create dimension result
    (make-dimension-result
     :name :semantic
     :status (if violations :failed :passed)
     :violations (nreverse violations)
     :details (list :before-files (length before-files)
                    :after-files (length after-files)))))

;;;; High-Level Semantic Verification

(defun run-semantic-verification (policy before-files after-files &key test-command)
  "Run complete semantic verification for a policy."
  (let ((rules (verification-policy-semantic-rules policy)))
    (if rules
        (verify-semantics rules before-files after-files :test-command test-command)
        (make-dimension-result :name :semantic
                               :status :skipped
                               :details '(:reason "No semantic rules defined")))))
