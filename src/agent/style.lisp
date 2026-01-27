;;;; Style conformance checking for th.agent
;;;; Verifies naming conventions, documentation, and code patterns

(in-package #:th.agent)

;;;; Naming Convention Detection

(defun detect-naming-convention (name)
  "Detect the naming convention of NAME."
  (cond
    ;; +EARMUFFS+ - constants
    ((and (> (length name) 2)
          (char= (char name 0) #\+)
          (char= (char name (1- (length name))) #\+))
     :earmuffs)
    ;; *EARMUFFS* - special variables
    ((and (> (length name) 2)
          (char= (char name 0) #\*)
          (char= (char name (1- (length name))) #\*))
     :stars)
    ;; kebab-case
    ((cl-ppcre:scan "^[a-z][a-z0-9]*(-[a-z0-9]+)*$" name)
     :kebab-case)
    ;; SCREAMING-KEBAB-CASE
    ((cl-ppcre:scan "^[A-Z][A-Z0-9]*(-[A-Z0-9]+)*$" name)
     :screaming-kebab)
    ;; PascalCase
    ((cl-ppcre:scan "^[A-Z][a-zA-Z0-9]*$" name)
     :pascal-case)
    ;; camelCase
    ((cl-ppcre:scan "^[a-z][a-zA-Z0-9]*$" name)
     :camel-case)
    ;; snake_case
    ((cl-ppcre:scan "^[a-z][a-z0-9]*(_[a-z0-9]+)*$" name)
     :snake-case)
    ;; Unknown
    (t :unknown)))

(defun naming-convention-matches-p (name convention)
  "Check if NAME matches the expected CONVENTION."
  (eq (detect-naming-convention name) convention))

;;;; Documentation Checking

(defun has-docstring-p (form)
  "Check if a defun/defmacro form has a docstring."
  (and (consp form)
       (member (car form) '(defun defmacro defgeneric defclass))
       (> (length form) 3)
       (stringp (fourth form))))

(defun extract-undocumented-definitions (filepath required-types)
  "Find definitions missing documentation."
  (let ((missing nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (member (car form) '(defun defmacro defgeneric defclass))
                              (member (car form) required-types))
                     (unless (has-docstring-p form)
                       (push (list :type (car form)
                                   :name (symbol-name (second form)))
                             missing)))))
      (error () nil))
    (nreverse missing)))

;;;; Line Length Checking

(defun check-line-lengths (filepath max-length)
  "Find lines exceeding MAX-LENGTH."
  (let ((violations nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for line = (read-line stream nil :eof)
                for line-num from 1
                until (eq line :eof)
                when (> (length line) max-length)
                  do (push (list :line line-num
                                 :length (length line)
                                 :max max-length)
                           violations)))
      (error () nil))
    (nreverse violations)))

;;;; Pattern Extraction from Existing Code

(defun extract-project-style (source-files)
  "Analyze existing code to determine style patterns."
  (let ((function-conventions nil)
        (class-conventions nil)
        (variable-conventions nil)
        (max-lines nil))

    (dolist (file source-files)
      (handler-case
          (with-open-file (stream file :direction :input)
            ;; Track max line length
            (loop for line = (read-line stream nil :eof)
                  until (eq line :eof)
                  do (push (length line) max-lines))
            ;; Re-read for definitions
            (file-position stream 0)
            (loop for form = (read stream nil :eof)
                  until (eq form :eof)
                  do (when (consp form)
                       (case (car form)
                         ((defun defmacro defgeneric)
                          (push (detect-naming-convention
                                 (string-downcase (symbol-name (second form))))
                                function-conventions))
                         ((defclass defstruct)
                          (push (detect-naming-convention
                                 (string-downcase (symbol-name (second form))))
                                class-conventions))
                         ((defvar defparameter)
                          (push (detect-naming-convention
                                 (symbol-name (second form)))
                                variable-conventions))))))
        (error () nil)))

    (list :function-convention (most-common function-conventions)
          :class-convention (most-common class-conventions)
          :variable-convention (most-common variable-conventions)
          :typical-max-line (when max-lines
                              (percentile max-lines 95)))))

(defun most-common (items)
  "Find the most common item in a list."
  (when items
    (let ((counts (make-hash-table :test 'eq)))
      (dolist (item items)
        (incf (gethash item counts 0)))
      (let ((max-item nil)
            (max-count 0))
        (maphash (lambda (item count)
                   (when (> count max-count)
                     (setf max-item item
                           max-count count)))
                 counts)
        max-item))))

(defun percentile (numbers p)
  "Calculate the P-th percentile of NUMBERS."
  (when numbers
    (let* ((sorted (sort (copy-list numbers) #'<))
           (n (length sorted))
           (index (floor (* n (/ p 100)))))
      (nth (min index (1- n)) sorted))))

;;;; Style Verification

(defun verify-style (rules files existing-files)
  "Verify files conform to style rules.
   EXISTING-FILES are used to extract project style patterns."
  (let ((violations nil)
        (project-style (when (style-rules-matches-existing-patterns rules)
                         (extract-project-style existing-files)))
        (naming (style-rules-naming-conventions rules))
        (max-line (style-rules-max-line-length rules))
        (doc-required (style-rules-documentation-required rules)))

    (dolist (file files)
      ;; Check naming conventions
      (when naming
        (handler-case
            (with-open-file (stream file :direction :input)
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    do (when (consp form)
                         (case (car form)
                           ((defun defmacro defgeneric)
                            (let* ((name (string-downcase (symbol-name (second form))))
                                   (expected (or (getf naming :functions)
                                                 (getf project-style :function-convention)))
                                   (actual (detect-naming-convention name)))
                              (when (and expected (not (eq actual expected)))
                                (push (make-style-violation
                                       :naming-convention file nil actual expected
                                       :suggestion (format nil "Use ~A for function names"
                                                           expected))
                                      violations))))))))
          (error () nil)))

      ;; Check line lengths
      (when max-line
        (dolist (long-line (check-line-lengths file max-line))
          (push (make-style-violation
                 :line-length file (getf long-line :line)
                 (getf long-line :length) max-line
                 :suggestion "Break long lines for readability")
                violations)))

      ;; Check documentation
      (when doc-required
        (dolist (missing (extract-undocumented-definitions file doc-required))
          (push (make-style-violation
                 :missing-documentation file nil
                 (getf missing :name) "docstring"
                 :suggestion "Add documentation for exported definitions")
                violations))))

    ;; Create dimension result
    (make-dimension-result
     :name :style
     :status (if violations :failed :passed)
     :violations (nreverse violations)
     :details (list :files-checked (length files)
                    :project-style project-style))))

;;;; High-Level Style Verification

(defun run-style-verification (policy new-files existing-files)
  "Run complete style verification for a policy."
  (let ((rules (verification-policy-style-rules policy)))
    (if rules
        (verify-style rules new-files existing-files)
        (make-dimension-result :name :style
                               :status :skipped
                               :details '(:reason "No style rules defined")))))
