;;;; Scope verification for th.agent
;;;; Checks that modifications are within allowed paths

(in-package #:th.agent)

;;;; Path Pattern Matching

(defun path-matches-pattern (path pattern)
  "Check if PATH matches glob-like PATTERN.
   Supports: * (any chars except /), ** (any path), ? (any single char)"
  (let ((regex (pattern-to-regex pattern)))
    (cl-ppcre:scan regex path)))

(defun pattern-to-regex (pattern)
  "Convert a glob pattern to a regex."
  (let ((regex pattern))
    ;; Escape special regex chars except our globs
    (setf regex (cl-ppcre:regex-replace-all "\\." regex "\\\\."))
    (setf regex (cl-ppcre:regex-replace-all "\\+" regex "\\\\+"))
    (setf regex (cl-ppcre:regex-replace-all "\\[" regex "\\\\["))
    (setf regex (cl-ppcre:regex-replace-all "\\]" regex "\\\\]"))
    (setf regex (cl-ppcre:regex-replace-all "\\(" regex "\\\\("))
    (setf regex (cl-ppcre:regex-replace-all "\\)" regex "\\\\)"))
    ;; Convert globs: **/ matches any path including empty - use placeholder
    (setf regex (cl-ppcre:regex-replace-all "\\*\\*/" regex "\x00STARSTAR\x00"))
    ;; ** at end matches any remaining path
    (setf regex (cl-ppcre:regex-replace-all "\\*\\*" regex "\x00STARSTARNOSLASH\x00"))
    ;; * matches anything except /
    (setf regex (cl-ppcre:regex-replace-all "\\*" regex "[^/]*"))
    ;; ? matches any single char except /
    (setf regex (cl-ppcre:regex-replace-all "\\?" regex "[^/]"))
    ;; Replace placeholders with actual patterns
    (setf regex (cl-ppcre:regex-replace-all "\x00STARSTAR\x00" regex "(?:.*/)?"))
    (setf regex (cl-ppcre:regex-replace-all "\x00STARSTARNOSLASH\x00" regex ".*"))
    ;; Anchor
    (concatenate 'string "^" regex "$")))

(defun path-allowed-p (path allowed-patterns)
  "Check if PATH matches any of the ALLOWED-PATTERNS."
  (some (lambda (pattern) (path-matches-pattern path pattern))
        allowed-patterns))

(defun path-forbidden-p (path forbidden-patterns)
  "Check if PATH matches any of the FORBIDDEN-PATTERNS."
  (some (lambda (pattern) (path-matches-pattern path pattern))
        forbidden-patterns))

;;;; Git Diff Parsing

(defun get-changed-files (before-ref after-ref &optional repo-path)
  "Get list of changed files between two git refs."
  (let* ((default-directory (or repo-path (uiop:getcwd)))
         (output (uiop:run-program
                  (list "git" "diff" "--name-status" before-ref after-ref)
                  :output :string
                  :directory default-directory
                  :ignore-error-status t)))
    (parse-name-status-output output)))

(defun parse-name-status-output (output)
  "Parse git diff --name-status output into file-change structs."
  (loop for line in (cl-ppcre:split "\\n" output)
        when (and line (> (length line) 2))
          collect (let ((status (char line 0))
                        (file (string-trim '(#\Tab #\Space) (subseq line 1))))
                    (make-file-change
                     :path file
                     :operation (case status
                                  (#\A :create)
                                  (#\M :modify)
                                  (#\D :delete)
                                  (#\R :rename)
                                  (t :modify))))))

(defun get-file-changes-detail (before-ref after-ref &optional repo-path)
  "Get detailed file changes with diff stats."
  (let* ((default-directory (or repo-path (uiop:getcwd)))
         (output (uiop:run-program
                  (list "git" "diff" "--stat" before-ref after-ref)
                  :output :string
                  :directory default-directory
                  :ignore-error-status t)))
    (parse-stat-output output)))

(defun parse-stat-output (output)
  "Parse git diff --stat output."
  (let ((files nil))
    (loop for line in (cl-ppcre:split "\\n" output)
          when (cl-ppcre:scan "\\|" line)
            do (cl-ppcre:register-groups-bind (file-part change-part)
                   ("^\\s*([^|]+)\\|\\s*(.*)" line)
                 (when file-part
                   (let ((path (string-trim '(#\Space) file-part)))
                     (multiple-value-bind (added removed)
                         (parse-change-stats change-part)
                       (push (make-file-change :path path
                                               :added-lines added
                                               :removed-lines removed)
                             files))))))
    (nreverse files)))

(defun parse-change-stats (change-part)
  "Parse the +/- part of git stat output."
  (let ((added 0) (removed 0))
    (when change-part
      (setf added (count #\+ change-part))
      (setf removed (count #\- change-part)))
    (values added removed)))

;;;; Scope Verification

(defun verify-scope-changes (rules file-changes)
  "Verify that file changes comply with scope rules.
   Returns a dimension-result."
  (let ((violations nil)
        (allowed (scope-rules-allowed-paths rules))
        (forbidden (scope-rules-forbidden-paths rules))
        (allowed-ops (scope-rules-allowed-operations rules))
        (forbidden-ops (scope-rules-forbidden-operations rules)))

    (dolist (change file-changes)
      (let ((path (file-change-path change))
            (op (file-change-operation change)))

        ;; Check forbidden paths first (takes precedence)
        (when (and forbidden (path-forbidden-p path forbidden))
          (push (make-scope-violation :forbidden-file path
                                      :operation op
                                      :message (format nil "File '~A' is in forbidden paths" path)
                                      :suggestion "This file should not be modified by the agent")
                violations))

        ;; Check allowed paths (if specified)
        (when (and allowed (not (path-allowed-p path allowed)))
          (push (make-scope-violation :out-of-scope path
                                      :operation op
                                      :message (format nil "File '~A' is outside allowed scope" path)
                                      :suggestion "Only modify files within the specified paths")
                violations))

        ;; Check forbidden operations
        (when (member op forbidden-ops)
          (push (make-scope-violation :forbidden-operation path
                                      :operation op
                                      :message (format nil "Operation ~A is forbidden" op)
                                      :suggestion "Request explicit permission for this operation")
                violations))

        ;; Check operation is allowed
        (when (and allowed-ops (not (member op allowed-ops)))
          (push (make-scope-violation :forbidden-operation path
                                      :operation op
                                      :message (format nil "Operation ~A not in allowed operations" op)
                                      :suggestion "Only :create and :modify operations are typically allowed")
                violations))))

    ;; Create dimension result
    (make-dimension-result
     :name :scope
     :status (if violations :failed :passed)
     :violations (nreverse violations)
     :details (list :files-checked (length file-changes)
                    :allowed-patterns allowed
                    :forbidden-patterns forbidden))))

;;;; High-Level Scope Verification

(defun run-scope-verification (policy files-or-changes &optional git-diff)
  "Run complete scope verification for a policy.
   FILES-OR-CHANGES can be:
   - A list of file path strings
   - A list of file-change structs
   - NIL (will return skipped)
   GIT-DIFF is optional raw git diff output (currently unused, for future parsing)."
  (declare (ignore git-diff))
  (let ((rules (verification-policy-scope-rules policy)))
    (if rules
        (let ((changes (etypecase (first files-or-changes)
                         (null nil)
                         (string (mapcar (lambda (path)
                                           (make-file-change :path path :operation :modify))
                                         files-or-changes))
                         (file-change files-or-changes))))
          ;; Also check max files changed
          (let ((result (verify-scope-changes rules changes))
                (max-files (scope-rules-max-files-changed rules)))
            (when (and max-files (> (length changes) max-files))
              (push (make-scope-violation
                     :too-many-files nil
                     :operation :modify
                     :message (format nil "Changed ~A files, maximum allowed is ~A"
                                      (length changes) max-files)
                     :suggestion "Reduce the scope of changes")
                    (dimension-result-violations result))
              (setf (dimension-result-status result) :failed))
            result))
        (make-dimension-result :name :scope
                               :status :skipped
                               :details '(:reason "No scope rules defined")))))
