;;;; Complexity analysis for th.agent
;;;; Measures function length, nesting depth, and cyclomatic complexity

(in-package #:th.agent)

;;;; Function Length Analysis

(defun count-lines (form)
  "Estimate line count of a form by counting subforms."
  (cond
    ((null form) 0)
    ((atom form) 1)
    ((stringp (car form)) 1)  ; docstring
    (t (let ((count 0))
         (dolist (subform form)
           (incf count (if (consp subform)
                           (1+ (count-lines subform))
                           1)))
         count))))

(defun extract-function-lengths (filepath)
  "Extract function definitions and their estimated lengths."
  (let ((functions nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (member (car form) '(defun defmacro defmethod)))
                     (let ((name (if (eq (car form) 'defmethod)
                                     (format nil "~A (~A)"
                                             (second form)
                                             (third form))
                                     (symbol-name (second form))))
                           (body (if (eq (car form) 'defmethod)
                                     (cdddr form)
                                     (cdddr form))))
                       (push (list :name name
                                   :length (count-lines body)
                                   :type (car form))
                             functions)))))
      (error () nil))
    (nreverse functions)))

;;;; Nesting Depth Analysis

(defun measure-nesting-depth (form &optional (current-depth 0))
  "Measure maximum nesting depth of control structures."
  (cond
    ((atom form) current-depth)
    ((member (car form) '(if when unless cond case typecase
                             loop dolist dotimes do do*
                             let let* flet labels block
                             handler-case handler-bind
                             unwind-protect progn prog1))
     (let ((max-depth current-depth))
       (dolist (subform (cdr form))
         (setf max-depth (max max-depth
                              (measure-nesting-depth subform (1+ current-depth)))))
       max-depth))
    (t
     (let ((max-depth current-depth))
       (dolist (subform form)
         (when (consp subform)
           (setf max-depth (max max-depth
                                (measure-nesting-depth subform current-depth)))))
       max-depth))))

(defun extract-nesting-depths (filepath)
  "Extract nesting depths for all functions in a file."
  (let ((functions nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (member (car form) '(defun defmacro defmethod)))
                     (let ((name (symbol-name (second form)))
                           (body (cdddr form)))
                       (push (list :name name
                                   :depth (apply #'max 0
                                                 (mapcar #'measure-nesting-depth body)))
                             functions)))))
      (error () nil))
    (nreverse functions)))

;;;; Cyclomatic Complexity (Simplified)

(defun count-decision-points (form)
  "Count decision points in a form for cyclomatic complexity."
  (cond
    ((atom form) 0)
    ((member (car form) '(if when unless))
     (+ 1 (count-decision-points-in-list (cdr form))))
    ((eq (car form) 'cond)
     (+ (1- (length (cdr form)))  ; Each clause except last is a decision
        (count-decision-points-in-list (cdr form))))
    ((member (car form) '(case typecase ecase etypecase))
     (+ (1- (length (cddr form)))
        (count-decision-points-in-list (cddr form))))
    ((member (car form) '(and or))
     (+ (1- (length (cdr form)))  ; Short-circuit operators
        (count-decision-points-in-list (cdr form))))
    ((member (car form) '(loop dolist dotimes))
     (+ 1 (count-decision-points-in-list (cdr form))))
    ((eq (car form) 'handler-case)
     (+ (length (cddr form))  ; Each handler is a branch
        (count-decision-points-in-list (cdr form))))
    (t (count-decision-points-in-list form))))

(defun count-decision-points-in-list (forms)
  "Count decision points in a list of forms."
  (loop for form in forms
        when (consp form)
          sum (count-decision-points form)))

(defun cyclomatic-complexity (form)
  "Calculate cyclomatic complexity of a form.
   CC = decision points + 1"
  (1+ (count-decision-points form)))

(defun extract-complexities (filepath)
  "Extract cyclomatic complexities for all functions."
  (let ((functions nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (member (car form) '(defun defmacro defmethod)))
                     (let ((name (symbol-name (second form)))
                           (body (cdddr form)))
                       (push (list :name name
                                   :complexity (apply #'+
                                                      (mapcar #'cyclomatic-complexity
                                                              body)))
                             functions)))))
      (error () nil))
    (nreverse functions)))

;;;; Argument Count Analysis

(defun count-arguments (lambda-list)
  "Count total arguments in a lambda list."
  (let ((count 0))
    (dolist (item lambda-list)
      (unless (member item '(&optional &rest &key &allow-other-keys &aux &body &whole))
        (incf count)))
    count))

(defun extract-argument-counts (filepath)
  "Extract argument counts for all functions."
  (let ((functions nil))
    (handler-case
        (with-open-file (stream filepath :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (member (car form) '(defun defmacro defgeneric)))
                     (push (list :name (symbol-name (second form))
                                 :args (count-arguments (third form)))
                           functions))))
      (error () nil))
    (nreverse functions)))

;;;; Complexity Verification

(defun verify-complexity (rules files)
  "Verify complexity bounds for files."
  (let ((violations nil)
        (max-length (complexity-rules-max-function-length rules))
        (max-complexity (complexity-rules-max-cyclomatic-complexity rules))
        (max-nesting (complexity-rules-max-nesting-depth rules)))

    (dolist (file files)
      ;; Check function lengths
      (when max-length
        (dolist (func (extract-function-lengths file))
          (when (> (getf func :length) max-length)
            (push (make-complexity-violation
                   :function-too-long file (getf func :name)
                   (getf func :length) max-length
                   :suggestion "Extract helper functions to reduce length")
                  violations))))

      ;; Check cyclomatic complexity
      (when max-complexity
        (dolist (func (extract-complexities file))
          (when (> (getf func :complexity) max-complexity)
            (push (make-complexity-violation
                   :complexity-too-high file (getf func :name)
                   (getf func :complexity) max-complexity
                   :suggestion "Simplify by extracting conditions or using polymorphism")
                  violations))))

      ;; Check nesting depth
      (when max-nesting
        (dolist (func (extract-nesting-depths file))
          (when (> (getf func :depth) max-nesting)
            (push (make-complexity-violation
                   :nesting-too-deep file (getf func :name)
                   (getf func :depth) max-nesting
                   :suggestion "Reduce nesting with early returns or extracted functions")
                  violations)))))

    ;; Create dimension result
    (make-dimension-result
     :name :complexity
     :status (if (some (lambda (v) (eq (violation-severity v) :error)) violations)
                 :failed
                 (if violations :warning :passed))
     :violations (nreverse violations)
     :details (list :files-checked (length files)))))

;;;; High-Level Complexity Verification

(defun run-complexity-verification (policy files)
  "Run complete complexity verification for a policy."
  (let ((rules (verification-policy-complexity-rules policy)))
    (if rules
        (verify-complexity rules files)
        (make-dimension-result :name :complexity
                               :status :skipped
                               :details '(:reason "No complexity rules defined")))))
