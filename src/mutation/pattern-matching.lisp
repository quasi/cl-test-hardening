;;; ABOUTME: Pattern matching for finding mutation sites
;;; Matches s-expression patterns with ?variables against source code

(in-package #:th.mutation)

;;;; Pattern Variables

(defun pattern-variable-p (x)
  "Check if X is a pattern variable (symbol starting with ?)."
  (and (symbolp x)
       (plusp (length (symbol-name x)))
       (char= #\? (char (symbol-name x) 0))))

(defun pattern-variable-name (var)
  "Extract the name from a pattern variable."
  (intern (subseq (symbol-name var) 1)
          (symbol-package var)))

;;;; Pattern Matching

(defun pattern-matches-p (pattern form &optional bindings)
  "Check if FORM matches PATTERN, returning bindings alist or NIL.
Pattern variables (symbols starting with ?) match any single form.
Returns (values match-p bindings)."
  (cond
    ;; Pattern variable - matches anything
    ((pattern-variable-p pattern)
     (let* ((var-name (pattern-variable-name pattern))
            (existing (assoc var-name bindings)))
       (if existing
           ;; Variable already bound - check consistency
           (if (equal (cdr existing) form)
               (values t bindings)
               (values nil nil))
           ;; New binding
           (values t (acons var-name form bindings)))))

    ;; Both are atoms - must be equal
    ((atom pattern)
     (if (equal pattern form)
         (values t bindings)
         (values nil nil)))

    ;; Both are conses - recursively match
    ((consp form)
     (multiple-value-bind (car-match car-bindings)
         (pattern-matches-p (car pattern) (car form) bindings)
       (if car-match
           (pattern-matches-p (cdr pattern) (cdr form) car-bindings)
           (values nil nil))))

    ;; Pattern is cons but form is atom - no match
    (t (values nil nil))))

;;;; Pattern Substitution

(defun substitute-pattern (pattern bindings)
  "Substitute pattern variables in PATTERN with values from BINDINGS."
  (cond
    ((pattern-variable-p pattern)
     (let ((binding (assoc (pattern-variable-name pattern) bindings)))
       (if binding
           (cdr binding)
           pattern)))  ; Leave unbound variables as-is

    ((atom pattern) pattern)

    (t (cons (substitute-pattern (car pattern) bindings)
             (substitute-pattern (cdr pattern) bindings)))))

;;;; Form Walking

(defun walk-forms (function form &optional path)
  "Walk FORM depth-first, calling FUNCTION on each subform.
FUNCTION receives (subform path) where path is list of indices from root.
Returns list of all non-nil function results."
  (let ((results '()))
    (labels ((walk (f current-path)
               (let ((result (funcall function f current-path)))
                 (when result
                   (push result results)))
               (when (consp f)
                 (loop for sub in f
                       for i from 0
                       do (walk sub (append current-path (list i)))))))
      (walk form path))
    (nreverse results)))

;;;; Finding Mutation Sites

(defstruct mutation-site
  "A location in source code that matches a mutation pattern."
  (form nil)                              ; The matching form
  (path nil :type list)                   ; Path from root to this form
  (bindings nil :type list)               ; Pattern variable bindings
  (operator nil :type mutation-operator)) ; The matching operator

(defun find-mutation-sites (form operators)
  "Find all sites in FORM that match any operator pattern."
  (let ((sites '()))
    (walk-forms
     (lambda (subform path)
       (dolist (op operators)
         (multiple-value-bind (match bindings)
             (pattern-matches-p (mutation-operator-pattern op) subform)
           (when match
             (push (make-mutation-site :form subform
                                       :path path
                                       :bindings bindings
                                       :operator op)
                   sites))))
       nil)  ; Don't collect in walk results
     form)
    (nreverse sites)))

;;;; Applying Mutations

(defun apply-mutation-at-path (form path replacement)
  "Return copy of FORM with subform at PATH replaced by REPLACEMENT."
  (if (null path)
      replacement
      (let ((result (copy-list form)))
        (labels ((set-at-path (lst path-remaining)
                   (if (= 1 (length path-remaining))
                       (setf (nth (car path-remaining) lst) replacement)
                       (set-at-path (nth (car path-remaining) lst)
                                    (cdr path-remaining)))))
          (set-at-path result path))
        result)))

(defun generate-mutants-for-site (site)
  "Generate all possible mutants for a mutation site."
  (let ((op (mutation-site-operator site))
        (bindings (mutation-site-bindings site)))
    (loop for mutation-pattern in (mutation-operator-mutations op)
          for replacement = (substitute-pattern mutation-pattern bindings)
          collect (make-mutant :original (mutation-site-form site)
                               :replacement replacement
                               :operator (mutation-operator-name op)))))

;;;; Exclusion Checking

(defun form-matches-exclusion-p (form exclusion-patterns)
  "Check if FORM matches any exclusion pattern."
  (some (lambda (pattern)
          (pattern-matches-p pattern form))
        exclusion-patterns))

(defun filter-excluded-sites (sites exclusion-patterns)
  "Remove sites that match exclusion patterns."
  (if (null exclusion-patterns)
      sites
      (remove-if (lambda (site)
                   (form-matches-exclusion-p (mutation-site-form site)
                                             exclusion-patterns))
                 sites)))
