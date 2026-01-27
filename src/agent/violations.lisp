;;;; Violation types for th.agent
;;;; Defines different categories of verification failures

(in-package #:th.agent)

;;;; Violation Structure

(defstruct violation
  "A verification violation."
  (type nil :type keyword)
  (severity :error :type keyword)  ; :error :warning :info
  (dimension nil :type keyword)    ; :scope :hallucination :style :semantic :complexity
  (file nil :type (or null string))
  (line nil :type (or null integer))
  (column nil :type (or null integer))
  (message nil :type (or null string))
  (suggestion nil :type (or null string))
  (context nil :type list))  ; Additional context data

;;;; Violation Constructors

(defun make-scope-violation (type file &key operation message suggestion)
  "Create a scope violation."
  (make-violation :type type
                  :severity :error
                  :dimension :scope
                  :file file
                  :message (or message (format nil "~A violation for ~A" type file))
                  :suggestion suggestion
                  :context (when operation (list :operation operation))))

(defun make-hallucination-violation (type file line reference &key suggestion confidence)
  "Create a hallucination violation."
  (make-violation :type type
                  :severity (if (and confidence (< confidence 0.8)) :warning :error)
                  :dimension :hallucination
                  :file file
                  :line line
                  :message (format nil "~A '~A' not found" type reference)
                  :suggestion suggestion
                  :context (list :reference reference :confidence confidence)))

(defun make-style-violation (type file line actual expected &key suggestion)
  "Create a style violation."
  (make-violation :type type
                  :severity :warning
                  :dimension :style
                  :file file
                  :line line
                  :message (format nil "~A: expected ~A, got ~A" type expected actual)
                  :suggestion suggestion
                  :context (list :actual actual :expected expected)))

(defun make-semantic-violation (type &key file message suggestion)
  "Create a semantic preservation violation."
  (make-violation :type type
                  :severity :error
                  :dimension :semantic
                  :file file
                  :message message
                  :suggestion suggestion))

(defun make-complexity-violation (type file function-name value limit &key suggestion)
  "Create a complexity violation."
  (make-violation :type type
                  :severity (if (> value (* limit 1.5)) :error :warning)
                  :dimension :complexity
                  :file file
                  :message (format nil "~A '~A': ~A exceeds limit of ~A"
                                   type function-name value limit)
                  :suggestion suggestion
                  :context (list :function function-name :value value :limit limit)))

;;;; Violation Types by Dimension

(defparameter *scope-violation-types*
  '(:out-of-scope          ; File not in allowed paths
    :forbidden-file        ; File in forbidden paths
    :forbidden-operation   ; Operation not allowed
    :unauthorized-delete)) ; Delete without confirmation

(defparameter *hallucination-violation-types*
  '(:undefined-function   ; Function called but not defined
    :undefined-package    ; Package referenced but doesn't exist
    :undefined-class      ; Class referenced but not defined
    :undefined-variable   ; Special variable used but not defined
    :invalid-import))     ; System in quickload doesn't exist

(defparameter *style-violation-types*
  '(:naming-convention    ; Name doesn't match convention
    :indentation          ; Incorrect indentation
    :line-length          ; Line too long
    :missing-documentation ; Required doc missing
    :pattern-mismatch))    ; Doesn't match existing patterns

(defparameter *semantic-violation-types*
  '(:test-failure         ; Existing test failed
    :removed-export       ; Exported symbol removed
    :signature-changed    ; Function signature changed
    :behavior-changed))   ; Behavior detectably different

(defparameter *complexity-violation-types*
  '(:function-too-long    ; Function exceeds line limit
    :complexity-too-high  ; Cyclomatic complexity too high
    :nesting-too-deep     ; Nesting exceeds limit
    :too-many-args))      ; Function has too many arguments

;;;; Violation Filtering

(defun filter-violations-by-severity (violations severity)
  "Filter violations to those at or above SEVERITY."
  (let ((severity-order '(:error :warning :info)))
    (remove-if-not (lambda (v)
                     (<= (position (violation-severity v) severity-order)
                         (position severity severity-order)))
                   violations)))

(defun filter-violations-by-dimension (violations dimension)
  "Filter violations to a specific dimension."
  (remove-if-not (lambda (v) (eq (violation-dimension v) dimension))
                 violations))

(defun filter-violations-by-file (violations file)
  "Filter violations for a specific file."
  (remove-if-not (lambda (v) (equal (violation-file v) file))
                 violations))

;;;; Violation Aggregation

(defun group-violations-by-file (violations)
  "Group violations by file path."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (v violations)
      (push v (gethash (violation-file v) groups)))
    groups))

(defun group-violations-by-dimension (violations)
  "Group violations by dimension."
  (let ((groups (make-hash-table :test 'eq)))
    (dolist (v violations)
      (push v (gethash (violation-dimension v) groups)))
    groups))

(defun count-violations-by-severity (violations)
  "Count violations by severity level."
  (list :errors (count-if (lambda (v) (eq (violation-severity v) :error)) violations)
        :warnings (count-if (lambda (v) (eq (violation-severity v) :warning)) violations)
        :infos (count-if (lambda (v) (eq (violation-severity v) :info)) violations)))
