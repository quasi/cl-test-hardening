;;; ABOUTME: Semantic core for th.property
;;; Defines property struct, generator protocol, and property registry

(in-package #:th.property)

;;;; Property Structure

(defstruct (property (:constructor %make-property))
  "A property-based test specification."
  (name nil :type symbol :read-only t)
  (forall-bindings nil :type list :read-only t)  ; ((var generator) ...)
  (holds-predicate nil :type function :read-only t)
  (precondition nil :type (or null function) :read-only t)
  (classifier nil :type (or null function) :read-only t)
  (tags nil :type list :read-only t)
  (timeout nil :type (or null (integer 1)) :read-only t)
  (source-form nil :read-only t))  ; Original form for error reporting

(defun make-property (&key name forall-bindings holds-predicate
                        precondition classifier tags timeout source-form)
  "Create a property. Validates arguments."
  (check-type name symbol)
  (check-type forall-bindings list)
  (check-type holds-predicate function)
  (when precondition (check-type precondition function))
  (when classifier (check-type classifier function))
  (when timeout (check-type timeout (integer 1)))
  (%make-property :name name
                  :forall-bindings forall-bindings
                  :holds-predicate holds-predicate
                  :precondition precondition
                  :classifier classifier
                  :tags tags
                  :timeout timeout
                  :source-form source-form))

;;;; Generator Protocol

(defclass generator ()
  ((name :initarg :name :reader generator-name :type (or null symbol))
   (documentation :initarg :documentation :initform nil :reader generator-documentation))
  (:documentation "Base class for all value generators."))

(defgeneric generate (generator random-state size)
  (:documentation "Generate a random value using GENERATOR.
RANDOM-STATE is the random state to use for generation.
SIZE is a non-negative integer controlling the 'complexity' of generated values."))

(defgeneric shrink (generator value)
  (:documentation "Return a list of values 'smaller' than VALUE.
Used to find minimal counterexamples. Returns empty list if VALUE cannot shrink further."))

;; Default shrink method - no shrinking
(defmethod shrink ((generator generator) value)
  (declare (ignore value))
  nil)

;;;; Property Registry

(defvar *properties* (make-hash-table :test 'eq)
  "Registry of defined properties, keyed by symbol name.")

(defun register-property (property)
  "Register a property in the global registry."
  (setf (gethash (property-name property) *properties*) property))

(defun find-property (name)
  "Find a property by name. Returns NIL if not found."
  (gethash name *properties*))

(defun list-properties (&key package tags)
  "List all registered properties, optionally filtered by package or tags."
  (let ((properties '()))
    (maphash (lambda (name prop)
               (declare (ignore name))
               (when (and (or (null package)
                              (eq (symbol-package (property-name prop)) package))
                          (or (null tags)
                              (intersection tags (property-tags prop))))
                 (push prop properties)))
             *properties*)
    (nreverse properties)))

(defun clear-properties (&optional package)
  "Clear all registered properties, optionally only from PACKAGE."
  (if package
      (maphash (lambda (name prop)
                 (declare (ignore prop))
                 (when (eq (symbol-package name) package)
                   (remhash name *properties*)))
               *properties*)
      (clrhash *properties*)))

;;;; Property Result

(defstruct property-result
  "Result of checking a property."
  (passed-p nil :type boolean)
  (property-name nil :type symbol)
  (iterations 0 :type (integer 0))
  (counterexample nil)           ; Original failing input
  (shrunk-counterexample nil)    ; Minimized failing input
  (seed nil :type (or null integer))
  (error nil)                    ; Error condition if any
  (classifications nil :type list)  ; Input classification stats
  (duration-ms 0 :type (integer 0)))
