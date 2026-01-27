;;; ABOUTME: DSL macros for property-based testing
;;; Provides define-property, defgenerator, and defshrink macros

(in-package #:th.property)

;;;; define-property macro

(defmacro define-property (name &key for-all (holds nil holds-supplied-p) when classify tags timeout)
  "Define a property-based test.

NAME: Symbol naming the property.
FOR-ALL: List of (variable generator) bindings.
HOLDS: Expression that must evaluate to true for all generated inputs.
WHEN: Optional precondition - only test when this is true.
CLASSIFY: Optional classifier expression for input categorization.
TAGS: List of keyword tags for filtering.
TIMEOUT: Optional timeout in seconds.

Example:
  (define-property list-reverse-involution
    :for-all ((lst (th.gen:lists (th.gen:integers))))
    :holds (equal lst (reverse (reverse lst))))

  (define-property sorted-list-first-is-minimum
    :for-all ((lst (th.gen:lists (th.gen:integers :min -100 :max 100))))
    :when (not (null lst))
    :holds (= (first (sort (copy-list lst) #'<))
              (reduce #'min lst))
    :tags (:sorting :invariant))"
  (unless for-all
    (error "define-property requires :for-all bindings"))
  (unless holds-supplied-p
    (error "define-property requires :holds predicate"))

  ;; Validate for-all syntax
  (dolist (binding for-all)
    (unless (and (listp binding) (= 2 (length binding)) (symbolp (first binding)))
      (error "Invalid :for-all binding: ~S. Expected (variable generator)" binding)))

  (let ((var-names (mapcar #'first for-all))
        (generators (mapcar #'second for-all)))
    (declare (ignore generators))
    `(progn
       (register-property
        (make-property
         :name ',name
         :forall-bindings (list ,@(loop for (var gen) in for-all
                                        collect `(list ',var ,gen)))
         :holds-predicate (lambda ,var-names
                            (declare (ignorable ,@var-names))
                            ,holds)
         :precondition ,(when when
                          `(lambda ,var-names
                             (declare (ignorable ,@var-names))
                             ,when))
         :classifier ,(when classify
                        `(lambda ,var-names
                           (declare (ignorable ,@var-names))
                           ,classify))
         :tags ',tags
         :timeout ,timeout
         :source-form '(define-property ,name
                         :for-all ,for-all
                         :holds ,holds
                         ,@(when when `(:when ,when))
                         ,@(when classify `(:classify ,classify))
                         ,@(when tags `(:tags ,tags))
                         ,@(when timeout `(:timeout ,timeout)))))
       ',name)))

;;;; defgenerator macro

(defmacro defgenerator (name lambda-list &body body)
  "Define a custom generator function.

NAME: Symbol naming the generator function.
LAMBDA-LIST: Arguments the generator accepts.
BODY: Body that returns a generator object.

Inside BODY, you can use other generators and combinators.

Example:
  (defgenerator point ()
    \"Generate a 2D point.\"
    (th.gen:tuple (th.gen:integers :min -100 :max 100)
                  (th.gen:integers :min -100 :max 100)))

  (defgenerator user ()
    \"Generate a user record.\"
    (th.gen:fmap (lambda (name age)
                   (make-user :name name :age age))
                 (th.gen:strings :min-length 1 :max-length 50)
                 (th.gen:integers :min 0 :max 120)))"
  (let ((docstring (when (stringp (first body))
                     (first body)))
        (actual-body (if (stringp (first body))
                         (rest body)
                         body)))
    `(defun ,name ,lambda-list
       ,@(when docstring (list docstring))
       ,@actual-body)))

;;;; defshrink macro

(defmacro defshrink (type-name (value-var) &body body)
  "Define a custom shrink function for TYPE-NAME.

TYPE-NAME: Symbol identifying the type (used in registry lookup).
VALUE-VAR: Variable bound to the value being shrunk.
BODY: Body returning list of smaller values.

Example:
  (defshrink point (p)
    \"Shrink a point toward origin.\"
    (let ((x (point-x p))
          (y (point-y p)))
      (list (make-point :x 0 :y 0)
            (make-point :x (floor x 2) :y y)
            (make-point :x x :y (floor y 2)))))"
  (let ((docstring (when (stringp (first body))
                     (first body)))
        (actual-body (if (stringp (first body))
                         (rest body)
                         body)))
    `(progn
       (register-shrink ',type-name
                        (lambda (,value-var)
                          ,@(when docstring (list docstring))
                          ,@actual-body))
       ',type-name)))

;;;; Convenience Macros

(defmacro with-property-context ((&key seed size) &body body)
  "Execute BODY with property testing context established.
Useful for testing generators directly."
  `(let* ((*test-random-state* (make-random-state-from-seed ,(or seed '(generate-seed))))
          (*current-size* ,(or size 30)))
     ,@body))
