;;; ABOUTME: Semantic core for th.fixture
;;; Fixture registry, factory functions, and context macros

(in-package #:th.fixture)

;;;; Registry

(defvar *fixtures* (make-hash-table :test 'eq)
  "Registry of fixture factories, keyed by keyword name.")

(defun register-fixture (name factory)
  "Register a fixture factory function under NAME."
  (check-type name keyword)
  (check-type factory function)
  (setf (gethash name *fixtures*) factory))

(defun find-fixture (name)
  "Find a fixture factory by NAME. Returns the factory function or NIL."
  (gethash name *fixtures*))

(defun list-fixtures ()
  "Return a list of all registered fixture names."
  (let ((names '()))
    (maphash (lambda (name factory)
               (declare (ignore factory))
               (push name names))
             *fixtures*)
    names))

(defun clear-fixtures ()
  "Remove all registered fixtures."
  (clrhash *fixtures*))

;;;; Instantiation

(defun build-fixture (name &rest args)
  "Create an instance of fixture NAME, passing ARGS to its factory.
Signals an error if NAME is not registered."
  (let ((factory (find-fixture name)))
    (unless factory
      (error "Fixture ~S is not registered" name))
    (apply factory args)))

;;;; Definition macro

(defmacro define-fixture (name lambda-list &body body)
  "Define a named fixture factory.

NAME: Keyword naming the fixture.
LAMBDA-LIST: Parameters the factory accepts (can be empty).
BODY: Body that returns a fixture value.

Example:
  (define-fixture :test-user (&key (name \"alice\") (age 30))
    (make-user :name name :age age))

  (with-fixture (user :test-user :name \"bob\")
    (is (string= \"bob\" (user-name user))))"
  (let ((docstring (when (stringp (first body))
                     (first body)))
        (actual-body (if (stringp (first body))
                         (rest body)
                         body)))
    `(register-fixture ,name
                       (lambda ,lambda-list
                         ,@(when docstring (list docstring))
                         ,@actual-body))))

;;;; Context macro

(defmacro with-fixture (binding-or-bindings &body body)
  "Execute BODY with fixture bindings in scope.

Single binding:
  (with-fixture (var :fixture-name arg1 arg2) body...)

Multiple bindings:
  (with-fixture ((var1 :fixture1) (var2 :fixture2 arg)) body...)

Each fixture is instantiated fresh via its factory function."
  (let ((bindings (if (symbolp (first binding-or-bindings))
                      ;; Single binding: (var :fix args...)
                      (list binding-or-bindings)
                      ;; Multiple bindings: ((var1 :fix1) (var2 :fix2))
                      binding-or-bindings)))
    `(let ,(mapcar (lambda (binding)
                     (destructuring-bind (var name &rest args) binding
                       `(,var (build-fixture ,name ,@args))))
                   bindings)
       ,@body)))
