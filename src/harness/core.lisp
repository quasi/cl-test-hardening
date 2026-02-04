;;; ABOUTME: Semantic core for th.harness
;;; Declarative test environment setup: systems, file loads, package, suite

(in-package #:th.harness)

;;;; Harness structure

(defstruct (harness (:constructor %make-harness))
  "A test environment configuration."
  (name nil :type keyword :read-only t)
  (systems nil :type list :read-only t)           ; quicklisp systems to load
  (extra-systems nil :type list :read-only t)     ; optional add-on systems
  (load-files nil :type list :read-only t)        ; source files to load in order
  (package nil :type keyword :read-only t))       ; target package for tests

;;;; Registry

(defvar *harnesses* (make-hash-table :test 'eq)
  "Registry of test harness configurations, keyed by keyword name.")

(defun register-harness (harness)
  "Register a harness configuration."
  (setf (gethash (harness-name harness) *harnesses*) harness))

(defun find-harness (name)
  "Find a harness by NAME. Returns the harness struct or NIL."
  (gethash name *harnesses*))

(defun list-harnesses ()
  "Return a list of all registered harness names."
  (let ((names '()))
    (maphash (lambda (name harness)
               (declare (ignore harness))
               (push name names))
             *harnesses*)
    names))

(defun clear-harnesses ()
  "Remove all registered harnesses."
  (clrhash *harnesses*))

;;;; Definition macro

(defmacro define-harness (name &key systems extra-systems load package)
  "Define a named test harness configuration.

NAME: Keyword naming the harness.
SYSTEMS: List of quicklisp system keywords to load.
EXTRA-SYSTEMS: Optional additional systems (for test-specific deps).
LOAD: List of source file paths to load in order.
PACKAGE: Keyword naming the target package for tests.

Example:
  (define-harness :my-project
    :systems (:fiveam :alexandria :serapeum :dexador :yason)
    :load (\"src/package.lisp\" \"src/core.lisp\" \"src/api.lisp\")
    :package :my-project)"
  `(register-harness
    (%make-harness :name ,name
                   :systems ',systems
                   :extra-systems ',extra-systems
                   :load-files ',load
                   :package ,package)))

;;;; Setup form generation

(defun harness-setup-forms (name &key suite-name suite-description
                                   extra-load extra-systems)
  "Generate the list of setup forms for harness NAME.

Returns a list of forms that, when evaluated, establish the test environment:
require, quickload, load files, in-package, def-suite, in-suite.

SUITE-NAME: Symbol for the FiveAM suite (optional).
SUITE-DESCRIPTION: String description (optional).
EXTRA-LOAD: Additional files to load beyond the harness definition.
EXTRA-SYSTEMS: Additional systems to quickload."
  (let ((harness (or (find-harness name)
                     (error "Harness ~S is not registered" name))))
    (let ((all-systems (append (harness-systems harness)
                               (harness-extra-systems harness)
                               extra-systems))
          (all-files (append (harness-load-files harness)
                             extra-load)))
      (append
       ;; 1. Require ASDF
       (list '(require :asdf))
       ;; 2. Quickload all systems
       (when all-systems
         (list `(ql:quickload ',(mapcar (lambda (s)
                                          (if (keywordp s) s
                                              (intern (string s) :keyword)))
                                        all-systems)
                              :silent t)))
       ;; 3. Load source files
       (mapcar (lambda (path) `(load ,path)) all-files)
       ;; 4. Switch to package
       (list `(in-package ,(harness-package harness)))
       ;; 5. Define suite (optional)
       (when suite-name
         (list `(fiveam:def-suite ,suite-name
                  :description ,(or suite-description
                                    (format nil "~A tests" suite-name)))
               `(fiveam:in-suite ,suite-name)))))))

;;;; Setup macro

(defmacro setup (harness-name &key suite-name suite-description
                                extra-load extra-systems)
  "Expand to top-level forms that establish the test environment.

Replaces the typical 20+ line preamble of quickloads, file loads,
package switching, and suite definition.

Example:
  ;; Instead of 25 lines of boilerplate:
  (th.harness:setup :my-project
    :suite-name streaming-suite
    :suite-description \"Streaming tests\"
    :extra-load (\"src/streaming.lisp\"))"
  `(progn
     ,@(harness-setup-forms harness-name
                            :suite-name suite-name
                            :suite-description suite-description
                            :extra-load extra-load
                            :extra-systems extra-systems)))
