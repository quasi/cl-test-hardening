;;; ABOUTME: Tests for th.harness module
;;; Verifies declarative test environment setup

(in-package #:th.tests)

(def-suite :th.harness-tests
  :description "Harness module tests"
  :in :th.tests)

(in-suite :th.harness-tests)

;;;; Harness definition and registry

(test define-harness-registers-harness
  "define-harness stores a harness configuration in the registry"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :test-h
    :systems (:alexandria)
    :load ()
    :package :cl-user)
  (is-true (th.harness:find-harness :test-h)))

(test find-harness-returns-nil-for-missing
  "find-harness returns nil for unregistered harness"
  (th.harness:clear-harnesses)
  (is (null (th.harness:find-harness :nonexistent))))

(test list-harnesses-returns-all-names
  "list-harnesses returns names of all registered harnesses"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :h1 :systems () :load () :package :cl-user)
  (th.harness:define-harness :h2 :systems () :load () :package :cl-user)
  (let ((names (th.harness:list-harnesses)))
    (is (= 2 (length names)))
    (is (member :h1 names))
    (is (member :h2 names))))

(test clear-harnesses-empties-registry
  "clear-harnesses removes all registered harnesses"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :temp :systems () :load () :package :cl-user)
  (is (= 1 (length (th.harness:list-harnesses))))
  (th.harness:clear-harnesses)
  (is (= 0 (length (th.harness:list-harnesses)))))

;;;; Harness configuration access

(test harness-stores-systems
  "harness configuration preserves the systems list"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :h
    :systems (:alexandria :serapeum)
    :load ()
    :package :cl-user)
  (let ((h (th.harness:find-harness :h)))
    (is (equal '(:alexandria :serapeum) (th.harness:harness-systems h)))))

(test harness-stores-load-files
  "harness configuration preserves the load file list"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :h
    :systems ()
    :load ("src/a.lisp" "src/b.lisp")
    :package :cl-user)
  (let ((h (th.harness:find-harness :h)))
    (is (equal '("src/a.lisp" "src/b.lisp") (th.harness:harness-load-files h)))))

(test harness-stores-package
  "harness configuration preserves the target package"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :h
    :systems ()
    :load ()
    :package :cl-user)
  (let ((h (th.harness:find-harness :h)))
    (is (eq :cl-user (th.harness:harness-package h)))))

(test harness-stores-extra-systems
  "define-harness supports :extra-systems for optional add-ons"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :h
    :systems (:alexandria)
    :extra-systems (:cl-ppcre)
    :load ()
    :package :cl-user)
  (let ((h (th.harness:find-harness :h)))
    (is (equal '(:cl-ppcre) (th.harness:harness-extra-systems h)))))

;;;; Setup expansion

(test harness-setup-generates-load-forms
  "harness-setup-forms produces the right structure"
  (th.harness:clear-harnesses)
  (th.harness:define-harness :h
    :systems (:alexandria)
    :load ("src/a.lisp")
    :package :cl-user)
  (let ((forms (th.harness:harness-setup-forms :h
                 :suite-name 'my-suite
                 :suite-description "Test suite")))
    ;; Should include require, quickload, load, in-package, def-suite, in-suite
    (is (listp forms))
    (is (>= (length forms) 4))))

(test harness-setup-signals-for-undefined
  "harness-setup-forms signals an error for unregistered harness"
  (th.harness:clear-harnesses)
  (let ((errored (block test-block
                   (handler-bind ((error (lambda (c)
                                          (declare (ignore c))
                                          (return-from test-block t))))
                     (th.harness:harness-setup-forms :nonexistent)
                     nil))))
    (is-true errored)))
