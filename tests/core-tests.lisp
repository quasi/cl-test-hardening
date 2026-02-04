;;; ABOUTME: Tests for th.core utilities
;;; Verifies with-internals macro and core infrastructure

(in-package #:th.tests)

(def-suite :th.core-tests
  :description "Core module tests"
  :in :th.tests)

(in-suite :th.core-tests)

;;;; Test target package with known internals

(defpackage #:th.core-tests/target
  (:use #:cl)
  (:export #:public-fn))

(in-package #:th.core-tests/target)

(defun public-fn () :public)
(defun private-fn () :private)
(defvar *private-var* 42)
(defclass internal-thing ()
  ((value :initarg :value :accessor thing-value)))

(in-package #:th.tests)

;;;; with-internals tests

(test with-internals-resolves-functions
  "with-internals makes internal functions callable without :: qualification"
  (th.core:with-internals :th.core-tests/target (private-fn)
    (is (eq :private (private-fn)))))

(test with-internals-resolves-special-variables
  "with-internals makes internal special variables accessible"
  (th.core:with-internals :th.core-tests/target (*private-var*)
    (is (= 42 *private-var*))))

(test with-internals-resolves-class-names
  "with-internals makes internal class names usable in make-instance and typep"
  (th.core:with-internals :th.core-tests/target (internal-thing thing-value)
    (let ((obj (make-instance 'internal-thing :value 99)))
      (is (typep obj 'internal-thing))
      (is (= 99 (thing-value obj))))))

(test with-internals-resolves-multiple-symbols
  "with-internals handles multiple symbols at once"
  (th.core:with-internals :th.core-tests/target (private-fn *private-var* internal-thing)
    (is (eq :private (private-fn)))
    (is (= 42 *private-var*))
    (is (typep (make-instance 'internal-thing :value 1) 'internal-thing))))

(test with-internals-signals-error-for-missing-symbol
  "with-internals signals an error at macroexpansion for nonexistent symbols"
  (let ((errored (block test-block
                   (handler-bind ((error (lambda (c)
                                          (declare (ignore c))
                                          (return-from test-block t))))
                     (macroexpand-1 '(th.core:with-internals :th.core-tests/target (no-such-symbol)
                                       no-such-symbol))
                     nil))))
    (is-true errored)))
