;;; ABOUTME: Tests for th.fixture module
;;; Verifies fixture registry, context macros, and composition

(in-package #:th.tests)

(def-suite :th.fixture-tests
  :description "Fixture module tests"
  :in :th.tests)

(in-suite :th.fixture-tests)

;;;; Registry basics

(test define-fixture-registers-fixture
  "define-fixture stores a fixture factory in the registry"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :test-widget ()
    (list :type :widget :id 1))
  (is-true (th.fixture:find-fixture :test-widget)))

(test find-fixture-returns-nil-for-missing
  "find-fixture returns nil for unregistered fixtures"
  (th.fixture:clear-fixtures)
  (is (null (th.fixture:find-fixture :nonexistent))))

(test list-fixtures-returns-all-names
  "list-fixtures returns names of all registered fixtures"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :alpha () :a)
  (th.fixture:define-fixture :beta () :b)
  (let ((names (th.fixture:list-fixtures)))
    (is (= 2 (length names)))
    (is (member :alpha names))
    (is (member :beta names))))

(test clear-fixtures-empties-registry
  "clear-fixtures removes all registered fixtures"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :temp () :temp)
  (is (= 1 (length (th.fixture:list-fixtures))))
  (th.fixture:clear-fixtures)
  (is (= 0 (length (th.fixture:list-fixtures)))))

;;;; Fixture instantiation

(test with-fixture-binds-value
  "with-fixture binds the fixture result to the given variable"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :greeting ()
    "hello")
  (th.fixture:with-fixture (msg :greeting)
    (is (string= "hello" msg))))

(test with-fixture-calls-factory-each-time
  "with-fixture creates a fresh instance on each invocation"
  (th.fixture:clear-fixtures)
  (let ((counter 0))
    (th.fixture:define-fixture :counted ()
      (incf counter)
      counter)
    (th.fixture:with-fixture (a :counted)
      (th.fixture:with-fixture (b :counted)
        (is (/= a b))))))

(test with-fixture-multiple-bindings
  "with-fixture supports multiple fixture bindings"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :name () "alice")
  (th.fixture:define-fixture :age () 30)
  (th.fixture:with-fixture ((n :name) (a :age))
    (is (string= "alice" n))
    (is (= 30 a))))

;;;; Parameterized fixtures

(test define-fixture-with-parameters
  "define-fixture supports parameters for customizable factories"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :sized-list (n)
    (make-list n :initial-element 0))
  (th.fixture:with-fixture (lst :sized-list 5)
    (is (= 5 (length lst)))))

(test define-fixture-with-default-parameters
  "define-fixture parameters can have defaults"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :ranged (&key (min 0) (max 10))
    (list min max))
  (th.fixture:with-fixture (r :ranged)
    (is (equal '(0 10) r)))
  (th.fixture:with-fixture (r :ranged :min 5)
    (is (equal '(5 10) r))))

;;;; Fixture composition

(test fixtures-can-reference-other-fixtures
  "a fixture factory can call build-fixture to compose"
  (th.fixture:clear-fixtures)
  (th.fixture:define-fixture :base-config ()
    '(:host "localhost" :port 8080))
  (th.fixture:define-fixture :full-config ()
    (let ((base (th.fixture:build-fixture :base-config)))
      (append base '(:debug t))))
  (th.fixture:with-fixture (cfg :full-config)
    (is (equal '(:host "localhost" :port 8080 :debug t) cfg))))

;;;; Error handling

(test with-fixture-signals-for-undefined
  "with-fixture signals an error for unregistered fixture names"
  (th.fixture:clear-fixtures)
  (let ((errored (block test-block
                   (handler-bind ((error (lambda (c)
                                          (declare (ignore c))
                                          (return-from test-block t))))
                     (th.fixture:build-fixture :nonexistent)
                     nil))))
    (is-true errored)))
