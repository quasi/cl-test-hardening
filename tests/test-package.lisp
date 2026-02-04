;;;; Test suite package definition

(defpackage #:th.tests
  (:use #:cl #:fiveam))

(in-package #:th.tests)

;; Root test suite - individual test files add themselves as children
(def-suite :th.tests
  :description "All cl-test-hardening tests")

;; Parent suites for each module - child suites defined in respective test files
(def-suite :th.core-tests
  :description "Core module tests"
  :in :th.tests)

(def-suite :th.property-tests
  :description "Property testing module tests"
  :in :th.tests)
