;;;; Test suite package definition

(defpackage #:th.tests
  (:use #:cl #:fiveam))

(in-package #:th.tests)

(def-suite :th.tests
  :description "All cl-test-hardening tests")

(def-suite :th.property-tests
  :description "Property testing module tests"
  :in :th.tests)

(def-suite :th.mutation-tests
  :description "Mutation testing module tests"
  :in :th.tests)

(def-suite :th.contract-tests
  :description "Contract testing module tests"
  :in :th.tests)

(def-suite :th.agent-tests
  :description "Agent verification module tests"
  :in :th.tests)
