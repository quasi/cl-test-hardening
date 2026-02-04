;;;; ASDF System Definition for cl-test-hardening
;;;; Test Hardening Framework - Advanced Testing Capabilities for Common Lisp

(defsystem "cl-test-hardening"
  :description "Advanced test hardening framework for Common Lisp - Core module"
  :author "quasiLabs"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("alexandria")
  :pathname "src/core"
  :serial t
  :components ((:file "package")
               (:file "results")
               (:file "reporting")))

;;; Property Testing Module
(defsystem "cl-test-hardening/property"
  :description "Property-based testing module with generators and shrinking"
  :depends-on ("cl-test-hardening")
  :pathname "src/property"
  :serial t
  :components ((:file "package")
               (:file "random")
               (:file "core")
               (:file "shrinking")
               (:file "runner")
               (:file "reporting")
               (:file "macros"))
  :in-order-to ((test-op (test-op "cl-test-hardening/tests"))))

;;; Property Generators - Part of property module
(defsystem "cl-test-hardening/generators"
  :description "Standard generators for property-based testing"
  :depends-on ("cl-test-hardening/property")
  :pathname "generators"
  :serial t
  :components ((:file "primitives")
               (:file "collections")
               (:file "combinators")))

;;; Mutation Testing Module
(defsystem "cl-test-hardening/mutation"
  :description "Mutation testing module for test suite quality assessment"
  :depends-on ("cl-test-hardening" "cl-ppcre")
  :pathname "src/mutation"
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "pattern-matching")
               (:file "mutator")
               (:file "runner")
               (:file "reporting")
               (:file "macros"))
  :in-order-to ((test-op (test-op "cl-test-hardening/tests"))))

;;; Mutation Operators - Part of mutation module
(defsystem "cl-test-hardening/operators"
  :description "Standard mutation operators"
  :depends-on ("cl-test-hardening/mutation")
  :pathname "operators"
  :components ((:file "standard")))

;;; Contract Testing Module
(defsystem "cl-test-hardening/contract"
  :description "Consumer-driven contract testing with Pact-style workflows"
  :depends-on ("cl-test-hardening"
               "cl-ppcre"
               "yason"
               "dexador"
               "local-time")
  :pathname "src/contract"
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "matchers")
               (:file "schema")
               (:file "interaction")
               (:file "pact-generator")
               (:file "mock-provider")
               (:file "verifier")
               (:file "reporting")
               (:file "macros"))
  :in-order-to ((test-op (test-op "cl-test-hardening/tests"))))

;;; Agent Verification Module
(defsystem "cl-test-hardening/agent"
  :description "Agent-generated code verification across five dimensions"
  :depends-on ("cl-test-hardening" "cl-ppcre" "uiop")
  :pathname "src/agent"
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "violations")
               (:file "scope")
               (:file "hallucination")
               (:file "style")
               (:file "semantics")
               (:file "complexity")
               (:file "reporting")
               (:file "macros"))
  :in-order-to ((test-op (test-op "cl-test-hardening/tests"))))

;;; Canon Adapter - Integration with Canon specification system
(defsystem "cl-test-hardening/canon"
  :description "Canon specification adapter for cl-test-hardening"
  :depends-on ("cl-test-hardening"
               "cl-test-hardening/property"
               "cl-test-hardening/generators"
               "cl-test-hardening/mutation"
               "cl-test-hardening/operators"
               "cl-test-hardening/contract"
               "cl-test-hardening/agent"
               "yason"
               "fiveam")
  :pathname "src/canon"
  :serial t
  :components ((:file "package")
               (:file "adapter")))

;;; Convenience System - Loads Everything
(defsystem "cl-test-hardening/all"
  :description "Load all cl-test-hardening modules"
  :depends-on ("cl-test-hardening"
               "cl-test-hardening/property"
               "cl-test-hardening/generators"
               "cl-test-hardening/mutation"
               "cl-test-hardening/operators"
               "cl-test-hardening/contract"
               "cl-test-hardening/agent"))

;;; Test System
(defsystem "cl-test-hardening/tests"
  :description "Test suite for cl-test-hardening"
  :depends-on ("cl-test-hardening/all" "fiveam")
  :pathname "tests"
  :serial t
  :components ((:file "test-package")
               (:file "core-tests")
               (:file "generator-tests")
               (:file "shrinking-tests")
               (:file "property-tests")
               (:file "mutation-tests")
               (:file "contract-tests")
               (:file "agent-tests"))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :th.tests)))
