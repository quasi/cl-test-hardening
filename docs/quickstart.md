# Quickstart Guide

Get started with cl-test-hardening in 5 minutes.

## Installation

```lisp
(ql:quickload "cl-test-hardening/all")
```

Or load individual modules:

```lisp
(ql:quickload "cl-test-hardening/property")  ; Property-based testing
(ql:quickload "cl-test-hardening/mutation")  ; Mutation testing
(ql:quickload "cl-test-hardening/contract")  ; Contract testing
(ql:quickload "cl-test-hardening/agent")     ; Agent verification
```

## Property-Based Testing (5am Integration)

```lisp
(use-package :th.property)
(use-package :th.gen)

;; Define a property
(defproperty reverse-twice-is-identity (list)
  (equal list (reverse (reverse list))))

;; Check it with generated test cases
(check-property 'reverse-twice-is-identity
                :generator (gen:lists (gen:integers))
                :num-tests 100)
```

**Output:**
```
✓ reverse-twice-is-identity passed 100 tests
```

## Mutation Testing

```lisp
(use-package :th.mutation)

;; Define a policy
(define-mutation-policy arithmetic-check
  :operators '(add-to-sub sub-to-add mul-to-div)
  :threshold 0.8)

;; Run mutation analysis
(run-mutation-analysis
  'arithmetic-check
  '("src/calculator.lisp")
  "sbcl --script run-tests.lisp")
```

**Output:**
```
Mutation Score: 0.85 (85%)
Killed: 85 mutants
Survived: 15 mutants
```

## Contract Testing

```lisp
(use-package :th.contract)

;; Define a contract between services
(define-contract user-api
  :consumer "web-app"
  :provider "user-service"
  :interactions
  ((interaction "get user"
     :request ((:method "GET") (:path "/users/123"))
     :response ((:status 200)
                (:body ((:id 123) (:name "Alice")))))))

;; Test consumer with mock provider
(with-mock-provider ('user-api :port 8080)
  (let ((user (fetch-user 123)))
    (assert (equal "Alice" (user-name user)))))
```

## Agent Verification

```lisp
(use-package :th.agent)

;; Define verification policy
(define-verification-policy strict
  :scope-check t
  :hallucination-check t
  :style-check t)

;; Verify agent-generated code
(verify-code
  :files '("src/generated.lisp")
  :policy 'strict
  :base-commit "HEAD~1")
```

**Output:**
```
✓ Scope: All changes within specified files
✓ Hallucination: No undefined symbols
✗ Style: 3 naming violations
```

## Next Steps

- **Tutorials**: Learn by example
  - [Property-Based Testing Tutorial](tutorials/property-based-testing.md)
  - [Mutation Testing Tutorial](tutorials/mutation-testing.md)
  - [Contract Testing Tutorial](tutorials/contract-testing.md)

- **Reference**: Complete API documentation
  - [API Overview](reference/api-overview.md)

- **Explanation**: Understand the concepts
  - [Why Test Hardening?](explanation/why-test-hardening.md)
  - [Glossary](explanation/glossary.md)

## Test Harness (Eliminate Boilerplate)

```lisp
(use-package :th.harness)

;; Define environment once
(define-harness :my-project
  :systems (:fiveam :alexandria)
  :load ("src/package.lisp" "src/core.lisp")
  :package :my-project.tests)

;; Reuse in every test file
(setup :my-project :suite-name :core-tests)
;; Now you're in the right package with all deps loaded
```

## Test Fixtures (Reusable Test Data)

```lisp
(use-package :th.fixture)

;; Define fixture once
(define-fixture :user (&key (name "Alice") (age 30))
  (make-user :name name :age age))

;; Use in tests
(with-fixture (user :user :name "Bob")
  (assert (string= "Bob" (user-name user))))
```

