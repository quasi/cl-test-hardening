# Quickstart

Get started with cl-test-hardening in under 5 minutes.

## Prerequisites

- Common Lisp implementation (SBCL recommended)
- Quicklisp installed

## Installation

```lisp
;; From your REPL
(ql:quickload "cl-test-hardening/all")
```

## Your First Property-Based Test

Property-based testing verifies invariants hold across many random inputs.

```lisp
;; Load the property module
(ql:quickload "cl-test-hardening/property")

;; Define a simple property: reversing twice gives back original
(th.property:defproperty reverse-twice-is-identity
  (list)
  (equal list (reverse (reverse list))))

;; Generate test data: lists of integers
(defparameter *list-gen*
  (th.property:gen-list (th.property:gen-integer 0 100)))

;; Run the test
(th.property:check-property 'reverse-twice-is-identity *list-gen*)
```

**Expected output**:
```
✓ Property holds for all 100 test cases
```

## Your First Mutation Test

Mutation testing checks if your tests actually catch bugs.

```lisp
;; Load the mutation module
(ql:quickload "cl-test-hardening/mutation")

;; Assume you have a function and tests
(defun add (a b) (+ a b))

;; Define test
(fiveam:test test-add
  (fiveam:is (= 5 (add 2 3)))
  (fiveam:is (= 0 (add 0 0))))

;; Run mutation testing
(th.mutation:mutate-and-test
  :target-function 'add
  :test-suite 'test-add
  :operators '(:flip-arithmetic))
```

**Expected output**:
```
Mutation Score: 100% (2/2 mutants killed)
✓ All mutants caught by tests
```

## Your First Contract Test

Contract testing verifies consumer-provider interactions.

```lisp
;; Load the contract module
(ql:quickload "cl-test-hardening/contract")

;; Define a contract
(th.contract:define-interaction "create-user"
  :request (th.contract:make-request
             :method :post
             :path "/users"
             :body '(("name" . "Alice")))
  :response (th.contract:make-response
              :status 201
              :body '(("id" . 123)
                     ("name" . "Alice"))))

;; Test consumer against mock
(th.contract:with-mock-provider (mock)
  (th.contract:add-interaction mock "create-user")
  ;; Your consumer code here
  (my-client:create-user "Alice"))

;; Generate Pact file
(th.contract:write-pact "consumer-provider.json")
```

## Next Steps

- **Learn property-based testing**: See `tutorials/property-based-testing.md`
- **Learn mutation testing**: See `tutorials/mutation-testing.md`
- **Learn contract testing**: See `tutorials/contract-testing.md`
- **API reference**: See `reference/` directory
- **Conceptual guides**: See `explanation/` directory
