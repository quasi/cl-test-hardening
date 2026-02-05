---
type: scenario
name: consumer-testing
version: 1.0.0
feature: contract
covers:
  - contract-definition
  - pact-workflow
tags:
  - consumer
  - mock-provider
---

# Consumer Testing Scenarios

**Status**: stable
**Source**: `tests/contract-tests.lisp`

## Scenario: Define Contract with Interactions

**Given**: Consumer and provider services

**When**: Contract is defined
```lisp
(define-contract user-service-contract
  :consumer "web-app"
  :provider "user-service"
  :interactions
  ((interaction "get user by ID"
     :request ((:method "GET")
               (:path "/users/123"))
     :response ((:status 200)
                (:body ((:id 123)
                       (:name "Alice")))))))
```

**Then**:
- Contract is registered
- Interaction stored with request/response
- Can be found with `find-contract`

**Rationale**: Contracts capture consumer expectations.

---

## Scenario: Generate Pact JSON File

**Given**: Registered contract

**When**: `generate-pact` is called
```lisp
(generate-pact 'user-service-contract :output "pacts/")
```

**Then**:
- File created: `pacts/web-app-user-service.json`
- JSON follows Pact v3 specification
- Interactions converted to Pact format
- Matchers preserved

**Rationale**: Pact files enable provider verification.

---

## Scenario: Consumer Tests with Mock Provider

**Given**: Contract with interaction

**When**: Consumer tests run with mock
```lisp
(with-mock-provider ('user-service-contract :port 8080)
  (let ((response (dex:get "http://localhost:8080/users/123")))
    (assert (= 200 (response-status response)))
    (assert (equal "Alice" (assoc-value (response-body response) :name)))))
```

**Then**:
- Mock provider serves expected response
- Consumer code works against mock
- Tests pass without real provider
- Interactions are recorded

**Rationale**: Mocks enable independent consumer testing.

---

## Scenario: Flexible Matching with Matchers

**Given**: Response with type matcher
```lisp
(:body ((:id '(type-of integer))
        (:email '(string-matching "@"))))
```

**When**: Provider returns `{:id 456, :email "bob@example.com"}`

**Then**:
- Match succeeds (ID is integer, email contains @)
- Exact values not required
- Contract is resilient to data changes

**Rationale**: Matchers prevent brittle contracts.

---

## Scenario: Unmatched Request Returns 404

**Given**: Mock provider with defined interactions

**When**: Consumer makes request not in contract
```lisp
(dex:get "http://localhost:8080/users/999")
```

**Then**:
- Mock returns 404
- Error indicates missing interaction
- Consumer discovers contract gap

**Rationale**: Mocks enforce contract boundaries.

---

## Scenario: Verify All Mock Interactions Occurred

**Given**: Mock provider with 3 interactions

**When**: Consumer tests run but only call 2 interactions

**Then**:
- `verify-mock-interactions` returns false
- Unmatched interaction listed
- Consumer has incomplete coverage

**Rationale**: Ensures all expectations are tested.

---

## Verification

Run tests:
```lisp
(5am:run! :th.contract-tests)
```

All consumer testing scenarios must pass.
