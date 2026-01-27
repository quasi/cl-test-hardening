# Provider Verification Scenarios

**Status**: stable
**Source**: `tests/contract-tests.lisp`
**Contracts**: `pact-workflow.md`

## Scenario: Provider Meets Contract

**Given**: Provider implementation running at `http://localhost:3000`

**And**: Contract received from consumer
```lisp
(define-contract payment-contract
  :consumer "checkout"
  :provider "payment-service"
  :interactions
  ((interaction "process payment"
     :given "user has valid payment method"
     :request ((:method "POST")
               (:path "/payments")
               (:body ((:amount 1000))))
     :response ((:status 200)
                (:body ((:status "success")))))))
```

**When**: Provider verification runs
```lisp
(verify-provider 'payment-contract
                 :base-url "http://localhost:3000"
                 :state-setup #'setup-state)
```

**Then**:
- State setup called with "user has valid payment method"
- HTTP POST to `/payments` succeeds
- Response matches expected structure
- Verification passes

**Rationale**: Provider confirms it meets consumer expectations.

---

## Scenario: Provider State Setup

**Given**: Interaction with precondition `:given "user 123 exists"`

**When**: Verification runs

**Then**:
- State setup function called before request
- Setup seeds database/prepares system
- Request succeeds with prepared state

**Rationale**: State setup enables deterministic verification.

---

## Scenario: Provider Verification Failure

**Given**: Provider returns incorrect response

**When**: Verification runs

**Then**:
- Verification fails for that interaction
- Mismatch details provided:
  - Expected: `{:status "success"}`
  - Actual: `{:status "pending"}`
- Failed interaction listed in report

**Rationale**: Failures pinpoint contract violations.

---

## Scenario: Matcher Validation in Verification

**Given**: Contract expects `(:id '(type-of integer))`

**When**: Provider returns `{:id "123"}` (string instead of integer)

**Then**:
- Type matcher fails
- Error indicates type mismatch
- Provider fix required

**Rationale**: Matchers validate response structure.

---

## Scenario: Can I Deploy Check

**Given**: Multiple consumer contracts verified

**When**: `can-i-deploy` is called for provider version

**Then**:
- Check confirms all consumer contracts verified
- Safe to deploy

**Or**: If any verification failed

**Then**:
- Check returns false
- Deploy blocked to prevent breaking consumers

**Rationale**: Prevents breaking changes in production.

---

## Verification

Run tests:
```lisp
(5am:run! :th.contract-tests)
```

All provider verification scenarios must pass.
