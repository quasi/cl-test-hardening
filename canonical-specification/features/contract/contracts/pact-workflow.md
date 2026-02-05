---
type: contract
name: pact-workflow
version: 1.0.0
feature: contract
---

# Pact Workflow

**Status**: stable
**Package**: `th.contract`
**Source**: `src/contract/pact-generator.lisp`, `src/contract/mock-provider.lisp`, `src/contract/verifier.lisp`

## Overview

The Pact workflow enables consumer and provider teams to work independently by using contract files as the integration point.

## Workflow Phases

### Phase 1: Consumer Defines Contract

Consumer team defines expectations:

```lisp
(define-contract payment-service-contract
  :consumer "checkout-app"
  :provider "payment-service"
  :interactions
  ((interaction "process payment"
     :given "user has valid payment method"
     :request ((:method "POST")
               (:path "/payments")
               (:body ((:amount 1000)
                       (:currency "USD"))))
     :response ((:status 200)
                (:body ((:id '(type-of string))
                        (:status "success")))))))
```

### Phase 2: Generate Pact File

```lisp
(generate-pact 'payment-service-contract :output "pacts/")
```

Creates `pacts/checkout-app-payment-service.json`.

### Phase 3: Consumer Tests with Mock

Consumer runs tests against mock provider:

```lisp
(with-mock-provider ('payment-service-contract :port 8080)
  ;; Run consumer tests
  (test-checkout-flow))
```

### Phase 4: Share Pact File

Consumer shares pact file with provider team (via git, Pact Broker, etc.).

### Phase 5: Provider Verifies

Provider team verifies they meet the contract:

```lisp
(verify-provider 'payment-service-contract
                 :base-url "http://localhost:3000"
                 :state-setup #'setup-provider-state)
```

## Generate Pact

### generate-pact

```lisp
(defun generate-pact (contract-name &key output) => pathname)
```

**Purpose**: Generate Pact JSON file from contract.

**Parameters**:
- `contract-name`: Name of registered contract
- `:output`: Output directory (default: `./pacts/`)

**Returns**: Path to generated Pact file.

**File Format**: Pact v3 specification JSON.

**Contract**:
- Creates file named `{consumer}-{provider}.json`
- Includes metadata (version, creation date)
- All interactions converted to Pact format
- Matchers preserved in Pact matcher syntax

**Example Output**:
```json
{
  "consumer": {"name": "checkout-app"},
  "provider": {"name": "payment-service"},
  "interactions": [...],
  "metadata": {
    "pactSpecification": {"version": "3.0.0"}
  }
}
```

## Mock Provider

### with-mock-provider

```lisp
(with-mock-provider (contract-name &key port) &body body)
```

**Purpose**: Run code with mock provider serving contract interactions.

**Parameters**:
- `contract-name`: Name of contract to mock
- `:port`: Port for mock server (default: random)

**Body**: Code that makes requests to mock provider.

**Contract**:
- Mock server started before body executes
- Server shut down after body completes
- Interactions recorded for verification
- Unmatched requests return 404

**Example**:
```lisp
(with-mock-provider ('user-service-contract :port 8080)
  (let ((response (dex:get "http://localhost:8080/users/123")))
    (assert (= 200 (response-status response)))))
```

### create-mock-provider

```lisp
(defun create-mock-provider (contract-name &key port) => mock-instance)
```

**Purpose**: Create mock provider without automatic cleanup.

**Returns**: Mock provider instance (must be manually shut down).

### mock-request

```lisp
(defun mock-request (mock method path &key query headers body) => response)
```

**Purpose**: Make request to mock provider programmatically.

### verify-mock-interactions

```lisp
(defun verify-mock-interactions (mock) => (values boolean mismatches))
```

**Purpose**: Verify all expected interactions occurred.

**Returns**:
- `boolean`: t if all interactions matched
- `mismatches`: List of unmatched interactions

## Provider Verification

### verify-provider

```lisp
(defun verify-provider (contract-name &key base-url state-setup) => contract-verification-result)
```

**Purpose**: Verify provider implementation against contract.

**Parameters**:
- `contract-name`: Name of contract
- `:base-url`: Provider base URL (e.g., `http://localhost:3000`)
- `:state-setup`: Function `(lambda (given-state) ...)` to set up provider state

**Workflow**:
1. For each interaction:
   - Call state-setup with `:given` clause
   - Make HTTP request to provider
   - Verify response matches expectations
2. Collect results
3. Return verification result

**Contract**:
- Provider must be running at `base-url`
- State setup must complete before request
- Response matching uses contract matchers
- Failures include detailed mismatch info

**Example**:
```lisp
(defun setup-payment-state (given)
  (case given
    ("user has valid payment method"
     (seed-database :user-id 123 :payment-method "card"))))

(verify-provider 'payment-service-contract
                 :base-url "http://localhost:3000"
                 :state-setup #'setup-payment-state)
```

### Verification Result

```lisp
(defstruct contract-verification-result
  passed
  failed
  errors)
```

**Accessors**:
- `contract-verification-result-passed`: List of passed interactions
- `contract-verification-result-failed`: List of failed interactions
- `contract-verification-result-errors`: List of error messages

### format-verification-report

```lisp
(defun format-verification-report (result stream))
```

**Purpose**: Generate human-readable verification report.

**Output Format**:
```
Contract Verification Results
==============================
Provider: payment-service
Consumer: checkout-app

Passed: 8/10 interactions

Failed Interactions:
--------------------
1. "process refund"
   Expected status: 200
   Actual status: 500

2. "get payment status"
   Expected body.status: "success"
   Actual body.status: "pending"
```

## Can I Deploy?

### can-i-deploy

```lisp
(defun can-i-deploy (contract-name version) => boolean)
```

**Purpose**: Check if it's safe to deploy consumer/provider.

**Parameters**:
- `contract-name`: Contract to check
- `version`: Version to deploy

**Returns**: `t` if safe to deploy, `nil` otherwise.

**Contract**:
- Checks if provider has verified this contract version
- Placeholder implementation (requires Pact Broker integration)

## Best Practices

1. **Start with happy path**: Define successful interactions first
2. **Add error cases**: Include 4xx, 5xx responses
3. **Use matchers**: Avoid brittle exact matches
4. **Version contracts**: Track contract evolution
5. **Automate verification**: Run in CI/CD pipeline

## Workflow Summary

```
Consumer Side:
1. define-contract
2. generate-pact
3. with-mock-provider + tests
4. Share pact file

Provider Side:
1. Receive pact file
2. verify-provider
3. Fix mismatches
4. can-i-deploy check
```

## References

- Implementation: `src/contract/pact-generator.lisp`, `src/contract/mock-provider.lisp`, `src/contract/verifier.lisp`
- Contract definition: `contract-definition.md`
- Package: `src/contract/package.lisp`
- Tests: `tests/contract-tests.lisp`
