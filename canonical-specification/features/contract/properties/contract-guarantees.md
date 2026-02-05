# Contract Testing Guarantees

**Status**: stable
**Applies to**: Contract testing system
**Source**: `src/contract/core.lisp`, `src/contract/verifier.lisp`, `src/contract/mock-provider.lisp`

## Contract Definition Properties

### P1: Consumer-Driven Specification

**Property**: Contracts specify consumer expectations, not provider implementation

**Formal Statement**:
```
∀ contract C:
  C defines WHAT provider must do, not HOW
```

**Rationale**: Enables provider implementation flexibility.

---

### P2: Interaction Completeness

**Property**: Each interaction specifies request and response

**Formal Statement**:
```
∀ interaction I in contract C:
  I.request ≠ nil ∧ I.response ≠ nil
```

**Rationale**: Complete interaction specification required.

---

### P3: Provider State Isolation

**Property**: Provider state (`:given`) affects only that interaction

**Formal Statement**:
```
∀ interactions I1, I2:
  I1.given setup doesn't affect I2 verification
```

**Rationale**: Interactions are independent.

## Matcher Properties

### P4: Matcher Transitivity

**Property**: If value matches matcher, transformed value may or may not match

**Example**:
- Value 123 matches `(type-of integer)`
- But (+ 123 1) = 124 also matches

**Rationale**: Matchers define value sets, not identity.

---

### P5: type-of Correctness

**Property**: `(type-of T)` matches iff value has type T

**Formal Statement**:
```
∀ value V, type T:
  matches(V, (type-of T)) ⟺ (type-of V) = T
```

---

### P6: string-matching Correctness

**Property**: `(string-matching pattern)` matches iff string contains pattern

**Formal Statement**:
```
∀ string S, pattern P:
  matches(S, (string-matching P)) ⟺ (search P S) succeeds
```

---

### P7: Range Matcher Inclusivity

**Property**: Range matchers include both endpoints

**Formal Statement**:
```
∀ value V, min M, max X:
  matches(V, (integer-in-range M X)) ⟺ M ≤ V ≤ X
```

---

### P8: any-value Universality

**Property**: `any-value` matches all values

**Formal Statement**:
```
∀ value V:
  matches(V, any-value) = true
```

---

### P9: optional-field Semantics

**Property**: Optional fields match absence or matcher

**Formal Statement**:
```
∀ field F with matcher M:
  matches({}, (optional-field F M)) = true
  matches({F: V}, (optional-field F M)) = matches(V, M)
```

## Pact Generation Properties

### P10: Pact Format Compliance

**Property**: Generated Pact files follow Pact v3 specification

**Formal Statement**:
```
∀ contract C:
  generate-pact(C) produces valid Pact v3 JSON
```

**Rationale**: Interoperability with Pact ecosystem.

---

### P11: Pact Idempotence

**Property**: Generating Pact multiple times produces identical files

**Formal Statement**:
```
∀ contract C:
  generate-pact(C) = generate-pact(C)
```

**Rationale**: Reproducible contracts.

---

### P12: Pact-Contract Bijection

**Property**: Pact file fully represents contract

**Formal Statement**:
```
∀ contract C:
  read-pact(generate-pact(C)) ≅ C
```

**Rationale**: No information loss in serialization.

## Mock Provider Properties

### P13: Mock Request Matching

**Property**: Mock returns response if request matches interaction

**Formal Statement**:
```
∀ interaction I, request R:
  if matches(R, I.request):
    then mock-response(R) = I.response
```

---

### P14: Mock Unmatched Behavior

**Property**: Mock returns 404 for requests not in contract

**Formal Statement**:
```
∀ request R where ∀ I ∈ contract: ¬matches(R, I.request):
  mock-response(R).status = 404
```

**Rationale**: Fails fast on contract violations.

---

### P15: Mock Interaction Recording

**Property**: Mock records all interactions

**Formal Statement**:
```
∀ request R to mock M:
  R ∈ M.recorded-interactions
```

**Rationale**: Enables verification that all interactions occurred.

---

### P16: Mock Isolation

**Property**: Multiple mocks don't interfere

**Formal Statement**:
```
∀ mocks M1, M2 on different ports:
  requests to M1 don't affect M2.state
```

## Provider Verification Properties

### P17: Verification Completeness

**Property**: All interactions verified

**Formal Statement**:
```
∀ contract C with interactions [I1, ..., In]:
  verify-provider(C) tests all n interactions
```

---

### P18: State Setup Ordering

**Property**: State setup occurs before request

**Formal Statement**:
```
∀ interaction I with :given G:
  execution order: state-setup(G) → send-request(I.request)
```

**Rationale**: Provider must be in correct state.

---

### P19: Verification Independence

**Property**: Each interaction verified independently

**Formal Statement**:
```
∀ interactions I1, I2:
  result(verify(I1)) is independent of result(verify(I2))
```

**Rationale**: Failures isolated to specific interactions.

---

### P20: Matcher Application in Verification

**Property**: Verification uses matchers, not exact equality

**Formal Statement**:
```
∀ interaction I, actual response A:
  verification-passes ⟺ matches(A, I.response)
```

**Rationale**: Flexible matching as specified by contract.

## Schema Validation Properties

### P21: Schema Field Validation

**Property**: Value validates iff all required fields present and match

**Formal Statement**:
```
∀ schema S, value V:
  validates(V, S) ⟺
    (∀ required field F ∈ S: F ∈ V ∧ matches(V[F], F.matcher)) ∧
    (∀ optional field F ∈ S: F ∉ V ∨ matches(V[F], F.matcher))
```

---

### P22: Schema Reusability

**Property**: Schemas can be referenced in multiple contracts

**Formal Statement**:
```
∀ schema S:
  S can appear in multiple contract definitions
```

**Rationale**: DRY principle for common structures.

## Can-I-Deploy Properties

### P23: Deploy Safety

**Property**: Deployment safe iff all consumer contracts verified

**Formal Statement**:
```
can-i-deploy(version V) = true ⟺
  ∀ consumer C: has-verified-contract(C, V)
```

**Rationale**: Prevents breaking consumer deployments.

## Verification

These properties are verified by:
1. Tests in `tests/contract-tests.lisp`
2. Mock provider integration tests
3. Pact file generation tests

To verify specific property, check test suite.
