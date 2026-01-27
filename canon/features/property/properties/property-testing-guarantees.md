# Property Testing Guarantees

**Status**: stable
**Applies to**: Property checking system
**Source**: `src/property/runner.lisp`, `src/property/shrinking.lisp`

## Execution Properties

### P1: Iteration Count Guarantee

**Property**: Exactly N iterations run if no failure

**Formal Statement**:
```
∀ property P, iterations N:
  if check-property(P, :iterations N) passes:
    then actual-iterations = N
```

**Exception**: Early termination on failure.

**Rationale**: Predictable test thoroughness.

---

### P2: Seed Reproducibility

**Property**: Same seed produces identical test sequence

**Formal Statement**:
```
∀ property P, seed S, iterations N:
  check-property(P, :iterations N, :seed S) =
  check-property(P, :iterations N, :seed S)
```

**Rationale**: Enables exact reproduction of failures.

---

### P3: First Failure Stops Execution

**Property**: Testing stops at first counterexample

**Formal Statement**:
```
∀ property P:
  if test case i fails:
    then iterations-run ≤ i
```

**Rationale**: Fast feedback on failures.

---

### P4: Precondition Filtering Correctness

**Property**: Only valid inputs tested when precondition provided

**Formal Statement**:
```
∀ property P with precondition C, test input I:
  if I tested: then C(I) = true
```

**Rationale**: Focus testing on relevant inputs.

## Shrinking Properties

### P5: Shrinking Preserves Failure

**Property**: All shrunk values also fail the property

**Formal Statement**:
```
∀ property P, counterexample C, shrunk S ∈ shrink-path(C):
  P(C) = false ⟹ P(S) = false
```

**Rationale**: Ensures minimal counterexample is valid.

---

### P6: Shrinking Finds Local Minimum

**Property**: Shrinking terminates at local minimum

**Formal Statement**:
```
∀ property P, final shrunk F:
  ∀ S ∈ shrink(F): P(S) = true
```

**Rationale**: Can't shrink further while preserving failure.

---

### P7: Shrinking Monotonic Simplicity

**Property**: Each shrink step produces simpler value

**Formal Statement**:
```
∀ shrink path [V1, V2, ..., Vn]:
  simplicity(Vi+1) > simplicity(Vi)
```

**Rationale**: Progress toward minimal counterexample.

---

### P8: Shrinking Bounded Time

**Property**: Shrinking completes in reasonable time

**Implementation**: Maximum shrink attempts prevents infinite loops.

**Rationale**: Practical usability.

## Result Properties

### P9: Result Completeness

**Property**: Result contains all necessary information

**Formal Statement**:
```
∀ property-result R:
  R.passed-p = true ⟹ R.counterexample = nil
  R.passed-p = false ⟹ (R.counterexample ≠ nil ∨ R.error ≠ nil)
```

**Rationale**: Users can diagnose any outcome.

---

### P10: Counterexample Reproducibility

**Property**: Counterexample can be replayed with seed

**Formal Statement**:
```
∀ result R with R.counterexample = C, R.seed = S:
  replaying with seed S reproduces C
```

**Rationale**: Essential for debugging.

---

### P11: Classification Accuracy

**Property**: Classification counts match actual test cases

**Formal Statement**:
```
∀ result R with classifications:
  Σ(classification-counts) = R.iterations
```

**Rationale**: Classifications provide test distribution insights.

## Registry Properties

### P12: Unique Property Names

**Property**: Property names are unique within package

**Formal Statement**:
```
∀ property names N1, N2 in package P:
  N1 = N2 ⟹ find-property(N1) = find-property(N2)
```

**Rationale**: Unambiguous property references.

---

### P13: Registry Isolation

**Property**: Clearing registry doesn't affect other packages

**Formal Statement**:
```
∀ package P1, P2 where P1 ≠ P2:
  clear-properties(P1) ⟹ properties(P2) unchanged
```

**Rationale**: Package isolation.

## Timeout Properties

### P14: Timeout Enforcement

**Property**: Long-running properties respect timeout

**Formal Statement**:
```
∀ property P with timeout T:
  execution-time(check-property(P)) ≤ T (approximately)
```

**Rationale**: Prevents hanging tests.

## Error Handling

### P15: Exception Capture

**Property**: Exceptions during testing are captured

**Formal Statement**:
```
∀ property P where holds-predicate throws exception E:
  result.error = E
  result.passed-p = false
```

**Rationale**: Exceptions are failures, not crashes.

---

### P16: Graceful Shrinking Failure

**Property**: Shrinking errors don't crash testing

**Formal Statement**:
```
∀ property P with counterexample C:
  if shrink(C) throws error:
    then result.shrunk-counterexample = C (original)
```

**Rationale**: Partial shrinking is better than no result.

## Verification

These properties are verified by:
1. Tests in `tests/property-tests.lisp`
2. Tests in `tests/shrinking-tests.lisp`
3. Integration tests with FiveAM

To verify specific property, check test suite.
