# Mutation Testing Correctness Properties

**Status**: stable
**Applies to**: Mutation testing system
**Source**: `src/mutation/mutator.lisp`, `src/mutation/runner.lisp`

## Mutant Generation Properties

### P1: Single Mutation Per Mutant

**Property**: Each mutant has exactly one syntactic change

**Formal Statement**:
```
∀ mutant M:
  edit-distance(M.original, M.replacement) = 1
```

**Rationale**: Isolates cause of test pass/fail.

---

### P2: Syntactic Validity

**Property**: All mutants are syntactically valid Lisp

**Formal Statement**:
```
∀ mutant M:
  (read-from-string M.replacement) succeeds
```

**Rationale**: Mutants must be compilable.

---

### P3: Completeness of Mutation

**Property**: All matching sites are mutated

**Formal Statement**:
```
∀ form F, operator O:
  |generate-mutants(F, O)| = count-matches(F, O.pattern)
```

**Rationale**: Thorough coverage of mutation sites.

---

### P4: Deterministic Generation

**Property**: Same code + operators = same mutants

**Formal Statement**:
```
∀ code C, operators O:
  generate-mutants(C, O) = generate-mutants(C, O)
```

**Rationale**: Reproducible mutation analysis.

---

### P5: Operator Independence

**Property**: Each operator creates independent mutants

**Formal Statement**:
```
∀ operators O1, O2:
  generate-mutants(C, [O1, O2]) =
    generate-mutants(C, [O1]) ∪ generate-mutants(C, [O2])
```

**Rationale**: Operators don't interfere.

## Pattern Matching Properties

### P6: Pattern Match Accuracy

**Property**: Pattern only matches intended forms

**Formal Statement**:
```
∀ pattern P, form F:
  pattern-matches-p(P, F) ⟹ F has structure of P
```

**Rationale**: Prevents incorrect mutations.

---

### P7: Variable Binding Consistency

**Property**: Variable bindings are consistent across pattern

**Formal Statement**:
```
∀ pattern with ?var appearing multiple times:
  all occurrences of ?var bind to same value
```

**Example**: Pattern `(+ ?x ?x)` only matches `(+ a a)`, not `(+ a b)`.

## Test Execution Properties

### P8: Isolation Between Mutants

**Property**: Testing one mutant doesn't affect others

**Formal Statement**:
```
∀ mutants M1, M2:
  result(test(M1)) is independent of result(test(M2))
```

**Implementation**: Original code restored between tests.

---

### P9: Exit Code Interpretation

**Property**: Test exit codes correctly determine mutant status

**Formal Statement**:
```
∀ mutant M, test-command T:
  exit-code(T, M) = 0 ⟹ M.status = :killed
  exit-code(T, M) ≠ 0 ⟹ M.status = :survived
```

**Rationale**: Standard Unix convention.

---

### P10: Original Code Restoration

**Property**: Original code is always restored after testing mutant

**Formal Statement**:
```
∀ mutant M, file F:
  after test(M): contents(F) = original-contents(F)
```

**Rationale**: Prevents contamination.

## Score Calculation Properties

### P11: Score Range

**Property**: Mutation score is between 0.0 and 1.0

**Formal Statement**:
```
∀ results R:
  0.0 ≤ calculate-mutation-score(R.killed, R.survived) ≤ 1.0
```

---

### P12: Perfect Score Condition

**Property**: Score is 1.0 iff all mutants killed

**Formal Statement**:
```
score = 1.0 ⟺ survived = 0 ∧ killed > 0
```

---

### P13: Zero Score Condition

**Property**: Score is 0.0 iff no mutants killed

**Formal Statement**:
```
score = 0.0 ⟺ killed = 0 ∧ survived > 0
```

---

### P14: Score Formula

**Property**: Score correctly represents kill rate

**Formal Statement**:
```
score = killed / (killed + survived)
```

**Note**: Timeout and error mutants excluded from calculation.

## Policy Properties

### P15: Threshold Enforcement

**Property**: Policy pass/fail determined by threshold

**Formal Statement**:
```
∀ policy P with threshold T, result R:
  R.passed-p ⟺ R.score ≥ T
```

---

### P16: Operator Set Composition

**Property**: Policy operators are union of specified sets

**Formal Statement**:
```
∀ policy P with operator-sets [S1, S2, ...]:
  P.operators = S1 ∪ S2 ∪ ...
```

## Reporting Properties

### P17: Survivor Traceability

**Property**: Each survived mutant traceable to source

**Formal Statement**:
```
∀ survived mutant M:
  M.file, M.line, M.operator, M.original, M.replacement all recorded
```

**Rationale**: Actionable feedback for test improvement.

---

### P18: Report Completeness

**Property**: Report includes all analysis results

**Formal Statement**:
```
∀ mutation-result R:
  report includes score, killed count, survived count,
  and list of survived mutants with locations
```

## Verification

These properties are verified by:
1. Tests in `tests/mutation-tests.lisp`
2. Integration tests with actual code mutation
3. Score calculation tests

To verify specific property, check test suite.
