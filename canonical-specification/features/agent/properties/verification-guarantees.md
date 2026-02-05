# Agent Verification Guarantees

**Status**: stable
**Applies to**: Agent verification system
**Source**: `src/agent/core.lisp`, `src/agent/*.lisp`

## Policy Properties

### P1: Multi-Dimensional Independence

**Property**: Each verification dimension operates independently

**Formal Statement**:
```
∀ dimensions D1, D2:
  result(D1) is independent of result(D2)
```

**Rationale**: Enables parallel verification and isolated failures.

---

### P2: Dimension Optionality

**Property**: nil rules skip dimension

**Formal Statement**:
```
∀ policy P, dimension D:
  P.rules[D] = nil ⟹ D not verified
```

**Rationale**: Configurable verification depth.

---

### P3: Severity Interpretation

**Property**: Errors fail verification, warnings don't

**Formal Statement**:
```
∀ verification result R:
  R.status = :failed ⟺ ∃ violation V: V.severity = :error
  R.status = :warnings ⟺ (∀ V: V.severity ≠ :error) ∧ (∃ V: V.severity = :warning)
  R.status = :passed ⟺ no violations
```

**Rationale**: Clear pass/warn/fail semantics.

## Scope Verification Properties

### P4: Path Matching Accuracy

**Property**: File matches pattern iff glob pattern matches

**Formal Statement**:
```
∀ file F, pattern P:
  matches(F, P) ⟺ glob-matches(F, P)
```

**Implementation**: Standard glob semantics (`**`, `*`, `?`).

---

### P5: Forbidden Supersedes Allowed

**Property**: File in both allowed and forbidden is forbidden

**Formal Statement**:
```
∀ file F, allowed A, forbidden B:
  (F ∈ A ∧ F ∈ B) ⟹ scope-violation(F)
```

**Rationale**: Safety-first approach.

---

### P6: Scope Completeness

**Property**: Every file is either allowed or forbidden (or neither)

**Formal Statement**:
```
∀ file F:
  (F ∈ allowed) ∨ (F ∈ forbidden) ∨ (F outside both)
```

## Hallucination Detection Properties

### P7: CL Symbol Verification

**Property**: All CL symbols must exist in COMMON-LISP package

**Formal Statement**:
```
∀ symbol S prefixed with cl::
  if S referenced in code:
    then (find-symbol (string S) :cl) must succeed
```

**Rationale**: Catch typos and non-existent APIs.

---

### P8: Project API Verification

**Property**: Project function calls must reference defined functions

**Formal Statement**:
```
∀ function call (F ...):
  if F appears to be project function:
    then F must be defined in project
```

**Rationale**: Detect hallucinated function names.

---

### P9: Package Existence

**Property**: Package references must exist

**Formal Statement**:
```
∀ package reference P:
  (find-package P) must succeed
```

## Style Verification Properties

### P10: Naming Convention Consistency

**Property**: All identifiers follow same convention within file

**Formal Statement**:
```
∀ file F, identifiers I1, I2 in F:
  convention(I1) = convention(I2)
```

**Conventions**: lisp-case, snake_case, camelCase.

---

### P11: Docstring Presence

**Property**: Public functions have docstrings if required

**Formal Statement**:
```
∀ exported function F in file with require-docstrings=true:
  F must have non-empty docstring
```

**Rationale**: Documentation completeness.

---

### P12: Project Style Extraction

**Property**: Detected style matches majority of existing code

**Formal Statement**:
```
∀ project P:
  detect-style(P) = mode(conventions in P.existing-code)
```

## Semantic Verification Properties

### P13: Export Preservation

**Property**: Exported symbols remain exported if required

**Formal Statement**:
```
∀ package P with preserve-exports=true:
  exported-before(P) ⊆ exported-after(P)
```

**Rationale**: Prevents breaking API changes.

---

### P14: Signature Compatibility

**Property**: Function signatures remain compatible if required

**Formal Statement**:
```
∀ function F with preserve-signatures=true:
  arity-compatible(F.old-lambda-list, F.new-lambda-list)
```

**Compatibility**: Optional params can be added, required cannot change.

---

### P15: Contract Preservation

**Property**: Pre/post conditions maintained

**Implementation**: Checks lambda list arity and &key parameter presence.

## Complexity Verification Properties

### P16: Line Count Accuracy

**Property**: Line count excludes comments and blank lines

**Formal Statement**:
```
∀ function F:
  count-lines(F) = |{non-blank, non-comment lines in F}|
```

---

### P17: Nesting Depth Measurement

**Property**: Nesting depth is maximum indent level

**Formal Statement**:
```
∀ function F:
  nesting-depth(F) = max(indent-level(line) for line in F)
```

---

### P18: Cyclomatic Complexity

**Property**: Complexity increases with control flow branches

**Formal Statement**:
```
cyclomatic-complexity(F) = 1 + decision-points(F)
```

Decision points: if, when, unless, cond branches, loop iterations, etc.

---

### P19: Complexity Thresholds

**Property**: Violations occur when limits exceeded

**Formal Statement**:
```
∀ function F, rule R:
  violation ⟺ measure(F) > R.threshold
```

## Violation Properties

### P20: Violation Traceability

**Property**: Every violation traceable to source

**Formal Statement**:
```
∀ violation V:
  V.file ∧ V.dimension ∧ V.message are non-null
```

**Rationale**: Actionable feedback.

---

### P21: Suggestion Availability

**Property**: Violations include suggestions when possible

**Formal Statement**:
```
∀ violation V where fix is mechanical:
  V.suggestion provides remediation guidance
```

**Example**: Hallucination "verify-tokne" suggests "verify-token".

## Result Properties

### P22: Dimension Result Aggregation

**Property**: Overall status is worst dimension status

**Formal Statement**:
```
∀ verification result R:
  R.status = max-severity(R.dimension-results[*].status)
```

Where severity ordering: failed > warnings > passed.

---

### P23: JSON Serialization Completeness

**Property**: JSON export preserves all violation information

**Formal Statement**:
```
∀ result R:
  deserialize(verification-to-json(R)) ≅ R
```

**Rationale**: Tooling integration.

## Git Integration Properties

### P24: Git Diff Accuracy

**Property**: verify-git-changes analyzes only modified files

**Formal Statement**:
```
∀ git-changes verification V:
  V.files = {F | F in git-diff output}
```

**Rationale**: Focus on agent's actual changes.

## Verification

These properties are verified by:
1. Tests in `tests/agent-tests.lisp`
2. Integration tests with real agent-generated code
3. Dimension-specific unit tests

To verify specific property, check test suite.
