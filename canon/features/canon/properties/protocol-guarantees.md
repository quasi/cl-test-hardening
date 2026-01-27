# Canon Protocol Guarantees

**Status**: stable
**Applies to**: Canon verification protocol
**Source**: `src/canon/adapter.lisp`

## Protocol Properties

### P1: Result File Always Written

**Property**: Verification always produces result file

**Formal Statement**:
```
∀ verification V:
  run-verification(V) completes ⟹ result-file exists
```

**Rationale**: Canon needs result even on partial failure.

---

### P2: JSON Round-Trip Validity

**Property**: Request/result JSON is valid

**Formal Statement**:
```
∀ request R:
  parse-json(R) succeeds ⟹ write-json(result) produces valid JSON
```

---

### P3: Test Count Accuracy

**Property**: Result counts match actual executions

**Formal Statement**:
```
∀ test results R:
  R.passed + R.failed + R.skipped = R.total-executed
```

---

### P4: Timestamp Monotonicity

**Property**: Result timestamp after request time

**Rationale**: Temporal ordering for tracking.

---

### P5: Duration Accuracy

**Property**: Duration measures actual verification time

**Implementation**: `measure-time` from th.core.

## Verification

Integration tests with Canon toolkit.
