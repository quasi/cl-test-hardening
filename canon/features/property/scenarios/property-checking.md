# Property Checking Scenarios

**Status**: stable
**Source**: `tests/property-tests.lisp`
**Contracts**: `generators.md`, `properties.md`

## Scenario: Define and Find Property

**Given**: Property system is initialized

**When**: User defines a property
```lisp
(define-property test-property-1
  :for-all ((x (gen:integers :min 0 :max 100)))
  :holds (>= x 0))
```

**Then**:
- Property is registered in system
- `(find-property 'test-property-1)` returns the property
- Property name matches the defined name

**Rationale**: Properties must be findable after definition for testing.

---

## Scenario: Passing Property Returns Success

**Given**: Property that always holds
```lisp
(define-property reverse-involution
  :for-all ((lst (gen:lists (gen:integers))))
  :holds (equal lst (reverse (reverse lst))))
```

**When**: Property is checked with seed 12345 for 50 iterations

**Then**:
- Result indicates pass (`property-result-passed-p` is `t`)
- Iterations count is 50
- No counterexample exists
- Seed is recorded for reproducibility

**Rationale**: Correct properties should pass consistently.

---

## Scenario: Failing Property Returns Counterexample

**Given**: Property that never holds
```lisp
(define-property always-false
  :for-all ((x (gen:integers :min 1 :max 100)))
  :holds nil)
```

**When**: Property is checked

**Then**:
- Result indicates failure (`property-result-passed-p` is `nil`)
- Counterexample is provided
- Original failing value is recorded
- Shrunk counterexample shows minimal failing case

**Rationale**: Failures must include actionable counterexamples.

---

## Scenario: Shrinking Finds Minimal Counterexample

**Given**: Property fails for some values
```lisp
(define-property fails-for-large
  :for-all ((x (gen:integers :min 0 :max 1000)))
  :holds (< x 10))
```

**When**: Property is checked and fails

**Then**:
- Original counterexample might be large (e.g., 847)
- Shrunk counterexample is minimal (10, the boundary)
- Shrinking path is deterministic

**Rationale**: Minimal counterexamples are easier to debug.

---

## Scenario: Reproducible Failure with Seed

**Given**: Property with failing test case

**When**: Property is checked with specific seed (e.g., 12345)

**Then**:
- Same counterexample is found on repeated runs
- Same shrinking path is taken
- Result is deterministic

**Rationale**: Reproducibility is essential for debugging.

---

## Scenario: Property with Precondition

**Given**: Property with precondition filter
```lisp
(define-property sorted-lists-stay-sorted
  :for-all ((lst (gen:lists (gen:integers))))
  :pre (not (null lst))
  :holds (apply #'<= (sort (copy-list lst) #'<)))
```

**When**: Property is checked

**Then**:
- Only non-empty lists are tested
- Empty lists are filtered by precondition
- Iterations count only tested cases

**Rationale**: Preconditions focus testing on valid inputs.

---

## Scenario: Check All Properties

**Given**: Multiple properties registered

**When**: `check-all-properties` is called

**Then**:
- All properties are tested
- Individual results returned in list
- Each property runs independently

**Rationale**: Bulk testing for test suite integration.

---

## Verification

Run tests:
```lisp
(5am:run! :th.property-tests)
```

All property checking scenarios must pass.
