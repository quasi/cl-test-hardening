---
type: scenario
name: generator-usage
version: 1.0.0
feature: property
covers:
  - generators
tags:
  - generators
  - data-generation
---

# Generator Usage Scenarios

**Status**: stable
**Source**: `tests/generator-tests.lisp`

## Scenario: Generate Integers in Range

**Given**: Integer generator with bounds
```lisp
(gen:integers :min 0 :max 100)
```

**When**: Values are generated with size 10

**Then**:
- All values are integers
- All values satisfy `0 <= n <= 100`
- Distribution is uniform within range

---

## Scenario: Generate Strings with Constraints

**Given**: String generator with length constraints
```lisp
(gen:strings :min-length 5 :max-length 10 :alphabet "abc")
```

**When**: Strings are generated

**Then**:
- All strings have length between 5 and 10
- All characters are from "abc"
- Empty strings never generated

---

## Scenario: Shrink Integer Toward Zero

**Given**: Integer value 847

**When**: Shrinking is applied

**Then**:
- Produces sequence like [423, 211, 105, 52, 26, 13, 6, 3, 1, 0]
- Each value is simpler than previous
- Sequence terminates at zero

---

## Scenario: Combine Generators with tuple

**Given**: Multiple generators
```lisp
(gen:tuple (gen:integers) (gen:strings) (gen:booleans))
```

**When**: Value is generated

**Then**:
- Result is a list with 3 elements
- First element is integer
- Second element is string
- Third element is boolean

---

## Scenario: Filter with such-that

**Given**: Filtered generator
```lisp
(gen:such-that #'evenp (gen:integers))
```

**When**: Values are generated

**Then**:
- All values are even
- Generation may take multiple attempts
- Throws error if max-tries exceeded

---

## Scenario: Transform with fmap

**Given**: Transformed generator
```lisp
(gen:fmap #'abs (gen:integers))
```

**When**: Values are generated

**Then**:
- All values are non-negative
- Distribution mirrors original but absolute

---

## Verification

Run tests:
```lisp
(5am:run! :th.property-tests)
```
