# Generator Invariants

**Status**: stable
**Applies to**: All generators
**Source**: `src/property/core.lisp`, `generators/`

## Core Properties

### P1: Reproducibility

**Property**: Same seed produces same sequence of values

**Formal Statement**:
```
∀ generator G, seed S, size Z:
  generate(G, random-state(S), Z) = generate(G, random-state(S), Z)
```

**Rationale**: Essential for debugging failing properties.

**Verified by**: `tests/generator-tests.lisp` - reproducibility tests

---

### P2: Size Monotonicity

**Property**: Larger size produces more complex values (on average)

**Formal Statement**:
```
∀ generator G, size Z1, Z2 where Z1 < Z2:
  complexity(generate(G, *, Z2)) ≥ complexity(generate(G, *, Z1))
```

Where complexity is domain-specific (length for strings, magnitude for integers, etc.)

**Rationale**: Enables gradual complexity growth during testing.

---

### P3: Type Safety

**Property**: Generators always produce values of declared type

**Formal Statement**:
```
∀ generator G of type T:
  type-of(generate(G, *, *)) = T
```

**Example**:
- `(integers)` always returns integers
- `(strings)` always returns strings
- `(booleans)` always returns `t` or `nil`

**Rationale**: Prevents type errors in property tests.

---

### P4: Shrinking Termination

**Property**: Shrinking always terminates with finite sequence

**Formal Statement**:
```
∀ generator G, value V:
  shrink*(G, V) is finite
```

Where `shrink*` is the transitive closure of shrinking.

**Rationale**: Prevents infinite loops during counterexample minimization.

---

### P5: Shrinking Simplicity

**Property**: Each shrunk value is simpler than its predecessor

**Formal Statement**:
```
∀ generator G, value V, shrunk S ∈ shrink(G, V):
  simplicity(S) > simplicity(V)
```

Where simplicity is measured by proximity to canonical "simple" value (0 for integers, empty for collections, etc.)

**Rationale**: Ensures progress toward minimal counterexample.

---

### P6: Shrinking Idempotence at Minimum

**Property**: Shrinking the simplest value produces empty list

**Formal Statement**:
```
∀ generator G, minimal value M:
  shrink(G, M) = []
```

**Examples**:
- `shrink(integers, 0) = []`
- `shrink(strings, "") = []`
- `shrink(booleans, nil) = [nil]` then `shrink(booleans, nil) = []`

**Rationale**: Defines termination condition for shrinking.

## Range-Constrained Generators

### P7: Bound Preservation

**Property**: Generated values respect min/max bounds

**Formal Statement**:
```
∀ min, max, generator G = (integers :min min :max max):
  min ≤ generate(G, *, *) ≤ max
```

**Applies to**: `integers`, `naturals`, `floats`, `strings` (length), `lists` (length)

**Rationale**: Contracts must be honored.

---

### P8: Shrinking Respects Bounds

**Property**: Shrunk values stay within bounds

**Formal Statement**:
```
∀ bounded generator G, value V where V satisfies bounds:
  ∀ S ∈ shrink(G, V): S satisfies bounds
```

**Example**: `(integers :min 5 :max 100)` shrinking 87 never produces values < 5.

## Collection Generators

### P9: Empty Collection Shrinking

**Property**: Collections shrink toward empty

**Formal Statement**:
```
∀ collection generator G, non-empty collection C:
  ∃ path from C to [] through shrink*(G, C)
```

**Applies to**: `lists`, `vectors`, `hash-tables`

---

### P10: Element-wise Shrinking

**Property**: Collections also shrink elements

**Formal Statement**:
```
∀ collection generator G with element generator E:
  shrink(G, [e1, e2, ...]) includes [shrink(E, e1), e2, ...]
```

**Rationale**: Enables minimal counterexamples in nested structures.

## Combinator Properties

### P11: one-of Uniformity

**Property**: `one-of` chooses uniformly among generators

**Formal Statement**:
```
∀ generators G1, G2, ..., Gn:
  P(one-of(G1, ..., Gn) uses Gi) = 1/n
```

---

### P12: such-that Termination

**Property**: `such-that` terminates or throws after max-tries

**Formal Statement**:
```
∀ predicate P, generator G, max-tries N:
  such-that(P, G, :max-tries N) either:
    - returns value V where P(V) is true, or
    - throws error after N attempts
```

**Rationale**: Prevents infinite loops with unsatisfiable predicates.

---

### P13: fmap Preserves Distribution

**Property**: `fmap` transforms values without changing distribution

**Formal Statement**:
```
∀ function F, generator G:
  distribution(generate(fmap(F, G))) = F(distribution(generate(G)))
```

## Verification

These properties are verified by:
1. Unit tests in `tests/generator-tests.lisp`
2. Property-based tests (meta-testing)
3. Manual inspection of shrinking sequences

To verify a specific property, check the corresponding test case.
