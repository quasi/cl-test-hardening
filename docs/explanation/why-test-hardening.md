# Why Test Hardening?

Tests are meant to catch bugs, but tests themselves can lie. Test hardening ensures your tests are actually doing their job.

## The Problem: Tests That Lie

### False Confidence

You have 100% code coverage. All tests pass. You ship with confidence. Then production breaks.

**Why?** Your tests passed because:
- They never checked edge cases
- They tested happy paths only
- They mocked critical integrations
- They had weak assertions

### Example: Weak Test

```lisp
(test addition-works
  (is (= 4 (+ 2 2))))
```

This test passes. But it doesn't prove `+` works for all inputs. What about negatives? Floats? Large numbers? Your test suite gives false confidence.

## Test Hardening Techniques

### 1. Property-Based Testing

Instead of testing examples, test properties (invariants):

```lisp
(defproperty addition-commutative (a b)
  (= (+ a b) (+ b a)))

(check-property 'addition-commutative
                :generator (gen:tuple (gen:integers) (gen:integers))
                :num-tests 1000)
```

Now you've tested 1000 random cases, not just `2 + 2`.

### 2. Mutation Testing

Your tests pass, but are they actually checking anything?

Mutation testing introduces bugs intentionally. If tests still pass, they're weak.

```lisp
;; Original code
(defun calculate-discount (price)
  (if (> price 100)
      (* price 0.9)    ; 10% discount
      price))

;; Mutant (bug introduced)
(defun calculate-discount (price)
  (if (>= price 100)   ; Changed > to >=
      (* price 0.9)
      price))
```

If your tests still pass after this mutation, they don't verify the boundary condition.

### 3. Contract Testing

Integration tests can be brittle and slow. Contract testing verifies the interface without full integration:

- Consumer defines expectations
- Provider verifies it meets them
- Teams work independently
- Fast, focused tests

### 4. Agent Verification

AI-generated code needs extra scrutiny:

- Did it stay within scope?
- Did it invent non-existent APIs?
- Does it follow project style?
- Is it unnecessarily complex?

Automated verification catches these issues before code review.

## When to Use Each Technique

| Technique | Use When | Benefit |
|-----------|----------|---------|
| **Property Testing** | Testing algorithms, data structures, pure functions | Finds edge cases you didn't think of |
| **Mutation Testing** | Checking test suite quality | Measures if tests actually verify behavior |
| **Contract Testing** | Testing service integrations | Fast, reliable integration tests |
| **Agent Verification** | Using AI-generated code | Catches hallucinations and scope violations |

## The Cost of Weak Tests

Weak tests cost more than no tests:

1. **False confidence** - You think you're safe when you're not
2. **Wasted time** - Tests that don't catch bugs waste CI time
3. **Tech debt** - Bad tests are harder to maintain than good tests
4. **Lost trust** - Teams stop trusting tests after false positives

## Philosophy

**Tests should fail when code is wrong, and only then.**

Test hardening ensures:
- Tests verify actual behavior, not just syntax
- Edge cases are explored automatically
- Integration points are validated
- Generated code meets quality standards

## Next Steps

- [Quickstart Guide](../quickstart.md)
- [Property Testing Tutorial](../tutorials/property-based-testing.md)
- [Mutation Testing Tutorial](../tutorials/mutation-testing.md)
