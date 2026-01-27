# Tutorial: Mutation Testing

<!-- Generated from: canon/features/mutation/scenarios/mutation-analysis.md -->

Learn mutation testing by analyzing and improving a test suite.

## What You'll Learn

- Running mutation analysis on code
- Interpreting mutation scores
- Identifying weak tests
- Improving test quality to kill mutants

## Prerequisites

- Common Lisp REPL running
- cl-test-hardening installed
- FiveAM test framework
- Basic testing knowledge

## Scenario: Testing a Discount Calculator

We'll test a discount calculator and use mutation testing to find gaps in our tests.

### Step 1: Load the Module

```lisp
(ql:quickload "cl-test-hardening/mutation")
(ql:quickload "fiveam")
(use-package :th.mutation)
```

### Step 2: Implement the Discount Calculator

```lisp
(defun calculate-discount (price customer-type)
  "Calculate discounted price based on customer type."
  (cond
    ((string= customer-type "VIP")
     (* price 0.8))  ; 20% off
    ((string= customer-type "MEMBER")
     (* price 0.9))  ; 10% off
    ((> price 100)
     (* price 0.95)) ; 5% off for large purchases
    (t price)))      ; No discount
```

### Step 3: Write Initial Tests

```lisp
(fiveam:def-suite discount-tests)
(fiveam:in-suite discount-tests)

(fiveam:test test-vip-discount
  (fiveam:is (= 80.0 (calculate-discount 100 "VIP"))))

(fiveam:test test-no-discount
  (fiveam:is (= 50.0 (calculate-discount 50 "REGULAR"))))
```

Run the tests:

```lisp
(fiveam:run! 'discount-tests)
```

You should see:
```
Running test suite DISCOUNT-TESTS
 Running test TEST-VIP-DISCOUNT ✓
 Running test TEST-NO-DISCOUNT ✓
 Did 2 tests.
   Pass: 2 (100%)
```

Great! All tests pass. But are they good tests?

### Step 4: Run Mutation Analysis

```lisp
(defparameter *result*
  (mutate-and-test
    :target-function 'calculate-discount
    :test-suite 'discount-tests
    :operators '(:flip-arithmetic :change-comparison :flip-boolean)))
```

You should see:
```
Mutation Testing Results:

Total mutants: 8
Killed: 3
Survived: 5
Mutation Score: 37.5%

SURVIVED MUTANTS:
  #1: Changed (* price 0.9) to (* price 0.8)
  #2: Changed (> price 100) to (>= price 100)
  #3: Changed (* price 0.95) to (* price 0.90)
  #4: Changed "MEMBER" to "VIP"
  #5: Changed 100 to 99

⚠ Low mutation score indicates weak test coverage
```

Only 37.5% of mutants were killed! Let's fix this.

### Step 5: Analyze Survived Mutants

Look at mutant #1: Changed `(* price 0.9)` to `(* price 0.8)`

**Why did it survive?** We never test the MEMBER discount!

Look at mutant #2: Changed `(> price 100)` to `(>= price 100)`

**Why did it survive?** We never test the boundary at exactly 100.

### Step 6: Add Tests to Kill Survivors

```lisp
(fiveam:test test-member-discount
  "Test MEMBER gets 10% discount"
  (fiveam:is (= 90.0 (calculate-discount 100 "MEMBER"))))

(fiveam:test test-large-purchase-discount
  "Test large purchases get 5% discount"
  (fiveam:is (= 95.0 (calculate-discount 100 "REGULAR"))))

(fiveam:test test-boundary-at-100
  "Test boundary: 99 vs 100"
  (fiveam:is (= 99.0 (calculate-discount 99 "REGULAR")))
  (fiveam:is (= 95.0 (calculate-discount 100 "REGULAR"))))

(fiveam:test test-all-customer-types
  "Test all customer type discounts"
  (fiveam:is (= 80.0 (calculate-discount 100 "VIP")))
  (fiveam:is (= 90.0 (calculate-discount 100 "MEMBER")))
  (fiveam:is (= 50.0 (calculate-discount 50 "REGULAR"))))
```

Run tests again:

```lisp
(fiveam:run! 'discount-tests)
```

You should see:
```
 Did 6 tests.
   Pass: 6 (100%)
```

### Step 7: Re-run Mutation Analysis

```lisp
(defparameter *result2*
  (mutate-and-test
    :target-function 'calculate-discount
    :test-suite 'discount-tests
    :operators '(:flip-arithmetic :change-comparison :flip-boolean)))
```

You should see:
```
Mutation Testing Results:

Total mutants: 8
Killed: 7
Survived: 1
Mutation Score: 87.5%

SURVIVED MUTANTS:
  #1: Changed "VIP" to "MEMBER"

✓ Good mutation score!
```

Much better! From 37.5% to 87.5%.

### Step 8: Kill the Last Survivor

The last mutant changes "VIP" to "MEMBER". We need a test that verifies VIP and MEMBER get different discounts:

```lisp
(fiveam:test test-vip-vs-member
  "VIP discount should be better than MEMBER"
  (let ((price 100))
    (fiveam:is (< (calculate-discount price "VIP")
                  (calculate-discount price "MEMBER")))))
```

Re-run mutation analysis:

```lisp
(mutate-and-test
  :target-function 'calculate-discount
  :test-suite 'discount-tests
  :operators '(:flip-arithmetic :change-comparison :flip-boolean))
```

You should see:
```
Total mutants: 8
Killed: 8
Survived: 0
Mutation Score: 100%

✓ All mutants killed!
```

Perfect score!

## Checkpoint

You've learned:
- ✓ How to run mutation analysis with `mutate-and-test`
- ✓ How to interpret mutation scores
- ✓ How to identify weak tests from survived mutants
- ✓ How to add targeted tests to kill mutants

## Understanding Mutation Operators

### :flip-arithmetic

Changes arithmetic operators:
- `+` → `-`
- `*` → `/`
- `-` → `+`

**Catches**: Tests that don't verify calculation correctness

### :change-comparison

Changes comparison operators:
- `<` → `<=` → `>` → `>=`
- `=` → `/=`

**Catches**: Tests that don't verify boundary conditions

### :flip-boolean

Changes boolean values:
- `t` → `nil`
- `nil` → `t`

**Catches**: Tests that don't verify boolean logic

### :remove-call

Removes function calls:
- `(func x)` → `x`

**Catches**: Tests that don't verify side effects or essential operations

## Common Patterns

### Boundary Testing

```lisp
;; Test both sides of boundary
(is (= expected (func (- boundary 1))))
(is (= expected (func boundary)))
(is (= expected (func (+ boundary 1))))
```

### Distinguishing Similar Paths

```lisp
;; Don't just test each path exists
(is (= result-a (func input-a)))
(is (= result-b (func input-b)))

;; Also test they're different
(is (/= result-a result-b))
```

### Testing Exact Values

```lisp
;; Weak: only tests non-nil
(is (func x))

;; Strong: tests exact value
(is (= expected (func x)))
```

## Next Steps

- **Custom operators**: Define mutation operators for your domain
- **Equivalent mutants**: Learn to identify untestable mutations
- **Integration**: Integrate mutation testing into CI pipeline
- **See API docs**: `reference/mutation-api.md`

## Troubleshooting

**Q: Mutation score is low but tests seem good?**

A: Some mutants may be "equivalent" (semantically identical to original). Review survived mutants manually.

**Q: Mutation testing is slow?**

A: Run fewer operators or target specific functions. Use `:max-mutants` parameter.

**Q: How do I know what mutation score to aim for?**

A: 80%+ is good, 90%+ is excellent. Focus on critical code paths first.
