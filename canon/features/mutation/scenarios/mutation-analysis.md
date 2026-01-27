# Mutation Analysis Scenarios

**Status**: stable
**Source**: `tests/mutation-tests.lisp`
**Contracts**: `mutation-operators.md`, `mutation-testing.md`

## Scenario: Generate Mutants from Operators

**Given**: Code with arithmetic operation
```lisp
(defun add-numbers (a b)
  (+ a b))
```

**And**: Standard arithmetic operators defined

**When**: Mutants are generated

**Then**:
- Mutant created: `(- a b)` (add-to-sub operator)
- Mutant records original form
- Mutant records replacement
- Mutant records operator name
- Each mutant has unique ID

**Rationale**: Operators must create valid mutants.

---

## Scenario: Pattern Matching for Mutations

**Given**: Pattern `(+ ?a ?b)`

**When**: Matched against `(+ x 5)`

**Then**:
- Match succeeds
- Bindings: `(?a . x)`, `(?b . 5)`
- Can apply mutation pattern using bindings

**When**: Matched against `(- x 5)`

**Then**:
- Match fails
- No bindings returned

**Rationale**: Pattern matching enables flexible operator definitions.

---

## Scenario: Killed Mutant (Strong Tests)

**Given**: Function with tests
```lisp
(defun absolute-value (n)
  (if (< n 0) (- n) n))

(test absolute-value-test
  (is (= 5 (absolute-value 5)))
  (is (= 5 (absolute-value -5))))
```

**And**: Mutant changes `<` to `<=`

**When**: Tests run against mutant

**Then**:
- Test fails (boundary case broken)
- Mutant status: `:killed`
- Tests are effective

**Rationale**: Good tests catch mutations.

---

## Scenario: Survived Mutant (Weak Tests)

**Given**: Function with incomplete tests
```lisp
(defun max-value (a b)
  (if (> a b) a b))

(test incomplete-test
  (is (= 5 (max-value 5 3))))
```

**And**: Mutant changes `>` to `>=`

**When**: Tests run against mutant

**Then**:
- Test passes (boundary case not tested)
- Mutant status: `:survived`
- Test gap revealed

**Rationale**: Survived mutants reveal test weaknesses.

---

## Scenario: Calculate Mutation Score

**Given**: Mutation analysis results
- 85 mutants killed
- 15 mutants survived
- 0 timeout
- 0 errors

**When**: Mutation score is calculated

**Then**:
- Score = 85 / (85 + 15) = 0.85 (85%)
- Interpretation: Good test coverage
- Some weak spots remain

**Rationale**: Score quantifies test suite quality.

---

## Scenario: Run Mutation Analysis with Policy

**Given**: Mutation policy defined
```lisp
(define-mutation-policy arithmetic-policy
  :operators '(standard-arithmetic)
  :threshold 0.8)
```

**When**: `run-mutation-analysis` is executed

**Then**:
- All arithmetic operators applied
- Tests run for each mutant
- Score compared to threshold
- Pass/fail determined

**Rationale**: Policies enforce quality standards.

---

## Scenario: Report Survived Mutants

**Given**: Mutation result with survived mutants

**When**: `format-survived-mutants` is called

**Then**:
- Each survivor listed with location
- Original and mutated code shown
- Operator identified
- Actionable for test improvement

**Rationale**: Survivors guide test enhancement.

---

## Verification

Run tests:
```lisp
(5am:run! :th.mutation-tests)
```

All mutation testing scenarios must pass.
