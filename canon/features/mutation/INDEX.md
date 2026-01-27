# Mutation Testing Feature

**Purpose**: Mutation testing for test suite quality assessment in Common Lisp.

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand what mutation testing is | `../../core/foundation/vocabulary.md` → "Mutation Testing" |
| Learn the mutation testing workflow | `contracts/mutation-testing.md` |
| Learn about mutation operators | `contracts/mutation-operators.md` |
| See mutation analysis in action | `scenarios/mutation-analysis.md` |
| Understand mutation correctness guarantees | `properties/mutation-correctness.md` |

## Contents

### Contracts (2)

| Contract | Description | ~Lines |
|----------|-------------|--------|
| `contracts/mutation-testing.md` | Mutation testing process and API | TBD |
| `contracts/mutation-operators.md` | Mutation operator definitions | TBD |

### Scenarios (1)

| Scenario | Description | ~Lines |
|----------|-------------|--------|
| `scenarios/mutation-analysis.md` | Running mutation analysis on a test suite | TBD |

### Properties (1)

| Property | Description | ~Lines |
|----------|-------------|--------|
| `properties/mutation-correctness.md` | Invariants for mutation process | TBD |

## Key Concepts

### Mutant

A mutant is source code with a single intentional defect. Characteristics:
- Exactly one syntactic change from original
- Must compile successfully
- Should behave differently at runtime

### Mutation Operator

A rule for creating mutations. Examples:
- Flip boolean constants (`t` ↔ `nil`)
- Change arithmetic operators (`+` → `-`)
- Modify comparison operators (`<` → `<=`)
- Remove function calls

### Killed vs. Survived

**Killed mutant**: Test suite detects the mutation (good)
- At least one test fails with mutant
- Indicates effective tests

**Survived mutant**: Tests pass despite mutation (bad)
- No tests detect the defect
- Indicates weak test coverage

### Mutation Score

Percentage of mutants killed by tests:
```
mutation-score = (killed-mutants / total-mutants) × 100%
```

Higher scores indicate more effective test suites.

## Implementation Location

| Artifact | Source File |
|----------|-------------|
| Mutation core | `src/mutation/core.lisp` |
| Pattern matching | `src/mutation/pattern-matching.lisp` |
| Mutator engine | `src/mutation/mutator.lisp` |
| Test runner | `src/mutation/runner.lisp` |
| Standard operators | `operators/standard.lisp` |

## Dependencies

- Depends on: `core/` (for verification result protocol)
- Depends on: `cl-ppcre` (for pattern matching)
- Used by: `canon/` (for specification verification)

## Typical Workflow

1. **Select target**: Which code to mutate?
2. **Choose operators**: Which mutations to apply?
3. **Generate mutants**: Create mutated versions
4. **Run tests**: Execute test suite against each mutant
5. **Analyze results**: Which mutants survived?
6. **Improve tests**: Add tests to kill survivors
7. **Re-run**: Verify improved mutation score
