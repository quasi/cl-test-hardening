---
type: contract
name: mutation-testing
version: 1.0.0
feature: mutation
---

# Mutation Testing Workflow

**Status**: stable
**Package**: `th.mutation`
**Source**: `src/mutation/package.lisp`, `src/mutation/runner.lisp`

## Overview

Mutation testing measures test suite quality by introducing defects and checking if tests catch them. This contract defines the workflow for running mutation analysis.

## Policy Definition

### define-mutation-policy

```lisp
(define-mutation-policy name &key operators threshold description)
```

**Purpose**: Define a mutation testing policy.

**Parameters**:
- `name`: Symbol naming the policy
- `:operators`: List of operator set names or operator instances
- `:threshold`: Minimum acceptable mutation score (0.0 - 1.0)
- `:description`: Human-readable description

**Example**:
```lisp
(define-mutation-policy strict-arithmetic
  :operators '(standard-arithmetic standard-boundary)
  :threshold 0.9
  :description "Requires 90% mutation kill rate for arithmetic")
```

**Contract**:
- Policy is registered globally
- Can be run with `run-mutation-analysis`
- Threshold determines pass/fail

## Policy Structure

```lisp
(defstruct mutation-policy
  name
  operators
  threshold)
```

**Accessors**:
- `policy-name`
- `policy-operators`
- `policy-threshold`

## Running Mutation Tests

### run-mutation-analysis

```lisp
(defun run-mutation-analysis (policy-name files test-command) => mutation-result)
```

**Purpose**: Run mutation analysis on code files.

**Parameters**:
- `policy-name`: Name of mutation policy
- `files`: List of file paths to mutate
- `test-command`: Shell command to run tests

**Returns**: `mutation-result` instance.

**Workflow**:
1. Load policy and operators
2. For each file:
   - Parse source code
   - Generate mutants using operators
3. For each mutant:
   - Apply mutation
   - Compile mutated code
   - Run test command
   - Record result (killed/survived)
4. Calculate mutation score
5. Return result

**Contract**:
- Each mutant tested independently
- Original code restored after each test
- Test command exit status determines kill (0 = killed, non-zero = survived)

### mutate-and-test

```lisp
(defun mutate-and-test (file operators test-command) => mutation-result)
```

**Purpose**: Simplified mutation testing without policy.

**Parameters**:
- `file`: Single file path
- `operators`: List of operators
- `test-command`: Test command string

## Mutant Structure

```lisp
(defstruct mutant
  id
  file
  line
  location
  original
  replacement
  operator
  status)
```

**Accessors**:
- `mutant-id`: Unique identifier
- `mutant-file`: Source file path
- `mutant-line`: Line number
- `mutant-location`: Form path (e.g., `(defun foo 2 1)`)
- `mutant-original`: Original code
- `mutant-replacement`: Mutated code
- `mutant-operator`: Operator that created it
- `mutant-status`: `:killed`, `:survived`, `:timeout`, `:error`

## Result Structure

```lisp
(defstruct mutation-result
  score
  killed
  survived
  total
  timeout
  error
  duration-ms
  mutants)
```

**Accessors**:
- `result-score`: Kill rate (0.0 - 1.0)
- `result-killed`: Number of killed mutants
- `result-survived`: Number of survived mutants
- `result-total`: Total mutants tested
- Includes `verification-result` fields (from `th.core`)

## Score Calculation

### calculate-mutation-score

```lisp
(defun calculate-mutation-score (killed survived) => float)
```

**Purpose**: Compute mutation score.

**Formula**:
```
score = killed / (killed + survived)
```

**Returns**: Value between 0.0 and 1.0.

**Contract**:
- Returns 0.0 if no mutants killed
- Returns 1.0 if all mutants killed
- Ignores timeout/error mutants in calculation

## Reporting

### format-mutation-report

```lisp
(defun format-mutation-report (result stream))
```

**Purpose**: Generate human-readable mutation report.

**Output Format**:
```
Mutation Testing Results
========================
Score: 0.85 (85%)
Total: 100 mutants
Killed: 85
Survived: 15
Timeout: 0
Error: 0
```

### format-survived-mutants

```lisp
(defun format-survived-mutants (result stream))
```

**Purpose**: List mutants that survived (weak spots in tests).

**Output Format**:
```
Survived Mutants (weaknesses):
==============================
1. file.lisp:42 - operator: add-to-sub
   Original: (+ x 5)
   Mutant:   (- x 5)

2. file.lisp:67 - operator: if-to-unless
   ...
```

## Registry

### find-policy

```lisp
(defun find-policy (name) => policy-or-nil)
```

### list-policies

```lisp
(defun list-policies () => list-of-policy-names)
```

### clear-policies

```lisp
(defun clear-policies ())
```

## Mutation Score Interpretation

| Score | Interpretation |
|-------|----------------|
| 0.0 - 0.5 | Weak tests - many defects undetected |
| 0.5 - 0.7 | Moderate coverage - significant gaps |
| 0.7 - 0.9 | Good coverage - some weak spots |
| 0.9 - 1.0 | Excellent coverage - highly effective tests |

**Note**: Unlike code coverage, mutation score directly measures test quality.

## Best Practices

1. **Start small**: Test critical modules first
2. **Fix survivors**: Each survived mutant reveals a test gap
3. **Not 100%**: Equivalent mutants exist; aim for 85-95%
4. **Run regularly**: Catches test suite degradation
5. **Fast tests**: Mutation testing multiplies test execution time

## Invariants

- **Independence**: Each mutant tested in isolation
- **Restoration**: Original code always restored
- **Reproducibility**: Same inputs produce same results
- **Status accuracy**: Exit codes correctly interpreted

## JSON Schema

Mutation result structure:

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "score": {"type": "number", "minimum": 0.0, "maximum": 1.0},
    "killed": {"type": "integer", "minimum": 0},
    "survived": {"type": "integer", "minimum": 0},
    "total": {"type": "integer", "minimum": 0},
    "timeout": {"type": "integer", "minimum": 0},
    "error": {"type": "integer", "minimum": 0},
    "duration_ms": {"type": "integer", "minimum": 0},
    "mutants": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "id": {"type": "string"},
          "file": {"type": "string"},
          "line": {"type": "integer", "minimum": 1},
          "location": {"type": "array"},
          "original": {"type": "string"},
          "replacement": {"type": "string"},
          "operator": {"type": "string"},
          "status": {"type": "string", "enum": ["killed", "survived", "timeout", "error"]}
        },
        "required": ["id", "file", "line", "original", "replacement", "operator", "status"]
      }
    }
  },
  "required": ["score", "killed", "survived", "total", "mutants"]
}
```

## References

- Implementation: `src/mutation/runner.lisp`, `src/mutation/mutator.lisp`
- Operators: `mutation-operators.md`
- Package: `src/mutation/package.lisp`
- Tests: `tests/mutation-tests.lisp`
