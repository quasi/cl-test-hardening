# Property Definition and Verification

**Status**: stable
**Package**: `th.property`
**Source**: `src/property/package.lisp:4-48`, `src/property/core.lisp`

## Overview

Properties are assertions that should hold for all valid inputs. This contract defines how to define properties, run them, and interpret results.

## Property Definition

### define-property

```lisp
(define-property name forall-bindings &key holds precondition tags timeout)
```

**Purpose**: Define a property-based test.

**Parameters**:
- `name`: Symbol naming the property
- `forall-bindings`: List of `(var generator)` pairs
- `:holds`: Predicate that should always be true (required)
- `:precondition`: Filter inputs (optional)
- `:tags`: Classification tags (optional)
- `:timeout`: Per-test timeout in seconds (optional)

**Example**:
```lisp
(define-property reverse-involution
  ((xs (th.gen:lists (th.gen:integers))))
  :holds (lambda (xs)
           (equal xs (reverse (reverse xs)))))
```

**Contract**:
- Property is registered in global registry
- Can be run with `check-property` or `check-all-properties`
- Name must be unique

### defgenerator

```lisp
(defgenerator name args &body body)
```

**Purpose**: Define a custom generator as a function.

**Contract**: Body must return a generator instance.

### defshrink

```lisp
(defshrink generator-name (value) &body body)
```

**Purpose**: Define custom shrinking logic for a generator.

**Contract**: Body must return a list of simpler values.

## Property Structure

```lisp
(defstruct property
  name
  forall-bindings
  holds-predicate
  precondition
  tags
  timeout)
```

**Accessors**:
- `property-name`
- `property-forall-bindings`
- `property-holds-predicate`
- `property-precondition`
- `property-tags`
- `property-timeout`

## Running Properties

### check-property

```lisp
(defun check-property (name &key iterations seed) => property-result)
```

**Purpose**: Run a single property test.

**Parameters**:
- `name`: Property name (symbol)
- `:iterations`: Number of test cases (default: `*default-iterations*`)
- `:seed`: Random seed for reproducibility (default: `*default-seed*`)

**Returns**: `property-result` instance.

**Contract**:
- Generates `:iterations` test cases
- Stops on first failure
- Attempts to shrink counterexamples
- Uses provided seed for reproducibility

### check-all-properties

```lisp
(defun check-all-properties (&key iterations seed tags) => list-of-results)
```

**Purpose**: Run all registered properties.

**Parameters**:
- `:iterations`: Number of test cases per property
- `:seed`: Random seed
- `:tags`: Filter by tags (optional)

**Returns**: List of `property-result` instances.

**Contract**:
- Runs all properties matching tags
- Each property runs independently
- Returns results in registration order

## Results

### property-result Structure

```lisp
(defstruct property-result
  passed-p                ; Boolean: did property pass?
  counterexample          ; Failing input (if any)
  shrunk-counterexample   ; Minimal failing input (if any)
  iterations              ; Number of tests run
  seed                    ; Random seed used
  error                   ; Error object (if crashed)
  classifications         ; Tag counts
  duration-ms)            ; Time taken
```

**Accessors**:
- `property-result-passed-p`
- `property-result-counterexample`
- `property-result-shrunk-counterexample`
- `property-result-iterations`
- `property-result-seed`
- `property-result-error`
- `property-result-classifications`
- `property-result-duration-ms`

**Contract**:
- If `passed-p` is `nil`, either `counterexample` or `error` is set
- If counterexample exists, `shrunk-counterexample` contains minimal failing case
- `seed` allows exact reproduction of failure

## Configuration

### *default-iterations*

Default number of test cases per property (default: 100).

### *default-seed*

Default random seed (default: `nil` for random).

## Registry

### find-property

```lisp
(defun find-property (name) => property-or-nil)
```

**Purpose**: Look up property by name.

### list-properties

```lisp
(defun list-properties (&key tags) => list-of-property-names)
```

**Purpose**: List all registered properties.

**Parameters**:
- `:tags`: Filter by tags

### clear-properties

```lisp
(defun clear-properties ())
```

**Purpose**: Remove all registered properties.

## Context

### with-property-context

```lisp
(with-property-context (&key seed size) &body body)
```

**Purpose**: Establish property testing context.

**Parameters**:
- `:seed`: Override random seed
- `:size`: Override initial size

### *test-random-state*

Dynamic variable holding current random state.

### *current-size*

Dynamic variable holding current size parameter.

## Workflow

1. **Define generators** for test data types
2. **Define property** with `define-property`
3. **Run property** with `check-property` or integrate with test framework
4. **On failure**:
   - Review `counterexample`
   - Examine `shrunk-counterexample` for minimal case
   - Use `seed` to reproduce
5. **Fix code** and re-run

## Invariants

- **Reproducibility**: Same seed produces same test cases
- **Shrinking minimality**: Shrunk counterexample is simpler than original
- **Precondition filtering**: Only valid inputs tested when precondition provided
- **Timeout safety**: Tests respect timeout limits

## References

- Implementation: `src/property/core.lisp`, `src/property/runner.lisp`
- Shrinking: `src/property/shrinking.lisp`
- Package: `src/property/package.lisp`
- Tests: `tests/property-tests.lisp`, `tests/shrinking-tests.lisp`
