# Mutation Operators

**Status**: stable
**Package**: `th.mutation`
**Source**: `src/mutation/package.lisp:4-83`, `operators/standard.lisp`

## Overview

Mutation operators define rules for introducing deliberate defects into code. They specify patterns to match and how to transform them.

## Operator Definition

### define-mutation-operators

```lisp
(define-mutation-operators name &body operator-specs)
```

**Purpose**: Define a set of mutation operators.

**Parameters**:
- `name`: Symbol naming the operator set
- `operator-specs`: List of `(pattern-name pattern mutations)` triples

**Example**:
```lisp
(define-mutation-operators standard-arithmetic
  (add-to-sub '(+ ?a ?b) '((- ?a ?b)))
  (sub-to-add '(- ?a ?b) '((+ ?a ?b)))
  (mul-to-div '(* ?a ?b) '((/ ?a ?b))))
```

**Contract**:
- Operator set is registered globally
- Can be referenced in policies
- Patterns use `?` prefix for variables

## Operator Structure

```lisp
(defstruct mutation-operator
  name
  pattern
  mutations)
```

**Accessors**:
- `operator-name`: Symbol identifying the operator
- `operator-pattern`: S-expression pattern with variables
- `operator-mutations`: List of replacement patterns

## Standard Operator Sets

### standard-arithmetic

Mutates arithmetic operators.

**Operators**:
- `+ → -`
- `- → +`
- `* → /`
- `/ → *`
- `1+ → 1-`
- `1- → 1+`

**Example**:
```lisp
;; Original: (+ x 5)
;; Mutant:   (- x 5)
```

---

### standard-conditional

Mutates conditional expressions.

**Operators**:
- `< → <=, >, >=`
- `> → >=, <, <=`
- `= → /=`
- `if → unless`
- `when → unless`

**Example**:
```lisp
;; Original: (< x 10)
;; Mutant:   (<= x 10)
```

---

### standard-logical

Mutates boolean logic.

**Operators**:
- `and → or`
- `or → and`
- `not → identity`

---

### standard-boundary

Mutates boundary conditions.

**Operators**:
- `0 → 1, -1`
- `1 → 0, 2`
- `-1 → 0, 1`
- `nil → t`
- `t → nil`

**Example**:
```lisp
;; Original: (loop for i from 0 below n ...)
;; Mutant:   (loop for i from 1 below n ...)
```

---

### standard-return

Mutates return values.

**Operators**:
- Return `nil` instead of computed value
- Return `0` for numeric functions
- Return empty collection for collection functions

## Pattern Matching

### pattern-matches-p

```lisp
(defun pattern-matches-p (pattern form) => boolean-or-bindings)
```

**Purpose**: Check if a form matches a pattern.

**Parameters**:
- `pattern`: Pattern with `?variables`
- `form`: Lisp form to match

**Returns**: `nil` or alist of variable bindings.

**Pattern Syntax**:
- `?var`: Matches any single form, binds to `var`
- `??rest`: Matches zero or more forms
- Literal atoms/lists: Match exactly

**Examples**:
```lisp
(pattern-matches-p '(+ ?a ?b) '(+ x 5))
;; => ((?a . x) (?b . 5))

(pattern-matches-p '(+ ?a ?b) '(- x 5))
;; => nil
```

### apply-mutation

```lisp
(defun apply-mutation (mutation bindings) => form)
```

**Purpose**: Apply mutation pattern with variable bindings.

**Parameters**:
- `mutation`: Mutation pattern
- `bindings`: Alist of variable bindings

**Returns**: Transformed form.

## Creating Operators

### make-mutation-operator

```lisp
(defun make-mutation-operator (&key name pattern mutations) => mutation-operator)
```

**Purpose**: Programmatically create operator.

### mutate-pattern

```lisp
(mutate-pattern pattern-form mutation-forms)
```

**Purpose**: Define pattern-based mutation (used in operator definitions).

### mutate-boundary

```lisp
(mutate-boundary value-pattern replacement-values)
```

**Purpose**: Define boundary value mutation.

## Mutant Generation

### generate-mutants

```lisp
(defun generate-mutants (form operators) => list-of-mutants)
```

**Purpose**: Generate all possible mutants of a form.

**Parameters**:
- `form`: Lisp form to mutate
- `operators`: List of mutation operators

**Returns**: List of `mutant` instances.

**Contract**:
- One mutant per applicable operator per site
- Each mutant has exactly one mutation
- Original form is never included

### mutate-form

```lisp
(defun mutate-form (form operator) => list-of-mutated-forms)
```

**Purpose**: Apply operator to all matching sites in form.

## Registry

### find-operator-set

```lisp
(defun find-operator-set (name) => list-of-operators-or-nil)
```

**Purpose**: Look up operator set by name.

### list-operator-sets

```lisp
(defun list-operator-sets () => list-of-names)
```

**Purpose**: List all registered operator sets.

### *all-standard-operators*

Special variable containing all standard operators combined.

## Operator Design Principles

1. **Single mutation**: Each operator changes exactly one thing
2. **Syntactic validity**: Mutations produce parseable Lisp
3. **Realistic defects**: Mimic common programming errors
4. **Testable**: Changes should be detectable by tests

## Contract Guarantees

- **Deterministic**: Same form + operators = same mutants
- **Complete**: All matching sites are mutated
- **Isolated**: Each mutant is independent
- **Traceable**: Each mutant records its source location

## References

- Implementation: `src/mutation/mutator.lisp`, `src/mutation/pattern-matching.lisp`
- Standard operators: `operators/standard.lisp`
- Package: `src/mutation/package.lisp`
- Tests: `tests/mutation-tests.lisp`
