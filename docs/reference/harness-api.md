# Test Harness API Reference

Complete API for `th.harness` package - declarative test environment setup.

## Overview

Test files often start with boilerplate:
```lisp
(ql:quickload :fiveam)
(ql:quickload :alexandria)
(ql:quickload :my-system)
(load "src/utils.lisp")
(in-package :my-system.tests)
(5am:def-suite my-tests)
(5am:in-suite my-tests)
```

**th.harness eliminates this.** Define your environment once, reuse everywhere.

## Core Concept

A **harness** is a named configuration describing:
- Which systems to load (Quicklisp)
- Which files to load (source files)
- Which package to use
- Which test suite to create (optional)

## API Reference

### `define-harness`

**Syntax**: `(define-harness name &key systems extra-systems load package)`

**Type**: Macro

**Description**: Define a reusable test environment configuration.

**Parameters**:
- `name` — Keyword naming the harness
- `:systems` — List of Quicklisp system keywords (always loaded)
- `:extra-systems` — Optional additional systems (for test-specific deps)
- `:load` — List of file paths to load in order
- `:package` — Keyword naming the target package

**Returns**: Harness object (registered in global registry)

**Example**:
```lisp
(define-harness :my-api
  :systems (:fiveam :dexador :yason :alexandria)
  :load ("src/package.lisp"
         "src/utils.lisp"
         "src/api.lisp")
  :package :my-api.tests)
```

---

### `setup`

**Syntax**: `(setup harness-name &key suite-name suite-description extra-load extra-systems)`

**Type**: Macro

**Description**: Set up test environment using a registered harness.

**Parameters**:
- `harness-name` — Keyword naming the harness to use
- `:suite-name` — Optional: FiveAM suite name to create
- `:suite-description` — Optional: Suite description
- `:extra-load` — Optional: Additional files to load (beyond harness definition)
- `:extra-systems` — Optional: Additional systems to load

**Side Effects**:
- Loads all specified systems
- Loads all specified files
- Switches to target package
- Creates test suite (if `:suite-name` provided)

**Example**:
```lisp
(setup :my-api
  :suite-name :api-tests
  :suite-description "API integration tests")

;; Now you're in :my-api.tests package with :api-tests suite active
```

---

### `find-harness`

**Syntax**: `(find-harness name)`

**Description**: Look up a registered harness by name.

**Returns**: Harness object or NIL

**Example**:
```lisp
(find-harness :my-api)  ; => #<HARNESS :MY-API>
```

---

### `list-harnesses`

**Syntax**: `(list-harnesses)`

**Description**: List all registered harness names.

**Returns**: List of keywords

**Example**:
```lisp
(list-harnesses)  ; => (:MY-API :BACKEND :FRONTEND)
```

---

### `clear-harnesses`

**Syntax**: `(clear-harnesses)`

**Description**: Remove all registered harnesses from the registry.

---

### Harness Accessors

Access harness configuration:

- `(harness-systems h)` — List of Quicklisp systems
- `(harness-extra-systems h)` — Optional additional systems
- `(harness-load-files h)` — List of file paths
- `(harness-package h)` — Target package keyword

## Complete Example

### Define Harness Once

In `test-config.lisp`:
```lisp
(ql:quickload "cl-test-hardening/harness")
(use-package :th.harness)

(define-harness :calculator
  :systems (:fiveam :alexandria)
  :load ("src/package.lisp"
         "src/math-utils.lisp"
         "src/calculator.lisp")
  :package :calculator.tests)
```

### Use in Every Test File

In `tests/basic-math-tests.lisp`:
```lisp
(setup :calculator
  :suite-name :basic-math
  :suite-description "Basic arithmetic tests")

;; You're now in :calculator.tests package with :basic-math suite active
;; All dependencies loaded, ready to write tests

(5am:test addition
  (5am:is (= 4 (add 2 2))))

(5am:test subtraction
  (5am:is (= 0 (subtract 2 2))))
```

In `tests/advanced-tests.lisp`:
```lisp
(setup :calculator
  :suite-name :advanced-math
  :extra-systems (:cl-ppcre)  ; Additional system for this test file
  :extra-load ("tests/test-helpers.lisp"))  ; Additional helper file

;; Same environment, different suite

(5am:test complex-calculation
  (5am:is (< 3.14 (calculate-pi) 3.15)))
```

## Benefits

### Before (Boilerplate in Every File)

```lisp
;; test-1.lisp
(ql:quickload :fiveam)
(ql:quickload :alexandria)
(ql:quickload :my-system)
(load "src/utils.lisp")
(in-package :my-system.tests)
(5am:def-suite suite-1)
(5am:in-suite suite-1)
;; ... tests ...

;; test-2.lisp
(ql:quickload :fiveam)
(ql:quickload :alexandria)
(ql:quickload :my-system)
(load "src/utils.lisp")
(in-package :my-system.tests)
(5am:def-suite suite-2)
(5am:in-suite suite-2)
;; ... tests ...
```

**Problems:**
- Duplicated setup code
- Easy to forget a system or file
- Hard to maintain when dependencies change

### After (With Harness)

```lisp
;; test-config.lisp (define once)
(define-harness :my-system
  :systems (:fiveam :alexandria :my-system)
  :load ("src/utils.lisp")
  :package :my-system.tests)

;; test-1.lisp (reuse)
(setup :my-system :suite-name :suite-1)
;; ... tests ...

;; test-2.lisp (reuse)
(setup :my-system :suite-name :suite-2)
;; ... tests ...
```

**Benefits:**
- No duplication
- Single source of truth
- Easy to update dependencies
- Consistent across all test files

## Common Patterns

### Multiple Environments

```lisp
;; Unit tests: lightweight, no external deps
(define-harness :unit
  :systems (:fiveam)
  :load ("src/core.lisp")
  :package :my-project.tests)

;; Integration tests: full system
(define-harness :integration
  :systems (:fiveam :dexador :postmodern)
  :load ("src/core.lisp" "src/db.lisp" "src/api.lisp")
  :package :my-project.tests)

;; Use appropriate harness per test file
(setup :unit :suite-name :unit-tests)
(setup :integration :suite-name :integration-tests)
```

### Test-Specific Extensions

```lisp
;; Base harness
(define-harness :base
  :systems (:fiveam)
  :load ("src/core.lisp")
  :package :tests)

;; Add extras per test file
(setup :base
  :suite-name :http-tests
  :extra-systems (:dexador)  ; Only for HTTP tests
  :extra-load ("tests/http-mocks.lisp"))
```

## Integration with FiveAM

`setup` automatically creates FiveAM suites:

```lisp
(setup :my-harness
  :suite-name :my-suite
  :suite-description "Description here")

;; Equivalent to:
;; (5am:def-suite :my-suite :description "Description here")
;; (5am:in-suite :my-suite)
```

If you don't need a suite, omit `:suite-name`:

```lisp
(setup :my-harness)
;; No suite created, just environment setup
```

## See Also

- [Fixture API](fixture-api.md) — Reusable test data
- [API Overview](api-overview.md) — All modules
