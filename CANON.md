# CANON.md

## Project Context

**Name**: cl-test-hardening
**Language**: Common Lisp
**Framework**: ASDF
**Test Runner**: FiveAM (5am)
**Version**: 0.1.0

## Build Commands

```lisp
;; Load and run all tests
(ql:quickload "cl-test-hardening/tests")
(5am:run! :th.tests)

;; Run specific module test suites
(5am:run! :th.property-tests)
(5am:run! :th.mutation-tests)
(5am:run! :th.contract-tests)
(5am:run! :th.agent-tests)

;; Load just one module
(ql:quickload "cl-test-hardening/property")
(ql:quickload "cl-test-hardening/mutation")
(ql:quickload "cl-test-hardening/contract")
(ql:quickload "cl-test-hardening/agent")

;; Load everything
(ql:quickload "cl-test-hardening/all")
```

## Code Conventions

### Package Structure

- All modules use namespace prefix `th.*` (e.g., `th.property`, `th.mutation`)
- Core module is `th.core` providing shared result protocols
- Each module exports its public API via `package.lisp`

### Naming Conventions

- **Predicates**: End with `-p` (e.g., `passed-p`, `mutant-killed-p`)
- **Classes**: Use lowercase with hyphens (e.g., `verification-result`, `mutation-operator`)
- **Generic functions**: Descriptive action verbs (e.g., `format-result`, `run-property-test`)
- **Constants**: Use `+constant+` notation for true constants

### Module Independence

Each module (`property`, `mutation`, `contract`, `agent`) is independently loadable:
- Has its own ASDF system definition
- Depends only on `th.core` (not on other modules)
- Can be loaded without pulling in unneeded dependencies

### Test Organization

- All tests use FiveAM with hierarchical suite structure
- Root suite: `:th.tests`
- Module suites: `:th.property-tests`, `:th.mutation-tests`, etc.
- Each test file uses `(in-suite :parent-suite-name)` directive
- Test files mirror source structure in `tests/` directory

## Architecture Rules

### RULE: All verification functions return standard result objects

**Statement**: Every module's verification operations must return a `verification-result` object from `th.core`.

**Correct**:
```lisp
(defun verify-property (property generator)
  (make-verification-result
    :passed-p t
    :timestamp (get-universal-time)
    :duration-ms 150
    :summary "Property holds for all 100 test cases"
    :details (list :cases-checked 100)))
```

**Violation**:
```lisp
(defun verify-property (property generator)
  ;; Don't return raw booleans or ad-hoc structures
  t)
```

### RULE: Modules don't cross-depend

**Statement**: Modules (`property`, `mutation`, `contract`, `agent`) depend only on `th.core`, never on each other.

**Correct**:
```lisp
;; In cl-test-hardening/mutation.asd
(defsystem "cl-test-hardening/mutation"
  :depends-on ("cl-test-hardening"))  ; Only core
```

**Violation**:
```lisp
;; Don't make modules depend on each other
(defsystem "cl-test-hardening/mutation"
  :depends-on ("cl-test-hardening" "cl-test-hardening/property"))
```

### RULE: Use :serial t for load-order dependencies

**Statement**: ASDF component lists that have load-order dependencies must specify `:serial t`.

**Correct**:
```lisp
(defsystem "cl-test-hardening/property"
  :serial t
  :components ((:file "package")      ; Must load first
               (:file "core")         ; Depends on package
               (:file "runner")))     ; Depends on core
```

**Violation**:
```lisp
;; Without :serial, load order is undefined
(defsystem "cl-test-hardening/property"
  :components ((:file "runner")
               (:file "package")
               (:file "core")))
```

### RULE: Generators and operators are separate loadable systems

**Statement**: Generators (for property testing) and operators (for mutation testing) are defined in separate ASDF systems to avoid forcing all users to load them.

**Correct**:
```lisp
;; User loads just what they need
(ql:quickload "cl-test-hardening/property")     ; Core property testing
(ql:quickload "cl-test-hardening/generators")   ; Built-in generators
```

## File Locations

| Type | Location |
|------|----------|
| Core module | `src/core/` |
| Property module | `src/property/` |
| Mutation module | `src/mutation/` |
| Contract module | `src/contract/` |
| Agent module | `src/agent/` |
| Canon adapter | `src/canon/` |
| Generators | `generators/` |
| Mutation operators | `operators/` |
| Tests | `tests/` |
| ASDF systems | `*.asd` (project root) |
| Canon specifications | `canon/` |
| Documentation | `docs/` |

## Invariants

### System Invariants

1. **All tests pass on main branch**
   - Verify: `(5am:run! :th.tests)` must return all tests passed
   - Enforcement: Pre-commit hook

2. **All modules loadable independently**
   - Verify: Each module system can load without others
   - Check: `(ql:quickload "cl-test-hardening/property")` succeeds in fresh REPL

3. **Result protocol consistency**
   - Verify: All verification functions return `verification-result` objects
   - Check: `(typep result 'th.core:verification-result)` is true

### Module-Specific Invariants

**Property Testing**:
- All generators must be pure functions (no side effects)
- Shrinking must preserve the property violation
- Generated values must satisfy their type constraints

**Mutation Testing**:
- Mutants must compile successfully
- Each mutant has exactly one syntactic change
- Mutation operators are deterministic

**Contract Testing**:
- Pact files are valid JSON
- Consumer tests run against mock provider
- Provider verification runs against real implementation

**Agent Verification**:
- Violations are categorized into exactly one of five dimensions
- Scope checking uses explicit allowed-symbols list
- Hallucination detection checks against actual exports

## Navigation

For detailed navigation of Canon specifications and understanding what artifacts exist for each feature, see:

- **Canon index**: `canon/INDEX.md`
- **Core foundation**: `canon/core/INDEX.md`
- **Feature catalog**: `canon/features/INDEX.md`

These INDEX files map development tasks to their corresponding Canon artifacts (contracts, scenarios, properties).
