# Canon Verification Protocol

**Status**: stable
**Package**: `th.canon`
**Source**: `src/canon/adapter.lisp`

## Overview

The Canon adapter integrates cl-test-hardening with the Canon specification system through a JSON-based verification protocol. It reads verification requests, executes tests, and reports results in a standard format.

## Main Entry Point

### run-verification

```lisp
(defun run-verification (&key (request-path *request-path*)
                              (result-path *result-path*)) => nil)
```

**Purpose**: Execute verification workflow from JSON request.

**Parameters**:
- `:request-path`: Path to verification request JSON (default: `"verification-request.json"`)
- `:result-path`: Path to write result JSON (default: `"verification-result.json"`)

**Workflow**:
1. Read verification request from `request-path`
2. Load specified ASDF system
3. Run unit tests if specified
4. Run property tests if specified
5. Run mutation analysis if enabled
6. Write results to `result-path`

**Contract**:
- Always writes result file (even on failure)
- Result includes timestamp and duration
- Missing test suites result in zero counts (not errors)

## Request Structure

### read-verification-request

```lisp
(defun read-verification-request (&optional (path *request-path*)) => verification-request)
```

**Purpose**: Parse JSON request file into Lisp structure.

**JSON Format**:
```json
{
  "system": "cl-test-hardening/tests",
  "test_suite": "th.tests",
  "properties": ["reverse-involution"],
  "mutation": {"enabled": true},
  "options": {"property_iterations": 100}
}
```

## Result Structure

### write-verification-result

```lisp
(defun write-verification-result (result &optional (path *result-path*)) => nil)
```

**Output Format**:
```json
{
  "timestamp": "2026-01-27T23:48:00Z",
  "duration_ms": 1234,
  "platform": "common-lisp",
  "framework": "fiveam",
  "summary": {"passed": true, "score": 1.0},
  "unit_tests": {"passed": 42, "failed": 0},
  "properties": {"checked": 5, "passed": 5},
  "mutations": {"score": 1.0}
}
```

## Test Execution

### run-unit-tests

```lisp
(defun run-unit-tests (test-suite) => plist)
```

Executes FiveAM test suite, returns `:passed`, `:failed`, `:failures`.

### run-properties

```lisp
(defun run-properties (property-names options) => plist)
```

Executes property tests, returns `:checked`, `:passed`, `:failed`, `:failures`.

### run-mutations

```lisp
(defun run-mutations (config) => plist)
```

Placeholder for mutation testing integration (TODO).

## References

- Implementation: `src/canon/adapter.lisp`
- Package: `src/canon/package.lisp`
