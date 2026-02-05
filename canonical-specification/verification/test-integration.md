# Test Integration

Verification runs the FiveAM test suite to ensure implementation matches specification.

## Running Tests

```lisp
;; Load and run all tests
(ql:quickload "cl-test-hardening/tests")
(5am:run! :th.tests)
```

## Test Suite Structure

```
:th.tests (root)
├── :th.property-tests
│   ├── generator tests
│   ├── shrinking tests
│   └── property tests
├── :th.mutation-tests
├── :th.contract-tests
└── :th.agent-tests
```

## Success Criteria

All tests must pass for verification to succeed.

## Test Files

- `tests/test-package.lisp` - Test suite definition
- `tests/generator-tests.lisp` - Generator functionality
- `tests/shrinking-tests.lisp` - Shrinking algorithms
- `tests/property-tests.lisp` - Property-based testing
- `tests/mutation-tests.lisp` - Mutation testing
- `tests/contract-tests.lisp` - Contract testing
- `tests/agent-tests.lisp` - Agent verification
