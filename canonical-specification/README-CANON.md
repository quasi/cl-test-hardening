# Canon Specification for cl-test-hardening

This is the formal specification for **cl-test-hardening**, a test hardening library for Common Lisp that ensures tests don't lie to you.

## Purpose

This Canon serves as the authoritative specification for:
- **Module contracts and interfaces** - What each module provides
- **Expected behaviors** - How the system should behave
- **Domain vocabulary** - Core terms and concepts
- **Architectural decisions** - Why things are built this way

## What is Test Hardening?

Test hardening makes tests more rigorous and reliable so they accurately reflect code quality. The library provides four independent approaches:

1. **Property-based testing** - Verify invariants across many generated cases
2. **Mutation testing** - Measure test suite effectiveness by introducing defects
3. **Contract testing** - Consumer-driven integration testing without full environments
4. **Agent verification** - Multi-dimensional verification of AI-generated code

## Structure

```
canon/
├── canon.yaml                 # Manifest
├── README-CANON.md            # This file
├── core/
│   └── foundation/
│       ├── vocabulary.md      # Core terms and concepts
│       └── ontology.md        # Relationships between concepts
├── features/                  # Module specifications
│   ├── property/              # Property-based testing
│   ├── mutation/              # Mutation testing
│   ├── contract/              # Contract testing
│   └── agent/                 # Agent verification
└── verification/
    └── test-integration.md    # How to verify implementation
```

## Module Specifications

### Property Testing (`cl-test-hardening/property`)
Property-based testing with automatic test case generation and shrinking. Verifies that properties (invariants) hold across many generated inputs.

### Mutation Testing (`cl-test-hardening/mutation`)
Test quality verification through code mutation. Introduces small defects and checks if tests catch them.

### Contract Testing (`cl-test-hardening/contract`)
Consumer-driven contract testing (Pact-style). Enables independent service testing without full integration environments.

### Agent Verification (`cl-test-hardening/agent`)
Multi-dimensional verification of AI-generated code across scope, hallucination, style, semantics, and complexity.

## Verification

Run the test suite to verify the implementation matches this specification:

```lisp
;; Load and run all tests
(ql:quickload "cl-test-hardening/tests")
(5am:run! :th.tests)

;; Run specific module tests
(5am:run! :th.property-tests)
(5am:run! :th.mutation-tests)
(5am:run! :th.contract-tests)
(5am:run! :th.agent-tests)
```

All tests must pass for the implementation to be considered conformant.

## Extraction Metadata

**Source**: Existing codebase (initialized 2026-01-27)
**Method**: Multi-source triangulation via `_initiate` skill
**Confidence**: 0.94 (very high)
**Convergence**: 4 features convergent (docs ∩ code ∩ tests)

See `.canon-initiation/triangulation-report.md` for detailed extraction analysis.

## Using This Specification

### For Development
Consult this specification when:
- Adding new features or capabilities
- Modifying existing behavior
- Making architectural decisions
- Understanding system concepts

### For Integration
Use the contracts to:
- Understand module interfaces
- Plan integrations
- Write consumer code
- Design extensions

### For Verification
Use the scenarios and properties to:
- Understand expected behavior
- Write additional tests
- Verify correctness
- Debug failures

## Current Status

**Phase**: INITIALIZED

The Canon has a solid foundation with vocabulary, ontology, and feature metadata. Detailed contract and scenario extraction is in progress.

**Next steps**:
1. Extract contracts from package exports
2. Extract scenarios from test files
3. Document invariants and properties
4. Run verification

See `.canon-check-plan.md` for detailed action items.

## License

MIT (same as implementation)
