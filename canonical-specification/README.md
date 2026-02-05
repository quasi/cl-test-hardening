# Canon Specification for cl-test-hardening

This directory contains the Canon specification for the cl-test-hardening library.

## What is Canon?

Canon is a specification-first development system that maintains a formal specification alongside the implementation. This specification was extracted from the existing codebase through multi-source triangulation.

## Structure

```
canon/
├── canon.yaml                 # Manifest
├── README.md                  # This file
├── core/
│   ├── foundation/
│   │   ├── vocabulary.md      # Core terms and concepts
│   │   └── ontology.md        # Relationships between concepts
│   ├── contracts/             # (Future: core contracts)
│   └── decisions/             # Architecture decision records
├── features/
│   ├── property/              # Property testing module spec
│   ├── mutation/              # Mutation testing module spec
│   ├── contract/              # Contract testing module spec
│   └── agent/                 # Agent verification module spec
└── verification/
    └── test-integration.md    # How verification works

```

## Extraction Summary

This Canon was initialized on 2026-01-27 through automated extraction:

- **Overall confidence**: 0.94 (very high)
- **Convergent features**: 4 (all core modules)
- **Code-only features**: 1 (Canon adapter - recently added)
- **Conflicts**: 0

See `.canon-initiation/triangulation-report.md` for detailed analysis.

## Using This Specification

### For Development
Consult the specification when adding features or making changes to ensure consistency with the architectural vision.

### For Verification
Run verification to ensure implementation matches specification:

```lisp
(ql:quickload "cl-test-hardening/tests")
(5am:run! :th.tests)
```

### For Documentation
Generate documentation from the specification using `/canon docs`.

## Module Specifications

Each module has its own specification in `features/<module>/`:

- **property**: Property-based testing with generators and shrinking
- **mutation**: Mutation testing for test quality verification
- **contract**: Consumer-driven contract testing (Pact-style)
- **agent**: AI-generated code verification across five dimensions

## Next Steps

1. Review the one observation in `.canon-initiation/observations.yaml`
2. Update README.md to include Canon adapter in module list
3. Evolve specification as new features are added using `/canon specify`
