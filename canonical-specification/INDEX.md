# Canon Index

**Project**: cl-test-hardening
**Canon Version**: 0.1.0
**Description**: Test hardening library for Common Lisp - hardens tests so they don't lie to you

## Quick Navigation

| I want to... | Start here |
|--------------|------------|
| Understand what this project does | `core/foundation/vocabulary.md` |
| Understand the domain model | `core/foundation/ontology.md` |
| Implement property-based testing | `features/property/INDEX.md` |
| Implement mutation testing | `features/mutation/INDEX.md` |
| Implement contract testing | `features/contract/INDEX.md` |
| Verify agent-generated code | `features/agent/INDEX.md` |
| Integrate with Canon system | `features/canon/INDEX.md` |
| Find all features at a glance | `features/INDEX.md` |

## Structure

```
canon/
├── core/                           # Shared foundation
│   ├── foundation/
│   │   ├── vocabulary.md          # Domain terms and definitions
│   │   └── ontology.md            # Entity relationships
│   ├── contracts/                 # Core contracts (if any)
│   └── decisions/                 # Architecture decisions
│
└── features/                       # Feature specifications
    ├── property/                   # Property-based testing
    ├── mutation/                   # Mutation testing
    ├── contract/                   # Contract testing (Pact-style)
    ├── agent/                      # Agent code verification
    └── canon/                      # Canon adapter integration
```

## Features Overview

| Feature | Description | Contracts | Scenarios | Properties |
|---------|-------------|-----------|-----------|------------|
| **property** | Property-based testing with generators and shrinking | 2 | 2 | 2 |
| **mutation** | Mutation testing for test suite quality | 2 | 1 | 1 |
| **contract** | Consumer-driven contract testing (Pact-style) | 2 | 2 | 1 |
| **agent** | Multi-dimensional AI code verification | 1 | 1 | 1 |
| **canon** | Canon specification protocol adapter | 1 | 0 | 1 |

See `features/INDEX.md` for detailed feature breakdown.

## Common Workflows

### Adding a New Test Hardening Technique

1. Read `core/foundation/vocabulary.md` to understand terminology
2. Study similar feature (e.g., `features/property/` for generator patterns)
3. Define contracts in `features/{new-feature}/contracts/`
4. Write scenarios in `features/{new-feature}/scenarios/`
5. Document properties in `features/{new-feature}/properties/`

### Understanding the Result Protocol

1. Read `core/foundation/vocabulary.md` → "Verification Result"
2. See implementation: `src/core/results.lisp`
3. Check how modules use it: `features/*/contracts/*.md`

### Debugging Failed Verification

1. Check which module failed: Read the summary in `verification-result`
2. Read relevant scenario: `features/{module}/scenarios/*.md`
3. Check expected properties: `features/{module}/properties/*.md`
4. Review contract: `features/{module}/contracts/*.md`
