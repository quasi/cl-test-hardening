# Contract Testing Feature

**Purpose**: Consumer-driven contract testing with Pact-style workflows for Common Lisp.

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand what contract testing is | `../../core/foundation/vocabulary.md` → "Contract Testing" |
| Learn the Pact workflow | `contracts/pact-workflow.md` |
| Learn how to define contracts | `contracts/contract-definition.md` |
| See consumer-side testing | `scenarios/consumer-testing.md` |
| See provider-side verification | `scenarios/provider-verification.md` |
| Understand contract guarantees | `properties/contract-guarantees.md` |

## Contents

### Contracts (2)

| Contract | Description | ~Lines |
|----------|-------------|--------|
| `contracts/pact-workflow.md` | End-to-end Pact workflow | TBD |
| `contracts/contract-definition.md` | Contract structure and API | TBD |

### Scenarios (2)

| Scenario | Description | ~Lines |
|----------|-------------|--------|
| `scenarios/consumer-testing.md` | Testing consumer against mock provider | TBD |
| `scenarios/provider-verification.md` | Verifying provider meets contracts | TBD |

### Properties (1)

| Property | Description | ~Lines |
|----------|-------------|--------|
| `properties/contract-guarantees.md` | Invariants for contract testing | TBD |

## Key Concepts

### Consumer-Driven Contracts

Consumers (services that depend on others) define their expectations as contracts. This approach:
- Enables independent testing
- Prevents breaking changes
- Documents API usage
- Supports parallel development

### Pact

A JSON file describing expected interactions:
- Consumer name
- Provider name
- Interactions (request/response pairs)
- Matchers for flexible matching

### Mock Provider

A test double that:
- Replays recorded interactions
- Validates consumer requests
- Returns expected responses
- Generates Pact file

### Provider Verification

Process where:
1. Provider reads Pact files
2. Replays consumer requests
3. Verifies responses match expectations
4. Reports violations

## Implementation Location

| Artifact | Source File |
|----------|-------------|
| Contract core | `src/contract/core.lisp` |
| Matchers | `src/contract/matchers.lisp` |
| Schema | `src/contract/schema.lisp` |
| Interaction | `src/contract/interaction.lisp` |
| Pact generator | `src/contract/pact-generator.lisp` |
| Mock provider | `src/contract/mock-provider.lisp` |
| Verifier | `src/contract/verifier.lisp` |

## Dependencies

- Depends on: `core/` (for verification result protocol)
- Depends on: `cl-ppcre` (pattern matching), `yason` (JSON), `dexador` (HTTP), `local-time` (timestamps)
- Used by: `canon/` (for specification verification)

## Typical Workflow

### Consumer Side

1. **Define expectations**: What should provider do?
2. **Create mock provider**: Set up test double
3. **Write consumer tests**: Test against mock
4. **Generate Pact**: Record interactions
5. **Publish Pact**: Share with provider team

### Provider Side

1. **Retrieve Pacts**: Get consumer expectations
2. **Set up provider**: Ensure provider is running
3. **Run verification**: Test provider against Pacts
4. **Fix violations**: Ensure provider meets contracts
5. **Publish results**: Confirm compliance

## Contract Evolution

When APIs change:
1. Update consumer tests
2. Generate new Pact
3. Verify provider compatibility
4. Coordinate breaking changes
