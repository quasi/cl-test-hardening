# Canon Integration Feature

**Purpose**: Adapter for integrating cl-test-hardening with the Canon specification system.

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand the verification protocol | `contracts/verification-protocol.md` |
| See protocol guarantees | `properties/protocol-guarantees.md` |
| Understand how Canon works | `../../README-CANON.md` |

## Contents

### Contracts (1)

| Contract | Description | ~Lines |
|----------|-------------|--------|
| `contracts/verification-protocol.md` | Canon verification protocol implementation | TBD |

### Scenarios (0)

No scenarios currently defined for this feature.

### Properties (1)

| Property | Description | ~Lines |
|----------|-------------|--------|
| `properties/protocol-guarantees.md` | Invariants for protocol adapter | TBD |

## Key Concepts

### Canon Verification Protocol

The protocol defines how Canon specifications are verified:
1. Parse Canon artifacts (contracts, scenarios, properties)
2. Map to appropriate test hardening module
3. Execute verification
4. Return standardized result

### Multi-Module Orchestration

The adapter coordinates verification across modules:
- Property-based tests verify properties from Canon
- Mutation tests assess test quality for scenarios
- Contract tests validate API specifications
- Agent verification checks generated implementations

## Implementation Location

| Artifact | Source File |
|----------|-------------|
| Protocol adapter | `src/canon/adapter.lisp` |
| Package definition | `src/canon/package.lisp` |

## Dependencies

- Depends on: `core/` (verification result protocol)
- Depends on: All feature modules (`property`, `mutation`, `contract`, `agent`)
- Depends on: `yason` (JSON parsing), `fiveam` (test integration)

## Typical Workflow

1. **Canon specifies feature**: Contracts, scenarios, properties defined
2. **Adapter parses specs**: Read Canon artifacts
3. **Map to modules**: Determine which module verifies what
4. **Execute verification**: Run appropriate test hardenings
5. **Aggregate results**: Combine verification results
6. **Report to Canon**: Return compliance status

## Integration Points

### Canon → cl-test-hardening

Canon specifications are verified by:
- **Properties** → Property-based testing module
- **Contracts** → Contract testing module
- **Scenarios** → Used to generate test cases
- **Implementation** → Agent verification

### cl-test-hardening → Canon

Verification results flow back:
- `verification-result` objects
- Standardized pass/fail status
- Detailed violation reports
- Suggestions for fixing issues

## Example Flow

```
Canon Specification
       ↓
   [Parser]
       ↓
Feature: "orders"
  Property: "total is sum of items"
  Contract: "POST /orders"
  Scenario: "happy path"
       ↓
   [Adapter Routes]
       ↓
  ┌──────┬──────────┬─────────┐
  ↓      ↓          ↓         ↓
Property Mutation Contract Agent
  Test   Test      Test     Verify
       ↓
[Aggregate Results]
       ↓
verification-result
       ↓
   Canon CCC
```
