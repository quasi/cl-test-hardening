# Features Index

**Purpose**: Catalog of all test hardening capabilities in cl-test-hardening.

## All Features

### Property-Based Testing (`property/`)

Verifies properties (invariants) hold across many generated test cases. Automatically finds minimal failing cases via shrinking.

**Entry point**: `property/INDEX.md`

| Artifact Type | Count | Examples |
|---------------|-------|----------|
| Contracts | 2 | generators.md, properties.md |
| Scenarios | 2 | generator-usage.md, property-checking.md |
| Properties | 2 | property-testing-guarantees.md, generator-invariants.md |

**Key capabilities**:
- Random test data generation
- Automatic shrinking of failing cases
- Composable generators
- Property verification

---

### Mutation Testing (`mutation/`)

Introduces intentional defects (mutations) to code and verifies tests catch them. Measures test suite effectiveness.

**Entry point**: `mutation/INDEX.md`

| Artifact Type | Count | Examples |
|---------------|-------|----------|
| Contracts | 2 | mutation-testing.md, mutation-operators.md |
| Scenarios | 1 | mutation-analysis.md |
| Properties | 1 | mutation-correctness.md |

**Key capabilities**:
- Mutation operator application
- Test suite execution against mutants
- Mutation score calculation
- Survivor analysis

---

### Contract Testing (`contract/`)

Consumer-driven integration testing. Consumers define expectations (contracts), providers verify compliance. Enables independent service testing.

**Entry point**: `contract/INDEX.md`

| Artifact Type | Count | Examples |
|---------------|-------|----------|
| Contracts | 2 | pact-workflow.md, contract-definition.md |
| Scenarios | 2 | consumer-testing.md, provider-verification.md |
| Properties | 1 | contract-guarantees.md |

**Key capabilities**:
- Pact file generation
- Mock provider creation
- Provider verification
- Contract evolution

---

### Agent Verification (`agent/`)

Multi-dimensional verification of AI-generated code across scope, hallucination, style, semantics, and complexity.

**Entry point**: `agent/INDEX.md`

| Artifact Type | Count | Examples |
|---------------|-------|----------|
| Contracts | 1 | agent-verification.md |
| Scenarios | 1 | multi-dimensional-verification.md |
| Properties | 1 | verification-guarantees.md |

**Key capabilities**:
- Scope boundary checking
- Hallucination detection
- Style consistency verification
- Semantic correctness validation
- Complexity analysis

---

### Canon Integration (`canon/`)

Adapter for integrating cl-test-hardening with the Canon specification system. Provides verification protocol implementation.

**Entry point**: `canon/INDEX.md`

| Artifact Type | Count | Examples |
|---------------|-------|----------|
| Contracts | 1 | verification-protocol.md |
| Scenarios | 0 | - |
| Properties | 1 | protocol-guarantees.md |

**Key capabilities**:
- Canon verification protocol
- Specification contract checking
- Multi-module verification orchestration

---

## Feature Selection Guide

| If you need to... | Use feature |
|-------------------|-------------|
| Verify invariants hold across many cases | `property` |
| Check if your tests actually catch bugs | `mutation` |
| Test integration without full environment | `contract` |
| Verify AI-generated code quality | `agent` |
| Integrate with Canon specifications | `canon` |

## Cross-Feature Dependencies

```
property ──┐
mutation ──┼──> core (th.core)
contract ──┤
agent ─────┤
canon ─────┴──> property, mutation, contract, agent
```

**Note**: `canon` depends on all other features to provide unified verification. All other features are independent of each other.
