# Agent Verification Feature

**Purpose**: Multi-dimensional verification of AI-generated code across scope, hallucination, style, semantics, and complexity.

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand what agent verification is | `../../core/foundation/vocabulary.md` → "Agent Verification" |
| Learn the verification API | `contracts/agent-verification.md` |
| See multi-dimensional checking | `scenarios/multi-dimensional-verification.md` |
| Understand verification guarantees | `properties/verification-guarantees.md` |

## Contents

### Contracts (1)

| Contract | Description | ~Lines |
|----------|-------------|--------|
| `contracts/agent-verification.md` | Agent code verification API and dimensions | TBD |

### Scenarios (1)

| Scenario | Description | ~Lines |
|----------|-------------|--------|
| `scenarios/multi-dimensional-verification.md` | Verifying AI code across all dimensions | TBD |

### Properties (1)

| Property | Description | ~Lines |
|----------|-------------|--------|
| `properties/verification-guarantees.md` | Invariants for verification process | TBD |

## Key Concepts

### Five Verification Dimensions

1. **Scope**: Code stays within specified boundaries
   - Checks: Uses only allowed symbols/packages
   - Detects: Unauthorized dependencies, forbidden APIs

2. **Hallucination**: References only real APIs
   - Checks: All symbols exist and are exported
   - Detects: Non-existent functions, imagined packages

3. **Style**: Matches codebase conventions
   - Checks: Naming, indentation, idioms
   - Detects: Inconsistent patterns, non-idiomatic code

4. **Semantics**: Logic is correct
   - Checks: Type correctness, control flow
   - Detects: Logic errors, type mismatches

5. **Complexity**: Implementation is appropriately simple
   - Checks: Nesting depth, expression complexity
   - Detects: Over-engineering, unnecessary abstraction

### Violation

A detected issue in one dimension:
```lisp
(make-violation
  :dimension :hallucination
  :severity :error
  :location "line 42"
  :description "Function FOO:BAR does not exist"
  :suggestion "Check package exports")
```

## Implementation Location

| Artifact | Source File |
|----------|-------------|
| Verification core | `src/agent/core.lisp` |
| Violation types | `src/agent/violations.lisp` |
| Scope checking | `src/agent/scope.lisp` |
| Hallucination detection | `src/agent/hallucination.lisp` |
| Style checking | `src/agent/style.lisp` |
| Semantic analysis | `src/agent/semantics.lisp` |
| Complexity analysis | `src/agent/complexity.lisp` |

## Dependencies

- Depends on: `core/` (for verification result protocol)
- Depends on: `cl-ppcre` (pattern matching), `uiop` (utilities)
- Used by: `canon/` (for specification verification)

## Typical Workflow

1. **Configure boundaries**: Define allowed symbols/patterns
2. **Receive generated code**: Get AI output
3. **Run verification**: Check all five dimensions
4. **Analyze violations**: Review detected issues
5. **Provide feedback**: Give corrections to agent
6. **Re-verify**: Check revised code
7. **Accept or iterate**: Continue until clean

## Use Cases

### Code Review Agent

Automatically verify AI-generated pull requests:
- Scope: Only touches intended modules
- Hallucination: All APIs exist
- Style: Matches team conventions
- Semantics: Logic is sound
- Complexity: Not over-engineered

### Interactive Coding Assistant

Real-time verification during development:
- Immediate feedback on hallucinations
- Style suggestions before commit
- Complexity warnings for simplification

### Specification Compliance

Verify code meets Canon specifications:
- Uses only specified contracts
- Follows documented patterns
- Maintains semantic guarantees
