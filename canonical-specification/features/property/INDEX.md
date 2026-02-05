# Property-Based Testing Feature

**Purpose**: Property-based testing with generators and shrinking for Common Lisp.

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand what property-based testing is | `../../core/foundation/vocabulary.md` → "Property-Based Testing" |
| Learn the generator contract | `contracts/generators.md` |
| Learn the property verification contract | `contracts/properties.md` |
| See how to use generators | `scenarios/generator-usage.md` |
| See how to check properties | `scenarios/property-checking.md` |
| Understand guarantees | `properties/property-testing-guarantees.md` |
| Understand generator constraints | `properties/generator-invariants.md` |

## Contents

### Contracts (2)

| Contract | Description | ~Lines |
|----------|-------------|--------|
| `contracts/generators.md` | Generator creation and composition | TBD |
| `contracts/properties.md` | Property definition and verification | TBD |

### Scenarios (2)

| Scenario | Description | ~Lines |
|----------|-------------|--------|
| `scenarios/generator-usage.md` | Creating and using generators for test data | TBD |
| `scenarios/property-checking.md` | Defining and checking properties | TBD |

### Properties (2)

| Property | Description | ~Lines |
|----------|-------------|--------|
| `properties/property-testing-guarantees.md` | Invariants for property verification | TBD |
| `properties/generator-invariants.md` | Constraints on generator behavior | TBD |

## Key Concepts

### Generator

A generator is a pure function that produces random test data. Generators:
- Take a size parameter controlling complexity
- Return different values on each invocation
- Are composable via combinators
- Respect type constraints

### Property

A property is an assertion that should hold for all valid inputs. Properties:
- Take generated inputs
- Return boolean or raise error
- Must be deterministic for same input
- Can use helper assertions

### Shrinking

When a property fails, shrinking finds the minimal failing case:
1. Property fails on some generated input
2. Shrinking attempts simpler variants
3. Continues until no simpler failure found
4. Reports minimal failing case

## Implementation Location

| Artifact | Source File |
|----------|-------------|
| Generator core | `src/property/core.lisp` |
| Random utilities | `src/property/random.lisp` |
| Shrinking | `src/property/shrinking.lisp` |
| Runner | `src/property/runner.lisp` |
| Standard generators | `generators/primitives.lisp`, `generators/collections.lisp`, `generators/combinators.lisp` |

## Dependencies

- Depends on: `core/` (for verification result protocol)
- Used by: `canon/` (for specification verification)

## Typical Workflow

1. **Define property**: What should always be true?
2. **Choose generator**: What inputs to test?
3. **Run verification**: Does property hold?
4. **Analyze failures**: What minimal case fails?
5. **Fix code**: Correct the implementation
6. **Re-verify**: Does property hold now?
