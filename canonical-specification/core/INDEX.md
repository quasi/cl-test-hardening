# Core Index

**Purpose**: Shared foundation providing terminology, domain model, and cross-cutting concerns for all test hardening modules.

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand key terminology | `foundation/vocabulary.md` |
| See entity relationships | `foundation/ontology.md` |
| Understand verification result structure | `foundation/vocabulary.md` → "Verification Result" |

## Contents

| File | Purpose | ~Lines |
|------|---------|--------|
| `foundation/vocabulary.md` | Core terms, module concepts, and verification result definition | 56 |
| `foundation/ontology.md` | Entity relationships and data model | TBD |
| `contracts/` | Core-level contracts (currently empty) | - |
| `decisions/` | Architectural decision records | - |

## Key Concepts

### Verification Result Protocol

All modules return standardized `verification-result` objects. This enables:
- Consistent reporting across modules
- Composable verification pipelines
- Uniform error handling

**Structure**:
```lisp
(make-verification-result
  :passed-p <boolean>          ; Success indicator
  :timestamp <universal-time>  ; When verification occurred
  :duration-ms <integer>       ; Execution time
  :summary <string>            ; Human-readable summary
  :details <plist>)            ; Module-specific data
```

### Module Independence

Each feature module:
- Depends only on `th.core` (defined here)
- Has its own namespace (`th.property`, `th.mutation`, etc.)
- Can be loaded independently
- Returns standard result objects

## Dependencies

This directory has no dependencies - it's the foundation everything else builds on.

## Used By

All feature modules:
- `features/property/`
- `features/mutation/`
- `features/contract/`
- `features/agent/`
- `features/canon/`
