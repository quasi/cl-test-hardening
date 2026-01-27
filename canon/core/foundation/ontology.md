# Ontology

Relationships between concepts in cl-test-hardening.

## Module Relationships

```
cl-test-hardening (core)
├── th.core (shared result protocol)
│   ├── verification-result
│   ├── format-result
│   └── format-summary
│
├── property (independent)
│   ├── depends-on: th.core
│   ├── generators/ (sub-system)
│   └── provides: property-based testing
│
├── mutation (independent)
│   ├── depends-on: th.core
│   ├── operators/ (sub-system)
│   └── provides: mutation testing
│
├── contract (independent)
│   ├── depends-on: th.core
│   └── provides: contract testing
│
├── agent (independent)
│   ├── depends-on: th.core
│   └── provides: agent code verification
│
└── canon (adapter)
    ├── depends-on: all modules
    └── provides: Canon specification integration
```

## Result Protocol Flow

All modules follow the same protocol:

```
Module Operation
    ↓
Generate verification-result
    ↓
Return result object
    ↓
format-result / format-summary / format-failure
    (generic functions)
    ↓
Human-readable output
```

## Independence Principle

Each module can be loaded independently. No cross-module dependencies except on `th.core`.

```lisp
;; Load only what you need
(ql:quickload "cl-test-hardening/property")  ;; Just property testing
(ql:quickload "cl-test-hardening/mutation")  ;; Just mutation testing
(ql:quickload "cl-test-hardening/contract")  ;; Just contract testing
(ql:quickload "cl-test-hardening/agent")     ;; Just agent verification
(ql:quickload "cl-test-hardening/all")       ;; Everything
```
