# cl-test-hardening

Test hardening library for Common Lisp. Hardens your tests so they don't lie to you.

## Modules

- **property** - Property-based testing with shrinking
- **mutation** - Mutation testing for test quality verification
- **contract** - Consumer-driven contract testing (Pact-style)
- **agent** - AI-generated code verification

## Quick Start

```lisp
;; Load just property testing
(ql:quickload "cl-test-hardening/property")

;; Load everything
(ql:quickload "cl-test-hardening")
```

## License

MIT
