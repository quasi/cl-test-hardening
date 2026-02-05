# cl-test-hardening

Test hardening library for Common Lisp. **Hardens your tests so they don't lie to you.**

[![Tests](https://img.shields.io/badge/tests-1064%20passing-success)]()
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Why Test Hardening?

Your tests pass. Coverage is 100%. Then production breaks.

**Why?** Your tests lied. They passed without actually verifying behavior.

Test hardening fixes this with six techniques:

1. **Property-Based Testing** — Test algorithms with 1000 random cases, not 3 examples
2. **Mutation Testing** — Introduce bugs to verify tests actually catch them
3. **Contract Testing** — Fast integration tests without brittle mocks
4. **Agent Verification** — Multi-dimensional checks for AI-generated code
5. **Test Harness** — Eliminate test boilerplate with declarative environment setup
6. **Test Fixtures** — Reusable test data with factory functions

[Learn why test hardening matters →](docs/explanation/why-test-hardening.md)

## Quick Start

### Installation

```lisp
(ql:quickload "cl-test-hardening/all")
```

### 30-Second Examples

**Property Testing:**
```lisp
(use-package :th.property)
(use-package :th.gen)

(defproperty reverse-twice-identity (list)
  (equal list (reverse (reverse list))))

(check-property 'reverse-twice-identity
                :generator (gen:lists (gen:integers))
                :num-tests 100)
;; ✓ Passed 100 tests
```

**Mutation Testing:**
```lisp
(use-package :th.mutation)

(define-mutation-policy arithmetic-check
  :operators '(add-to-sub sub-to-add)
  :threshold 0.8)

(run-mutation-analysis 'arithmetic-check
                       '("src/calculator.lisp")
                       "sbcl --script run-tests.lisp")
;; Score: 0.85 (85%)  Killed: 85  Survived: 15
```

**Contract Testing:**
```lisp
(use-package :th.contract)

(define-contract user-api
  :consumer "web-app"
  :provider "user-service"
  :interactions
  ((interaction "get user"
     :request ((:method "GET") (:path "/users/123"))
     :response ((:status 200) (:body ((:id 123) (:name "Alice")))))))

(with-mock-provider ('user-api :port 8080)
  (let ((user (fetch-user 123)))
    (assert (equal "Alice" (user-name user)))))
;; ✓ Consumer test passed
```

## Documentation

### Get Started

- **[Quickstart (5 minutes)](docs/quickstart.md)** — Installation and first tests
- **[Why Test Hardening?](docs/explanation/why-test-hardening.md)** — Philosophy and benefits

### Tutorials

Learn by building real examples:

- [Property-Based Testing Tutorial](docs/tutorials/property-based-testing.md)
- [Mutation Testing Tutorial](docs/tutorials/mutation-testing.md)
- [Contract Testing Tutorial](docs/tutorials/contract-testing.md)
- [Test Organization Tutorial](docs/tutorials/test-organization.md) — Harness & Fixtures

### API Reference

Complete function documentation:

- [API Overview](docs/reference/api-overview.md)
- [Property Testing API](docs/reference/property-api.md)
- [Test Harness API](docs/reference/harness-api.md)
- [Test Fixture API](docs/reference/fixture-api.md)
- [Mutation Testing API (coming soon)](docs/reference/mutation-api.md)
- [Contract Testing API (coming soon)](docs/reference/contract-api.md)
- [Agent Verification API (coming soon)](docs/reference/agent-api.md)

### Concepts

- [Glossary](docs/explanation/glossary.md) — Key terms explained

## Modules

Load individual modules as needed:

```lisp
(ql:quickload "cl-test-hardening/property")  ; Property-based testing
(ql:quickload "cl-test-hardening/mutation")  ; Mutation testing
(ql:quickload "cl-test-hardening/contract")  ; Contract testing
(ql:quickload "cl-test-hardening/agent")     ; Agent verification
(ql:quickload "cl-test-hardening/harness")   ; Test environment setup
(ql:quickload "cl-test-hardening/fixture")   ; Test fixtures
(ql:quickload "cl-test-hardening/all")       ; Everything
```

Each module is independent — load only what you need.

## Requirements

- Common Lisp (SBCL recommended)
- Quicklisp
- FiveAM (for property testing integration)

## Testing

Run all tests:

```lisp
(ql:quickload "cl-test-hardening/tests")
(5am:run! :th.tests)
;; All 1064 tests pass
```

## Project Status

**Stable** — All modules functional, tests passing, ready for use.

## Contributing

See [canonical-specification/](canonical-specification/) for formal specifications.

For development guidance, see [CLAUDE.md](CLAUDE.md).

## License

MIT License. See [LICENSE](LICENSE) for details.

## Authors

Abhijit Rao (@quasi) — quasiLabs Consulting
