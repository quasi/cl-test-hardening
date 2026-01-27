# cl-test-hardening

Test hardening library for Common Lisp. Hardens your tests so they don't lie to you.

## Modules

- **property** - Property-based testing with shrinking
- **mutation** - Mutation testing for test quality verification
- **contract** - Consumer-driven contract testing (Pact-style)
- **agent** - AI-generated code verification
- **canon** - Canon specification integration

## Quick Start

```lisp
;; Load just property testing
(ql:quickload "cl-test-hardening/property")

;; Load everything
(ql:quickload "cl-test-hardening/all")
```

See the [quickstart guide](docs/quickstart.md) for a hands-on introduction.

## Documentation

### For Human Users

- **[Quickstart](docs/quickstart.md)** - Get started in 5 minutes
- **[Tutorials](docs/tutorials/)** - Learn by doing
  - [Property-Based Testing](docs/tutorials/property-based-testing.md)
  - [Mutation Testing](docs/tutorials/mutation-testing.md)
- **[Reference](docs/reference/)** - API documentation
  - [API Overview](docs/reference/api-overview.md)
- **[Explanation](docs/explanation/)** - Conceptual guides
  - [Glossary](docs/explanation/glossary.md)

### For Contributing Agents

See [CANON.md](CANON.md) for development instructions and [canon/INDEX.md](canon/INDEX.md) for navigating Canon specifications.

### For Agent Consumers

Agent consumers can read formal API specifications directly:

- Public contracts: `canon/features/*/contracts/`
- Vocabulary: `canon/core/foundation/vocabulary.md`
- Properties: `canon/features/*/properties/`

These formal specifications include schemas, behavior rules, and verification criteria.

## License

MIT
