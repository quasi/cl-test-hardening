# Documentation Index

Complete guide to all cl-test-hardening documentation.

## Getting Started

Start here if you're new:

1. **[README](../README.md)** — Project overview and quick examples
2. **[Quickstart Guide](quickstart.md)** — Installation and 5-minute intro
3. **[Why Test Hardening?](explanation/why-test-hardening.md)** — Philosophy and benefits

## Tutorials (Learn by Doing)

Step-by-step guides with complete examples:

- **[Property-Based Testing](tutorials/property-based-testing.md)** — Test a stack implementation
- **[Mutation Testing](tutorials/mutation-testing.md)** — Measure test suite quality
- **[Contract Testing](tutorials/contract-testing.md)** — Test service integrations
- **[Test Organization](tutorials/test-organization.md)** — Harness and fixtures

## API Reference (Look Up Functions)

Complete function documentation:

- **[API Overview](reference/api-overview.md)** — All modules at a glance
- **[Property Testing API](reference/property-api.md)** — Generators and properties
- **[Test Harness API](reference/harness-api.md)** — Declarative environment setup
- **[Test Fixture API](reference/fixture-api.md)** — Reusable test data
- **[Mutation Testing API](reference/mutation-api.md)** — Coming soon
- **[Contract Testing API](reference/contract-api.md)** — Coming soon
- **[Agent Verification API](reference/agent-api.md)** — Coming soon

## Explanation (Understand Concepts)

Conceptual guides:

- **[Why Test Hardening?](explanation/why-test-hardening.md)** — Problems and solutions
- **[Glossary](explanation/glossary.md)** — Key terms defined

## By Module

### Property Testing (`th.property`, `th.gen`)
- [Quickstart example](quickstart.md#property-based-testing-5am-integration)
- [Tutorial](tutorials/property-based-testing.md)
- [API Reference](reference/property-api.md)

### Mutation Testing (`th.mutation`)
- [Quickstart example](quickstart.md#mutation-testing)
- [Tutorial](tutorials/mutation-testing.md)
- [API Reference](reference/mutation-api.md) (coming soon)

### Contract Testing (`th.contract`)
- [Quickstart example](quickstart.md#contract-testing)
- [Tutorial](tutorials/contract-testing.md)
- [API Reference](reference/contract-api.md) (coming soon)

### Agent Verification (`th.agent`)
- [Quickstart example](quickstart.md#agent-verification)
- [API Reference](reference/agent-api.md) (coming soon)

### Test Harness (`th.harness`)
- [Quickstart example](quickstart.md#test-harness-eliminate-boilerplate)
- [Tutorial](tutorials/test-organization.md)
- [API Reference](reference/harness-api.md)

### Test Fixtures (`th.fixture`)
- [Quickstart example](quickstart.md#test-fixtures-reusable-test-data)
- [Tutorial](tutorials/test-organization.md)
- [API Reference](reference/fixture-api.md)

## Examples

Copy-paste examples to try immediately:

- **[examples/getting-started.lisp](../examples/getting-started.lisp)** — Runnable examples

## For Contributors

Development documentation:

- **[CLAUDE.md](../CLAUDE.md)** — Agent development guide
- **[canonical-specification/](../canonical-specification/)** — Formal specifications

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Get started quickly | [Quickstart](quickstart.md) |
| Learn property testing | [Property Tutorial](tutorials/property-based-testing.md) |
| Look up a function | [API Overview](reference/api-overview.md) |
| Understand test hardening | [Why Test Hardening?](explanation/why-test-hardening.md) |
| Reduce test boilerplate | [Test Organization](tutorials/test-organization.md) |
| See all terms | [Glossary](explanation/glossary.md) |
| Copy examples | [examples/getting-started.lisp](../examples/getting-started.lisp) |

## Documentation Status

✅ Complete:
- README, Quickstart
- Property testing (tutorial + API)
- Harness and Fixture (tutorial + API)
- Contract testing tutorial
- Mutation testing tutorial
- Why test hardening, Glossary

🚧 Coming Soon:
- Mutation testing API reference
- Contract testing API reference
- Agent verification API reference

## Feedback

Found an issue? Have a suggestion? Please open an issue on GitHub.
