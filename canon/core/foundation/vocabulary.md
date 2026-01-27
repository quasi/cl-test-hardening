# Vocabulary

Core terms and concepts for cl-test-hardening.

## Primary Concepts

### Test Hardening
The practice of making tests more rigorous and reliable so they accurately reflect code quality and don't provide false confidence. Tests should reveal problems, not hide them.

### Verification Result
Standard result object returned by all modules. Contains:
- `passed-p`: Boolean success indicator
- `timestamp`: When verification occurred
- `duration-ms`: How long it took
- `summary`: Human-readable summary
- `details`: Additional diagnostic information

## Module-Specific Concepts

### Property-Based Testing
Testing approach that verifies properties (invariants) hold across many generated test cases rather than checking specific examples. Includes automatic shrinking to find minimal failing cases.

**Key terms:**
- **Generator**: Function that produces random test data
- **Property**: Assertion that should hold for all valid inputs
- **Shrinking**: Process of reducing a failing test case to its simplest form

### Mutation Testing
Quality verification technique that introduces small changes (mutations) to code and checks if tests catch them. Measures test suite effectiveness, not code coverage.

**Key terms:**
- **Mutant**: Code with a single intentional defect
- **Mutation Operator**: Rule for creating mutations (e.g., flip boolean, change arithmetic)
- **Killed Mutant**: Mutation caught by tests (good)
- **Survived Mutant**: Mutation not caught by tests (indicates weak tests)

### Contract Testing
Consumer-driven approach to integration testing. Consumers define expectations (contracts), providers verify they meet them. Enables independent service testing without full integration environments.

**Key terms:**
- **Contract**: Specification of expected interactions
- **Consumer**: Service that depends on another service
- **Provider**: Service being depended upon
- **Pact**: Contract format (JSON file describing interactions)
- **Mock Provider**: Test double that replays recorded interactions

### Agent Verification
Multi-dimensional verification of AI-generated code across scope, hallucination, style, semantics, and complexity.

**Key terms:**
- **Scope Violation**: Code outside specified boundaries
- **Hallucination**: References to non-existent APIs or features
- **Style Violation**: Inconsistent with codebase conventions
- **Semantic Violation**: Logically incorrect despite syntactic validity
- **Complexity Violation**: Unnecessarily complex implementation
