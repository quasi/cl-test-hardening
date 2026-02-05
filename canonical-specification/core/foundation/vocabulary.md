# Vocabulary

Core terms and concepts for cl-test-hardening.

## Primary Concepts

### Common
Shortened reference to Common Lisp in compound terms.

### Common Lisp
The programming language used for this library. A multi-paradigm, dynamically-typed language standardized as ANSI Common Lisp.

### Lisp
Family of programming languages characterized by S-expression syntax and code-as-data philosophy. Common Lisp is a member of this family.

### Canon
Specification-first development system that ensures implementation matches documented contracts and requirements. Provides verification protocols and completion contracts.

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

### Generator
Function that produces random test data for property-based testing. Can combine to create complex data structures.

### Property
Assertion that should hold for all valid inputs. Describes invariants that must remain true.

### Shrinking
Process of reducing a failing test case to its simplest form. Makes debugging easier by finding minimal reproduction.

### Mutation Testing
Quality verification technique that introduces small changes (mutations) to code and checks if tests catch them. Measures test suite effectiveness, not code coverage.

### Mutant
Code with a single intentional defect introduced by a mutation operator. Used to test if test suite can detect the change.

### Mutation Operator
Rule for creating mutations in code. Examples: flip boolean, change arithmetic operator, remove statement.

### Killed Mutant
Mutation caught by tests. Indicates test suite detected the introduced defect (desirable outcome).

### Survived Mutant
Mutation not caught by tests. Indicates weak tests that fail to detect the defect (requires test improvement).

### Contract Testing
Consumer-driven approach to integration testing. Consumers define expectations (contracts), providers verify they meet them. Enables independent service testing without full integration environments.

### Contract
Specification of expected interactions between services. Defines request/response patterns that must be satisfied.

### Consumer
Service that depends on another service. Defines contracts specifying expectations from providers.

### Provider
Service being depended upon. Must verify it satisfies contracts defined by consumers.

### Pact
Contract format using JSON to describe interactions. Industry-standard format for consumer-driven contract testing.

### Broker
Central repository for storing and sharing pact contracts between consumer and provider teams. Enables decoupled testing.

### Mock Provider
Test double that replays recorded interactions. Allows consumer testing without real provider.

### Agent Verification
Multi-dimensional verification of AI-generated code across scope, hallucination, style, semantics, and complexity.

### Scope Violation
Code outside specified boundaries. Agent wrote more or different code than requested.

### Hallucination
References to non-existent APIs, functions, or features. Agent invented functionality that doesn't exist.

### Style Violation
Code inconsistent with codebase conventions. Doesn't match established patterns and formatting.

### Semantic Violation
Code that is logically incorrect despite being syntactically valid. Works but does the wrong thing.

### Complexity Violation
Unnecessarily complex implementation. Simpler solution exists but agent over-engineered.

## Development Terms

### HEAD
Git reference pointing to the current branch tip. Used to compare code changes against the latest committed state.

### Function
Named unit of code that performs a specific task. In Lisp, a first-class object that can be passed, returned, and manipulated.

### File
Named storage unit containing source code, specifications, or data. Organized hierarchically in a filesystem.

### Check
Verification step that validates a specific condition or requirement. Returns pass/fail result.

### Passed
State indicating a test or verification succeeded. Opposite of failed.

### Extract
Operation that isolates and retrieves specific information or code from a larger context.

### Use
Dependency relationship where one component relies on functionality provided by another.

### Removed
State indicating an element was deleted or is no longer present in the current version.

### Inconsistent
State where multiple elements conflict or don't align with expected patterns or rules.
