# Glossary

<!-- Generated from: canon/core/foundation/vocabulary.md -->

Core terms and concepts for cl-test-hardening.

## Test Hardening

The practice of making tests more rigorous and reliable so they accurately reflect code quality and don't provide false confidence. Tests should reveal problems, not hide them.

**Why it matters**: Traditional code coverage metrics can give false confidence. A test suite with 100% coverage might still miss bugs if tests don't actually verify behavior. Test hardening ensures your tests are actually doing their job.

## Verification Result

Standard result object returned by all modules. Every verification operation in cl-test-hardening returns this consistent structure.

**Fields**:
- `passed-p`: Boolean success indicator
- `timestamp`: When verification occurred (universal time)
- `duration-ms`: How long it took (milliseconds)
- `summary`: Human-readable summary
- `details`: Additional diagnostic information (property list)

**Example**:
```lisp
#<VERIFICATION-RESULT
  PASSED-P: T
  TIMESTAMP: 3907012345
  DURATION-MS: 150
  SUMMARY: "Property holds for all 100 test cases"
  DETAILS: (:CASES-CHECKED 100 :SHRINKS 0)>
```

## Property-Based Testing

Testing approach that verifies properties (invariants) hold across many generated test cases rather than checking specific examples.

**How it works**:
1. Define a property: "What should always be true?"
2. Generate random inputs
3. Check if property holds for each input
4. If failure found, shrink to minimal failing case
5. Report results

**Example property**: "Sorting a list twice gives the same result as sorting once"

### Generator

Function that produces random test data with controlled complexity.

**Characteristics**:
- Takes a size parameter controlling complexity
- Returns different values on each call
- Pure function (no side effects)
- Composable with other generators

**Example**: A generator for integers between 0 and 100:
```lisp
(gen-integer 0 100)
```

### Shrinking

Process of reducing a failing test case to its simplest form.

**Why it matters**: If property fails on input `(list 42 99 -5 108 3 17)`, shrinking finds the minimal failure, perhaps just `(list -5)`. This makes debugging much easier.

**How it works**:
1. Property fails on some input
2. Try simpler variants (remove elements, reduce numbers, etc.)
3. Keep variants that still fail
4. Continue until no simpler failure exists
5. Report minimal case

## Mutation Testing

Quality verification technique that introduces small changes (mutations) to code and checks if tests catch them. Measures test suite effectiveness, not code coverage.

**Why it matters**: Tests can have 100% code coverage but still miss bugs. Mutation testing answers: "Would your tests catch a real bug?"

### Mutant

Code with a single intentional defect introduced by a mutation operator.

**Example**:
```lisp
;; Original
(defun calculate-discount (price)
  (if (> price 100)
      (* price 0.9)
      price))

;; Mutant (operator: flip comparison)
(defun calculate-discount (price)
  (if (< price 100)  ; Changed > to <
      (* price 0.9)
      price))
```

### Mutation Operator

Rule for creating mutations.

**Common operators**:
- **Flip boolean**: `t` ↔ `nil`
- **Change arithmetic**: `+` → `-`, `*` → `/`
- **Modify comparison**: `<` → `<=`, `>` → `>=`, `=` → `/=`
- **Remove function call**: `(func x)` → `x`
- **Change constant**: `0` → `1`, `1` → `0`

### Killed Mutant

Mutation caught by tests (good outcome).

**Meaning**: At least one test fails when this mutation is present. This shows your tests are actually checking that behavior.

### Survived Mutant

Mutation not caught by tests (bad outcome).

**Meaning**: All tests pass despite the code being broken. This indicates:
- Missing test cases
- Weak assertions
- Dead code (no tests exercise this path)

### Mutation Score

Percentage of mutants killed by tests.

**Formula**:
```
mutation-score = (killed-mutants / total-mutants) × 100%
```

**Interpretation**:
- **90-100%**: Excellent test suite
- **75-90%**: Good test coverage
- **50-75%**: Adequate but room for improvement
- **<50%**: Weak test suite, many gaps

## Contract Testing

Consumer-driven approach to integration testing. Consumers define expectations (contracts), providers verify they meet them. Enables independent service testing without full integration environments.

**Why it matters**: Integration tests are slow and fragile. Contract testing lets services test independently while ensuring compatibility.

### Contract

Specification of expected interactions between consumer and provider.

**Contains**:
- Request details (method, path, headers, body)
- Response expectations (status, headers, body)
- Matching rules (exact, regex, type)

**Example**:
```
Interaction: "Get user by ID"
  Request: GET /users/123
  Response: 200 OK
    Body: {"id": 123, "name": <string>}
```

### Consumer

Service that depends on another service (the provider).

**Role**:
1. Define what it expects from provider
2. Test against mock provider
3. Generate contract (Pact file)
4. Publish contract for provider

### Provider

Service being depended upon by consumers.

**Role**:
1. Receive contracts from consumers
2. Verify it meets all expectations
3. Fix any contract violations
4. Publish verification results

### Pact

Contract format (JSON file describing interactions).

**Structure**:
```json
{
  "consumer": {"name": "frontend"},
  "provider": {"name": "api"},
  "interactions": [...]
}
```

### Mock Provider

Test double that replays recorded interactions for consumer testing.

**How it works**:
1. Consumer defines expected interactions
2. Mock provider is configured with these expectations
3. Consumer makes requests to mock
4. Mock validates requests and returns expected responses
5. Generates Pact file from recorded interactions

## Agent Verification

Multi-dimensional verification of AI-generated code across scope, hallucination, style, semantics, and complexity.

**Why it matters**: AI code generators can produce syntactically valid code that's wrong in subtle ways. Agent verification catches these issues.

### Five Dimensions

1. **Scope**: Code stays within specified boundaries
2. **Hallucination**: Only references real APIs
3. **Style**: Matches codebase conventions
4. **Semantics**: Logic is correct
5. **Complexity**: Appropriately simple

### Scope Violation

Code outside specified boundaries.

**Examples**:
- Using forbidden packages
- Calling unauthorized APIs
- Importing blacklisted dependencies

### Hallucination

References to non-existent APIs or features.

**Examples**:
- Function that doesn't exist: `(frobnicate x)`
- Package that doesn't exist: `foo:bar`
- Method with wrong signature: `(calculate a)` when it needs 2 args

### Style Violation

Code inconsistent with codebase conventions.

**Examples**:
- Wrong naming: `getFoo` instead of `get-foo`
- Wrong indentation
- Non-idiomatic patterns

### Semantic Violation

Logically incorrect despite syntactic validity.

**Examples**:
- Type mismatch: passing string where integer expected
- Logic error: `(if x (do-y) (do-y))` — both branches identical
- Off-by-one: `(< i len)` instead of `(<= i len)`

### Complexity Violation

Unnecessarily complex implementation.

**Examples**:
- Excessive nesting depth
- Over-abstraction for simple task
- Complex expression when simple one suffices
