# Multi-Dimensional Verification Scenarios

**Status**: stable
**Source**: `tests/agent-tests.lisp`
**Contracts**: `agent-verification.md`

## Scenario: Define Verification Policy

**Given**: Need to verify agent-generated code

**When**: Policy is defined
```lisp
(define-agent-verification code-writer-policy
  :agent-type :code-writer
  :scope-rules (make-scope-rules
                :allowed-paths '("src/**/*.lisp"))
  :hallucination-rules (make-hallucination-rules
                        :check-cl-symbols t)
  :complexity-rules (make-complexity-rules
                     :max-lines-per-function 50))
```

**Then**:
- Policy is registered
- Rules can be retrieved
- Policy can be used for verification

**Rationale**: Policies define quality standards for agents.

---

## Scenario: Scope Violation Detected

**Given**: Policy allows only `src/` modifications

**When**: Agent modifies `config/settings.lisp`

**Then**:
- Scope verification fails
- Violation created:
  - Type: `:scope-violation`
  - File: `config/settings.lisp`
  - Message: "File outside allowed scope"
- Verification status: `:failed`

**Rationale**: Agents must stay within assigned boundaries.

---

## Scenario: Hallucination Detected

**Given**: Agent generates code calling `(verify-tokne user)`

**When**: Hallucination verification runs

**Then**:
- Symbol `verify-tokne` not found in CL or project
- Violation created with suggestion: "Check for 'verify-token'"
- Likely typo detected

**Rationale**: Catch references to non-existent APIs.

---

## Scenario: Style Violation Detected

**Given**: Project uses `lisp-case` naming

**When**: Agent writes `def_function_name`

**Then**:
- Style verification detects `snake_case`
- Violation: "Inconsistent naming convention"
- Suggestion: "Use lisp-case: def-function-name"

**Rationale**: Maintain consistent project style.

---

## Scenario: Semantic Violation - Removed Export

**Given**: Function `user-login` is exported

**When**: Agent removes it from exports

**Then**:
- Semantic verification fails
- Violation: "Removed exported symbol user-login"
- Breaking change detected

**Rationale**: Preserve public API.

---

## Scenario: Complexity Violation Detected

**Given**: Max function length is 50 lines

**When**: Agent writes 75-line function

**Then**:
- Complexity verification fails
- Violation: "Function exceeds 50 lines"
- Suggestion: "Extract helper functions"

**Rationale**: Enforce code simplicity.

---

## Scenario: All Dimensions Pass

**Given**: Agent generates clean code

**When**: Verification runs all dimensions

**Then**:
- Scope: ✓ Passed
- Hallucination: ✓ Passed
- Style: ✓ Passed
- Semantic: ✓ Passed
- Complexity: ✓ Passed
- Overall status: `:passed`

**Rationale**: Well-formed code passes all checks.

---

## Scenario: Warnings vs Errors

**Given**: Minor style inconsistency

**When**: Violation has severity `:warning`

**Then**:
- Overall status: `:warnings`
- Verification does not fail
- User notified for review

**Rationale**: Warnings inform without blocking.

---

## Scenario: Verify Git Changes

**Given**: Uncommitted changes in repository

**When**: `verify-git-changes` is called

**Then**:
- `git diff` finds modified files
- Only changed files verified
- Results cover actual agent work

**Rationale**: Focus verification on agent changes.

---

## Scenario: JSON Export for Tooling

**Given**: Verification result

**When**: `verification-to-json` is called

**Then**:
- Result serialized to JSON
- Violations include all metadata
- Tooling can parse and display

**Rationale**: Enable IDE/CI integration.

---

## Verification

Run tests:
```lisp
(5am:run! :th.agent-tests)
```

All agent verification scenarios must pass.
