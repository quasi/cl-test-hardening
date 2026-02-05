---
type: contract
name: agent-verification
version: 1.0.0
feature: agent
---

# Agent Verification

**Status**: stable
**Package**: `th.agent`
**Source**: `src/agent/package.lisp:4-100`, `src/agent/core.lisp`

## Overview

Agent verification evaluates AI-generated code changes across five dimensions: scope, hallucination, style, semantics, and complexity. This ensures generated code meets quality standards before integration.

## Policy Definition

### define-agent-verification

```lisp
(define-agent-verification name &key agent-type scope-rules hallucination-rules
                                      style-rules semantic-rules complexity-rules)
```

**Purpose**: Define verification policy for an agent type.

**Parameters**:
- `name`: Symbol naming the policy
- `:agent-type`: Agent classification (e.g., `:code-writer`, `:refactorer`)
- `:scope-rules`: Rules for allowed file changes
- `:hallucination-rules`: Rules for API usage
- `:style-rules`: Code style requirements
- `:semantic-rules`: Semantic preservation rules
- `:complexity-rules`: Complexity limits

**Example**:
```lisp
(define-agent-verification feature-writer-policy
  :agent-type :code-writer
  :scope-rules (make-scope-rules
                :allowed-paths '("src/**/*.lisp")
                :forbidden-paths '("tests/" "config/"))
  :hallucination-rules (make-hallucination-rules
                        :check-cl-symbols t
                        :check-project-apis t)
  :style-rules (make-style-rules
                :naming-convention :lisp-case
                :require-docstrings t)
  :semantic-rules (make-semantic-rules
                   :preserve-exports t
                   :preserve-signatures t)
  :complexity-rules (make-complexity-rules
                     :max-lines-per-function 50
                     :max-nesting-depth 4
                     :max-cyclomatic-complexity 10))
```

**Contract**:
- Policy is registered globally
- Can be used with `verify-agent-work`
- Each dimension is optional (nil = skip verification)

## Policy Structure

```lisp
(defstruct verification-policy
  name
  agent-type
  scope-rules
  hallucination-rules
  style-rules
  semantic-rules
  complexity-rules)
```

**Accessors**:
- `verification-policy-name`
- `verification-policy-agent-type`
- `verification-policy-scope-rules`
- `verification-policy-hallucination-rules`
- `verification-policy-style-rules`
- `verification-policy-semantic-rules`
- `verification-policy-complexity-rules`

## Running Verification

### verify-agent-work

```lisp
(defun verify-agent-work (policy-name task-description files) => agent-verification-result)
```

**Purpose**: Verify agent-generated changes.

**Parameters**:
- `policy-name`: Name of verification policy
- `task-description`: What the agent was asked to do
- `files`: List of file paths to verify

**Returns**: `agent-verification-result` instance.

**Workflow**:
1. Load policy
2. Run each enabled dimension verification
3. Collect violations
4. Determine overall status
5. Return result

**Contract**:
- Verifies all files specified
- Each dimension runs independently
- Violations collected across all dimensions
- Status is `:passed`, `:warnings`, or `:failed`

### quick-verify

```lisp
(defun quick-verify (files) => agent-verification-result)
```

**Purpose**: Run verification with default policy.

**Parameters**:
- `files`: List of file paths

**Returns**: Verification result.

**Contract**: Uses standard default policy.

### verify-and-report

```lisp
(defun verify-and-report (policy-name task-description files &key output))
```

**Purpose**: Run verification and print report.

**Parameters**:
- Same as `verify-agent-work`
- `:output`: Output stream (default: `*standard-output*`)

### verify-git-changes

```lisp
(defun verify-git-changes (policy-name task-description &key ref) => agent-verification-result)
```

**Purpose**: Verify uncommitted git changes.

**Parameters**:
- `policy-name`: Verification policy
- `task-description`: Agent task description
- `:ref`: Git ref to compare against (default: `HEAD`)

**Contract**:
- Uses `git diff` to find changed files
- Verifies only modified files

## Dimension Verifications

### run-scope-verification

```lisp
(defun run-scope-verification (files scope-rules) => dimension-result)
```

**Purpose**: Verify files are within allowed scope.

**Checks**:
- Files match allowed patterns
- No forbidden paths touched
- No files outside scope

**Violations**:
- File in forbidden path
- File outside allowed patterns

---

### run-hallucination-verification

```lisp
(defun run-hallucination-verification (files hallucination-rules) => dimension-result)
```

**Purpose**: Detect references to non-existent APIs.

**Checks**:
- All CL symbols exist in Common Lisp
- Project functions/variables defined
- Package references valid

**Violations**:
- Reference to non-existent CL symbol
- Call to undefined project function
- Use of non-existent package

---

### run-style-verification

```lisp
(defun run-style-verification (files style-rules) => dimension-result)
```

**Purpose**: Verify code style consistency.

**Checks**:
- Naming convention adherence
- Docstring presence (if required)
- Indentation consistency
- Line length limits

**Violations**:
- Inconsistent naming (snake_case vs lisp-case)
- Missing docstrings
- Style inconsistency with project

---

### run-semantic-verification

```lisp
(defun run-semantic-verification (files semantic-rules) => dimension-result)
```

**Purpose**: Verify semantic preservation.

**Checks**:
- Exported symbols unchanged (if required)
- Function signatures compatible
- Contract preservation

**Violations**:
- Removed exports
- Changed function arity
- Incompatible signature change

---

### run-complexity-verification

```lisp
(defun run-complexity-verification (files complexity-rules) => dimension-result)
```

**Purpose**: Verify code complexity within limits.

**Checks**:
- Lines per function
- Nesting depth
- Cyclomatic complexity

**Violations**:
- Function too long
- Excessive nesting
- High cyclomatic complexity

## Rule Structures

### make-scope-rules

```lisp
(defun make-scope-rules (&key allowed-paths forbidden-paths) => scope-rules)
```

**Parameters**:
- `:allowed-paths`: List of glob patterns for allowed files
- `:forbidden-paths`: List of glob patterns for forbidden files

---

### make-hallucination-rules

```lisp
(defun make-hallucination-rules (&key check-cl-symbols check-project-apis
                                       project-root) => hallucination-rules)
```

**Parameters**:
- `:check-cl-symbols`: Verify CL symbol existence (boolean)
- `:check-project-apis`: Verify project function calls (boolean)
- `:project-root`: Root directory for project analysis

---

### make-style-rules

```lisp
(defun make-style-rules (&key naming-convention require-docstrings
                              max-line-length) => style-rules)
```

**Parameters**:
- `:naming-convention`: `:lisp-case`, `:snake-case`, or `:any`
- `:require-docstrings`: Require docstrings on public functions
- `:max-line-length`: Maximum characters per line

---

### make-semantic-rules

```lisp
(defun make-semantic-rules (&key preserve-exports preserve-signatures) => semantic-rules)
```

**Parameters**:
- `:preserve-exports`: Don't remove exported symbols
- `:preserve-signatures`: Maintain function arities

---

### make-complexity-rules

```lisp
(defun make-complexity-rules (&key max-lines-per-function max-nesting-depth
                                    max-cyclomatic-complexity) => complexity-rules)
```

**Parameters**:
- `:max-lines-per-function`: Function length limit
- `:max-nesting-depth`: Maximum nesting of control structures
- `:max-cyclomatic-complexity`: McCabe complexity limit

## Violation Structure

```lisp
(defstruct violation
  type
  severity
  dimension
  file
  line
  message
  suggestion)
```

**Accessors**:
- `violation-type`: Symbol (e.g., `:scope-violation`, `:hallucination`)
- `violation-severity`: `:error` or `:warning`
- `violation-dimension`: Which dimension found it
- `violation-file`: File path
- `violation-line`: Line number (if applicable)
- `violation-message`: Human-readable description
- `violation-suggestion`: Remediation advice (optional)

### make-violation

```lisp
(defun make-violation (&key type severity dimension file line message suggestion))
```

## Result Structure

```lisp
(defstruct agent-verification-result
  policy-name
  task-description
  status
  violations
  warnings
  duration-ms
  dimension-results)
```

**Accessors**:
- `agent-verification-result-policy-name`
- `agent-verification-result-task-description`
- `agent-verification-result-status`: `:passed`, `:warnings`, or `:failed`
- `agent-verification-result-violations`: List of errors
- `agent-verification-result-warnings`: List of warnings
- `agent-verification-result-duration-ms`

### verification-passed-p

```lisp
(defun verification-passed-p (result) => boolean)
```

**Purpose**: Check if verification passed (no errors).

**Returns**: `t` if no errors (warnings OK), `nil` if errors exist.

## Reporting

### format-verification-report

```lisp
(defun format-verification-report (result stream))
```

**Purpose**: Generate human-readable report.

**Output Format**:
```
Agent Verification Results
==========================
Policy: feature-writer-policy
Task: "Add user authentication"
Status: FAILED

Dimension Results:
------------------
✓ Scope: PASSED (0 violations)
✗ Hallucination: FAILED (2 violations)
✓ Style: PASSED (1 warning)
✓ Semantic: PASSED (0 violations)
✗ Complexity: FAILED (1 violation)

Violations:
-----------
[ERROR] Hallucination in src/auth.lisp:45
  Called undefined function 'verify-tokne' (typo?)
  Suggestion: Check for 'verify-token'

[ERROR] Complexity in src/auth.lisp:12
  Function 'authenticate-user' has 75 lines (limit: 50)
  Suggestion: Extract helper functions
```

### verification-to-json

```lisp
(defun verification-to-json (result) => json-string)
```

**Purpose**: Convert result to JSON for tooling integration.

### violation-to-json

```lisp
(defun violation-to-json (violation) => json-string)
```

## JSON Schema

Agent verification result structure:

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "passed": {"type": "boolean"},
    "dimensions": {
      "type": "object",
      "properties": {
        "scope": {"type": "boolean"},
        "hallucination": {"type": "boolean"},
        "style": {"type": "boolean"},
        "semantics": {"type": "boolean"},
        "complexity": {"type": "boolean"}
      },
      "required": ["scope", "hallucination", "style", "semantics", "complexity"]
    },
    "violations": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "dimension": {"type": "string", "enum": ["scope", "hallucination", "style", "semantics", "complexity"]},
          "severity": {"type": "string", "enum": ["error", "warning", "info"]},
          "message": {"type": "string"},
          "location": {
            "type": "object",
            "properties": {
              "file": {"type": "string"},
              "line": {"type": "integer", "minimum": 1}
            }
          }
        },
        "required": ["dimension", "severity", "message"]
      }
    },
    "duration_ms": {"type": "integer", "minimum": 0}
  },
  "required": ["passed", "dimensions", "violations"]
}
```

## References

- Implementation: `src/agent/core.lisp`, `src/agent/scope.lisp`, `src/agent/hallucination.lisp`, `src/agent/style.lisp`, `src/agent/semantics.lisp`, `src/agent/complexity.lisp`
- Package: `src/agent/package.lisp`
- Tests: `tests/agent-tests.lisp`
