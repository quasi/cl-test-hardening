---
type: scenario
name: verification-workflow
version: 1.0.0
feature: canon
covers:
  - verification-protocol
tags:
  - canon
  - workflow
---

# Canon Verification Workflow

**Status**: stable
**Source**: Integration tests

## Scenario: Complete Verification Workflow

**Given**: Verification request JSON file
```json
{
  "system": "cl-test-hardening/tests",
  "test_suite": "th.tests"
}
```

**When**: `run-verification` is called

**Then**:
- System loaded via ASDF
- FiveAM test suite executed
- Results written to JSON
- Result includes timestamp, duration, test counts

**Rationale**: Standard verification workflow for Canon integration.

---

## Scenario: Property Testing Verification

**Given**: Request specifies properties
```json
{
  "properties": ["reverse-involution"],
  "options": {"property_iterations": 50}
}
```

**When**: Verification runs

**Then**:
- Property tests execute with 50 iterations
- Results include checked/passed/failed counts
- Failures include counterexamples and seeds

---

## Scenario: Missing Request File

**Given**: No verification-request.json file

**When**: `run-verification` is called

**Then**:
- Error thrown
- No result file written

**Rationale**: Fail fast on missing input.

---

## Verification

Integration with Canon specification system.
