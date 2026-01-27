# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Test hardening library for Common Lisp. Hardens tests so they don't lie to you. Four independent modules: property-based testing, mutation testing, contract testing (Pact-style), and agent-generated code verification.

## Running Tests

```lisp
;; Load and run all tests
(ql:quickload "cl-test-hardening/tests")
(5am:run! :th.tests)

;; Run specific module test suites
(5am:run! :th.property-tests)
(5am:run! :th.mutation-tests)
(5am:run! :th.contract-tests)
(5am:run! :th.agent-tests)
```

## Loading the System

```lisp
;; Load just one module
(ql:quickload "cl-test-hardening/property")
(ql:quickload "cl-test-hardening/mutation")
(ql:quickload "cl-test-hardening/contract")
(ql:quickload "cl-test-hardening/agent")

;; Load everything
(ql:quickload "cl-test-hardening/all")
```

## Architecture

**Modular design**: Each module (`property`, `mutation`, `contract`, `agent`) is independently loadable with its own ASDF system. All depend on `th.core` which provides shared result protocols and reporting.

**Module structure**:
- Core package in `src/<module>/package.lisp` defines exports
- Each module has its own namespace (e.g., `th.property`, `th.mutation`)
- Generators and operators are separate loadable sub-systems
- Canon adapter in `src/canon/` integrates with Canon specification system

**Result protocol**: All modules return `verification-result` objects with standard slots (`passed-p`, `timestamp`, `duration-ms`, `summary`, `details`). Generic functions `format-result`, `format-summary`, `format-failure` handle reporting.

**Test organization**: Test suites use FiveAM with hierarchical structure - `:th.tests` is root, each module has parent suite (`:th.property-tests`, etc.), individual test files define child suites.

## Key Conventions

- Package nicknames: `th.property`, `th.mutation`, `th.contract`, `th.agent`, `th.core`
- All test files use `:in-suite` directive to register with parent suite
- Use `:serial t` in ASDF components for load order dependencies
- The `/all` system loads every module for convenience
