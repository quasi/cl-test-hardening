# API Reference Overview

<!-- Generated from: canon/features/*/contracts/*.md -->

Complete API reference for all cl-test-hardening modules.

## Module Structure

cl-test-hardening is organized into independent modules, each loadable separately:

```lisp
;; Load individual modules
(ql:quickload "cl-test-hardening/property")   ; Property-based testing
(ql:quickload "cl-test-hardening/mutation")   ; Mutation testing
(ql:quickload "cl-test-hardening/contract")   ; Contract testing
(ql:quickload "cl-test-hardening/agent")      ; Agent verification

;; Load everything
(ql:quickload "cl-test-hardening/all")
```

## Core Module (`th.core`)

All modules return standardized verification results.

### `verification-result`

**Type**: Class

**Slots**:
- `passed-p` — Boolean indicating success
- `timestamp` — Universal time when verification occurred
- `duration-ms` — Duration in milliseconds
- `summary` — Human-readable summary string
- `details` — Property list with module-specific data

**Usage**:
```lisp
(defparameter *result* (make-verification-result ...))
(verification-result-passed-p *result*)  ; => T or NIL
```

### `format-result`

**Signature**: `(format-result result &optional stream)`

**Description**: Formats verification result for display.

**Returns**: String (if stream is NIL) or NIL (if stream provided)

---

## Property Module (`th.property`)

Property-based testing with generators and shrinking.

### Core Functions

#### `defproperty`

**Macro**: `(defproperty name (args...) &body body)`

**Description**: Define a property (invariant) to verify.

**Example**:
```lisp
(defproperty reverse-twice-identity (list)
  (equal list (reverse (reverse list))))
```

#### `check-property`

**Function**: `(check-property property generator &key num-tests)`

**Parameters**:
- `property` — Property name (symbol)
- `generator` — Generator for test inputs
- `num-tests` — Number of test cases (default: 100)

**Returns**: `verification-result`

**Example**:
```lisp
(check-property 'my-property (gen-integer 0 100) :num-tests 200)
```

### Generators

#### `gen-integer`

**Function**: `(gen-integer min max)`

**Description**: Generate random integers in range [min, max].

**Example**:
```lisp
(gen-integer -100 100)
```

#### `gen-string`

**Function**: `(gen-string min-length max-length)`

**Description**: Generate random strings.

**Example**:
```lisp
(gen-string 0 50)  ; Strings of length 0-50
```

#### `gen-list`

**Function**: `(gen-list element-gen &key min-length max-length)`

**Description**: Generate lists of elements.

**Example**:
```lisp
(gen-list (gen-integer 0 10) :min-length 0 :max-length 20)
```

#### `gen-one-of`

**Function**: `(gen-one-of &rest generators)`

**Description**: Choose randomly from multiple generators.

**Example**:
```lisp
(gen-one-of (gen-integer 0 10)
            (gen-string 0 5))
```

#### `gen-tuple`

**Function**: `(gen-tuple &rest generators)`

**Description**: Generate tuples by combining generators.

**Example**:
```lisp
(gen-tuple (gen-integer 0 100)
           (gen-string 0 20))
;; => (42 "hello")
```

For complete generator API, see standalone `reference/property-api.md`.

---

## Mutation Module (`th.mutation`)

Mutation testing for test suite quality assessment.

### Core Functions

#### `mutate-and-test`

**Function**: `(mutate-and-test &key target-function test-suite operators max-mutants)`

**Parameters**:
- `target-function` — Function symbol to mutate
- `test-suite` — FiveAM test suite symbol
- `operators` — List of mutation operator keywords
- `max-mutants` — Maximum mutants to generate (optional)

**Returns**: `verification-result` with mutation score

**Example**:
```lisp
(mutate-and-test
  :target-function 'calculate-total
  :test-suite 'calculator-tests
  :operators '(:flip-arithmetic :change-comparison))
```

### Mutation Operators

Available operators (keyword symbols):

- `:flip-arithmetic` — Change `+` ↔ `-`, `*` ↔ `/`
- `:change-comparison` — Modify `<`, `>`, `<=`, `>=`, `=`, `/=`
- `:flip-boolean` — Change `t` ↔ `nil`
- `:remove-call` — Remove function calls
- `:change-constant` — Modify numeric constants

For complete mutation API, see standalone `reference/mutation-api.md`.

---

## Contract Module (`th.contract`)

Consumer-driven contract testing (Pact-style).

### Core Functions

#### `define-interaction`

**Macro**: `(define-interaction name &key request response)`

**Description**: Define an expected interaction between consumer and provider.

**Example**:
```lisp
(define-interaction "get-user"
  :request (make-request :method :get :path "/users/123")
  :response (make-response :status 200 :body '(("id" . 123))))
```

#### `with-mock-provider`

**Macro**: `(with-mock-provider (var) &body body)`

**Description**: Create mock provider for consumer testing.

**Example**:
```lisp
(with-mock-provider (mock)
  (add-interaction mock "get-user")
  ;; Test consumer code here
  (my-client:fetch-user 123))
```

#### `write-pact`

**Function**: `(write-pact filepath &key consumer provider interactions)`

**Description**: Write Pact file with recorded interactions.

**Example**:
```lisp
(write-pact "consumer-provider.json"
  :consumer "frontend"
  :provider "api")
```

#### `verify-provider`

**Function**: `(verify-provider pact-file &key base-url)`

**Description**: Verify provider meets contract expectations.

**Example**:
```lisp
(verify-provider "consumer-provider.json"
  :base-url "http://localhost:8080")
```

For complete contract API, see standalone `reference/contract-api.md`.

---

## Agent Module (`th.agent`)

Multi-dimensional verification of AI-generated code.

### Core Functions

#### `verify-code`

**Function**: `(verify-code code &key allowed-symbols style-rules max-complexity)`

**Parameters**:
- `code` — Lisp form or code string to verify
- `allowed-symbols` — List of permitted symbols (scope checking)
- `style-rules` — Style conventions to enforce
- `max-complexity` — Maximum nesting depth

**Returns**: `verification-result` with violations by dimension

**Example**:
```lisp
(verify-code '(defun foo (x) (+ x 1))
  :allowed-symbols '(cl:defun cl:+ cl:1)
  :max-complexity 3)
```

### Verification Dimensions

Each dimension can be checked independently:

- `check-scope` — Verify allowed symbols
- `check-hallucination` — Detect non-existent functions
- `check-style` — Enforce naming/formatting
- `check-semantics` — Validate logic
- `check-complexity` — Measure nesting/complexity

For complete agent API, see standalone `reference/agent-api.md`.

---

## Result Protocol

All modules implement the result protocol:

```lisp
;; Create result
(make-verification-result
  :passed-p t
  :timestamp (get-universal-time)
  :duration-ms 100
  :summary "Verification passed"
  :details '(:detail-key value))

;; Access fields
(verification-result-passed-p result)
(verification-result-summary result)
(verification-result-details result)

;; Format for display
(format-result result)
(format-summary result)
(format-failure result)
```

## For Agent Consumers

Agent consumers can read formal API specifications directly:

- Public contracts: `canon/features/*/contracts/`
- Vocabulary: `canon/core/foundation/vocabulary.md`
- Properties: `canon/features/*/properties/`

These formal specifications include schemas, behavior rules, and verification criteria.
