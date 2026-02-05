---
type: contract
name: contract-definition
version: 1.0.0
feature: contract
---

# Contract Definition

**Status**: stable
**Package**: `th.contract`
**Source**: `src/contract/package.lisp:4-75`, `src/contract/core.lisp`

## Overview

Consumer-driven contract testing allows services to define expectations about their dependencies without requiring the full provider implementation. This contract defines how to specify interactions and schemas.

## Contract Definition

### define-contract

```lisp
(define-contract name &key consumer provider version interactions)
```

**Purpose**: Define a contract between consumer and provider.

**Parameters**:
- `name`: Symbol naming the contract
- `:consumer`: Consumer service name (string)
- `:provider`: Provider service name (string)
- `:version`: Contract version (string, optional)
- `:interactions`: List of interaction specifications

**Example**:
```lisp
(define-contract user-service-contract
  :consumer "web-app"
  :provider "user-service"
  :version "1.0"
  :interactions
  ((interaction "get user by ID"
     :given "user 123 exists"
     :request ((:method "GET")
               (:path "/users/123"))
     :response ((:status 200)
                (:body ((:id 123)
                       (:name "Alice")
                       (:email '(string-matching "@"))))))))
```

**Contract**:
- Contract is registered globally
- Can generate Pact JSON file
- Can be used to create mock provider
- Provider can verify against it

## Contract Structure

```lisp
(defstruct contract
  name
  consumer
  provider
  version
  interactions)
```

**Accessors**:
- `contract-name`
- `contract-consumer`
- `contract-provider`
- `contract-version`
- `contract-interactions`

## Interaction Definition

### interaction

```lisp
(interaction description &key given request response)
```

**Purpose**: Define a single interaction within a contract.

**Parameters**:
- `description`: Human-readable description
- `:given`: Provider state setup (optional)
- `:request`: Expected request specification (required)
- `:response`: Expected response specification (required)

### Interaction Structure

```lisp
(defstruct interaction-struct
  name
  description
  given
  request
  response)
```

**Accessors**:
- `interaction-name`
- `interaction-description`
- `interaction-given`: Provider state precondition
- `interaction-request`: Request specification
- `interaction-response`: Response specification

## Request Specification

Request is an alist with keys:

- `:method`: HTTP method (string, required)
- `:path`: URL path (string, required)
- `:query`: Query parameters (alist, optional)
- `:headers`: HTTP headers (alist, optional)
- `:body`: Request body (optional)

**Example**:
```lisp
(:request ((:method "POST")
           (:path "/users")
           (:headers (("Content-Type" . "application/json")))
           (:body ((:name "Bob")
                   (:email "bob@example.com")))))
```

## Response Specification

Response is an alist with keys:

- `:status`: HTTP status code (integer, required)
- `:headers`: HTTP headers (alist, optional)
- `:body`: Response body (optional)

**Example**:
```lisp
(:response ((:status 201)
            (:headers (("Content-Type" . "application/json")))
            (:body ((:id '(type-of integer))
                    (:name "Bob")
                    (:email "bob@example.com")))))
```

## Schema Definition

### defschema

```lisp
(defschema name &body field-specs)
```

**Purpose**: Define reusable schema for request/response bodies.

**Example**:
```lisp
(defschema user-schema
  (:id integer :required t)
  (:name string :required t)
  (:email (string-matching "@") :required t)
  (:age integer :required nil))
```

**Field Spec Format**:
```lisp
(field-name type-or-matcher &key required default)
```

### Schema Structure

```lisp
(defstruct schema
  name
  fields)
```

**Accessors**:
- `schema-name`
- `schema-fields`: Alist of `(field-name . field-spec)`

## Matchers

Matchers define flexible expectations for values that aren't exact.

### type-of

```lisp
'(type-of type-name)
```

**Purpose**: Match any value of given type.

**Examples**:
```lisp
'(type-of integer)
'(type-of string)
'(type-of list)
```

---

### string-matching

```lisp
'(string-matching pattern)
```

**Purpose**: Match strings containing pattern.

**Example**:
```lisp
'(string-matching "@")  ; Email validation
'(string-matching "^[0-9]+$")  ; Numeric string
```

---

### integer-in-range

```lisp
'(integer-in-range min max)
```

**Purpose**: Match integers in range [min, max].

---

### number-in-range

```lisp
'(number-in-range min max)
```

**Purpose**: Match numbers in range (floats or integers).

---

### object-with

```lisp
'(object-with (field matcher) ...)
```

**Purpose**: Match objects containing specified fields.

**Example**:
```lisp
'(object-with (:id (type-of integer))
              (:name (type-of string)))
```

---

### array-of

```lisp
'(array-of element-matcher)
```

**Purpose**: Match arrays where all elements match.

**Example**:
```lisp
'(array-of (type-of integer))
```

---

### one-of

```lisp
'(one-of value1 value2 ...)
```

**Purpose**: Match if value equals any option.

**Example**:
```lisp
'(one-of "active" "inactive" "pending")
```

---

### any-value

```lisp
'any-value
```

**Purpose**: Match any value (wildcard).

---

### optional-field

```lisp
'(optional-field matcher)
```

**Purpose**: Field may be absent or match matcher.

## Validation

### validate-against-schema

```lisp
(defun validate-against-schema (value schema-name) => (values boolean errors))
```

**Purpose**: Validate a value against a registered schema.

**Returns**:
- `boolean`: t if valid, nil if invalid
- `errors`: List of error messages (if invalid)

## Registry

### find-contract

```lisp
(defun find-contract (name) => contract-or-nil)
```

### list-contracts

```lisp
(defun list-contracts () => list-of-contract-names)
```

### clear-contracts

```lisp
(defun clear-contracts ())
```

### find-schema

```lisp
(defun find-schema (name) => schema-or-nil)
```

### list-schemas

```lisp
(defun list-schemas () => list-of-schema-names)
```

### clear-schemas

```lisp
(defun clear-schemas ())
```

## Contract Guarantees

- **Consumer-driven**: Consumer defines expectations
- **Flexible matching**: Matchers allow non-deterministic responses
- **Reusable schemas**: Define once, use many times
- **Validation**: Schemas validate request/response structures

## JSON Schema

Contract structure in JSON representation:

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "name": {"type": "string"},
    "consumer": {"type": "string"},
    "provider": {"type": "string"},
    "version": {"type": "string"},
    "interactions": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "description": {"type": "string"},
          "given": {"type": "string"},
          "request": {
            "type": "object",
            "properties": {
              "method": {"type": "string", "enum": ["GET", "POST", "PUT", "DELETE", "PATCH"]},
              "path": {"type": "string"},
              "query": {"type": "object"},
              "headers": {"type": "object"},
              "body": {}
            },
            "required": ["method", "path"]
          },
          "response": {
            "type": "object",
            "properties": {
              "status": {"type": "integer", "minimum": 100, "maximum": 599},
              "headers": {"type": "object"},
              "body": {}
            },
            "required": ["status"]
          }
        },
        "required": ["description", "request", "response"]
      }
    }
  },
  "required": ["name", "consumer", "provider", "interactions"]
}
```

## References

- Implementation: `src/contract/core.lisp`, `src/contract/schema.lisp`
- Matchers: `src/contract/matchers.lisp`
- Package: `src/contract/package.lisp`
- Tests: `tests/contract-tests.lisp`
