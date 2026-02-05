# Test Fixture API Reference

Complete API for `th.fixture` package - reusable test data with factory functions.

## Overview

Tests often need sample data:
```lisp
(test user-creation
  (let ((user (make-user :name "Alice" :age 30 :email "alice@example.com")))
    (is (valid-user-p user))))

(test user-authentication
  (let ((user (make-user :name "Alice" :age 30 :email "alice@example.com")))
    (is (authenticate user "password123"))))
```

**Problem:** Duplicated setup, maintenance burden when structure changes.

**th.fixture solves this.** Define fixtures once, reuse everywhere.

## Core Concept

A **fixture** is a factory function that creates test data. Register it once with a name, then use it in any test.

## API Reference

### `define-fixture`

**Syntax**: `(define-fixture name lambda-list &body body)`

**Type**: Macro

**Description**: Define a named fixture factory.

**Parameters**:
- `name` — Keyword naming the fixture
- `lambda-list` — Parameters the factory accepts (can be empty)
- `body` — Forms that create and return the fixture value

**Returns**: Fixture factory (registered in global registry)

**Example**:
```lisp
(define-fixture :user (&key (name "Alice") (age 30) (email "alice@example.com"))
  "Create a test user with customizable attributes"
  (make-user :name name :age age :email email))
```

---

### `build-fixture`

**Syntax**: `(build-fixture name &rest args)`

**Type**: Function

**Description**: Create an instance of a registered fixture.

**Parameters**:
- `name` — Keyword naming the fixture
- `args` — Arguments passed to the fixture factory

**Returns**: Whatever the factory returns

**Signals**: Error if fixture not registered

**Example**:
```lisp
(define-fixture :user (&key (name "Alice") (age 30))
  (make-user :name name :age age))

(build-fixture :user)                    ; Default values
(build-fixture :user :name "Bob")        ; Custom name
(build-fixture :user :name "Carol" :age 25)  ; Multiple overrides
```

---

### `with-fixture`

**Syntax**: `(with-fixture (var name &rest args) &body body)`

**Type**: Macro

**Description**: Create a fixture, bind it to a variable, and execute body.

**Parameters**:
- `var` — Variable name to bind the fixture to
- `name` — Keyword naming the fixture
- `args` — Arguments passed to the fixture factory
- `body` — Forms executed with fixture in scope

**Returns**: Value of last form in body

**Example**:
```lisp
(with-fixture (user :user :name "Bob")
  (is (string= "Bob" (user-name user)))
  (is (= 30 (user-age user))))
```

---

### `find-fixture`

**Syntax**: `(find-fixture name)`

**Description**: Look up a registered fixture factory by name.

**Returns**: Factory function or NIL

**Example**:
```lisp
(find-fixture :user)  ; => #<FUNCTION ...>
```

---

### `list-fixtures`

**Syntax**: `(list-fixtures)`

**Description**: List all registered fixture names.

**Returns**: List of keywords

**Example**:
```lisp
(list-fixtures)  ; => (:USER :POST :COMMENT :PRODUCT)
```

---

### `clear-fixtures`

**Syntax**: `(clear-fixtures)`

**Description**: Remove all registered fixtures from the registry.

---

## Complete Example

### Define Fixtures

```lisp
(ql:quickload "cl-test-hardening/fixture")
(use-package :th.fixture)

;; Simple fixture with defaults
(define-fixture :user (&key (name "Alice") (age 30) (email "alice@example.com"))
  "Create a test user"
  (make-user :name name :age age :email email))

;; Fixture that uses other fixtures
(define-fixture :admin-user ()
  "Create an admin user"
  (let ((user (build-fixture :user :name "Admin")))
    (setf (user-role user) :admin)
    user))

;; Fixture with complex setup
(define-fixture :blog-post (&key (title "Test Post") (author nil))
  "Create a blog post with an author"
  (let ((post-author (or author (build-fixture :user))))
    (make-post :title title
               :author post-author
               :created-at (get-universal-time))))

;; Fixture with cleanup
(define-fixture :database-connection (&key (db "test"))
  "Create a database connection (cleanup needed)"
  (let ((conn (connect-to-database db)))
    (values conn
            ;; Return cleanup function
            (lambda () (disconnect conn)))))
```

### Use in Tests

```lisp
;; Simple usage with defaults
(test user-validation
  (with-fixture (user :user)
    (is (valid-email-p (user-email user)))))

;; Custom parameters
(test underage-user
  (with-fixture (user :user :age 15)
    (is (not (can-vote-p user)))))

;; Multiple fixtures
(test blog-post-creation
  (with-fixture (author :user :name "Bob")
    (with-fixture (post :blog-post :author author)
      (is (string= "Bob" (user-name (post-author post)))))))

;; Nested fixtures
(test admin-privileges
  (with-fixture (admin :admin-user)
    (is (can-delete-users-p admin))
    (is (can-modify-settings-p admin))))
```

## Patterns and Best Practices

### 1. Sensible Defaults

Provide defaults for all parameters so fixtures work with no arguments:

```lisp
(define-fixture :user (&key (name "Alice") (age 30))
  (make-user :name name :age age))

;; Works with no args
(with-fixture (user :user)
  (is (valid-user-p user)))
```

### 2. Composable Fixtures

Build complex fixtures from simpler ones:

```lisp
(define-fixture :user (&key (name "Alice"))
  (make-user :name name))

(define-fixture :user-with-posts (&key (user nil) (num-posts 3))
  (let ((u (or user (build-fixture :user))))
    (dotimes (i num-posts)
      (add-post u (make-post :title (format nil "Post ~D" i))))
    u))
```

### 3. Fixture Families

Group related fixtures:

```lisp
;; User fixtures
(define-fixture :basic-user () ...)
(define-fixture :premium-user () ...)
(define-fixture :admin-user () ...)

;; Product fixtures
(define-fixture :digital-product () ...)
(define-fixture :physical-product () ...)
(define-fixture :subscription-product () ...)
```

### 4. Data Builders

Use fixtures as data builders:

```lisp
(define-fixture :order (&key
                        (customer nil)
                        (products nil)
                        (status :pending))
  "Build a test order"
  (let ((cust (or customer (build-fixture :user)))
        (prods (or products
                   (list (build-fixture :product)
                         (build-fixture :product)))))
    (make-order :customer cust
                :products prods
                :status status
                :total (calculate-total prods))))
```

### 5. Edge Case Fixtures

Create fixtures for edge cases:

```lisp
(define-fixture :empty-cart ()
  (make-cart :items nil))

(define-fixture :cart-at-limit ()
  (let ((cart (make-cart)))
    (dotimes (i 100)  ; Assume 100 is max
      (add-item cart (build-fixture :product)))
    cart))

(define-fixture :invalid-email-user ()
  (build-fixture :user :email "not-an-email"))
```

## Integration with FiveAM

Fixtures work seamlessly with FiveAM tests:

```lisp
(5am:def-suite user-tests)
(5am:in-suite user-tests)

(5am:test user-creation
  (with-fixture (user :user :name "Test User")
    (5am:is (string= "Test User" (user-name user)))))

(5am:test user-email-validation
  (with-fixture (user :user)
    (5am:is (valid-email-p (user-email user)))))
```

## Cleanup Pattern

For fixtures that need cleanup (files, connections, etc.):

```lisp
(define-fixture :temp-file (&key (content "test"))
  "Create a temporary file"
  (let ((path (make-temp-file)))
    (write-file path content)
    (values path
            ;; Return cleanup function
            (lambda () (delete-file path)))))

;; Use with manual cleanup
(multiple-value-bind (file cleanup) (build-fixture :temp-file)
  (unwind-protect
       (progn
         (is (file-exists-p file))
         ;; ... test code ...
         )
    (funcall cleanup)))
```

## Comparison: Fixture vs Factory Function

**Without th.fixture:**
```lisp
(defun make-test-user (&key (name "Alice") (age 30))
  (make-user :name name :age age))

;; Every test file needs to know about make-test-user
;; No registry, no discovery
```

**With th.fixture:**
```lisp
(define-fixture :user (&key (name "Alice") (age 30))
  (make-user :name name :age age))

;; Global registry - use anywhere
(list-fixtures)  ; => (:USER ...)
(with-fixture (user :user) ...)
```

**Benefits:**
- Global registry for discovery
- Consistent naming (keywords)
- Self-documenting via docstrings
- `with-fixture` macro for clean scoping

## See Also

- [Harness API](harness-api.md) — Test environment setup
- [API Overview](api-overview.md) — All modules
