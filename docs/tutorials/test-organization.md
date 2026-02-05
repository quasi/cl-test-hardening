# Tutorial: Test Organization with Harness and Fixtures

Learn how to organize tests efficiently using declarative setup and reusable data.

## The Problem

Test files often start with repetitive boilerplate:

```lisp
;; test-user.lisp
(ql:quickload :fiveam)
(ql:quickload :alexandria)
(ql:quickload :my-system)
(load "src/utils.lisp")
(in-package :my-system.tests)
(5am:def-suite :user-tests)
(5am:in-suite :user-tests)

(5am:test user-creation
  (let ((user (make-user :name "Alice" :age 30 :email "alice@example.com")))
    (5am:is (valid-user-p user))))

;; test-auth.lisp
(ql:quickload :fiveam)
(ql:quickload :alexandria)
(ql:quickload :my-system)
(load "src/utils.lisp")
(in-package :my-system.tests)
(5am:def-suite :auth-tests)
(5am:in-suite :auth-tests)

(5am:test authentication
  (let ((user (make-user :name "Alice" :age 30 :email "alice@example.com")))
    (5am:is (authenticate user "password"))))
```

**Problems:**
- Duplicated setup in every file
- Duplicated test data creation
- Hard to maintain when structure changes
- Easy to forget dependencies

## The Solution

Use **test harness** for environment setup and **fixtures** for test data.

## Step 1: Define Test Harness

Create `tests/test-config.lisp`:

```lisp
(ql:quickload "cl-test-hardening/harness")
(ql:quickload "cl-test-hardening/fixture")

(use-package :th.harness)
(use-package :th.fixture)

;; Define harness: systems to load, files to load, target package
(define-harness :my-system
  :systems (:fiveam :alexandria :my-system)
  :load ("src/utils.lisp")
  :package :my-system.tests)
```

## Step 2: Define Fixtures

Add to `tests/test-config.lisp`:

```lisp
;; User fixture with sensible defaults
(define-fixture :user (&key (name "Alice") (age 30) (email "alice@example.com"))
  "Create a test user"
  (make-user :name name :age age :email email))

;; Admin user fixture
(define-fixture :admin ()
  "Create an admin user"
  (let ((user (build-fixture :user :name "Admin")))
    (setf (user-role user) :admin)
    user))

;; User with posts fixture
(define-fixture :user-with-posts (&key (user nil) (num-posts 3))
  "Create a user with blog posts"
  (let ((u (or user (build-fixture :user))))
    (dotimes (i num-posts)
      (add-post u (make-post :title (format nil "Post ~D" i))))
    u))
```

## Step 3: Use in Test Files

Now test files are clean and focused:

### tests/test-user.lisp

```lisp
(load "tests/test-config.lisp")

(setup :my-system :suite-name :user-tests)
;; That's it! Environment ready, package set, suite created

(5am:test user-creation
  (with-fixture (user :user)
    (5am:is (valid-user-p user))))

(5am:test user-with-custom-age
  (with-fixture (user :user :age 15)
    (5am:is (not (can-vote-p user)))))

(5am:test admin-privileges
  (with-fixture (admin :admin)
    (5am:is (can-delete-users-p admin))))
```

### tests/test-auth.lisp

```lisp
(load "tests/test-config.lisp")

(setup :my-system :suite-name :auth-tests)

(5am:test authentication-success
  (with-fixture (user :user)
    (5am:is (authenticate user "password"))))

(5am:test authentication-failure
  (with-fixture (user :user)
    (5am:is (not (authenticate user "wrong-password")))))
```

### tests/test-blog.lisp

```lisp
(load "tests/test-config.lisp")

(setup :my-system :suite-name :blog-tests)

(5am:test user-can-create-posts
  (with-fixture (user :user)
    (add-post user (make-post :title "My First Post"))
    (5am:is (= 1 (length (user-posts user))))))

(5am:test user-with-multiple-posts
  (with-fixture (user :user-with-posts :num-posts 5)
    (5am:is (= 5 (length (user-posts user))))))
```

## Benefits

### Before

- 20 lines of boilerplate per file
- Duplicated user creation in every test
- Hard to change user structure
- 10 test files = 200 lines of boilerplate

### After

- 1 line setup per file (`setup :my-system ...`)
- User creation defined once
- Change fixture definition in one place
- 10 test files = 10 lines of setup

**Result:** More time writing tests, less time maintaining boilerplate.

## Advanced Patterns

### Multiple Environments

```lisp
;; Unit tests: minimal dependencies
(define-harness :unit
  :systems (:fiveam)
  :load ("src/core.lisp")
  :package :my-system.tests)

;; Integration tests: full stack
(define-harness :integration
  :systems (:fiveam :dexador :postmodern)
  :load ("src/core.lisp" "src/db.lisp" "src/api.lisp")
  :package :my-system.tests)

;; Use appropriate harness
(setup :unit :suite-name :unit-tests)
(setup :integration :suite-name :integration-tests)
```

### Composable Fixtures

```lisp
(define-fixture :basic-user ()
  (make-user :name "User"))

(define-fixture :user-with-profile ()
  (let ((user (build-fixture :basic-user)))
    (setf (user-profile user) (make-profile))
    user))

(define-fixture :user-with-everything ()
  (let ((user (build-fixture :user-with-profile)))
    (setf (user-settings user) (make-settings))
    (setf (user-preferences user) (make-preferences))
    user))
```

### Edge Case Fixtures

```lisp
(define-fixture :empty-cart ()
  (make-cart :items nil))

(define-fixture :cart-at-limit ()
  (let ((cart (make-cart)))
    (dotimes (i 100)
      (add-item cart (build-fixture :product)))
    cart))

(define-fixture :expired-user ()
  (build-fixture :user :account-expiry (- (get-universal-time) 86400)))
```

## Running Tests

```lisp
;; Run all tests
(ql:quickload :my-system/tests)
(5am:run! :user-tests)
(5am:run! :auth-tests)
(5am:run! :blog-tests)

;; Or run everything
(5am:run! :my-system-tests)
```

## Summary

**Test Harness** eliminates environment setup boilerplate.  
**Test Fixtures** eliminate test data duplication.

Together, they make tests:
- Easier to write
- Easier to maintain
- More focused on behavior
- Less coupled to structure

## Next Steps

- [Harness API Reference](../reference/harness-api.md)
- [Fixture API Reference](../reference/fixture-api.md)
- [API Overview](../reference/api-overview.md)
