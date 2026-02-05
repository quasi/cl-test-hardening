# Tutorial: Contract Testing

Learn consumer-driven contract testing by building a mock API interaction.

## What You'll Learn

- Defining contracts between services
- Testing consumers with mock providers
- Generating Pact files
- Verifying providers meet contracts

## Prerequisites

- Common Lisp REPL running
- cl-test-hardening installed (`(ql:quickload "cl-test-hardening/contract")`)
- Basic HTTP client knowledge

## Scenario: Testing a User Service Client

We'll create a contract between a web app (consumer) and user service (provider).

### Step 1: Load the Module

```lisp
(ql:quickload "cl-test-hardening/contract")
(use-package :th.contract)
```

### Step 2: Define the Contract

```lisp
(define-contract user-service-contract
  :consumer "web-app"
  :provider "user-service"
  :interactions
  ((interaction "get user by ID"
     :request ((:method "GET")
               (:path "/users/123"))
     :response ((:status 200)
                (:body ((:id 123)
                       (:name "Alice")
                       (:email "alice@example.com")))))))
```

### Step 3: Test Consumer with Mock

```lisp
(with-mock-provider ('user-service-contract :port 8080)
  ;; Consumer code that calls the API
  (let ((user (my-client:fetch-user 123)))
    (assert (equal "Alice" (user-name user)))
    (assert (equal "alice@example.com" (user-email user)))))
```

### Step 4: Generate Pact File

```lisp
(generate-pact 'user-service-contract :output "pacts/")
;; Creates: pacts/web-app-user-service.json
```

### Step 5: Verify Provider

Provider team runs verification:

```lisp
(verify-provider
  :pact-file "pacts/web-app-user-service.json"
  :base-url "http://localhost:3000")
```

## Using Matchers for Flexibility

Avoid brittle exact matches:

```lisp
(define-contract flexible-user-contract
  :consumer "web-app"
  :provider "user-service"
  :interactions
  ((interaction "get user"
     :request ((:method "GET") (:path "/users/123"))
     :response ((:status 200)
                (:body ((:id '(type-of integer))
                       (:email '(string-matching "@"))
                       (:created-at '(string-matching "^[0-9]{4}-"))))))))
```

Now the provider can return any integer ID and any email address - tests won't break due to data changes.

## Next Steps

- [Contract Testing API Reference](../reference/contract-api.md)
- [API Overview](../reference/api-overview.md)
