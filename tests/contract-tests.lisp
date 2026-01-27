;;; ABOUTME: Tests for th.contract
;;; Verifies matchers, schemas, contracts, and pact generation

(defpackage #:th.contract-tests
  (:use #:cl #:fiveam #:th.contract))

(in-package #:th.contract-tests)

(def-suite th.contract-tests
  :description "Tests for th.contract"
  :in :th.tests)

(in-suite th.contract-tests)

;;;; Matcher Tests

(test type-matcher-string
  (let ((result (th.contract::matches-p '(type-of string) "hello" nil)))
    (is (th.contract::match-result-matched result))))

(test type-matcher-string-fail
  (let ((result (th.contract::matches-p '(type-of string) 123 nil)))
    (is (not (th.contract::match-result-matched result)))))

(test type-matcher-integer
  (let ((result (th.contract::matches-p '(type-of integer) 42 nil)))
    (is (th.contract::match-result-matched result))))

(test string-matching-success
  (let ((result (th.contract::matches-p '(string-matching "^ord_[a-z0-9]+$")
                                         "ord_abc123" nil)))
    (is (th.contract::match-result-matched result))))

(test string-matching-fail
  (let ((result (th.contract::matches-p '(string-matching "^ord_[a-z0-9]+$")
                                         "order123" nil)))
    (is (not (th.contract::match-result-matched result)))))

(test integer-in-range-success
  (let ((result (th.contract::matches-p '(integer-in-range 1 100) 50 nil)))
    (is (th.contract::match-result-matched result))))

(test integer-in-range-fail
  (let ((result (th.contract::matches-p '(integer-in-range 1 100) 150 nil)))
    (is (not (th.contract::match-result-matched result)))))

(test one-of-success
  (let ((result (th.contract::matches-p '(one-of "draft" "submitted" "shipped")
                                         "draft" nil)))
    (is (th.contract::match-result-matched result))))

(test one-of-fail
  (let ((result (th.contract::matches-p '(one-of "draft" "submitted" "shipped")
                                         "cancelled" nil)))
    (is (not (th.contract::match-result-matched result)))))

(test array-of-success
  (let ((result (th.contract::matches-p '(array-of (type-of integer))
                                         '(1 2 3) nil)))
    (is (th.contract::match-result-matched result))))

(test array-of-fail
  (let ((result (th.contract::matches-p '(array-of (type-of integer))
                                         '(1 "two" 3) nil)))
    (is (not (th.contract::match-result-matched result)))))

(test array-of-min-constraint
  (let ((result (th.contract::matches-p '(array-of (type-of integer) :min 2)
                                         '(1) nil)))
    (is (not (th.contract::match-result-matched result)))))

(test object-with-success
  (let* ((obj (make-hash-table :test 'equal)))
    (setf (gethash "name" obj) "John")
    (setf (gethash "age" obj) 30)
    (let ((result (th.contract::matches-p '(object-with :name (type-of string)
                                                         :age (type-of integer))
                                           obj nil)))
      (is (th.contract::match-result-matched result)))))

;;;; Schema Tests

(test schema-definition
  (defschema test-order
    (:order-id (string-matching "ord_.*"))
    (:amount (type-of integer)))
  (is (find-schema 'test-order))
  (is (= 2 (length (th.contract::schema-fields (find-schema 'test-order))))))

(test schema-validation
  (defschema test-item
    (:name (type-of string))
    (:price (type-of number)))
  (let* ((valid (make-hash-table :test 'equal)))
    (setf (gethash "name" valid) "Widget")
    (setf (gethash "price" valid) 9.99)
    (let ((result (validate-against-schema 'test-item valid)))
      (is (th.contract::match-result-matched result)))))

;;;; Contract Tests

(test contract-definition
  (define-contract test-checkout-orders
    :consumer :checkout
    :provider :orders

    (interaction create-order
      :description "Create a new order"
      :request (:method :post
                :path "/orders"
                :body (:customer-id (type-of string)))
      :response (:status 201
                 :body (:order-id (string-matching "ord_.*")))))

  (is (find-contract 'test-checkout-orders))
  (let ((contract (find-contract 'test-checkout-orders)))
    (is (eq :checkout (contract-consumer contract)))
    (is (eq :orders (contract-provider contract)))
    (is (= 1 (length (contract-interactions contract))))))

(test contract-interaction-details
  (define-contract test-user-api
    :consumer :frontend
    :provider :user-service
    :version "2.0.0"

    (interaction get-user
      :description "Get user by ID"
      :given "User 123 exists"
      :request (:method :get
                :path "/users/123")
      :response (:status 200
                 :body (:id (type-of integer)
                        :name (type-of string)))))

  (let* ((contract (find-contract 'test-user-api))
         (interaction (first (contract-interactions contract))))
    (is (string= "2.0.0" (contract-version contract)))
    (is (eq 'get-user (th.contract::interaction-struct-name interaction)))
    (is (string= "User 123 exists" (th.contract::interaction-struct-given interaction)))))

;;;; Pact Generation Tests

(test pact-generation
  (define-contract pact-test-contract
    :consumer :consumer-app
    :provider :provider-api

    (interaction fetch-data
      :description "Fetch data from provider"
      :request (:method :get
                :path "/data")
      :response (:status 200
                 :body (:items (array-of (type-of string))))))

  (let ((pact (generate-pact 'pact-test-contract)))
    (is (hash-table-p pact))
    (is (string= "consumer-app"
                 (gethash "name" (gethash "consumer" pact))))
    (is (string= "provider-api"
                 (gethash "name" (gethash "provider" pact))))
    (is (= 1 (length (gethash "interactions" pact))))))

(test pact-interaction-structure
  (define-contract pact-interaction-test
    :consumer :test-consumer
    :provider :test-provider

    (interaction test-create
      :description "Test create endpoint"
      :request (:method :post
                :path "/items"
                :body (:name (type-of string)))
      :response (:status 201)))

  (let* ((pact (generate-pact 'pact-interaction-test))
         (interaction (first (gethash "interactions" pact)))
         (request (gethash "request" interaction))
         (response (gethash "response" interaction)))
    (is (string= "POST" (gethash "method" request)))
    (is (string= "/items" (gethash "path" request)))
    (is (= 201 (gethash "status" response)))))

;;;; Mock Provider Tests

(test mock-provider-creation
  (define-contract mock-test-contract
    :consumer :test
    :provider :mock

    (interaction get-resource
      :request (:method :get :path "/resource")
      :response (:status 200)))

  (let ((mock (create-mock-provider 'mock-test-contract)))
    (is (not (null mock)))
    (is (equal '(get-resource)
               (th.contract::mock-provider-expected-interactions mock)))))

(test mock-provider-request
  (define-contract mock-request-test
    :consumer :test
    :provider :api

    (interaction list-items
      :request (:method :get :path "/items")
      :response (:status 200
                 :body (:items (array-of (type-of string))))))

  (let ((mock (create-mock-provider 'mock-request-test)))
    (multiple-value-bind (response interaction)
        (mock-request mock :get "/items")
      (is (not (null interaction)))
      (is (= 200 (th.contract::http-response-status response))))))

;;;; Response Matching Tests

(test response-body-matching
  (let ((expected (th.contract::make-http-response
                   :status 200
                   :body '(:id (type-of integer) :name (type-of string))))
        (actual-good (th.contract::make-http-response
                      :status 200
                      :body (let ((h (make-hash-table :test 'equal)))
                              (setf (gethash "id" h) 123)
                              (setf (gethash "name" h) "Test")
                              h))))
    (let ((mismatches (th.contract::compare-responses expected actual-good)))
      (is (null mismatches)))))

(test response-status-mismatch
  (let ((expected (th.contract::make-http-response :status 200))
        (actual (th.contract::make-http-response :status 404)))
    (let ((mismatches (th.contract::compare-responses expected actual)))
      (is (not (null mismatches)))
      (is (search "Status" (first mismatches))))))

;;;; Integration Test

(test full-contract-workflow
  ;; Define schema
  (defschema workflow-order
    (:id (string-matching "ord_.*"))
    (:status (one-of "pending" "complete")))

  ;; Define contract
  (define-contract workflow-test
    :consumer :checkout
    :provider :orders
    :version "1.0.0"

    (interaction create-order
      :description "Create order"
      :request (:method :post
                :path "/orders"
                :body (:customer-id (type-of string)))
      :response (:status 201
                 :body workflow-order)))

  ;; Generate pact
  (let ((pact (generate-pact 'workflow-test)))
    (is (hash-table-p pact))

    ;; Verify structure
    (let* ((interactions (gethash "interactions" pact))
           (interaction (first interactions))
           (response (gethash "response" interaction))
           (body (gethash "body" response)))
      (is (= 201 (gethash "status" response)))
      (is (hash-table-p body))
      ;; Body should have example values from schema
      (is (stringp (gethash "id" body)))
      (is (stringp (gethash "status" body))))))
