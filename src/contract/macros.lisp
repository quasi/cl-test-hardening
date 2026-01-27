;;; ABOUTME: DSL macros for th.contract
;;; Provides define-contract, defschema, and interaction macros

(in-package #:th.contract)

;;;; Schema Definition Macro

(defmacro defschema (name &body fields)
  "Define a reusable schema.

   Example:
   (defschema order-response
     (:order-id (string-matching \"ord_[a-z0-9]+\"))
     (:status (one-of \"draft\" \"submitted\" \"shipped\"))
     (:created-at (type-of string)))"
  `(progn
     (register-schema
      (make-schema ',name ',(parse-schema-fields fields)))
     ',name))

;;;; Contract Definition Macro

(defmacro define-contract (name &body body)
  "Define a consumer-driven contract.

   Example:
   (define-contract checkout-orders
     :consumer :checkout-service
     :provider :order-service
     :version \"1.0.0\"

     (interaction create-order
       :description \"Create a new order\"
       :request (:method :post
                 :path \"/orders\"
                 :body (:customer-id (type-of string)))
       :response (:status 201
                  :body (:order-id (string-matching \"ord_.*\")))))"
  ;; Parse options (keyword-value pairs) from the front of body
  (let ((remaining body)
        (options nil))
    ;; Collect keyword-value pairs
    (loop while (and remaining (keywordp (first remaining)))
          do (push (first remaining) options)
             (push (second remaining) options)
             (setf remaining (cddr remaining)))
    (setf options (nreverse options))
    ;; Remaining items are interactions
    (let* ((interactions (remove-if-not (lambda (x)
                                          (and (listp x) (eq 'interaction (first x))))
                                        remaining))
           (consumer (getf options :consumer))
           (provider (getf options :provider))
           (version (or (getf options :version) "1.0.0")))
      `(progn
         (register-contract
          (make-contract :name ',name
                         :consumer ',consumer
                         :provider ',provider
                         :version ,version
                         :interactions (list ,@(mapcar #'expand-interaction interactions))))
         ',name))))

(defun expand-interaction (form)
  "Expand an interaction form into make-interaction call."
  (destructuring-bind (interaction-kw name &rest args) form
    (declare (ignore interaction-kw))
    `(make-interaction :name ',name
                       :description ,(getf args :description)
                       :given ,(getf args :given)
                       :request (parse-request-spec ',(getf args :request))
                       :response (parse-response-spec ',(getf args :response)))))

;;;; Interaction Macro (for use within define-contract)

(defmacro interaction (name &rest args)
  "Define an interaction within a contract.
   This is only valid inside define-contract."
  (declare (ignore name args))
  (error "INTERACTION must be used within DEFINE-CONTRACT"))

;;;; Shorthand Matchers
;;;
;;; Note: These are not macros because they would shadow CL symbols.
;;; Users write matcher expressions as quoted lists:
;;;   '(type-of string)
;;;   '(string-matching "pattern")
;;;   '(one-of "a" "b" "c")
;;;
;;; The DSL uses plain quoted lists which are more readable anyway.

;;;; Contract Testing DSL

(defmacro with-contract-test ((contract-name) &body body)
  "Set up a contract testing context.
   Within BODY, you can use mock-provider operations."
  (let ((contract-var (gensym "CONTRACT"))
        (mock-var (gensym "MOCK")))
    `(let* ((,contract-var (find-contract ',contract-name))
            (,mock-var (create-mock-provider ',contract-name)))
       (declare (ignorable ,contract-var ,mock-var))
       (macrolet ((request (method path &rest args)
                    `(mock-request ,',mock-var ,method ,path ,@args))
                  (verify-all-exercised ()
                    `(verify-mock-interactions ,',mock-var)))
         ,@body))))

;;;; Utility Macros

(defmacro define-contracts-from-spec (spec-form)
  "Define multiple contracts from a specification form.
   Used for loading contract definitions from external sources."
  `(progn
     ,@(mapcar (lambda (contract-spec)
                 `(define-contract ,@contract-spec))
               (eval spec-form))))

;;;; Debug Macros

(defmacro trace-contract-matching (&body body)
  "Execute BODY with contract matching tracing enabled."
  `(let ((*contract-trace* t))
     ,@body))

(defvar *contract-trace* nil
  "When T, print trace information during matching.")

;;;; Validation Macros

(defmacro check-contract-coverage (contract-name test-fn)
  "Check that TEST-FN exercises all interactions in CONTRACT-NAME.
   Signals an error if any interactions are not exercised."
  `(with-mock-provider (mock ,contract-name)
     (funcall ,test-fn mock)
     (let ((missing (verify-mock-interactions mock)))
       (when missing
         (error "Contract ~A has unexercised interactions: ~{~A~^, ~}"
                ',contract-name missing)))))
