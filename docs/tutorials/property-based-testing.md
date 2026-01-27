# Tutorial: Property-Based Testing

<!-- Generated from: canon/features/property/scenarios/*.md -->

Learn property-based testing by writing tests for a simple data structure.

## What You'll Learn

- Creating generators for custom data types
- Defining properties (invariants)
- Running property checks
- Understanding shrinking when properties fail

## Prerequisites

- Common Lisp REPL running
- cl-test-hardening installed (`(ql:quickload "cl-test-hardening/property")`)
- Basic understanding of testing

## Scenario: Testing a Stack Implementation

We'll test a simple stack to ensure it behaves correctly.

### Step 1: Load the Module

```lisp
(ql:quickload "cl-test-hardening/property")
(use-package :th.property)
```

### Step 2: Implement a Simple Stack

```lisp
(defclass stack ()
  ((items :initform nil :accessor stack-items)))

(defun make-stack ()
  (make-instance 'stack))

(defun push-item (stack item)
  (push item (stack-items stack))
  stack)

(defun pop-item (stack)
  (when (stack-items stack)
    (pop (stack-items stack))))

(defun stack-empty-p (stack)
  (null (stack-items stack)))
```

You should see:
```
STACK-EMPTY-P
```

### Step 3: Define Your First Property

**Property**: "Pushing then popping returns the original value"

```lisp
(defproperty push-pop-identity (item)
  (let ((s (make-stack)))
    (push-item s item)
    (equal item (pop-item s))))
```

This property says: "For any value, if I push it and immediately pop it, I get the same value back."

### Step 4: Create a Generator

We need to generate test data (items to push):

```lisp
(defparameter *item-gen*
  (gen-one-of
    (gen-integer -100 100)
    (gen-string 0 20)
    (gen-list (gen-integer 0 10))))
```

This generator produces:
- Integers between -100 and 100, OR
- Strings of length 0-20, OR
- Lists of integers 0-10

### Step 5: Run the Property Check

```lisp
(check-property 'push-pop-identity *item-gen* :num-tests 100)
```

You should see:
```
✓ Property holds for all 100 test cases
Duration: 45ms
```

Success! The property holds.

### Step 6: Test a More Complex Property

**Property**: "The size after N pushes is N"

```lisp
(defproperty push-n-times-size (items)
  (let ((s (make-stack)))
    (dolist (item items)
      (push-item s item))
    (= (length items)
       (length (stack-items s)))))
```

Create generator for lists of items:

```lisp
(defparameter *items-gen*
  (gen-list *item-gen* :min-length 0 :max-length 50))
```

Run the check:

```lisp
(check-property 'push-n-times-size *items-gen* :num-tests 200)
```

You should see:
```
✓ Property holds for all 200 test cases
Duration: 120ms
```

### Step 7: See Shrinking in Action

Let's intentionally break our stack to see shrinking work.

**Broken implementation**:
```lisp
(defun pop-item (stack)
  ;; BUG: Always returns first item, doesn't remove it
  (first (stack-items stack)))
```

Run the property check:

```lisp
(check-property 'push-pop-identity *item-gen*)
```

You should see:
```
✗ Property failed after 3 test cases
Failing input (after shrinking): 0
Expected: 0
Got: NIL

Original failing input was: -42
Shrunk to simpler case: 0
```

The shrinking process found that the simplest failing case is pushing `0`.

### Step 8: Fix and Re-verify

Fix the implementation:

```lisp
(defun pop-item (stack)
  (when (stack-items stack)
    (pop (stack-items stack))))
```

Run again:

```lisp
(check-property 'push-pop-identity *item-gen*)
```

You should see:
```
✓ Property holds for all 100 test cases
```

## Checkpoint

You've learned:
- ✓ How to define properties using `defproperty`
- ✓ How to create generators with `gen-*` functions
- ✓ How to run property checks with `check-property`
- ✓ How shrinking finds minimal failing cases

## Next Steps

- **Learn more generators**: See `reference/property-api.md` → Generators section
- **Learn composition**: Combine generators with `gen-tuple`, `gen-one-of`, `gen-bind`
- **Learn custom shrinking**: Define shrink strategies for custom types
- **See real examples**: Check `tests/property-tests.lisp` in the source

## Common Patterns

### Testing Inverse Operations

```lisp
(defproperty encode-decode-identity (data)
  (equal data (decode (encode data))))
```

### Testing Commutativity

```lisp
(defproperty addition-is-commutative (a b)
  (= (+ a b) (+ b a)))
```

### Testing Idempotency

```lisp
(defproperty sort-is-idempotent (list)
  (equal (sort (copy-seq list) #'<)
         (sort (sort (copy-seq list) #'<) #'<)))
```

### Testing Invariants

```lisp
(defproperty sorted-stays-sorted (list)
  (let ((sorted (sort (copy-seq list) #'<)))
    (or (null sorted)
        (every #'<= sorted (rest sorted)))))
```
