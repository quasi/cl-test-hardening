# Property Testing API Reference

Complete API for `th.property` and `th.gen` packages.

## Package: `th.property`

Property-based testing functions.

### `defproperty`

**Syntax**: `(defproperty name (parameters...) &body body)`

**Type**: Macro

**Description**: Define a property (invariant) that should hold for all inputs.

**Parameters**:
- `name` — Symbol naming the property
- `parameters` — Lambda list of property arguments
- `body` — Forms that return truthy value if property holds

**Returns**: Property definition (registered globally)

**Example**:
```lisp
(defproperty list-length-after-cons (item list)
  (= (length (cons item list))
     (1+ (length list))))
```

---

### `check-property`

**Syntax**: `(check-property name &key generator num-tests seed timeout)`

**Type**: Function

**Description**: Run property with generated test cases.

**Parameters**:
- `name` — Property name (symbol)
- `:generator` — Generator for input values
- `:num-tests` — Number of test cases (default: 100)
- `:seed` — Random seed for reproducibility (optional)
- `:timeout` — Timeout per test in seconds (default: 5)

**Returns**: `verification-result` object

**Example**:
```lisp
(check-property 'my-property
                :generator (gen:tuple (gen:integers) (gen:strings))
                :num-tests 200
                :seed 42)
```

---

### `find-property`

**Syntax**: `(find-property name)`

**Description**: Look up registered property by name.

**Returns**: Property object or NIL

---

### `list-properties`

**Syntax**: `(list-properties)`

**Description**: List all registered property names.

**Returns**: List of symbols

---

## Package: `th.gen`

Test data generators with shrinking support.

### Primitive Generators

#### `integers`

**Syntax**: `(integers &key min max)`

**Description**: Generate random integers.

**Parameters**:
- `:min` — Minimum value (default: -1000)
- `:max` — Maximum value (default: 1000)

**Example**:
```lisp
(gen:integers :min 0 :max 100)
```

---

#### `strings`

**Syntax**: `(strings &key min-length max-length alphabet)`

**Description**: Generate random strings.

**Parameters**:
- `:min-length` — Minimum string length (default: 0)
- `:max-length` — Maximum string length (default: 20)
- `:alphabet` — Character set (default: alphanumeric)

**Example**:
```lisp
(gen:strings :min-length 5 :max-length 10 :alphabet "abc123")
```

---

#### `booleans`

**Syntax**: `(booleans)`

**Description**: Generate T or NIL with equal probability.

**Example**:
```lisp
(gen:booleans)
```

---

#### `characters`

**Syntax**: `(characters &key alphabet)`

**Description**: Generate random characters.

**Parameters**:
- `:alphabet` — String of allowed characters

**Example**:
```lisp
(gen:characters :alphabet "ABCDEF0123456789")  ; Hex digits
```

---

### Collection Generators

#### `lists`

**Syntax**: `(lists element-generator &key min-length max-length)`

**Description**: Generate lists of elements.

**Parameters**:
- `element-generator` — Generator for list elements
- `:min-length` — Minimum list length (default: 0)
- `:max-length` — Maximum list length (default: 20)

**Example**:
```lisp
(gen:lists (gen:integers) :min-length 1 :max-length 10)
```

---

#### `vectors`

**Syntax**: `(vectors element-generator &key min-length max-length)`

**Description**: Generate vectors.

**Example**:
```lisp
(gen:vectors (gen:booleans) :max-length 5)
```

---

### Combinator Generators

#### `tuple`

**Syntax**: `(tuple &rest generators)`

**Description**: Generate tuples by combining generators.

**Example**:
```lisp
(gen:tuple (gen:integers) (gen:strings) (gen:booleans))
;; => (42 "hello" T)
```

---

#### `one-of`

**Syntax**: `(one-of &rest generators)`

**Description**: Choose randomly from multiple generators.

**Example**:
```lisp
(gen:one-of (gen:integers :min 0 :max 10)
            (gen:strings :max-length 5))
```

---

#### `frequency`

**Syntax**: `(frequency &rest (weight generator) pairs)`

**Description**: Weighted choice between generators.

**Example**:
```lisp
(gen:frequency (70 (gen:integers))     ; 70% integers
               (30 (gen:strings)))      ; 30% strings
```

---

#### `such-that`

**Syntax**: `(such-that predicate generator &key max-tries)`

**Description**: Filter generated values by predicate.

**Parameters**:
- `predicate` — Function returning truthy for valid values
- `generator` — Base generator
- `:max-tries` — Maximum generation attempts (default: 100)

**Example**:
```lisp
(gen:such-that #'evenp (gen:integers))  ; Even integers only
```

**Note**: Throws error if max-tries exceeded without valid value.

---

#### `fmap`

**Syntax**: `(fmap function generator)`

**Description**: Transform generated values.

**Example**:
```lisp
(gen:fmap #'abs (gen:integers))  ; Absolute values
(gen:fmap #'reverse (gen:lists (gen:integers)))  ; Reversed lists
```

---

### Custom Generators

#### `sized`

**Syntax**: `(sized function)`

**Description**: Create generator that depends on size parameter.

**Parameters**:
- `function` — `(lambda (size) generator)` receiving size (0-100)

**Example**:
```lisp
(gen:sized
  (lambda (size)
    (gen:lists (gen:integers) :max-length size)))
```

---

## Shrinking

When a property fails, shrinking finds the minimal counterexample.

### How Shrinking Works

1. Property fails on input `X`
2. Generator produces simpler variants of `X`
3. Test each variant - if still fails, use that
4. Repeat until no simpler failing input found

### Example

```lisp
;; Property fails on input: (42 17 -5 0 8 3)
;; Shrinking finds minimal: (0)
```

### Controlling Shrinking

All built-in generators support automatic shrinking:
- Integers shrink toward 0
- Strings shrink toward ""
- Lists shrink toward []
- Tuples shrink element-wise

No configuration needed - shrinking is automatic.

---

## Full Example

```lisp
(use-package :th.property)
(use-package :th.gen)

;; Property: Reversing twice returns original
(defproperty reverse-twice-identity (list)
  (equal list (reverse (reverse list))))

;; Check with random lists of integers
(check-property 'reverse-twice-identity
                :generator (gen:lists (gen:integers))
                :num-tests 500)
;; => ✓ reverse-twice-identity passed 500 tests

;; Property: Sorting is idempotent
(defproperty sort-idempotent (list)
  (equal (sort (copy-seq list) #'<)
         (sort (sort (copy-seq list) #'<) #'<)))

(check-property 'sort-idempotent
                :generator (gen:lists (gen:integers))
                :num-tests 200)
;; => ✓ sort-idempotent passed 200 tests
```
