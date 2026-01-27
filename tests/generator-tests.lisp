;;; ABOUTME: Tests for generators
;;; Verifies generators produce valid values and respect constraints

(defpackage #:th.property-tests/generators
  (:use #:cl #:fiveam #:th.property)
  (:local-nicknames (#:gen #:th.gen)))

(in-package #:th.property-tests/generators)

(def-suite generator-tests
  :description "Tests for th.gen generators"
  :in :th.property-tests)

(in-suite generator-tests)

;;;; Integer Generator Tests

(test integer-generator-produces-integers
  (with-property-context (:seed 12345)
    (let ((gen (gen:integers)))
      (dotimes (i 100)
        (is (integerp (generate gen *test-random-state* 30)))))))

(test integer-generator-respects-bounds
  (with-property-context (:seed 12345)
    (let ((gen (gen:integers :min 0 :max 10)))
      (dotimes (i 100)
        (let ((val (generate gen *test-random-state* 30)))
          (is (<= 0 val 10)))))))

;;;; String Generator Tests

(test string-generator-produces-strings
  (with-property-context (:seed 12345)
    (let ((gen (gen:strings)))
      (dotimes (i 100)
        (is (stringp (generate gen *test-random-state* 30)))))))

(test string-generator-respects-length
  (with-property-context (:seed 12345)
    (let ((gen (gen:strings :min-length 5 :max-length 10)))
      (dotimes (i 100)
        (let ((val (generate gen *test-random-state* 30)))
          (is (<= 5 (length val) 10)))))))

;;;; List Generator Tests

(test list-generator-produces-lists
  (with-property-context (:seed 12345)
    (let ((gen (gen:lists (gen:integers))))
      (dotimes (i 100)
        (is (listp (generate gen *test-random-state* 30)))))))

(test list-generator-respects-length
  (with-property-context (:seed 12345)
    (let ((gen (gen:lists (gen:integers) :min-length 2 :max-length 5)))
      (dotimes (i 100)
        (let ((val (generate gen *test-random-state* 30)))
          (is (<= 2 (length val) 5)))))))

;;;; Combinator Tests

(test one-of-picks-from-generators
  (with-property-context (:seed 12345)
    (let ((gen (gen:one-of (gen:integers :min 0 :max 0)
                           (gen:strings :min-length 1 :max-length 1))))
      (let ((results (loop repeat 100 collect (generate gen *test-random-state* 30))))
        (is (some #'integerp results))
        (is (some #'stringp results))))))

(test such-that-filters
  (with-property-context (:seed 12345)
    (let ((gen (gen:such-that #'evenp (gen:integers :min 0 :max 100))))
      (dotimes (i 50)
        (is (evenp (generate gen *test-random-state* 30)))))))

(test tuple-produces-correct-length
  (with-property-context (:seed 12345)
    (let ((gen (gen:tuple (gen:integers) (gen:strings) (gen:booleans))))
      (dotimes (i 50)
        (let ((val (generate gen *test-random-state* 30)))
          (is (= 3 (length val)))
          (is (integerp (first val)))
          (is (stringp (second val)))
          (is (typep (third val) 'boolean)))))))
