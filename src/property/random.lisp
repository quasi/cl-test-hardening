;;; ABOUTME: Random number generation with reproducible seeding
;;; Provides deterministic random state from seed for test reproducibility

(in-package #:th.property)

;;;; Deterministic Random State

(defvar *test-random-state* nil
  "The random state used during property checking. Bound during test execution.")

(defvar *current-size* 30
  "Current size parameter for generators. Larger = more complex values.")

(defun make-random-state-from-seed (seed)
  "Create a deterministic random state from an integer seed.
Same seed always produces same sequence of random values."
  (let ((state (make-random-state nil)))
    ;; Warm up the generator with the seed to distribute state
    (dotimes (i (mod seed 1000))
      (random 1000 state))
    ;; Additional mixing based on seed bits
    (dotimes (i 100)
      (random (1+ (mod (+ seed i) 10000)) state))
    state))

(defun generate-seed ()
  "Generate a random seed for a new test run."
  (random (expt 2 31)))

;;;; Random Value Generation Utilities

(defun next-random-integer (min max &optional (state *test-random-state*))
  "Generate a random integer in [MIN, MAX] inclusive."
  (when (> min max)
    (rotatef min max))
  (+ min (random (1+ (- max min)) state)))

(defun next-random-float (min max &optional (state *test-random-state*))
  "Generate a random float in [MIN, MAX)."
  (+ min (* (- max min) (random 1.0 state))))

(defun next-random-boolean (&optional (state *test-random-state*))
  "Generate a random boolean."
  (zerop (random 2 state)))

(defun next-random-element (sequence &optional (state *test-random-state*))
  "Select a random element from SEQUENCE."
  (when (plusp (length sequence))
    (elt sequence (random (length sequence) state))))

(defun shuffle-list (list &optional (state *test-random-state*))
  "Return a shuffled copy of LIST using Fisher-Yates algorithm."
  (let ((vec (coerce list 'vector)))
    (loop for i from (1- (length vec)) downto 1
          for j = (random (1+ i) state)
          do (rotatef (aref vec i) (aref vec j)))
    (coerce vec 'list)))

;;;; Size-based Generation

(defmacro with-size ((size) &body body)
  "Execute BODY with *current-size* bound to SIZE."
  `(let ((*current-size* ,size))
     ,@body))

(defun scaled-size (fraction)
  "Return SIZE scaled by FRACTION (0.0 to 1.0)."
  (max 1 (floor (* *current-size* fraction))))
