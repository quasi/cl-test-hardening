;;; ABOUTME: Package definition for th.fixture module
;;; Test fixture registry with factory functions and context macros

(defpackage #:th.fixture
  (:use #:cl #:alexandria #:th.core)
  (:export
   ;; Fixture definition
   #:define-fixture

   ;; Registry
   #:find-fixture
   #:list-fixtures
   #:clear-fixtures

   ;; Instantiation
   #:build-fixture
   #:with-fixture))
