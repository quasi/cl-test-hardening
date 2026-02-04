;;; ABOUTME: Package definition for th.harness module
;;; Declarative test environment setup to eliminate boilerplate preambles

(defpackage #:th.harness
  (:use #:cl #:alexandria #:th.core)
  (:export
   ;; Harness definition
   #:define-harness

   ;; Registry
   #:find-harness
   #:list-harnesses
   #:clear-harnesses

   ;; Harness accessors
   #:harness-systems
   #:harness-extra-systems
   #:harness-load-files
   #:harness-package

   ;; Setup
   #:harness-setup-forms
   #:setup))
