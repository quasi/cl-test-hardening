;;;; Core package for cl-test-hardening
;;;; Shared types and utilities across all modules

(defpackage #:th.core
  (:use #:cl #:alexandria)
  (:export
   ;; Result protocol
   #:verification-result
   #:verification-result-passed-p
   #:verification-result-timestamp
   #:verification-result-duration-ms
   #:verification-result-summary
   #:verification-result-details

   ;; Reporting protocol
   #:format-result
   #:format-summary
   #:format-failure

   ;; Common utilities
   #:current-timestamp
   #:measure-time

   ;; Test helpers
   #:with-internals))
