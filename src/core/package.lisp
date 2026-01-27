;;;; Core package for cl-test-hardening
;;;; Shared types and utilities across all modules

(defpackage #:th.core
  (:use #:cl #:alexandria)
  (:export
   ;; Result protocol
   #:verification-result
   #:result-passed-p
   #:result-summary
   #:result-details
   #:result-timestamp
   #:result-duration-ms

   ;; Reporting protocol
   #:format-result
   #:format-summary
   #:format-failure

   ;; Common utilities
   #:current-timestamp
   #:measure-time))
