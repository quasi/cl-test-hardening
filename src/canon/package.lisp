;;;; Canon verification protocol adapter

(defpackage #:th.canon
  (:use #:cl #:alexandria #:th.core)
  (:export
   #:run-verification
   #:read-verification-request
   #:write-verification-result
   #:*request-path*
   #:*result-path*))
