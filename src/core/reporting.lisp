;;;; Common reporting utilities

(in-package #:th.core)

(defgeneric format-result (result stream)
  (:documentation "Format a verification result for human consumption"))

(defgeneric format-summary (result)
  (:documentation "Return a one-line summary string"))

(defmethod format-result ((result verification-result) stream)
  (format stream "~&~A~%  Status: ~A~%  Duration: ~Dms~%"
          (format-summary result)
          (if (verification-result-passed-p result) "PASSED" "FAILED")
          (verification-result-duration-ms result)))
