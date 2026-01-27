;;;; Common result types for verification modules

(in-package #:th.core)

(defstruct verification-result
  "Base result structure for all verification types"
  (passed-p nil :type boolean)
  (timestamp (current-timestamp) :type string)
  (duration-ms 0 :type integer)
  (summary "" :type string)
  (details nil :type list))

(defun current-timestamp ()
  "Return ISO 8601 timestamp string"
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

(defmacro measure-time (&body body)
  "Execute BODY and return (values result milliseconds)"
  (let ((start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (values
        (progn ,@body)
        (round (* 1000 (/ (- (get-internal-real-time) ,start)
                          internal-time-units-per-second)))))))
