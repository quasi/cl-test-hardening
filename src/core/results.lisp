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

(defmacro with-internals (package-designator (&rest symbol-names) &body body)
  "Execute BODY with SYMBOL-NAMES resolved from PACKAGE-DESIGNATOR.
Performs compile-time symbol substitution so unqualified names in BODY
refer to internal symbols of the target package. Useful for testing
private implementation details without :: clutter.

Example:
  (with-internals :my-library (internal-class internal-fn)
    (let ((obj (make-instance 'internal-class)))
      (is (internal-fn obj))))"
  (let* ((pkg (or (find-package package-designator)
                  (error "Package ~A not found" package-designator)))
         (mappings (mapcar (lambda (name)
                             (cons name (or (find-symbol (string name) pkg)
                                           (error "Symbol ~A not found in ~A"
                                                  name package-designator))))
                           symbol-names)))
    `(progn ,@(sublis mappings body :test #'eq))))
