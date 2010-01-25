(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (load "clwordpresstest.lisp")))

(in-package :cl-wordpress-test)

(defun run-all-tests ()
  (format t "~%Beginning test suite....~%")
  (run-all-wp-tests)
  (format t "~%Completed test suite...~%"))

; Default is to just run the tests....

(run-all-tests)