(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (load "clwordpresstest.lisp")))

(in-package :cl-wordpress-test)

(setf *test-describe-if-not-successful?* t)
(defun run-all-tests ()
  (run-tests)
  (format t "~%~%Test Output: ~%~a~%" (run-tests)))

; Default is to just run the tests....

(run-all-tests)