(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(load "clwordpresstest.lisp")

(in-package :cl-wordpress-test)

(setf *test-describe-if-not-successful?* t)
(defun run-all-tests ()
  (format t "Test Output: ~%~a~%" (run-tests)))

; Default is to just run the tests....

(run-all-tests)