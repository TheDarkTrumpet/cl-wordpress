; Tests to test cl-wordpress

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (require :asdf)
	 (require :cl-wordpress)
	 (require :lift)))

(defpackage :cl-wordpress-test
  (:use :cl :cl-wordpress :lift))

(in-package :cl-wordpress-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper macro for defining the test, since the general theme is roughly the same
(defmacro def-acc-test (sym)
  `(ensure (eql (cdr (assoc ,sym wp-info-alist))
		(funcall ,sym wp-info-object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Class Definition Tests - make sure we can access each class, and their respective slots for public access,
; setting and getting each element.

(deftestsuite wordpress-class-tests ()
  ((wp-info-object nil)
   (wp-info-alist nil))
  (:run-setup :once-per-suite)
  (:setup 
   (progn
     (setf wp-info-alist '((host . "myhost.mytestdomain.com")
			   (url . "/my/path")
			   (uid . "mytestuser")
			   (pass . "mytestpass")
			   (blogid . 1)))
     (setf wp-info-object (make-instance 'wp-information 
					 :host (cdr (assoc 'host wp-info-alist))
					 :url (cdr (assoc 'url wp-info-alist))
					 :uid (cdr (assoc 'uid wp-info-alist))
					 :pass (cdr (assoc 'pass wp-info-alist))
					 :blogid (cdr (assoc 'blogid wp-info-alist))))))
  ;Accessing host
  (:test (acc-host (def-acc-test 'host)))
  ;Accessing url
  (:test (acc-url (def-acc-test 'url)))
  ;Accessing uid
  (:test (acc-uid (def-acc-test 'uid)))
  ;Accessing pass
  (:test (acc-pass (def-acc-test 'pass)))
  ;Accessing blogid
  (:test (acc-blogid (def-acc-test 'blogid))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Our soap calls - method calls.
;;

(deftestsuite wordpress-soap-stubbed-tests ()
  (xml-rpc-structs
   test-server-location)
  (:run-setup :once-per-suite)
  (:setup
   (progn
     (setf test-server-location (make-instance 'wp-information :blogid 1
					       :uid "testuid"
					       :pass "testpass"
					       :url "/foo/bar"
					       :host "localhost"))
     (setf xml-rpc-structs 1)))
  (:test (test-macro-creation (ensure (macroexpand '(with-xml-rpc-call test-server-location c "foo.bar"))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wrapper code.

(defun run-all-wp-tests ()
  (loop for x in '(wordpress-class-tests wordpress-soap-stubbed-tests) do
       (run-tests :suite x)
       (format t "~%~%Test Output: ~%~a~%" (run-tests :suite x))))