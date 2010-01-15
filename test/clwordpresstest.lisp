; Tests to test cl-wordpress

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (require :asdf)
	 (require :cl-wordpress)
	 (require :lift)))

(defpackage :cl-wordpress-test
  (:use :cl :cl-wordpress :lift))

(in-package :cl-wordpress-test)

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
					 :blogid (cdr (assoc 'blogid wp-info-alist)))))))
  
(addtest (wordpress-class-tests)
  wp-info-acc-host
  (lift:ensure (eql (cdr (assoc 'host wp-info-alist)) (host wp-info-object))))
