; Tests to test cl-wordpress

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (require :asdf)
	 (require :cl-wordpress)
	 (require :lift)))

(defpackage :cl-wordpress-test
  (:use :cl :cl-wordpress :lift))


; Class Definition Tests - make sure we can access each class, and their respective slots for public access,
; setting and getting each element.

(lift:deftestsuite wordpress-class-tests ()
  ())

(lift:addtest (wordpress-class-tests)
  eql-test
  (lift:ensure (eql 1 2)))
