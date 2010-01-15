; Tests to test cl-wordpress

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (require :cl-wordpress)
	 (require :lift)))

(defpackage :cl-wordpress-test
  (:use :cl :cl-wordpress :lift))




	 