(in-package #:cl-user)

(asdf:defsystem :cl-wordpress-test
  :name "cl-wordpress-test"
  :components ((:file "clwordpresstest"))
  :serial t
  :depends-on (:s-xml-rpc
	       :cl-wordpress))
	       