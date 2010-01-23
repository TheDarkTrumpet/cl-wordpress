(in-package #:cl-user)

(asdf:defsystem :cl-wordpress
  :name "cl-wordpress"
  :maintainer "David Thole"
  :author "David Thole"
  :license "LGPL"
  :description "A simple library for interacting with a wordpress host"
  :components ((:file "package")
	       (:file "classdefs")
	       (:file "wordpress"))
  :serial t
  :depends-on (:s-xml-rpc))
	       