(in-package #:cl-user)

(asdf:defsystem :cl-wordpress
  :name "cl-wordpress"
  :version "0.1"
  :maintainer "David Thole"
  :author "David Thole"
  :license "LGPL"
  :description "A simple library for interacting with a wordpress host"
  :components ((:file "package")
	       (:file "wordpress"))
  :serial t
  :depends-on (:s-xml-rpc))
	       