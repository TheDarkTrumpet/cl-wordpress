(in-package #:cl-user)

(defpackage #:wordpress-publish
  (:use :cl
	:cl-fad
	:clsql
	:clsql-mysql
	:clsql-sqlite3
	:md5))

(in-package :wordpress-publish)

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
	       