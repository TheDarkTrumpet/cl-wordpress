; Package file - defines the package and the visible interface functions to the world.

(in-package :cl-user)

(defpackage :cl-wordpress
  (:use :cl :s-xml-rpc)
  (:export 
   ; class wp-information
   :wp-information
   :host
   :url
   :uid
   :pass
   :blogid))

