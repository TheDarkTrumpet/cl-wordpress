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
   :blogid
   ; Helper macros and methods
   :with-xml-rpc-call
   :getAvailableOptions
   :getBlogEntries
   :postBlog
   :deleteblogEntry
   :addCategory
   :getCategories
   :deleteCategory
   :deleteAllCategories
   :findBlogByTitle
   ;For the tests, and general cleanup if others needed
   :deleteAllBlogEntries
   :deleteAllCategories))

