; Wordpress.lisp, central file for the library itself.

(in-package :cl-wordpress)

;;;;;;; helper macros that the other methods can use ;;;;;;;
(defmacro with-xml-rpc-call (conn xmlrpcs method extra  &body body)
  `(progn
     (setf ,xmlrpcs (xml-rpc-call (encode-xml-rpc-call ,method
						    (blogid ,conn)
						    (uid ,conn)
						    (pass ,conn)
						    ,@extra)
				  :host (host ,conn)
				  :url (url ,conn)))
     ,@body))
 

;;;;;;; API Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getAvailableOptions (connspec)
  "Returns a con listing of all the supported methods"
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "mt.supportedMethods" () retval)
    retval))

(defun postBlog (connspec &key (content nil) (title nil))
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "metaWeblog.newPost" ((xml-rpc-struct "title" title "description" content) 1) retval)
    retval))

(defun getBlogEntries (connspec)
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "metaWeblog.getRecentPosts" (100000) retval)
    retval))
