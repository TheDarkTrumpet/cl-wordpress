; Wordpress.lisp, central file for the library itself.

(in-package :cl-wordpress)

;;;;;;; helper macros that the other methods can use ;;;;;;;
(defmacro with-xml-rpc-call (conn xmlrpcs method &body body)
  `(progn
     (setf ,xmlrpcs (xml-rpc-call (encode-xml-rpc-call ,method
						    (blogid ,conn)
						    (uid ,conn)
						    (pass ,conn))
				  :host (host ,conn)
				  :url (url ,conn)))
     ,@body))
 

;;;;;;; API Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getAvailableOptions (connspec)
  "Returns a con listing of all the supported methods"
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "mt.supportedMethods" retval)
    retval))

(defun getBlogEntries ()
  (let ((bt nil))
    (loop for x in 
	 (xml-rpc-call 
	  (encode-xml-rpc-call "mt.getRecentPostTitles" 1 "admin" "w7PE5pFyYSYWCtAqTsBP") :host *host* :url "/xmlrpc.php") do
	 (push `((id . ,(get-xml-rpc-struct-member x :|postid|))
		 (title . ,(get-xml-rpc-struct-member x :|title|))
		 (time . ,(xml-rpc-time-universal-time (get-xml-rpc-struct-member x :|dateCreated|)))) bt))
    bt))

