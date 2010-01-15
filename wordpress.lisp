; Wordpress.lisp, central file for the library itself.

(in-package :cl-wordpress)

(defvar *host* nil)

(defun getAvailableOptions ()
  (xml-rpc-call (encode-xml-rpc-call "mt.supportedMethods") :host *host* :url "/xmlrpc.php"))

(defun getBlogEntries ()
  (let ((bt nil))
    (loop for x in 
	 (xml-rpc-call 
	  (encode-xml-rpc-call "mt.getRecentPostTitles" 1 "admin" "w7PE5pFyYSYWCtAqTsBP") :host *host* :url "/xmlrpc.php") do
	 (push `((id . ,(get-xml-rpc-struct-member x :|postid|))
		 (title . ,(get-xml-rpc-struct-member x :|title|))
		 (time . ,(xml-rpc-time-universal-time (get-xml-rpc-struct-member x :|dateCreated|)))) bt))
    bt))

