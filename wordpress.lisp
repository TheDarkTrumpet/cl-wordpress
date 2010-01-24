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

;; Blog category addition and deletion functions.
(defun addCategory (connspec category)
  t
  )

(defun addCategories (connspec categories)
  t)

(defun getCategories (connspec)
  (let ((categories NIL))
    (with-xml-rpc-call connspec categories "metaWeblog.getCategories" ()
      (if (typep categories 'cons)
	  (mapcar 'xml-rpc-struct-alist categories)
	  0))))

	categories)))

;; Blog addition and deletion functions.
(defun postBlog (connspec &key (content nil) (title nil) (categories nil))
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "metaWeblog.newPost" ((xml-rpc-struct "title" title "description" content "categories" categories) 1) retval)
    retval))

(defun getBlogEntries (connspec)
  (let ((blogs NIL) (retval '()))
    (with-xml-rpc-call connspec blogs "metaWeblog.getRecentPosts" (100000)
      (if (typep blogs 'cons)
	  (progn 
	    (loop for x in blogs do
		 (progn
		   (setf x (xml-rpc-struct-alist x))
		   (rplacd (assoc :|dateCreated| x) (xml-rpc-time-universal-time (cdr (assoc :|dateCreated| x))))
		   (push x retval))
	       finally (reverse retval)))
	  (setf retval 0)))
    retval))

(defun removeBlogEntry (connspec id)
  (xml-rpc-call (encode-xml-rpc-call "metaWeblog.deletePost"
				     (blogid connspec)
				     id
				     (uid connspec)
				     (pass connspec))
		:host (host connspec)
		:url (url connspec)))

(defun removeAllBlogEntries (connspec)
  (loop for x in (getblogEntries connspec) do
       (removeBlogEntry connspec (cdr (assoc :|postid| x)))))