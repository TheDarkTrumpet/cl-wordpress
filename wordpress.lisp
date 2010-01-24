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
(defun addCategory (connspec &key (name nil) (slug "") (parent_id 0) (description ""))
  "Adds a new category, upon success will return an integer greater than 0, that being the category id"
  (if (not name)
      (error "You must pass a name at the very least"))
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "wp.newCategory" ((xml-rpc-struct "name" name "slug" slug "parent_id" parent_id "description" description)) retval)))

(defun getCategories (connspec)
  "Returns a list of alists, of the following symbols:
:|rssUrl|
:|htmlUrl|
:|categoryName|
:|categoryDescription|
:|description|
:|parentId|
:|categoryId|"
  (let ((categories NIL))
    (with-xml-rpc-call connspec categories "metaWeblog.getCategories" ()
      (if (typep categories 'cons)
	  (mapcar 'xml-rpc-struct-alist categories)
	  0))))

(defun deleteCategory (connspec id)
  "Deletes a category from the server, given an id.
We are not able to delete the default category, Uncategorized, so if that's all that remains, this will return 0"
  (let ((retval NIL))
    (with-xml-rpc-call connspec retval "wp.deleteCategory" (id) retval)))

(defun deleteAllCategories (connspec)
  (loop for x in (getcategories connspec) do
       (deleteCategory connspec (cdr (assoc :|categoryId| x)))))


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

(defun deleteBlogEntry (connspec id)
  (xml-rpc-call (encode-xml-rpc-call "metaWeblog.deletePost"
				     (blogid connspec)
				     id
				     (uid connspec)
				     (pass connspec))
		:host (host connspec)
		:url (url connspec)))

(defun deleteAllBlogEntries (connspec)
  (loop for x in (getblogEntries connspec) do
       (deleteBlogEntry connspec (cdr (assoc :|postid| x)))))