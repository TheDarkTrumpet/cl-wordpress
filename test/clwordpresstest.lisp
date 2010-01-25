; Tests to test cl-wordpress

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (require :asdf)
	 (require :cl-wordpress)
	 (require :lift)))

(defpackage :cl-wordpress-test
  (:use :cl :cl-wordpress :lift))

(in-package :cl-wordpress-test)

(setf *test-describe-if-not-successful?* t)
(setf *test-print-testsuite-names* nil)
(setf *test-print-test-case-names* nil)
(setf *lift-debug-output* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper macro for defining the test, since the general theme is roughly the same
(defmacro def-acc-test (sym)
  `(ensure (eql (cdr (assoc ,sym wp-info-alist))
		(funcall ,sym wp-info-object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic tests to make sure our test suite is sane
;

(deftestsuite basic-tests ()
  ()
  (:test (pkg-name (ensure (equal (package-name *package*) "CL-WORDPRESS-TEST")
			   :report "Package name supposed to be CL-WORDPRESS-TEST, got: ~a"
			   :arguments ((package-name *package*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Class Definition Tests - make sure we can access each class, and their respective slots for public access,
; setting and getting each element.

(deftestsuite wordpress-class-tests ()
  ((wp-info-object nil)
   (wp-info-alist nil))
  (:run-setup :once-per-suite)
  (:setup 
   (progn
     (setf wp-info-alist '((host . "myhost.mytestdomain.com")
			   (url . "/my/path")
			   (uid . "mytestuser")
			   (pass . "mytestpass")
			   (blogid . 1)))
     (setf wp-info-object (make-instance 'wp-information 
					 :host (cdr (assoc 'host wp-info-alist))
					 :url (cdr (assoc 'url wp-info-alist))
					 :uid (cdr (assoc 'uid wp-info-alist))
					 :pass (cdr (assoc 'pass wp-info-alist))
					 :blogid (cdr (assoc 'blogid wp-info-alist))))))
  ;Accessing host
  (:test (acc-host (def-acc-test 'host)))
  ;Accessing url
  (:test (acc-url (def-acc-test 'url)))
  ;Accessing uid
  (:test (acc-uid (def-acc-test 'uid)))
  ;Accessing pass
  (:test (acc-pass (def-acc-test 'pass)))
  ;Accessing blogid
  (:test (acc-blogid (def-acc-test 'blogid))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Our soap calls - method calls.
;;

(deftestsuite wordpress-soap-stubbed-tests ()
  (xml-rpc-structs
   test-server-location)
  (:run-setup :once-per-suite)
  (:setup
   (progn
     (setf test-server-location (make-instance 'wp-information :blogid 1
					       :uid "testuid"
					       :pass "testpass"
					       :url "/foo/bar"
					       :host "localhost"))
     (setf xml-rpc-structs 1)))
  (:test (test-macro-creation (ensure (macroexpand '(with-xml-rpc-call test-server-location c "foo.bar" ()))))))


; Note these tests need to run in order
(deftestsuite wordpress-soap-nonstubbed-tests ()
  (test-server-location content)
  (:run-setup :once-per-suite)
  (:setup
   (progn
     (setf test-server-location (make-instance 'wp-information :blogid 1
					       :uid "admin"
					       :pass "w7PE5pFyYSYWCtAqTsBP"
					       :url "/xmlrpc.php"
					       :host "wordpress.tdtdev"))
     (setf content "This is a test blog post <br><br> We're going to see how well this actually works!")))
  (:teardown
   (progn
     (deleteAllBlogEntries test-server-location)
     (deleteAllCategories test-server-location)))
  (:test (test-blog-count-with-zero
	  (ensure (= (getBlogEntries test-server-location) 0))))
  (:test (ensure-available-options 
	  (ensure (intersection (getAvailableOptions test-server-location)
				'("mt.supportedMethods" "metaWeblog.deletePost" "metaweblog.getRecentPosts" "metaweblog.newPost") :test 'string-equal))))
  (:test (test-blog-post
	  (ensure (postBlog test-server-location :content "This is a test blog post" :title "This is a blog title"))))
  (:test (test-get-categories
	  (ensure (= (getCategories test-server-location) 0))))
  (:test (test-add-category-error
	  (ensure-error (addCategory test-server-location))))
  (:test (test-add-category
	  (ensure (progn
		    (addCategory test-server-location :name "Programming")
		    (addCategory test-server-location :name "Random")))))
  (:test (test-get-categories
	  (ensure (length (getCategories test-server-location)))))
  (:test (test-add-parent-category
	  (ensure (addCategory test-server-location :name "Common Lisp" :parent_id (cdr (assoc :|categoryId| (first (getcategories test-server-location))))))))
  (:test (test-blog-post-with-category
	  (ensure (progn
		    (postBlog test-server-location :content "This is a test blog post" :title "This is a blog title" :categories '("Programming"))
		    (equal (cdr (assoc :|categories| (car (last (getblogentries test-server-location))))) '("Programming"))))))
  (:test (test-blog-post-with-categories
	  (ensure (progn
		    (postBlog test-server-location :content "This is a another test blog" :title "blog title" :categories '("Programming" "Random"))
		    (equal (cdr (assoc :|categories| (car (last (getblogentries test-server-location))))) '("Programming" "Random"))))))
  (:test (test-category-deletion
	  (ensure (deleteCategory test-server-location (cdr (assoc :|categoryId| (car (last (getcategories test-server-location)))))))))
  (:test (test-blog-count
	  (ensure (> (length (getBlogEntries test-server-location)) 0))))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wrapper code.

(defun run-all-wp-tests ()
  (loop for x in '(basic-tests wordpress-class-tests wordpress-soap-stubbed-tests wordpress-soap-nonstubbed-tests) do
       (format t "Test Output: ~%~a~%" (run-tests :suite x))))