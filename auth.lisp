; Auth.lisp - Objects that hold information such as the needed login information and so on.

(defclass wp-information ()
  ((host :accessor host :type (string 255) :initarg :host)
   (url :accessor url :type (string 255) :initarg :url)
   (uid :accessor uid :type (string 20) :initarg :uid)
   (pass :accessor pass :type (string 50) :initarg :pass)
   (blogid :accessor blogid :type integer :initarg :blogid)))

