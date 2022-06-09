;;;; this function will house all of the utility functions, usually smaller sized functions like macros
;;;;    and such will live here

;;; steps of processing
;; tokenizer
;; AST
;; evaluator	

;; need a token class

(defun token-p (object)
  (eql (type-of object) 'token))

(defmethod left-p ((token token))
  "a predicate which returns a boolean if the token has a left child"
  (not (null (token-left token))))

(defmethod right-p ((token token))
  "a predicate which returns a boolean if the token has a right child"
  (not (null (token-right token))))

(defmethod children-p ((token token))
  "this predicate returns true if the token has at least 1 child"
  (>
   (length
    (get-children token)))
  0)

(defclass token ()
  ((character :accessor token-char
	      :initarg :char
	      :documentation "the token's symbol")
   (left :accessor token-left
	 :initarg :left
	 :initform nil
	 :allocation :instance
	 :documentation "the token's left child")
   (right :accessor token-right
	  :initarg :right
	  :initform nil
	  :allocation :instance
	  :documentation "the token's right child")))

(defmethod get-children ((token token))
  "this method returns the children of the token,
this function can return a list of nil values"
  (remove nil
	  (list
	   (token-left token)
	   (token-right token))))

(defmethod print-object ((tok token) stream)
  "this function nicely prints a token out to the user"
  (print-unreadable-object (tok stream :type t)
    (with-accessors ((char token-char)
		     (children token-children))
	tok
      (let ((children (length (get-children tok))))
	(format stream "~s, ~d ~[children~;child~:;children~]"
		char
		children
		children)))))

;;; AST class

(defmethod tokenize ((string sequence))
  "this method turns a string into an AST"
  (let ((context nil) (head))
    (loop for char across string
	  do (if (null context)
		 (progn
		   (setf context (make-instance
				  'token :char char))
		   (setq head context))
		 (progn
		   (setf (token-right context)
			 (make-instance 'token
					:char char))
		   (setq context (token-right context)))))
    head))

(defmethod walk-token-tree ((token token))
  (progn
    (if (left-p token)
	(walk-token-tree (token-left token)))
    (format t "~a~[->~;~:;~]"
	    (token-char token)
	    (if (right-p token) 0 1))
    (if (right-p token)
	(walk-token-tree (token-right token))))
  t) ; returning t as a default return value

;;; evaluater class
