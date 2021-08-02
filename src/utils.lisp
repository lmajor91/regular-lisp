;;;; this file contains all of the functions which are used in the main regular expression file

;;;; this deals exclusively with strings, not streams

;;;; gotta implement
;; ^ and $
;; a case to escape functionality of the next character appending a \

;;;;; HIGH ORDER FUNCTIONS ;;;;;
  `(let ((matcher (eval-pattern ,pattern)))
     (loop for string in ,strings
	   ;; debugging purposes to see what the expression looks like
	   ;; do (format t "~S~%" `(,@sexpr ,string))
	   collect (funcall matcher string))))

;;;;; % OR SUB-FUNCTIONS ;;;;;

;; make implementation for special characters

;; this will return a function to produce a true or false for the regular expression
;; things like [A-Z] will return a lambda that returns a boolean
(defmacro eval-closure (closure &key
				  (sq-brac #'%eval-square-closure)
				  (pr-brac #'%eval-paren-closure)
				  (cr-brac #'%eval-curly-closure))
  `(case (char ,closure 0)
     (#\( (funcall ,pr-brac ,closure))
     (#\[ (funcall ,sq-brac ,closure))
     (#\{ (funcall ,cr-brac ,closure))
     (t nil)))

;; this function assumes that it has a correct closure, but it will still assert
;; returns a function which produces a boolean

;; add clause for "-"
(defun %eval-square-closure (closure)
  (eval
   (let ((series
	   (loop for char across closure
		 when (not
		       (or
			(char= char #\[)
			(char= char #\])))
		   collect char)))
     `#'(lambda (test-char)
	  (not
	   (null
	    (member test-char ',series)))))))


(defun %eval-paren-closure (closure) #\))

(defun %eval-curly-closure (closure) #\})

(defun %eval-pipe-closure (closure) #\|)

;;;;; EVAL FUNCTIONS ;;;;;

;;;;; SIMPLE FUNCTIONS ;;;;;
