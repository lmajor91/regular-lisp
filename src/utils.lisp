;;;; this file contains all of the functions which are used in the main regular expression file

;;;; this deals exclusively with strings, not streams

(defconstant *special-chars* '(#\( #\) #\[ #\] #\{ #\}))

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

;; this function produces a function which takes a char as a param and returns a boolean
;; - a fancy way of determining whether a char belongs in a set (literally all a [] closure does)
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

;; This is just a nice wrapper for a junction to pass closures into for them to get parsed correctly
;; - this function accepts a pure, complete closure e.g., [abc], (test), {3,}
(defun eval-closure (closure &key
			       (sq-brac #'%eval-square-closure)
			       (pr-brac #'%eval-paren-closure)
			       (cr-brac #'%eval-curly-closure))
  (eval
   `(case (char ,closure 0)
      (#\( (funcall ,pr-brac ,closure))
      (#\[ (funcall ,sq-brac ,closure))
      (#\{ (funcall ,cr-brac ,closure))
      (t nil))))

;;;;; SIMPLE FUNCTIONS ;;;;;

;; This function is a shorthand to determine whether the character entered is to be treated as a special char within regex
(defun special-char? (char &key (chars *special-chars*))
  (not
   (null
    (member char chars))))

(defun %get-closing-closure (char)
  (case char
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (t nil)))
