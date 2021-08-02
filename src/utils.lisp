;;;; this file contains all of the functions which are used in the main regular expression file

;;;; this deals exclusively with strings, not streams

(defconstant *special-chars* '(#\( #\) #\[ #\] #\{ #\}))

;;;; gotta implement
;; ^ and $
;; a case to escape functionality of the next character appending a \

;;;;; HIGH ORDER FUNCTIONS ;;;;;

;; collecting the chars
(defmacro reg-match (pattern string)
  `(let ((matcher (eval-pattern ,pattern)))
     (loop for char across ,string
	   ;; extending the list in case there being no char-matcher pair
	   for list-extension = matcher then (cdr list-extension)
	   ;; reading from the extended list
	   for match = (or (car list-extension) nil)
	   ;; will terminate when (progn) returns nil
	   always (progn
		    (format t "char: ~A expr: ~S~%" char match)
		    ;; if match is a function call it, otherwise compare the chars
		    (if (functionp match)
			(funcall match char)
			(char= match char))))))

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


(defun %eval-paren-closure (closure)
  (eval
   `#'(lambda ())))

(defun %eval-curly-closure (closure) #\})

(defun %eval-pipe-closure (closure) #\|)

(defun %parse-closure (pattern starting-index terminator)
  "This parses a regex closure in a string starting at *starting-index* and ending at the first instance of *terminator*"
  (let ((ending-index (position terminator pattern
				:start starting-index
				:from-end nil
				:test #'char-equal)))
    (when (not (null ending-index))
      (subseq pattern starting-index
	      (+ 1 ending-index)))))

;;;;; EVAL FUNCTIONS ;;;;;

;; this takes a pattern and forms a function to parse a string, basically a lexer
(defun eval-pattern (pattern)
  ;; char-at is a shorthand
  (flet ((char-at (index) (aref pattern index)))
    (loop for i from 0 to (- (length pattern) 1)
	  if (special-char? (char-at i))
	    collect (eval-closure
		     (%parse-closure pattern i
				     (%get-closing-closure (char-at i))))
	    and do (setf i (position (%get-closing-closure (char-at i))
				     pattern
				     :start i))
	  else
	    collect (char-at i))))

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
