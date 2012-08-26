
(defvar bin-ops '(+ - * /) )

(defun process-branches-for (ops seq process)
  "returns nil if no op ; seq must be list"
  (if ops (let* ((op (car ops))
		 (pos (position op seq)))
	    (if pos
		(list op 
		      (let ((operand (subseq seq 0 pos))) 
			(if operand (funcall process operand) 0))
		      (funcall process (subseq seq (1+ pos))))
		(process-branches-for (cdr ops) seq process)))))

(defun m-to-s (expr)
  "returns s-expression made from m-expression"
  (cond
    ((atom expr) expr)
    ((process-branches-for bin-ops expr #'m-to-s))
    ((cdr expr) (mapcar #'m-to-s expr))
    (t (m-to-s (car expr)))))

(defun listify (expr-str &optional (res ""))
  "returns m-exp made from string expr-str"
  (if (zerop (length expr-str)) 
      (read-from-string res)
      (let ((c (elt expr-str 0)))
	(listify (subseq expr-str 1)
		 (if (position c (remove #\Space (princ-to-string bin-ops)))
		     (concatenate 'string res " " (string c) " ")
		     (concatenate 'string res (string c)))))))

(defun mexp (expr)
  (m-to-s (listify (princ-to-string expr))))
    
(defmacro @ (&body body)
  (mexp body))