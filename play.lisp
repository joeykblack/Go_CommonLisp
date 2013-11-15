;;;; Go play commands
;;;; joeykblack
;;;; 2/27/12

(in-package #:go2)


(defun col-to-char (col)
  (if (>= col 8)
      (setf col (1+ col)))
  (code-char (+ col (char-code #\A))))

(defun to-vertex (vertex-string)
  (let ((col (-
	      (char-code (char-upcase (char vertex-string 0)))
	      (char-code #\A)))
	(row (1- (parse-integer (subseq vertex-string 1)))))
    (if (>= col 8) ; >= 'I'
	(setf col (1- col)))
    (cons col row)))

(defun from-vertex (vertex)
  (format nil "~c~d"
	  (col-to-char (car vertex))
	  (1+ (cdr vertex))))


(defcommand play (&rest params)
  (let* ((color (if (> (length params) 1)
		    (car params)
		    (if *isblack-turn* 'b 'w)))
	 (vertex-sym (if (> (length params) 1)
			 (cadr params)
			 (car params)))
	 (vertex-string (if (symbolp vertex-sym)
			    (symbol-name vertex-sym)
			    vertex-sym)))
    (push (cons color vertex-string) *history*)
    (if (or (string-equal vertex-string "pass")
	    (string-equal vertex-string "resign"))
	(ok "")
	(let ((isblack (blackp color))
	      (vertex (to-vertex vertex-string))	      
	      (new-board nil)
	      (deadvs nil)
	      (kov nil))
	  (multiple-value-setq (new-board deadvs kov)
	    (attempt-move vertex isblack *board* *kov* *size*))
	  (if new-board
	      (progn
		(setf *isblack-turn* (not isblack))
		(setf *board* new-board)
		(setf *kov* kov)
		(if deadvs
		    (if isblack
			(push deadvs *captured-white*)
			(push deadvs *captured-black*)))
		(ok ""))
	      (fails "illegal move"))))))


(defun restore-libs (v board amount)
  (if v
      (let ((s (find-set v board)))
	(if s
	    (let ((cell (get-cell s board)))
	      (setf (get-lib-count cell)
		    (+ (get-lib-count cell) amount))
	      (setf (get-cell s board) cell))))))


(defcommand undo ()
  (if *history*
      (progn
	(setf *board* (make-array `(,*size* ,*size*) :initial-element nil))
	(setf *captured-black* nil)
	(setf *captured-white* nil)
	(setf *isblack-turn* t)
	(setf *kov* nil)
	(let ((history (cdr (copy-tree *history*))))
	  (setf *history* nil) 
	  (mapcar (lambda (move)
		    (play (car move) (cdr move)))
		  (reverse history))) ; if only it was that easy
	(ok ""))
      (fails "cannot undo")))

