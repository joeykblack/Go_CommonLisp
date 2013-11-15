;;;; Debug commands
;;;; joeykblack
;;;; 3/8/2012

(in-package #:go2)

(defun print-board (board size)
  (loop for i downfrom (1- size) to 0 do
       (format t "~d " (1+ i))
       (loop for j below size do
	    (let ((cell (aref board j i)))
	      (if cell
		  (format t "~a "
			  (if (get-isblack (if (parentp cell)
					       cell
					       (get-cell (find-set cell board)
							 board)))
			      'x 'o))
		  (format t "+ "))))
       (format t "~%"))
  (format t "  ~{~c ~}~%"
	  (do* ((code (1- (+ size (char-code #\A)))
		      (1- code))
		(i (char-code #\I))
		(line nil))
	       ((= code (1- (char-code #\A))) line)
	    (push (code-char (if (>= code i)
				 (1+ code)
				 code))
		  line))))


(defcommand showboard ()
  (ok (print-board *board* *size*)))

(defcommand showd ()
  (loop for i downfrom (1- *size*) to 0 do
       (loop for j below *size* do
	    (let ((cell (aref *board* j i)))
	      (format t "~a " cell)))
       (format t "~%"))
  (format t "~%")
  (ok ""))


(defcommand show-area ()
  (let ((bitboards (get-areas *board* *size*)))
    (ok (print-board (from-bitboard
		      (car bitboards)
		      (cdr bitboards)
		      *size*)
		     *size*))))
