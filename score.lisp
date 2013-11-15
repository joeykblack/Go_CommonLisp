;;;; Caluclate Score
;;;; joeykblack
;;;; 3/22/2012

(in-package #:go2)


(defun get-areas (board size)
  (let* ((bitboards (to-bitboard board size))
	 (black-area (full-expand-blocking (car bitboards)
					   (cdr bitboards)
					   size))
	 (white-area (full-expand-blocking (cdr bitboards)
					   (car bitboards)
					   size))
	 (not-neutral (lognand black-area white-area)))
    (setf black-area (logand black-area
			     (logandc2 not-neutral (car bitboards))))
    (setf white-area (logand white-area
			     (logandc2 not-neutral (cdr bitboards))))
    (cons black-area white-area)))

(defun score-area (board captured-black captured-white size)
  (let ((areas (get-areas board size)))
    (cons
     (+ (logcount (car areas)) (count-captured captured-white))
     (+ (logcount (cdr areas)) (count-captured captured-black)))))

(defun score-komi (s komi)
  (cons (car s)
	(+ (cdr s) komi)))

(defun score-eval (board captured-black captured-white komi size)
  (score-area board captured-black captured-white size))


(defcommand score ()
  (let ((score (score-area *board*
			   *captured-black* *captured-white* *size*)))
    (ok (format nil "b:~d w:~d"
		(car score)
		(+ (cdr score) *komi*)))))
		
(defcommand final_score ()
  (score))
