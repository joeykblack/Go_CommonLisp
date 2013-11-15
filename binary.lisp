;;;; Bitboard opperations
;;;; joeykblack
;;;; 4/2/2012

(in-package #:go2)


(defun to-bitboard (board size)
  (let ((bitboard-black 0)
	(bitboard-white 0))
    (loop for r below size do
	 (loop for c below size do
	      (let* ((s (find-set `(,c . ,r) board))
		     (cell (if s (get-cell s board)))
		     (isblack (if cell (get-isblack cell))))
		(if cell
		    (let* ((board (if isblack
				      bitboard-black
				      bitboard-white))
			   (updated (logior board
					    (ash 1 (+ c (* size r))))))
		      (if isblack
			  (setf bitboard-black updated)
			  (setf bitboard-white updated)))))))
    (cons bitboard-black bitboard-white)))


(defun from-bitboard (black white size)
  (let ((board (make-array `(,size ,size) :initial-element nil)))
    (loop for r below size do
	 (loop for c below size do
	      (let ((vertex `(,c . ,r))
		    (index (+ c (* r size))))
		(if (logbitp index black)
		    (create-set vertex t 0 board))
		(if (logbitp index white)
		    (create-set vertex nil 0 board)))))
    board))
		



	      
(defmacro left-mask (size)
  `(lognot (let ((board 0))
	    (loop for i below ,size do
		 (setf board (logior (ash 1 (1- ,size))
				     (ash board ,size))))
	    board)))

(defmacro right-mask (size)
  `(lognot (let ((board 0))
	    (loop for i below ,size do
		 (setf board (logior 1 (ash board ,size))))
	    board)))

(defun expand-blocking (expander blocker size)
  (let ((byte-size (* size size)))
    (logandc2
     (logior
      (ldb (byte byte-size  0) (ash expander size))
      (ldb (byte byte-size 0) (logand (ash expander 1) (right-mask size)))
      (ash expander (* -1 size))
      (logand (ash expander -1) (left-mask size))
      expander)
     blocker)))

(defun full-expand-blocking (expander blocker size &optional (old-count -1))
  (let ((bit-count (logcount expander)))
    (if (eq bit-count old-count)
	expander
	(full-expand-blocking (expand-blocking expander blocker size)
			      blocker
			      size
			      bit-count))))
  
						
