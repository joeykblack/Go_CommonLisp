;;;; Random player
;;;; joeykblack
;;;; 3/22/2012

(in-package #:go2)


(defun rnd-vertex (size)
  (cons
   (random size)
   (random size)))

(defun rnd-game (board isblack kov size captured-black captured-white komi
		 max-depth &optional (depth 0))
  (if (> depth max-depth)
      (score-eval board captured-black captured-white komi size)
      (let ((new-board nil)
	    (deadvs nil))
	(rnd-game
	 (do ((v (rnd-vertex size) (rnd-vertex size))
	      (c 0 (1+ c)))
	     ((or new-board (> c 10)) (if new-board new-board board))
	   (multiple-value-setq (new-board deadvs kov)
	     (attempt-move v isblack board kov size))
	   (if (and new-board deadvs)
	       (if isblack
		   (push deadvs captured-black)
		   (push deadvs captured-white))))
	 (not isblack)
	 kov
	 size
	 captured-black
	 captured-white
	 komi
	 max-depth
	 (1+ depth)))))

(defun play-rnd-game (vertex isblack start-board size start-kov
		      start-captured-black start-captured-white
		      komi max-depth)
  (let ((board (copy-array-trees start-board))
	(kov (copy-tree start-kov))
	(captured-black (copy-tree start-captured-black))
	(captured-white (copy-tree start-captured-white))
	(deadvs nil))
    (multiple-value-setq (board deadvs kov)
      (attempt-move vertex isblack board kov size))
    (if board
	(progn
	  (if deadvs
	      (if isblack
		  (push deadvs captured-black)
		  (push deadvs captured-white)))
	  (rnd-game board isblack kov size
		    captured-black captured-white
		    komi max-depth)))))

(defun nplay-rnd-game (vertex isblack board size kov
		      captured-black captured-white
		      komi percent-full)
  (let* ((deadvs nil)
	 (bitboard (to-bitboard board size))
	 (occupied (logcount (logior (car bitboard)
				     (cdr bitboard))))
	 (max-depth (- (* percent-full (* size size)) occupied)))
    (multiple-value-setq (board deadvs kov)
      (attempt-move vertex isblack board kov size))
    (if board
	(progn
	  (if deadvs
	      (if isblack
		  (push deadvs captured-black)
		  (push deadvs captured-white)))
	  (rnd-game board isblack kov size
		    captured-black captured-white
		    komi max-depth)))))
     
    
