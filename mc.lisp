;;;; Monte Carlo
;;;; joeykblack
;;;; 3/22/2012

(in-package #:go2)


(defun count-wins (vertex isblack board size
		   kov captured-black captured-white
		   komi max-depth num-rnd-games)
  (flet ((rnd-game ()
	   (play-rnd-game vertex isblack board size
			  kov captured-black captured-white
			  komi max-depth))
	 (calc-wins (score)
		    (if score
			(if isblack
			    (if (> (car score) (cdr score))
				1 0)
			    (if (> (cdr score) (car score))
				1 0))
			nil)))
    (do* ((game 0 (1+ game))
	  (score (rnd-game) (rnd-game))
	  (wins (calc-wins score) (+ wins (calc-wins score))))
	 ((or (>= game num-rnd-games)
	      (not score))
	  wins))))

(defun mc-reg-genmove (num-rnd-games isblack board size
		       kov captured-black captured-white
		       komi max-depth)
  (do ((best-v nil)
       (most-wins 0)
       (r (1- size) (1- r)))
      ((< r 0) best-v)
    ;(format t "~%")
    (do* ((c 0 (1+ c))
	  (vertex `(,c . ,r) `(,c . ,r)))
	((>= c size))
      (let ((wins (count-wins vertex isblack board size kov
			      captured-black captured-white
			      komi max-depth num-rnd-games)))
	;(format t " ~d" wins)
	(if (and wins (>= wins most-wins))
	    (progn
	      (setf most-wins wins)
	      (setf best-v vertex)))))))
	    
	    
    
