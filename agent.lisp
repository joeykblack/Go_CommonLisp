;;;; AI agent
;;;; joeykblack
;;;; 4/4/2012

(in-package #:go2)


(defcommand reg_genmove (color)
  (let ((vertex (if (eq 'mc *agent-type*)
		    (mc-reg-genmove *num-rnd-games* (blackp color)
				    *board* *size* *kov*
				    *captured-black* *captured-white*
				    *komi* *max-depth*)
		    (uct-reg-genmove *num-visits-root* *time-limit*
				     (blackp color)
				     *board* *size* *kov*
				     *captured-black* *captured-white*
				     *komi* *percent-full*
				     *expand-point*))))
    (if vertex
	(ok (from-vertex vertex))
	(fails "no valid moves"))))

(defcommand genmove (color)
  (let* ((reg-move (reg_genmove color)))
    (if (eq (car reg-move) '=)
	(progn
	  (play color (cadr reg-move))
	  (ok (cadr reg-move)))
	(fails (cdr reg-move)))))
