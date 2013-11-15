;;;; MC-UCT
;;;; joeykblack
;;;; 4/4/2012

(in-package #:go2)


;; tree format:
;; (vertex visits wins [nil=not expanded]
;;  or [c1 c2 ...]
;;  or [t] no children

(defun rnd-game-win-p (vertex isblack board size
		       kov captured-black captured-white
		       komi percent-full)
  (let ((score
	 (score-eval board captured-black captured-white komi size)))
;	 (nplay-rnd-game vertex isblack board size
;			 kov captured-black captured-white
;			 komi percent-full)))
    (funcall (if isblack #'> #'<) (car score) (cdr score))))


(defun create-uct-tree (children)
  `(nil 0 0 ,@children))

(defmacro get-vertex (node)
  `(car ,node))

(defmacro get-visits (node)
  `(cadr ,node))

(defmacro get-wins (node)
  `(caddr ,node))

(defmacro get-children (node)
  `(cdddr ,node))       



(defun find-children (isblack board size kov)
  "Finds all valid moves"
  (do ((children nil)
       (r (1- size) (1- r)))
      ((< r 0) (if children children t))
    (do* ((c 0 (1+ c))
	  (vertex `(,c . ,r) `(,c . ,r)))
	 ((>= c size))
      (let ((neighbors (valid-neighbors vertex size)))
	(if (valid-move-p vertex isblack board kov
			  (calc-vertex-lib-count board neighbors)
			  neighbors)
	    (push `(,vertex 0 0) children))))))

(defmacro make-move (node isblack board size kov
		     captured-black captured-white)
  (let ((deadvs (gensym)))
    `(let ((,deadvs nil))
       (multiple-value-setq (,board ,deadvs ,kov)
	 (attempt-move (get-vertex ,node) ,isblack ,board ,kov ,size))
       (if (and ,board ,deadvs)
	   (if ,isblack
	       (push ,deadvs ,captured-black)
	       (push ,deadvs ,captured-white))))))

(defun calc-uct (node parent-visits)
  (if node
      (if (> (get-visits node) 0)
	  ;; (wins/visits) + (K * sqrt( log(parent-visits) / (5 * visits)))
	  (+ (/ (get-wins node)
		(get-visits node))
	     (* *UCTK*
		(sqrt (/ (log parent-visits 10)
			 (* 5 (get-visits node))))))
	  (random 1000))))

(defun select-child (children parent-visits)
  "Select best child based on uct"
  (do* ((sublst children (cdr sublst))
	(cur (car sublst) (car sublst))
	(cur-uct (add-rave (calc-uct cur parent-visits) (get-vertex cur))
		 (add-rave (calc-uct cur parent-visits) (get-vertex cur)))
	(best-uct 0)
	(best-child cur))
       ((not cur)  best-child)
    (if (> cur-uct best-uct)
	(progn
	  (setf best-uct cur-uct)
	  (setf best-child cur)))))

(defun visit (node expand-point isblack board size kov
	      captured-black captured-white komi percent-full)
  "Update wins and visits for node. Returns updated node."
  (let ((children (get-children node)))
    `(,(get-vertex node) ,(1+ (get-visits node))
       ,(+ (get-wins node)
	   (update-wins (get-vertex node)
			(if (< (get-visits node) expand-point)
			    (rnd-game-win-p (get-vertex node)
					    isblack board size kov
					    captured-black captured-white
					    komi percent-full)
			    (progn
			      (make-move node isblack board size kov
					 captured-black captured-white)
			      (if (not children)
				  (setf children
					(find-children (not isblack)
						       board size kov)))
			      (let* ((selected
				      (select-child children
						    (get-visits node)))
				     (visited (visit selected expand-point
						     (not isblack)
						     board size kov
						     captured-black
						     captured-white komi
						     percent-full)))
				(setf children (merge-children
						children visited))
				(if (eq (get-wins selected) (get-wins visited))
				    t nil))))))
       ,@children)))



			
(defun merge-children (children updated)
  "Replaces the updated child in children"
  (acons (car updated) (cdr updated)
	 (remove-if (lambda (x)
		      (veq (car x) (car updated)))
		    children)))


(defun calc-winr (node)
  (if node
      (if (> (get-visits node) 0.0)
	  (/ (get-wins node)
	     (get-visits node))
	  0.0)
      0.0))

(defun select-winning-child (children)
  "Select best child based on uct"
  (do* ((sublst children (cdr sublst))
	(cur (car sublst) (car sublst))
	(cur-winr (add-rave (calc-winr cur) (get-vertex cur))
		  (add-rave (calc-winr cur) (get-vertex cur)))
	(best-winr 0.0)
	(best-child cur))
       ((not cur) best-child)
    (if (> cur-winr best-winr)
	(progn
	  (setf best-winr cur-winr)
	  (setf best-child cur)))))


;; these are just for the debugging method below (explain)
(defparameter *children* nil)
(defparameter *total-visits* 0)
(defparameter *selected* nil)

(defun uct-reg-genmove (num-visits-root time-limit
			isblack board size
			kov captured-black captured-white
			komi percent-full expand-point)
  "Visists children and returns best result (vertex)"
  (do* ((start-time (get-internal-real-time))
	(cur-time (get-internal-real-time)
		  (get-internal-real-time))
	(num-visits 0 (1+ num-visits))
	(children (find-children isblack board size kov)
		  (merge-children
		   children
		   (visit (select-child children num-visits) expand-point
			  isblack (copy-array-trees board) size
			  (copy-tree kov) (copy-tree captured-black)
			  (copy-tree captured-white) komi
			  percent-full))))
       ;; end condiiton
       ((if num-visits-root
	    (>= num-visits num-visits-root)
	    (>= (/ (- cur-time start-time)
		   internal-time-units-per-second)
		time-limit))
	;; result
	(progn
	  (setf *children* children)
	  (setf *total-visits* num-visits)
	  (setf *selected* (select-winning-child children))
	  (get-vertex *selected*)))))

(defun print-nodes (nodes indent)
  (do* ((node-lst nodes (cdr node-lst))
	(node (car node-lst) (car node-lst))
	(vertex (get-vertex node) (get-vertex node))
	(wins (get-wins node) (get-wins node))
	(visits (get-visits node) (get-visits node))
	(children (get-children node) (get-children node)))
       ((not node-lst))
    (if (not (zerop visits))
	(progn
	  (do ((i 0 (1+ i)))
	      ((>= i indent))
	    (format t " "))
	  (format t "v:~s w:~d v:~d rave:~d~%"
		  vertex wins visits
		  (get-rave-value vertex))))))
	  ;(print-nodes children (1+ indent))))))
	

(defcommand explain ()
  (print-nodes (reverse *children*) 0)
  (format t "Total visits: ~d~%" *total-visits*)
  (format t "Selected v:~s w:~d v:~d~%"
	  (get-vertex *selected*)
	  (get-wins *selected*)
	  (get-visits *selected*))
  (ok ""))
    
