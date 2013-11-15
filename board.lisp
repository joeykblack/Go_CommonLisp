;;;; board configuration

(in-package #:go2)

(defun blackp (color)
  (or
   (and (symbolp color)
	(char-equal #\b (char (symbol-name color) 0)))
   (eq color 0)
   (and (stringp color)
	(char-equal #\b (char color 0)))))

(defun valid-neighbors (vertex size)
  (let ((col (car vertex))
	(row (cdr vertex))
	(max (- size 1))
	(neighbors nil))
    (if (> col 0)
	(push (cons (1- col) row) neighbors))
    (if (< col max)
	(push (cons (1+ col) row) neighbors))
    (if (> row 0)
	(push (cons col (1- row)) neighbors))
    (if (< row max)
	(push (cons col (1+ row)) neighbors))
    neighbors))



  





(defun nrestore-libs (deadv isblack board)
  (mapcar (lambda (neighbor)
	    (let* ((neighbor-set (find-set neighbor board))
		   (neighbor-set-cell (if neighbor-set
					  (get-cell neighbor-set
						    board))))
	      (if (and neighbor-set-cell
		       (eq (get-isblack neighbor-set-cell)
			   isblack))
		  (setf (get-lib-count (get-cell neighbor-set board))
			(1+ (get-lib-count neighbor-set-cell))))))
	  (valid-neighbors deadv *size*)))


(defun nupdate-neighbor (vertex isblack new-board filled-libs deadvs
			 unique-neighbor-cell unique-neighbor-set)
  "Updates nieghbors and evaluates to
   values: 'gives libs to move?' 'deadv + dead stones from this neighbor'
            'the updated board (destrucive)'"
  (if (eq isblack (get-isblack unique-neighbor-cell))
      (values 
       (< 0 (merge-sets unique-neighbor-set
			vertex
			new-board
			:filled-libs filled-libs))
       deadvs new-board)
      (if (>= 0 (setf (get-lib-count
		       (get-cell unique-neighbor-set
				 new-board))
		      (- (get-lib-count
			  unique-neighbor-cell)
			 filled-libs)))
	  (values t
		  (push
		   (mapcar (lambda (deadv)
			     (nrestore-libs deadv isblack new-board)
			     (setf (get-cell deadv new-board) nil)
			     deadv)
			   `(,@(get-members unique-neighbor-cell)
			       ,unique-neighbor-set))
		   deadvs)
		  new-board)
	  (values nil deadvs new-board))))


(defun ntest-neighbors (vertex isblack new-board neighbor-sets)
  "Tests if there is a captured group or neighbor group w/ libs
   Also updates neighbors"
  (let ((gives-libs nil)
	(deadvs nil))
    (if (reduce
	 (lambda (a b) (or a b))
	 (mapcar
	  (lambda (unique-neighbor-set)
	    (or
	     (not unique-neighbor-set)	; liberty
	     (multiple-value-setq (gives-libs deadvs new-board)
	       (nupdate-neighbor vertex isblack new-board
				 (count unique-neighbor-set
					neighbor-sets
					:test #'veq)
				 deadvs
				 (get-cell unique-neighbor-set
					   new-board)
				 unique-neighbor-set))))
	  (remove-duplicates neighbor-sets :test #'veq)))
	(values new-board deadvs `(,isblack ,@deadvs)))))

(defun emptyp (vertex board)
  "Is vertex empty on board"
  (null (get-cell vertex board)))

(defun kop (vertex kov isblack)
  "Is vertex a ko move for isblack"
  (and kov
   (eq (not isblack) (car kov))
   (eq 1 (length (cadr kov)))
   (veq vertex (caadr kov))))
	 

;;; Attempt to do move on copy of board
;;; evaluates to updated board and deadvs if success
;;;  otherwise null
(defun attempt-move (vertex isblack board kov size)
  (if (and
       (emptyp vertex board) ; empty test
       (not (kop vertex kov isblack))) ; ko test
      (let ((new-board (copy-array-trees board))
	    (neighbor-sets (mapcar (lambda (n) (find-set n board))
				    (valid-neighbors vertex size))))
	(create-set vertex isblack (count nil neighbor-sets) new-board)
	(ntest-neighbors vertex
			 isblack
			 new-board
			 neighbor-sets))))
			 





;;; These are used to find valid children for uct

(defun calc-vertex-lib-count (board neighbors)
  (length (remove-if (lambda (x) x)
		     (mapcar (lambda (neighbor)
			       (get-cell neighbor board))
			     neighbors))))

(defun valid-move-p (vertex isblack board kov
		     vertex-lib-count neighbors)
  (and
   (emptyp vertex board)
   (not (kop vertex kov isblack))
   (or
    (> vertex-lib-count 0)
    (let* ((neighbor-sets (mapcar (lambda (n)
				    (find-set n board))
				  neighbors))
	   (unique-neighbor-sets
	    (remove-duplicates neighbor-sets :test #'veq)))
      (reduce (lambda (a b) (or a b))	; find neighbor with libs
	      (mapcar (lambda (unique-neighbor-set)
			(if unique-neighbor-set
			    (let ((unique-neighbor-cell
				   (get-cell
				    unique-neighbor-set board)))
			      (apply
			       (if (eq isblack
				       (get-isblack
					unique-neighbor-cell))
				   #'< #'>=)
			       `(0
				 ,(- (get-lib-count
				      unique-neighbor-cell)
				     (count unique-neighbor-set
					    neighbor-sets
					    :test #'veq)))))))
		      unique-neighbor-sets))))))
		       

;;;; the rest of these are no longer being used.
;;;; this is an alternate way of making a move
;;;; that first checks if the move is valid.


(defun nupdate-opp-libs (unique-neighbor-set amount-to-update
			 isblack board)
  (let* ((unique-neighbor-cell (get-cell unique-neighbor-set
					 board))
	 (new-lib-count (- (get-lib-count unique-neighbor-cell)
			   amount-to-update)))
    (if (> new-lib-count 0)
	(setf (get-lib-count unique-neighbor-cell)		   
	      new-lib-count)
	(mapcar (lambda (deadv)
		  (nrestore-libs deadv isblack board)
		  (setf (get-cell deadv board) nil))
		`(,@(get-members unique-neighbor-cell)
		    ,unique-neighbor-set)))))
	     
	

(defun ndo-valid-move (vertex isblack board
		       vertex-lib-count neighbors)
  (create-set vertex isblack vertex-lib-count board)
  (let* ((neighbor-sets (mapcar #'find-set neighbors))
	 (unique-neighbor-sets
	  (remove-duplicates neighbor-sets :test #'veq)))
    (mapcar (lambda (unique-neighbor-set)
	      (if unique-neighbor-set
		  (let ((neighbor-set-cell
			 (get-cell unique-neighbor-set board)))
		    (if neighbor-set-cell
			(if (eq (get-isblack neighbor-set-cell)
				isblack)
			    (merge-sets vertex
					unique-neighbor-set
					board
					:filled-libs
					(count unique-neighbor-set
					       neighbor-sets
					       :test #'veq))
			    (nupdate-opp-libs unique-neighbor-set
					      (count
					       unique-neighbor-set
					       neighbor-sets
					       :test #'veq)
					      isblack
					      board))))))
	    unique-neighbor-sets)))
  
