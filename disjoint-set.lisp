;;;; Disjoint Set methods
;;;; 3/14/2012
;;;; joeykblack

(in-package :go2)




;; getters

(defmacro get-isblack (parent)
  `(car ,parent))

;; this is only pseudo liberties
;; it is accurate when it goes to 0
(defmacro get-lib-count (parent)
  `(cadr ,parent))

(defmacro get-rank (parent)
  `(caddr ,parent))

(defmacro get-members (parent)
  `(cdddr ,parent))

(defmacro get-col (vertex)
  `(car ,vertex))

(defmacro get-row (vertex)
  `(cdr ,vertex))

(defmacro get-cell (vertex board)
  `(aref ,board
	 (get-col ,vertex)
	 (get-row ,vertex)))


;; predicates

(defun veq (v1 v2)
  (and (and (consp v1) (consp v2))
       (eq (car v1) (car v2))
       (eq (cdr v1) (cdr v2))))

(defun parentp (cell)
  (and cell
       (listp (cdr cell))))


;; Find-Merge methods

(defun create-set (vertex isblack lib-count board)
  (setf (get-cell vertex board)
	(list isblack lib-count 0)))

(defun find-set (vertex board)
  (let ((cell (if vertex (get-cell vertex board))))
    (if cell
	(if (parentp cell)
	    (setf cell vertex)
	    (setf cell
		  (setf (get-cell vertex board)
			(find-set cell board))))
	cell)))

(defun merge-cells (parent old-parent
		    parent-cell old-parent-cell board
		    &key (filled-libs 0))
  (setf (get-cell old-parent board) parent)
  (setf (get-lib-count parent-cell)
	(+ (get-lib-count parent-cell)
	   (- (get-lib-count old-parent-cell)
	      filled-libs)))
  (setf (get-cell parent board)
	(append parent-cell
		(get-members old-parent-cell)
		`(,old-parent)))
  (get-lib-count parent-cell))

(defun merge-sets (x y board &key (filled-libs 0))
  (let* ((px (find-set x board))
	 (py (find-set y board))
	 (pxcell (get-cell px board))
	 (pycell (get-cell py board))
	 (pxrank (get-rank pxcell))
	 (pyrank (get-rank pycell)))
    (if (eq pxrank pyrank)
	(setf (get-rank pycell)
	      (1+ (get-rank pycell))))
    (if (> (get-rank pxcell)
	   (get-rank pycell))
	(merge-cells px py pxcell pycell board
		     :filled-libs filled-libs)
	(merge-cells py px pycell pxcell board
		     :filled-libs filled-libs))))

(defun remove-set (x board)
  (let* ((px (find-set x board))
	 (pxmembers (get-members (get-cell px board))))
    (setf (get-cell px board) nil)
    (apply (lambda (v)
	     (setf (get-cell v board) nil))
	   pxmembers)
    (append px pxmembers)))

