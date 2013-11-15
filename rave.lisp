;;;; RAVE
;;;; joeykblack
;;;; 4/6/2012

(in-package #:go2)


;; cell = (wins . visits)
(defparameter RAVE (make-array `(,*size* ,*size*) :initial-element nil))

(defun clear-rave ()
  (setf RAVE
	(make-array `(,*size* ,*size*) :initial-element nil)))

(defun get-rave () RAVE)

(defun update-wins (vertex iswin)
  (if (not (get-cell vertex RAVE))
      (setf (get-cell vertex RAVE) (cons 0 0)))
  (setf (cdr (get-cell vertex RAVE))
	(1+ (cdr (get-cell vertex RAVE))))
  (if iswin
      (setf (car (get-cell vertex RAVE))
	    (1+ (car (get-cell vertex RAVE)))))
  (if iswin 1 0))

(defun get-rave-value (vertex)
  (let ((cell (get-cell vertex RAVE)))
    (if cell
	(/ (car cell)
	   (cdr cell))
	0)))
  
(defun add-rave (uct-value vertex)
  (if vertex
      (let ((cell (get-cell vertex RAVE)))
	(+ uct-value
	   (if cell
	       (* *RAVE-MOD*
		  (/ (car cell)
		     (cdr cell)))
	       0)))
      0))

    
