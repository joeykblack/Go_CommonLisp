;;;; Array utils
;;;; joeykblack
;;;; 3/21/2012

(in-package #:go2)


(defun copy-array-trees (array)
  "This does a deep copy on all elements"
  (let* ((dims (array-dimensions array))
	 (dimsq (reduce #'* dims))
	 (new-array (make-array dims)))
    (map-into (make-array dimsq
			  :displaced-to new-array)
	      #'copy-tree
	      (make-array dimsq
			  :displaced-to array))
    new-array))
    

