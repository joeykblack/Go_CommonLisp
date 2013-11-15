;;;; Go setup commands
;;;; joeykblack
;;;; 2/27/12

(in-package #:go2)

(defcommand clear_board ()
  (setf *board* (make-array `(,*size* ,*size*) :initial-element nil))
  (setf *history* nil)
  (setf *captured-black* nil)
  (setf *captured-white* nil)
  (setf *isblack-turn* t)
  (setf *kov* nil)
  (clear-rave)
  (ok ""))

(defcommand boardsize (size)
  (setf *size* size)
  (clear_board)
  (ok ""))

(defcommand komi (new-komi)
  (setf *komi* new-komi)
  (ok ""))

;(defcommand fixed_handicap ()
;  (fails "not implemented"))

;(defcommand place_free_handicap ()
;  (fails "not implemented"))

;(defcommand set_free_handicap ()
;    (fails "not implemented"))


  
