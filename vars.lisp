;;;; All global variables
;;;; joeykblack
;;;; 4/4/2012


(in-package :go2)



;; elements are:
;; refference to parent: (c . r)
;; or if parent: (color lib-count rank members of group)
(defparameter *quit* '(quit q exit))
(defparameter *commands* '(quit))

(defparameter *size* 9)
(defparameter *komi* 7.5)
(defparameter *history* nil)
(defparameter *captured-black* nil)
(defparameter *captured-white* nil)
(defparameter *isblack-turn* t)
(defparameter *kov* nil)

(defparameter *board* (make-array `(,*size* ,*size*) :initial-element nil))


(defparameter *agent-type* 'uct)

; for rnd games
(defparameter *max-depth* 30) ; old
(defparameter *percent-full* 0.8) ; used for uct

; mc values
(defparameter *num-rnd-games* 100) 

; UCT values
(defparameter *UCTK* 0.5) ; constant
(defparameter *num-visits-root* nil) ; if nil time-limit is used
(defparameter *time-limit* 10)
(defparameter *expand-point* 10)

; Rave
(defparameter *RAVE-MOD* 0)
