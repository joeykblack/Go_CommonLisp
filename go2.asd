;;;; go2.asd

(asdf:defsystem #:go2
  :serial t
  :components ((:file "package")
	       (:file "vars")
               (:file "go2")
	       (:file "disjoint-set")
	       (:file "array-util")
	       (:file "board") ; cleanup
	       (:file "binary")
	       (:file "score") ; todo: terr.
	       (:file "rave")
	       (:file "setup")
	       (:file "play")
	       (:file "debug")
	       (:file "timing")
	       (:file "rnd-player")
	       (:file "mc") 
	       (:file "uct")
	       (:file "agent")
	       (:file "test")
	       ))
;; todo:
;; in uct: large capture not found
;; may be result of captured not passing
;; try scoring without rnd moves
;; uct->rnd-game-win-p

;; commands todo:
;; fixed_handicap
;; place_free_handicap
;; set_free_handicap
;; time_settings
;; time_left
;; final_status_list
;; load_sgf
