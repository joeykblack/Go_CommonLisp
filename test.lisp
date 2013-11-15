;;;; Test cases
;;;; joeykblack
;;;; 4/6/2012

(in-package #:go2)


(defun test-uct ()
  (boardsize 9)
  (clear_board)
  
  (play 'b 'a1)
  (play 'b 'a2)
  (play 'b 'a3)
  (play 'b 'b1)
  (play 'b 'b2)
  (play 'b 'b3)
  (play 'b 'c1)
  (play 'b 'c2)
  (play 'b 'c3)
  
  (play 'w 'a4)
  (play 'w 'b4)
  (play 'w 'c4)
  (play 'w 'd1)
  (play 'w 'd2)

  (veq '(3 . 2)
       (uct-reg-genmove 5000 nil nil
			*board* *size* *kov*
			*captured-black* *captured-white*
			0 0.8 10)))
  
