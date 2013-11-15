;;;; go2.lisp
;;;; Implementation of Go using Disjoint Sets
;;;; 3/14/2012
;;;; joeykblack

(in-package #:go2)

;;; "go2" goes here. Hacks and glory await!







(defun main ()
  "Go entry point"
  (clear_board)
  (start-gtp-cmd-line))

(defun resume ()
  "Resume gtp-cmd-line without clear_board"
  (start-gtp-cmd-line))

(defun quick ()
  (clear_board)
  (play 'b 'a1)
  (play 'w 'a2)
  (play 'b 'a3)
  (start-gtp-cmd-line))

(defun game-read ()
  "Read command line into list"
  (read-from-string
   (concatenate 'string "(" (read-line) ")")))

(defun respond (result id)
  "format response"
  (if id
      (format t "~a~a ~a~&~%"
	      (car result)
	      id
	      (cadr result))
      (format t "~a ~a~&~%"
	      (car result)
	      (cadr result))))

(defun pack-it (sym)
  "convert input to package symbol"
  (intern (format nil "~A" sym) :go2))

(defun pack-member (sym lst)
  "is sym in list of package symbols"
  (member (pack-it sym) lst))


(defun start-gtp-cmd-line ()
  "Go Text Protocol command line interface"
  (do ((command (game-read) (game-read)))
      ((or (pack-member (car command) *quit*)
	   (pack-member (cadr command) *quit*)))
    (let* ((id (if (numberp (car command))
		   (car command)))
	   (command-name (if id
			     (cadr command)
			     (car command)))
	   (command-sym (pack-it command-name))
	   (args (if id
		     (cddr command)
		     (cdr command))))
      (respond
       (if (member command-sym *commands*)
	   (if args
	       (apply command-sym args)
	       (funcall command-sym))
	   `(? ,(format nil
			"Command ~a not found~%"
			command-name)))
       id))))

(defmacro defcommand (command-name args &body body)
  "defun that stores command in *commands*"
  `(progn (defun ,command-name ,args
	    ,@body)
	  (push ',command-name *commands*)))

(defmacro ok (&body body)
  "wraps result for success"
  `(list '= ,@body))

(defmacro fails (&body body)
  "wraps result for fail"
  `(list '? ,@body))

(defcommand list_commands ()
  (ok (format nil "~{~a~%~}" *commands*)))

(defcommand protocol_version ()
  (ok "2"))

(defcommand name ()
  (ok "jkb-lisp-go"))

(defcommand version ()
  (ok "0.1"))

(defcommand known_command (command-name)
  (ok (if (pack-member command-name *commands*)
	  "true" "false")))

(defun count-captured (captured)
  (length (apply #'append
		 (apply #'append captured))))


