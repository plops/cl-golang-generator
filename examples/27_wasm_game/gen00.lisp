(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))


(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/27_wasm_game"
	    (user-homedir-pathname)))
  (defparameter *idx* "00")
  (defun lprint-init ()
    `(defun TimeNow ()
       (declare (values string))
       (return
	 (dot time
	      (Now)
	      (Format
	       (string
		"2006-01-02 15:04:05.000"))))))
  (defun lprint (&key (msg "") vars)
    "generate go code to print variables in log output"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v~^ ~}\\n"
		msg (loop for v in vars
			  collect
			  (emit-go :code v))))
      (TimeNow)
      ,@vars
      ))
  (defun tprint (&key (msg "") vars)
    "generate go code to print variables in log output with their types"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v (%T)~^ ~}\\n"
		msg vars))
      (TimeNow)
      ,@(loop for e in vars
	      appending
	      `(,e ,e))))
  (let ((err-nr 0))
    (defun panic (var-cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd)
	    var-cmd
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(unless (== ,err "nil")
		  ,(lprint :msg (substitute #\' #\" (emit-go :code cmd))
			   :vars `(,err))
		  (panic ,err)))
	    (incf err-nr)))))
    (defun panic0 (cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (unless (== ,err "nil")
		,(lprint :msg (substitute #\' #\" (emit-go :code cmd))
			 :vars `(,err))
		(panic ,err))
	      )
	  (incf err-nr)))))

  (let ((file-count 0))
    (defun write-go (&key name code folder)
      (prog1
	  (progn
     	    (let ((dir-name (format nil "~a/source00/~@[~a/~]"
				    *path*
				    folder)))
	      (format t "ensure dir-name exists ~a" dir-name)
	      (ensure-directories-exist
	       dir-name))
	    (unless folder
	     (with-open-file (s (format nil "~a/source00/go.mod"
					*path*
					)
				:direction :output
				:if-exists nil
				:if-does-not-exist :create)
	       (format s "module wasmgame~%")
	       (format s "go 1.19~%")))
	    (write-source
	     (format nil "~a/source00/~@[~a/~]g~2,'0d_~a"
		     *path* folder file-count name)
	     code))
	(incf file-count))))

  (let ((name "main")
	(folder nil))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package main)
      (import
       ;log
       ;time
       fmt
       github.com/hajimehoshi/ebiten/v2
       ("." wasmgame/cltimelog)
       (snake wasmgame/snake)
       )

      (defun main ()
	,(lprint :msg (format nil "~@[~a/~]~a" folder name))
	(assign game (snake.NewGame))
	(ebiten.SetWindowSize
	 snake.ScreenWidth
	 snake.ScreenHeight)
	(ebiten.SetWindowTitle
	 (string "snake"))
	,(panic0 `(ebiten.RunGame game))
	))))
  (let ((name "game")
	(folder "snake"))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package snake)
      (import
       image/color
       
       fmt
       ("." wasmgame/cltimelog)
       github.com/hajimehoshi/ebiten/v2
       ;github.com/hajimehoshi/ebiten/v2/ebitenutil
       )
      (const ScreenWidth 600
	     ScreenHeight 600
	     boardRows 20
	     boardCols 20)
      (let ((backgroundColor
	      (curly color.RGBA 50 100 50 50)))
	(defstruct0 Game
	  )
	(defun NewGame ()
	  (declare (values *Game))
	  ,(lprint :msg "NewGame")
	  (return (curly &Game)))
	(defmethod Layout ((g *Game)
			   outsideWidth
			   outsideHeight)
	  (declare (type int
			 outsideWidth
			 outsideHeight)
		   (values int int))
	  (return (ntuple ScreenWidth
			  ScreenHeight)))
	(defmethod Update ((g *Game)
			   )
	  (declare (values error))
	  (return "nil"))
	(defmethod Draw ((g *Game)
			 screen)
	  (declare (type *ebiten.Image screen))
	  (return (dot screen
		       (Fill backgroundColor))))

	))))

  (let ((name "cltimelog")
	(folder "cltimelog"))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package cltimelog)
      (import
       time
       ;fmt
       )
      ,(lprint-init)
      ))))
