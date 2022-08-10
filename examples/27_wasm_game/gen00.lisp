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
      )))
  
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
	  (dot screen
	       (Fill backgroundColor)))

	))))

  (let ((name "food")
	(folder "snake"))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package snake)
      (defstruct0 Food
	  (x int)
	(y int))
      (defun NewFood (x y)
	(declare (values *Food)
		 (type int x y))
	(return (curly &Food
		       :x x
		       :y y))))))

  (let ((name "input")
	(folder "snake"))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package snake)
      (import
       github.com/hajimehoshi/ebiten/v2
       github.com/hajimehoshi/ebiten/inpututil)
      (defstruct0 Input
	  )
      (defun NewInput ()
	(declare (values *Input))
	(return (curly &Input)))
      (defmethod Dir ((i *Input)
		      )
	(declare (values ebiten.Key bool))
	,@(loop for e in `(KeyArrowUp
			   KeyArrowLeft
			   KeyArrowRight
			   KeyArrowDown
			   )
		collect
		`(when (inpututil.IsKeyJustPressed
			(dot ebiten ,e))
		   (return (ntuple (dot ebiten
					,e)
				   true))))
	(return (ntuple 0 false))
	))))

  (let ((name "input")
	(folder "snake"))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package snake)
      (import
       github.com/hajimehoshi/ebiten/v2
       github.com/hajimehoshi/ebiten/inpututil)
      (defstruct0 Input
	  )
      (defun NewInput ()
	(declare (values *Input))
	(return (curly &Input)))
      (defmethod Dir ((i *Input)
		      )
	(declare (values ebiten.Key bool))
	,@(loop for e in `(KeyArrowUp
			   KeyArrowLeft
			   KeyArrowRight
			   KeyArrowDown
			   )
		collect
		`(when (inpututil.IsKeyJustPressed
			(dot ebiten ,e))
		   (return (ntuple (dot ebiten
					,e)
				   true))))
	(return (ntuple 0 false))
	))))

  (let ((name "snake")
	(folder "snake"))
   (write-go
    :name name
    :folder folder
    :code
    `(do0
      (package snake)
      (import
       github.com/hajimehoshi/ebiten/v2)
      (defstruct0 Coord
	  (x int)
	(y int))
      (defstruct0 Snake
	  (body []Coord)
	(direction ebiten.Key)
	(justAte bool))
      (defun NewSnake (body direction)
	(declare (type []Coord body)
		 (type ebiten.Key direction))
	(declare (values *Snake))
	(return (curly &Snake
		       :body body
		       :direction direction)))
      (defmethod Head ((s *Snake)
		      )
	(declare (values Coord))
	(return (dot s
		     (aref body
			   (- (len s.body)
			      1)))))
      (defmethod ChangeDirection ((s *Snake)
				  newDir)
	(declare (type ebiten.Key newDir))
	(assign
	 opposites
	 (curly "map[ebiten.Key]ebiten.Key"
		,@(loop for (e f)
			  in `((Up Down)
			       (Right Left)
			       (Down Up)
			       (Left Right))
			appending
			`(,(make-keyword
			    (format
			     nil
			     "ebiten.KeyArrow~a" e))
			  ,(format
			     nil
			     "ebiten.KeyArrow~a" f)
			  ))))
	(comments "change to opposite not allowed")
	(assign (ntuple o ok)
		(aref opposites
		      newDir))
	(when (logand ok
		      (!= o s.direction))
	  (setf s.direction newDir)))
      (defmethod HeadHits ((s *Snake)
			   x y)
	(declare (type int x y)
		 (values bool))
	(assign h (s.Head))
	(return (logand (== h.x x)
			(== h.y y)))
	)
      (defmethod HeadHitsBody ((s *Snake)
			       )
	(declare 
		 (values bool))
	(assign h (s.Head))
	(assign
	 bodyWithoutHead
	 (aref s.body (slice ""
			     (- (len s.body)
				1))))
	(foreach ((ntuple _ b)
		  (range bodyWithoutHead))
		 (when
		     (logand (== b.x h.x)
			     (== b.y h.y))
		   (return true)))
	(return false))
      (defmethod Move ((s *Snake))
	(assign h (s.Head))
	(assign
	 newHead
	 (curly Coord
		:x h.x
		:y h.y))
	(switch s.direction
	  (ebiten.KeyArrowUp
	   (decf newHead.x))
	  (ebiten.KeyArrowRight
	   (incf newHead.y))
	  (ebiten.KeyArrowDown
	   (incf newHead.x))
	  (ebiten.KeyArrowLeft
	   (decf newHead.y)))
	(if s.justAte
	    (setf s.body (append s.body newHead)
		  s.justAte false)
	    (setf s.body (append
			  (aref s.body (slice 1 ""))
			  newHead))))
      )))

  )
