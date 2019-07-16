(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/08_fayne_gui_fractal")
  (defparameter *code-file* "fractal")
  
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  ;; https://github.com/fyne-io/examples/blob/develop/fractal/main.go
  (let* ((code
	  `(do0
	    (package main)
	    "//  go get -u fyne.io/fyne"
	    "//  go get -u github.com/fyne-io/examples/img/icon"
	    
	     
	    (import
	     image/color
	     math
	     fyne.io/fyne
	     fyne.io/fyne/canvas
	     fyne.io/fyne/theme
	     fyne.io/fyne/app
	     fyne.io/fyne/widget
	     github.com/fyne-io/examples/img/icon)
	    (defstruct0 fractal
		(currIterations int)
	      (currScale float64)
	      (currX float64)
	      (currY float64)
	      (window fyne.Window)
	      (canvas fyne.CanvasObject))

	    (defmethod Layout ((f *fractal)
			       objects
			       size)
	      (declare (type "[]fyne.CanvasObject" objects)
		       (type fyne.Size size))
	      (f.canvas.Resize size))
	    (defmethod MinSize ((f *fractal)
			       objects
			       )
	      (declare (type "[]fyne.CanvasObject" objects)
		       (values fyne.Size))
	      (return (fyne.NewSize 320 240)))

	    (defmethod refresh ((f *fractal)
			       )
	      (if (<= 1.0 f.currScale)
		  (setf f.currIterations 100)
		  (setf f.currIterations
			(int (* 100 (+ 1
					(math.Pow
					 (math.Log10 (/ f.currScale))
					 1.25))))))
	      (dot (f.window.Canvas)
		   (Refresh f.canvas)))

	    
	    (defmethod scaleChannel ((f *fractal)
				     c start end)
	      (declare (type float64 c)
		       (type uint32 start end)
		       (values uint8))
	      (when (<= start end)
		(return (+
			 (uint8 start)
			 (uint8 (* c (float64
				      (uint8 (- end
						start))))))))
	      (return (+
		       (uint8 start)
		       (uint8 (* (- 1.0 c) (float64
					    (uint8 (- end
						      start))))))))

	    (defmethod scaleColor ((f *fractal)
				   c start end)
	      (declare (type float64 c)
		       (type color.Color start end)
		       (values color.Color))
	      (assign (ntuple r1 g1 b1 _) (start.RGBA)
		      (ntuple r2 g2 b2 _) (end.RGBA))
	      (return (curly color.RGBA
			     (f.scaleChannel c r1 r2)
			     (f.scaleChannel c g1 g2)
			     (f.scaleChannel c b1 b2)
			     "0xff")))

	    (defmethod mandelbrot ((f *fractal)
				    px py w h)
	      (declare (type int px py w h)
		       (values color.Color))
	      (assign drawScale (* 3.5 f.currScale)
		      aspect (/ (float64 h)
				(float64 w))
		      cRe (+ f.currX
			     (* drawScale
				(- (/ (float64 px)
				      (float64 w))
				   .5)))
		      cIm (+ (- f.currY)
			     (* drawScale
				(- (/ (float64 py)
				      (float64 w)) ;; FIXME h?
 				   (* aspect .5)))))
	      (let (i
		    x y xsq ysq)
		(declare (type int i)
			 (type float64 x y xsq ysq))
		(for ((setf i 0)
		      (and (< i f.currIterations)
			   (<= (+ xsq ysq) 4))
		      (incf i))
		     (assign xNew (+ cRe (float64 (- xsq ysq)))
			     )
		     (setf y (+ cIm (* 2 x y))
			   x xNew
			   xsq (* x x)
			   ysq (* y y)))
		(when (== i f.currIterations)
		  (return (theme.BackgroundColor)))
		(assign mu (/ (float64 i)
			      (float64 f.currIterations))
			c (math.Sin (* mu math.Pi .5)))
		(return (f.scaleColor c
				      (theme.PrimaryColor)
				      (theme.TextColor)))))

	    (defmethod fractalRune ((f *fractal)
				    r)
	      (declare (type rune r))
	      (when (== r (char +))
		(/= f.currScale 1.1))
	      (when (== r (char -))
		(*= f.currScale 1.1))
	      (f.refresh))

	    (defmethod fractalKey ((f *fractal)
				   ev)
	      (declare (type *fyne.KeyEvent ev))
	      (assign delta (* .2 f.currScale))
	      (ecase ev.Name
		(fyne.KeyUp (decf f.currY delta))
		(fyne.KeyDown (incf f.currY delta))
		(fyne.KeyLeft (incf f.currX delta))
		(fyne.KeyRight (decf f.currX delta)))
	      (f.refresh))

	    (defun Show (app)
	      (declare (type fyne.App app))
	      (assign window (app.NewWindow (string "fractal")))
	      (window.SetIcon icon.FractalBitmap)
	      (window.SetPadded false)
	      (assign fractal (curly &fractal :window window))
	      (setf fractal.canvas (canvas.NewRasterWithPixels fractal.mandelbrot)
		    fractal.currIterations 100
		    fractal.currScale 1.0
		    fractal.currX -.75
		    fractal.currY 0.0)
	      (window.SetContent (fyne.NewContainerWithLayout fractal fractal.canvas))
	      (dot (window.Canvas)
		   (SetOnTypedRune fractal.fractalRune))
	      (dot (window.Canvas)
		   (SetOnTypedKey fractal.fractalKey))
	      (window.Show)
	      )
	    
	    ;; https://github.com/fyne-io/examples/blob/develop/main.go
	    (defun main ()
	      (assign a (app.New)
		      win (a.NewWindow (string "Hello World!"))
		      )
	      (win.SetContent
	       (widget.NewVBox
		(widget.NewLabel (string "Hello World!"))
		(widget.NewButton (string "Quit") (lambda ()
						    (a.Quit)))))
	      (win.ShowAndRun)
	      )))
	 )
    (write-source *source* code)
    ))


