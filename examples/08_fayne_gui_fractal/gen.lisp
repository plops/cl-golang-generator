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
	    
	    
	    (import
	     image/color
	     math
	     fyne.io/fyne
	     fyne.io/fyne/canvas
	     fyne.io/fyne/theme
	     github.com/fyne.io/examples/img/icon)
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
	      (return (f.NewSize 320 240)))

	    (defmethod refresh ((f *fractal)
			       )
	      (if (<= 1.0 f.currScale)
		  (setf f.currIterations 100)
		  (setf f.currIterations
			(uint (* 100 (+ 1
					(math.Pow
					 (math.Log10 (/ currScale))
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


