(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/04_gopl_ch3_mandelbrot")
  (defparameter *code-file* "mandelbrot")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import image image/color image/png math/cmplx os)
	    (defun main ()
	      (const
	       xmin -2
	       ymin -2
	       xmax 2
	       ymax 2
	       width 1024
	       height 1024)
	      (assign
	       img (image.NewRGBA (image.Rect 0 0 width height)))
	      (dotimes (py height)
		(assign y (+ ymin (/ (float64   py)
				     (* height (- ymax ymin)))))
		(dotimes (px width)
		  (assign x (+ xmin (/ (float64 px)
				       (* width (- xmax xmin))))
			  z (complex x y))
		  (img.Set px py (mandelbrot z))))
	      (png.Encode os.Stdout img))
	    (defun mandelbrot (z)
	      (declare (type complex128 z)
		       (values color.Color &optional))
	      (const
	       iterations 200
	       contrast 15)
	      (let ((v 0))
		(declare (type complex128 v))
		(for ((:= n (uint8 0))
		      (< n iterations)
		      (incf n))
		     (setf v (+ z (* v v)))
		     (if (< 2 (cmplx.Abs v))
			 (return (curly color.Gray (- 255 (* contrast n))))))
		(return color.Black))))))
    (write-source *source* code)))


;; go build echo.go
