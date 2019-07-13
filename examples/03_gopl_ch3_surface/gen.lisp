(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/03_gopl_ch3_surface")
  (defparameter *code-file* "surface")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import fmt math)
	    (const
	     width 600
	     height 320
	     cells 100
	     xyrange 30
	     xyscale (/ width (* 2 xyrange))
	     zscale (* .4 height)
	     angle (/ math.Pi 6)
	     sin30 (math.Sin angle)
	     cos30 (math.Cos angle))
	    
	    (defun main ()
	      (fmt.Printf
	       (string
		"<svg xmlns='http://www.w3.org/200/svg' style='stroke: grey; fill:white; stroke-width: 0.7' width='%d' height='%d'" width height))
	      (dotimes (i cells)
		(dotimes (j cells)
		  ,@(loop for e in `((a 1 0)
				     (b 0 0)
				     (c 0 1)
				     (d 1 1))
		       collect
			 (destructuring-bind (name x y) e
			  `(assign
			    (ntuple
			     ,(format nil "~ax" name)
			     ,(format nil "~ay" name))
			    (corner (+ i ,x) (+ j ,y))
			    )))
		  (fmt.Printf (string "<polygon points='%g,%g %g,%g %g,%g %g,%g'/>\\n")
			      ax ay bx by cx cy dx dy)))
	      (fmt.Println (string "</svg>")))
	    (defun corner (i j)
	      (declare (type int i j)
		       (values float64 float64 &optional))
	      (assign
	       x (* xyrange (- (/ (float64 i) cells) .5))
	       y (* xyrange (- (/ (float64 j) cells) .5))
	       z (f x y)
	       sx (+ (* .5 width)
		     (* (- x y)
			cos30
			xyscale))
	       sy (+ (* .5 width)
		     (* (+ x y)
			sin30
			xyscale)
		     (* -1 z zscale)))
	      (return (ntuple sx sy)))
	    (defun f (x y)
	      (declare (type float64 x y)
		       (values float64 &optional))
	      (assign
	       r (math.Hypot x y))
	      (return (/ (math.Sin r) r))))))
    (write-source *source* code)))


;; go build echo.go