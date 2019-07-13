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
	     cells 30
	     xyrange 30
	     xyscale (/ width (* 2 xyrange))
	     zscale (* .4 height)
	     angle (/ math.Pi 6)
	     )
	    (let ((sin30 (math.Sin angle))
		  (cos30 (math.Cos angle)))
	      (defun main ()
		(fmt.Printf
		 (string
		  "<svg xmlns='http://www.w3.org/200/svg' style='stroke: grey; fill:white; stroke-width: 0.7' width='%d' height='%d'>")  width height)
		(dotimes (i cells)
		  (dotimes (j cells)
		    ,(let ((l `((a 1 0)
				(b 0 0)
				(c 0 1)
				(d 1 1))))
		       `(do0
			 ,@(loop for (name x y) in l collect
				`(assign
				  (ntuple
				   ,(format nil "~ax" name)
				   ,(format nil "~ay" name))
				  (corner (+ i ,x) (+ j ,y))
				  ))
			 (fmt.Printf (string ,(format nil "<polygon points='~{~a~^ ~}'/>\\n"
						      (loop for e in l collect "%6.3f,%6.3f")))
				     ,@(loop for (name x y) in l appending
					    (list
					     (format nil "~ax" name)
					     (format nil "~ay" name))))))))
		(fmt.Println (string "</svg>"))))
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
	      (if (!= r 0.0)
		  (return (/ (math.Sin r) r))
		  (return 1.0))))))
    (write-source *source* code)))


;; go build echo.go
