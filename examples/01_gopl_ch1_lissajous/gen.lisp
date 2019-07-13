(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/01_gopl_ch1_lissajous")
  (defparameter *code-file* "lissajous")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import image image/color image/gif io math math/rand os)
	    (let ((palette (curly "[]color.Color" color.White color.Black)))
	      (const witeIndex 0
		     blackIndex 1)
	      (defun main ()
		(lissajous os.Stdout))
	      (defun lissajous (out)
		(declare (type io.Writer out))
		
		(const cycles 5
		       res .001
		       size 100
		       nframes 64
		       delay 8)
		(:= freq (* 3.0 (rand.Float64)))
		)
	      (defun main ()
		(flag.Parse)
		(fmt.Print
		 (strings.Join
		  (flag.Args)
		  *sep))
		(if (not *n)
		    (fmt.Println)))))))
    (write-source *source* code)))


;; go build echo.go
