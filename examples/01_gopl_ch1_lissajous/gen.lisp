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
