(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/00_gopl_ch2_echo4")
  (defparameter *code-file* "echo")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import flag fmt string)
	    (defun main ()
	      (flag.Parse)
	      (fmt.Print
	       (strings.Join
		(flag.Args)
		*sep))
	      (if (not *n)
		  (fmt.Println))))))
    (write-source *source* code)))
