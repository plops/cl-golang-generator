(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/06_gopl_ch7_eval")
  (defparameter *code-file* "eval")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    
	    ;(import fmt sort)
	    (deftype Var ()
	      string)
	    (deftype literal ()
	      float64)
	    (defstruct0 unary
		(op rune)
	      (x Expr))
	    (defstruct0 binary
		(op rune)
	      (x Expr)
	      (y Expr))
	    (defstruct0 call
		(fn string)
	      (args "[]Expr"))
	    (deftype Env ()
	      "map[Var]float64")
	    )))
    (write-source *source* code)))


