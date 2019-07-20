(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/11_grpc")
  (defparameter *code-file* "main")
  
  (defparameter *source-server* (format nil "~a/source/server/~a" *path*  *code-file*))
  (let* ((code-server
	  `(do0
	    (package main)
	    (import sync)

	    (deftype Func ()
	      (defun-declaration func (key)
		(declare (type string key)
			 (values "interface{}"
				 error))))

	    (defstruct0 result
		(value "interface{}")
	      (err error)))))
    (write-source *source-server* code-server)))


