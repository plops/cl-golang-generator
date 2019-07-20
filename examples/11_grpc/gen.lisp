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
	    (import context
		    source/proto)
	    (defstruct0 server)
	    
	    (defun main ()
	      )

	    (defmethod Add ((s *server)
			    ctx proto)
	      (declare (type context.Context ctx)
		       (type *proto.Request proto)
		       (values *proto.Response error)))
	    )))
    (write-source *source-server* code-server)))


