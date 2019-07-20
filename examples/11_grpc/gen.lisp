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
		    11_grpc/source/proto
		    net
		    google.golang.org/grpc
		    google.golang.org/grpc/reflection)
	    (defstruct0 server)
	    
	    (defun main ()
	      (assign (ntuple listener err) (net.Listen (string "tcp")
							(string ":4040")))
	      (unless (== err "nil")
		(panic err))

	      (assign srv (grpc.NewServer))
	      (proto.RegisterAddServiceServer srv (curly &server))
	      (reflection.Register srv)


	      (progn
		(assign e (srv.Serve listener))
		(unless (== e "nil")
		  (panic e)))
	      
	      )
	    
	    (defmethod Add ((s *server)
			    ctx request)
	      (declare (type context.Context ctx)
		       (type *proto.Request request)
		       (values *proto.Response error))
	      (assign a (request.GetA)
		      b (request.GetB)
		      result (+ a b))
	      (return (ntuple (curly &proto.Response :Result result)
			      "nil")))

	    (defmethod Multiply ((s *server)
			    ctx request)
	      (declare (type context.Context ctx)
		       (type *proto.Request request)
		       (values *proto.Response error))
	      (assign a (request.GetA)
		      b (request.GetB)
		      result (* a b))
	      (return (ntuple (curly &proto.Response :Result result)
			      "nil")))
	    )))
    (write-source *source-server* code-server)))


