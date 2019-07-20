(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/11_grpc")
  (defparameter *code-file* "main")
  
  (defparameter *source-server* (format nil "~a/source/server/~a" *path*  *code-file*))
  (defparameter *source-client* (format nil "~a/source/client/~a" *path*  *code-file*))
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
		  (panic e))))
	    
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
	    ))
	 (code-client
	  `(do0
	    (package main)
	    (import google.golang.org/grpc
		    github.com/gin-gonic/gin
		    strconv
		    net/http)
	    
	    (defun main ()
	      (assign (ntuple conn err)
		      (grpc.Dial (string "localhost:4040")
				 ;; insecure, no https
				 (grpc.WithInsecure))
		      )
	      (unless (== err "nil")
		(panic err))

	      (assign client (proto.NewAddServiceClient conn)
		      g (gin.Default))

	      (g.GET (string "/add/:a/:b")
		     (lambda (ctx)
		       (declare (type *gin.Context ctx))

		       (do0
			(assign (ntuple a err)
				(strconv.ParseUint (ctx.Param (string "a"))
						   10 ;; base
						   64 ;; size
						   ))

			(unless (== err "nil")
			  (ctx.JSON http.StatusBadRequest
				    (cast gin.H (dict 
				      ((string "error")
				       (string "invalid parameter a")))))
			  return))

		       (do0
			(assign (ntuple b err)
				(strconv.ParseUint (ctx.Param (string "b"))
						   10 ;; base
						   64 ;; size
						   ))

			(unless (== err "nil")
			  (ctx.JSON http.StatusBadRequest
				    (cast gin.H
					  (dict  
				      ((string "error")
				       (string "invalid parameter b")))))
			  return))
		         (assign req (curly &proto.Request :A (int64 a)
					    :B (int64 b)))
			 (progn
			   (assign (ntuple response err)
				   (client.Add ctx req))
			   (if (== err "nil")
			       (ctx.JSON http.StatusOK
					 (cast gin.H
					       (dict ((string "result")
						      (fmt.Sprint response.Result)))))
			       (ctx.JSON http.StatusInternalServerError
					 (cast gin.H
					       (dict  
						((string "error")
						 (err.Error)))))))
			 
		       ))
	      (g.GET (string "/mult/:a/:b")
		     (lambda (ctx)
		       (declare (type *gin.Context ctx))

		       (do0
			(assign (ntuple a err)
				(strconv.ParseUint (ctx.Param (string "a"))
						   10 ;; base
						   64 ;; size
						   ))

			(unless (== err "nil")
			  (ctx.JSON http.StatusBadRequest
				    (cast gin.H (dict 
				      ((string "error")
				       (string "invalid parameter a")))))
			  return))

		       (do0
			(assign (ntuple b err)
				(strconv.ParseUint (ctx.Param (string "b"))
						   10 ;; base
						   64 ;; size
						   ))

			(unless (== err "nil")
			  (ctx.JSON http.StatusBadRequest
				    (cast gin.H
					  (dict  
				      ((string "error")
				       (string "invalid parameter b")))))
			  return))
		         (assign req (curly &proto.Request :A (int64 a)
					    :B (int64 b)))
			 (progn
			   (assign (ntuple response err)
				   (client.Multiply ctx req))
			   (if (== err "nil")
			       (ctx.JSON http.StatusOK
					 (cast gin.H
					       (dict ((string "result")
						      (fmt.Sprint response.Result)))))
			       (ctx.JSON http.StatusInternalServerError
					 (cast gin.H
					       (dict  
						((string "error")
						 (err.Error)))))))
		       ))
	    )
	    )))

    (write-source *source-server* code-server)
    (write-source *source-client* code-client)))


