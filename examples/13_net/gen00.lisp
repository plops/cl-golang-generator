(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/13_net")
  (write-source (format nil "~a/source/listen_test" *path*)
		`(do0
		  ;; A Woodbeck Network Programming with Go p.51
		  ;; run with go test listen_test.go
		  (package ch03)
		  (import net
			  testing)
		  (defun TestListener (te)
		    (declare (type *testing.T te))
		    (assign (ntuple listener
				  err)
			  (net.Listen (string "tcp")
				      (string "127.0.0.1:0")))
		    (unless (== err "nil")
		      (te.Fatal err))
		    (defer ((lambda ()
			      (setf _ (listener.Close)))))
		    (te.Logf (string "bound to %q")
			     (listener.Addr)))
		  ))
  (write-source (format nil "~a/source/dial_test" *path*)
		`(do0
		  ;; A Woodbeck Network Programming with Go p.53
		  (package ch03)
		  (import io
			  net
			  testing)
		  (defun TestDial (te)
		    (declare (type *testing.T te))

		    (do0
		     (assign (ntuple listener
				     err)
			     (net.Listen (string "tcp")
					 (string "127.0.0.1:0")))
		     (unless (== err "nil")
		       (te.Fatal err)))
		    (do0
		     (assign done (make (chan (curly struct)))))
		    (go ((lambda ()
			   (defer ((lambda ()
				     (<- done (curly "struct{}")))))
			   (for ()
				(do0
				 (assign (ntuple conn err)
					 (listener.Accept))
				 (unless (== err "nil")
				   (te.Log err)
				   return))
				(go ((lambda (c)
				       (declare (type net.Conn c))
				       (defer ((lambda ()
						 (c.Close)
						 (<- done (curly "struct{}")))))
				       (do0
					(assign buf (make "[]byte" 1024))
					(for ()
					     (do0 (assign (ntuple n err)
							  (c.Read buf))
						  (unless (== err "nil")
						    (unless (== err io.EOF)
						      (te.Error err))
						    return)
						  (te.Logf (string "received: %q")
							   (aref buf (slice "" n))))
					     ))) conn)))
			   )))
		  )
		  )))


