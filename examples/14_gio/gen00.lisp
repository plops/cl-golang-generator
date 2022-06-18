(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/14_gio")
  (let ((file-count 0))

    (defun write-go (name code)
      (prog1
	  (write-source (format nil "~a/source/~2,'0d_~a" *path* file-count name)
			code)
	(incf file-count))))

  (with-open-file (s (format nil "~a/source/go.mod" *path*)
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (format s "module gio.test~%")
    (format s "go 1.18~%"))

  (write-go "main"
	    `(do0
	      (package main)
	      (import image/color
		      log
		      os
		      gioui.org/app
		      gioui.org/font/gofont
		      gioui.org/io/system
		      gioui.org/layout
		      gioui.org/op
		      gioui.org/text
		      gioui.org/widget/material
		      )
	      (defun main ()
		(go ((lambda ()
		       (assign w (app.NewWindow)
			       err (run w))
		       (unless (== err "nil")
			 (log.Fatal err))
		       (os.Exit 0))))
		(app.Main))
	      (defun run (w)
		(declare (type *app.Window w)
			 (values error))
		(assign th (material.NewTheme (gofont.Collection)))
		"var ops op.Ops"
		(for ()
		     (assign ev (<- (w.Events))
			     e (dot ev "(type)"))
		     (case e
		       (system.DestroyEvent
			(return e.Err))
		       (system.FrameEvent
			(assign gtx (layout.NewCotnext &ops e)))
		       )
		     )))
	    )

  #+nil ((write-go "listen_test"
		   `(do0
		     (comments  "A Woodbeck Network Programming with Go p.51"
				"run with `go test listen_test.go`")
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
	 (write-go "dial_test"
		   `(do0
		     (comments  "A Woodbeck Network Programming with Go p.53"
				"run with `go test dial_test.go`")
		     (package ch03)
		     (import io
			     net
			     testing)
		     (defun TestDial (te)
		       (declare (type *testing.T te))

		       (do0
			(comments "create listener on random port")
			(assign (ntuple listener
					err)
				(net.Listen (string "tcp")
					    (string "127.0.0.1:0")))
			(unless (== err "nil")
			  (te.Fatal err)))
		       (do0
			(comments "spin off listener in goroutine")
			(assign done (make (chan (curly struct))))
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
							(comments "Read returns io.EOF when FIN packet is received")
							return)
						      (te.Logf (string "received: %q")
							       (aref buf (slice "" n))))
						 ))) conn)))
			       ))))
		       (do0
			(comments "connect to the server (that runs in its own goroutine)")
			(assign (ntuple conn err)
				(net.Dial (string "tcp")
					  (dot listener
					       (Addr)
					       (String))))
			(unless (== err "nil")
			  (te.Fatal err)))
		       (do0
			(comments "graceful termination from client side")
			(conn.Close)
			<-done
			(comments "Accept will return and stop the goroutine")
			(listener.Close)
			<-done)))

		   (write-go "dial_timeout_test"
			     `(do0
			       (comments  "A Woodbeck Network Programming with Go p.53"
					  "run with `go test dial_test.go`")
			       (package ch03)
			       (import net syscall testing time)
			       (defun DialTimeout (network address timeout)
				 (declare (type "" network)
					  (type string address)
					  (type time.Duration timeout)
					  (values net.Conn error))
				 (assign d (curly net.Dialer
						  :Control (lambda (_0 addr _)
							     (declare (type syscall.RawConn _)
								      (type string addr)
								      (type "" _0)
								      (values error))
							     (return (curly &net.DNSError
									    :Err (string "connection timed out")
									    :Name addr
									    :Server (string "127.0.0.1")
									    :IsTimeout true
									    :IsTemporary true)))
						  :Timeout timeout))
				 (return (d.Dial network address)))
			       (defun TestDialTimeout (te)
				 (declare (type *testing.T te))
				 (do0 (assign (ntuple c
						      err)
					      (DialTimeout (string "tcp")
							   (string "10.0.0.1:http")
							   (* 5 time.Second)))
				      (when (== err "nil")
					(c.Close)
					(te.Fatal (string "connection did not time out"))))
				 (do0
				  (assign (ntuple nErr ok)
					  (err. net.Error))
				  (unless ok
				    (te.Fatal err))
				  (unless (nErr.Timeout)
				    (te.Fatal (string "error is not a timeout"))))))))))
