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
		  )))


