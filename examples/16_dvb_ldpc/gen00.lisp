(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

;; call
;; go mod tidy
;; on first run


(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/16_dvb_ldpc"
	    (user-homedir-pathname)))
  (let ((file-count 0))

    (defun write-go (name code)
      (prog1
	  (write-source (format nil "~a/source/~2,'0d_~a" *path* file-count name)
			code)
	(incf file-count))))

  ;; overwriting this file makes it necessary to call setup01_tidy again
  (with-open-file (s (format nil "~a/source/go.mod" *path*)
		     :direction :output
		     :if-exists nil
		     :if-does-not-exist :create)
    (format s "module ldpc~%")
    (format s "go 1.18~%"))

  (write-go "main"
	    `(do0
	      (package main)
	      (import math/rand
		      )
	      (defun randomPoints (n)
		(declare (type int n)
			 (values plotter.XYs))
		(assign pts (make plotter.XYs n))
		(foreach (i (range pts))
		     (if (== 0 i)
			 (setf (dot (aref pts i)
				    X)
			       (rand.Float64))
			 (setf (dot (aref pts i)
				    X)
			       (+ (dot (aref pts (- i 1))
				       X)
				  (rand.Float64))))
		     (setf (dot (aref pts i)
				Y)
			   (+ (* 10 (rand.Float64))
			      (dot (aref pts i)
				   X))))
		(return pts))
	      (defun main ()
		(rand.Seed (int64 0))
		))
	    ))
