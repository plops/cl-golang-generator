(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

;; call
;; go mod tidy
;; on first run


(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/15_gonum_plot"
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
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (format s "module gio.test~%")
    (format s "go 1.18~%"))

  (write-go "main"
	    `(do0
	      (package main)
	      (import math/rand
		      gonum.org/v1/plot
		      gonum.org/v1/plot/plotter
		      gonum.org/v1/plot/plotutil
		      gonum.org/v1/plot/vg
		      )
	      (defun main ()
		(rand.Seed (int64 0))
		(assign p (plot.New))
		(setf p.Title.Text (string "plotutil example")
		      p.X.Label.Text (string "X")
		      p.Y.Label.Text (string "Y"))))
	    ))
