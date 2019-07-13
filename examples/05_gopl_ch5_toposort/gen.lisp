(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/05_gopl_ch5_toposort")
  (defparameter *code-file* "toposort")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import fmt sort)
	    (let ((prereqs (curly "map[string][]string"
				  :algorithms (string "data structures"))))
	     (lambda (items)
	       (declare (type "[]string" items)))))))
    (write-source *source* code)))


