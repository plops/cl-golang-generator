(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/07_fayne_helloworld")
  (defparameter *code-file* "hello")
  
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  
  (let* ((code
	  `(do0
	    (package eval)
	    
	    (import fyne.io/fyne/app
		    fyne.io/fyne/widget)
	    ))
	 )
    (write-source *source* code)
    ))


