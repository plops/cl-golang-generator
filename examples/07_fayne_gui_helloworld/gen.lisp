(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/07_fayne_helloworld")
  (defparameter *code-file* "hello")
  
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  
  (let* ((code
	  `(do0
	    (package main)
	    "//  go get -u fyne.io/fyne"
	    "// the initial `go build hello.go` takes a while"
	    
	    (import fyne.io/fyne/app
		    fyne.io/fyne/widget)
	    (defun main ()
	      (assign a (app.New)
		      win (a.NewWindow (string "Hello World!"))
		      )
	      (win.SetContent
	       (widget.NewVBox
		(widget.NewLabel (string "Hello World!"))
		(widget.NewButton (string "Quit") (lambda ()
						    (a.Quit)))))
	      (win.ShowAndRun)
	      )))
	 )
    (write-source *source* code)
    ))


