(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/17_xml"
	    (user-homedir-pathname)))
  (defparameter *idx* "01")
  (defun lprint (&key (msg "") vars)
    `(fmt.Printf (string ,(format nil "%v ~a ~{~a=%v~^ ~}\\n" msg vars))
		 
		 (dot time
		      (Now)
		      (Format
		       #+nil time.StampMilli
		       #+nil (string "Jan _2 15:04:05.000")
		       (string
			"2006-01-02 15:04:05.000")))
		 
		 ,@vars
		 ))
  (let ((file-count 0))
    (defun write-go (name code)
      (prog1
	  (write-source (format nil "~a/source~a/~2,'0d_~a"
				*path* *idx* file-count name)
			code)
	(incf file-count))))

  (ensure-directories-exist (format nil "~a/source~a/"
				    *path*
				    *idx*))
  
  ;; overwriting this file makes it necessary to call setup01_tidy again
  (with-open-file (s (format nil "~a/source~a/go.mod"
			     *path*
			     *idx*)
		     :direction :output
		     :if-exists nil
		     :if-does-not-exist :create)
    (format s "module edgar~%")
    (format s "go 1.18~%"))
  (write-go
   "main"
   `(do0
     (package main)
     (import fmt
	     time
	     
	     )
     
     (defun main ()
       ,(lprint :msg "main")

       #+nil
       (unless (== err "nil")
	 (panic err))
       ,(lprint :msg "" :vars `())
       )
     )))
