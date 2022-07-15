(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

;; call
;; go mod tidy
;; on first run


(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/17_edgar"
	    (user-homedir-pathname)))
  (defparameter *idx* "00")
  (defun lprint (&key (msg "") vars)
    `(fmt.Printf (string ,(format nil "%v ~a ~{~a=%v~^ ~}\\n" msg vars))
		 
		 (dot time
		      (Now)
		      #+nil (Format
		       (string
			"2017-09-07 17:06:04"
					;"2017-09-07 17:06:04.000000"
			)))
		 
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
	     github.com/piquette/edgr/core
	     )
     
     (defun main ()
       ,(lprint :msg "main")
       )
     )))
