(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/22_leet"
	    (user-homedir-pathname)))
  (defparameter *idx* "00")
  (defun lprint-init ()
    `(defun timeNow ()
       (declare (values string))
       (return
	 (dot time
	      (Now)
	      (Format
	       (string
		"2006-01-02 15:04:05.000"))))))
  (defun lprint (&key (msg "") vars)
    "generate go code to print variables in log output"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v~^ ~}\\n"
		msg (loop for v in vars
			  collect
			  (emit-go :code v))))
      (timeNow)
      ,@vars
      ))
  (defun tprint (&key (msg "") vars)
    "generate go code to print variables in log output with their types"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v (%T)~^ ~}\\n"
		msg vars))
      (timeNow)
      ,@(loop for e in vars
	      appending
	      `(,e ,e))))
  (let ((err-nr 0))
    (defun panic (var-cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd)
	    var-cmd
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(unless (== ,err "nil")
		  ,(lprint :msg (substitute #\' #\" (emit-go :code cmd))
			   :vars `(,err))
		  (panic ,err)))
	    (incf err-nr)))))
    (defun panic0 (cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (unless (== ,err "nil")
		,(lprint :msg (substitute #\' #\" (emit-go :code cmd))
			 :vars `(,err))
		(panic ,err))
	      )
	  (incf err-nr)))))

  (let ((file-count 0))
    (defun write-go (name code)
      (prog1
	  (progn
     	    (ensure-directories-exist
	     (format nil "~a/source~a/"
		     *path*
		     *idx*))
	    (with-open-file (s (format nil "~a/source~2,'0d/go.mod"
				       *path*
				       file-count)
			       :direction :output
			       :if-exists nil
			       :if-does-not-exist :create)
	      (format s "module main~%")
	      (format s "go 1.18~%"))
	    (write-source
	     (format nil "~a/source~2,'0d/~a"
		     *path* file-count name)
	     code)
	    )
	(incf file-count))))

  (let ((name "main674longestSubsequence"))
   (write-go
    name
    `(do0
      (package main)
      (import
					;io
					;bufio
       fmt
       time
					;io/ioutil
					;os
					;flag
					;encoding/binary
					;log
					;net
       ;github.com/samber/lo
					;github.com/samber/lo
					;github.com/schollz/progressbar/v3
					;runtime/pprof
       )
      ,(lprint-init)

      (defun traverse (nums)
	(declare (type []int nums)
		 (values int))
	(when (== 0 (len nums))
	  (return 0))

	(let ((res 1)
	      (length 1)
	      (old (aref nums 0)))
	  (foreach (el (range nums))
		   (if (< old el)
		       (incf length)
		       (do0
			(setf res (max res length)
			      length 1)))
		   (setf old el))
	  (return (max res length))))
      (defun max (a b)
	(declare (type int a b)
		 (values int))
	(if (< b a)
	    (return a))
	(return b))

      (defun main ()
	,(lprint :msg (format nil "~a" name))
	,(lprint :vars `((traverse (curly []int 1 3 5 4 7))))
       
	)))))
