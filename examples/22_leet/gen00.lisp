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
	     (format nil "~a/source~2,'0d/"
		     *path*
		     file-count))
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
	fmt
	time
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
	   (foreach ((ntuple idx el) (range (aref nums (slice 1 ""))))
		    (if (< old el)
			(incf length)
			(do0
			 ,(lprint :msg "sequence broken" :vars `(old el idx res length))
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
	 ,(lprint :msg "should be 3" :vars `((traverse (curly []int 1 3 5 4 7))))
	 ,(lprint :msg "should be 1" :vars `((traverse (curly []int  2 2 2 2 2
							      ))))

	 ))))
  (let ((name "main946validateStack"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
	time
	)
       ,(lprint-init)

       (defun validate (pushed popped)
	 (declare (type []int pushed popped)
		  (values bool))


	 (let ((stack (curly []int))
	       (j 0)
	       (N (len pushed)))
	   (foreach ((ntuple _ el) (range pushed))
		    (setf stack (append stack el))
		    (while (and
			    (!= (len stack) 0)
			    (< j N)
			    (== (aref stack
				      (- (len stack)
					 1))
				(aref popped j)))
		      (setf stack (aref stack (slice 0 (- (len stack)
							  1))))
		      (incf j)))
	   (return (== j N))))


       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 ,(lprint :msg "should be true" :vars `((validate (curly []int 1 2 3 4 5)
							  (curly []int 5 4 3 2 1))))
	 ,(lprint :msg "should be false" :vars `((validate (curly []int 1 2 3 4 5)
							   (curly []int 4 3 5 1 2)))))))))
