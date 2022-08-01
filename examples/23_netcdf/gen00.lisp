(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/23_netcdf"
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
	     code))
	(incf file-count))))

  (let ((name "netcdf"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
	math
	time
	"github.com/samber/lo"
	,@(loop for e in `(api cdf ;util
			       )
		collect
		(format nil "github.com/batchatco/go-native-netcdf/netcdf/~a" e))
	)
       ,(lprint-init)

       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 (assign fn (string "newdata.nc"))
	 ,(panic `(:var cw
		   :cmd (cdf.OpenWriter fn)))
	 ,@(loop for e in `((:name x :dims (x) :n 128)
			    (:name y :dims (y) :n 127)
			    (:name z :dims (z) :n 123)
			    (:name val :dims (x y z))
			    )
		 collect
		 (destructuring-bind (&key name dims n) e
		   `(do0
		     ,(if n
			  `(do0
			    ,(lprint :msg (format nil "define coordinate ~a" name))
			    (assign ,(format nil "n~a" name) ,n)
			    (assign ,name ("lo.Map[int,uint8]"
					 (lo.Range ,n)
					 (lambda (x _)
					   (declare (type int x _)
						    (values uint8))
					   (return (uint8 x))))))
			
			  `(do0
			    ,(lprint :msg (format nil "compute ~a" name))
			    (assign ,name (make [][][]float32 nx))
			    (dotimes (i nx)
			    (setf (aref ,name i) (make "[][]float32" ny))
			    (dotimes (j ny)
			      (setf (aref ,name i j) (make "[]float32" nz))
			      (dotimes (k nz)
				,@(loop for (ii nn) in `((i nx)
							 (j ny)
							 (k nz))
					collect
					`(assign ,(format nil "f~a2" ii)
					       (* (/ (float64 1)
						     (float64 (/ (* ,nn ,nn) 4)))
						  (float64
						      (* (- ,ii (/ ,nn 2))
							 (- ,ii (/ ,nn 2)))))))
				(assign r2 (+ fi2 fj2 fk2)
					r (math.Sqrt r2)
					s (/ (math.Sin r)
					     r))
				(setf (aref ,name i j k) (float32 s)))))))
		     ,(panic0 `(cw.AddVar (string ,name)
					  (curly api.Variable
						 ,name
						 (curly []string
							,@(loop for f in dims
								collect `(string ,f)))
						 "nil")))))
		 )
	 (defer ((lambda ()
		   ,(lprint :msg "close" :vars `(fn))
		   ,(panic0 `(cw.Close)))))
	 )))))
