(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/33_netcdf_tensor"
	    (user-homedir-pathname)))
  (defparameter *idx* "00")
  (defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))
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
	;math
	time
	runtime/debug
	"github.com/samber/lo"
	,@(loop for e in `(api cdf ;util
			       )
		collect
		(format nil "github.com/batchatco/go-native-netcdf/netcdf/~a" e))
	gorgonia.org/tensor
	)
       ,(lprint-init)

       (defun reportDependencies ()
	 (do0
	  (assign (ntuple bi ok) (debug.ReadBuildInfo))
	  (if ok
	      (do0
	       (foreach ((ntuple _ dep)
			 (range bi.Deps))
			,(lprint  :vars `(dep)))
	       )
	      (do0
	       ,(lprint :msg "failed to read build info")))))

       (defun reportGenerator ()
	 (assign
	  code_git_version
	  (string ,(let ((str (with-output-to-string (s)
				(sb-ext:run-program "/usr/bin/git" (list "rev-parse" "HEAD") :output s))))
		     (subseq str 0 (1- (length str)))))
	  code_repository (string ,(format nil "https://github.com/plops/cl-golang-generator/tree/master/examples/33_netcdf_tensor"))
	  code_generation_time
	  (string ,(multiple-value-bind
			 (second minute hour date month year day-of-week dst-p tz)
		       (get-decoded-time)
		     (declare (ignorable dst-p))
		     (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d-~2,'0d-~2,'0d (GMT~@d)"
			     hour
			     minute
			     second
			     (nth day-of-week *day-names*)
			     year
			     month
			     date
			     (- tz)))))
	 ,@(loop for e in `(code_git_version
			    code_repository
			    code_generation_time)
		 collect
		 `(do0
		   ,(lprint :vars `(,e)))))

       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 (reportGenerator)
	 (reportDependencies)

	 (do0
	  (comments "create 2,3,4 3-Tensor of float32, column-major backing")
	  (assign b (tensor.New (tensor.WithBacking
				 (tensor.Range tensor.Float32
					       0 24))
				(tensor.WithShape 2 3 4)
				(tensor.AsFortran "nil")))
	  ,(lprint :vars `(b)))


	 (assign fn (string "newdata.nc"))
	 ,(panic `(:var cw
			:cmd (cdf.OpenWriter fn)))
	 ,@(loop for e in `((:name x :dims (x) :n 2)
			    (:name y :dims (y) :n 3)
			    (:name z :dims (z) :n 4)
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
				  ,(panic `(:var x
						 :cmd (b.At i j k)))
				  ,(tprint :vars `(x))
				  #+nil (setf (aref ,name i j k)
					x
					))))))
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
