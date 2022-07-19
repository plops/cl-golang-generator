(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/19_binary"
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
		msg vars))
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
	  (write-source
	   (format nil "~a/source~a/~2,'0d_~a"
		   *path* *idx* file-count name)
	   code)
	(incf file-count))))

  (ensure-directories-exist
   (format nil "~a/source~a/"
	   *path*
	   *idx*))
  
  ;; overwriting this file makes it necessary to call setup01_tidy again
  (with-open-file (s (format nil "~a/source~a/go.mod"
			     *path*
			     *idx*)
		     :direction :output
		     :if-exists nil
		     :if-does-not-exist :create)
    (format s "module satplan~%")
    (format s "go 1.18~%"))
  (write-go
   "main"
   `(do0
     (package main)
     (import
      io
      bufio
      fmt
      time
      ;io/ioutil
      os
      ;encoding/binary
					;github.com/samber/lo
					;github.com/schollz/progressbar/v3
	     runtime/pprof
	     )
     ,(lprint-init)

     ,(let ((l `((:id len :length 1)
		 (:id len2 :length 1)
		 (:id dtype)
		 (:id sub)
		 (:id data :length :variable)))
	    (fn "00_main.go"))
	`(do0
	  (defstruct0 Record
	      ,@(loop for e in l
		      collect
		      (destructuring-bind (&key id (length 1))
			  e
			`(,id ,(ecase length
				 (1 `uint8)
				 (2 `uint16)
				 (4 `uint32)
				 (:variable "[]byte"))))))
	  (defun main ()
	    ,(lprint :msg "main")

	    (let ((prof_fn (string "satplan.prof")))

	      ;; go tool pprof satpla satplan.prof
	      ,(lprint :msg "start profiling" :vars `(prof_fn))
	      ,(lprint :msg "you can view the profile with: go tool pprof satplan satplan.prof")
	      ,(panic `(:var prof_f
			:cmd (os.Create prof_fn)))
	      (pprof.StartCPUProfile prof_f)
	      (defer (pprof.StopCPUProfile)))

	    #+nil (do0
	     ,(lprint :msg "write")
	     (do0
	      ,(panic `(:var fo
			:cmd (os.Create
			      (string ,fn))))
	      (defer ((lambda ()
			,(panic0 `(fo.Close)))))
	      )
	     "var rec_out Record"
	     ,@(loop for (e f) in `((len 4)
				    (dtype 2)
				    (sub 3))
		     collect
		     `(setf (dot rec_out
				 ,e)
			    ,f))
	     (setf rec_out.data (make "[]byte"
				      rec_out.len))
	     ,(panic0 `(binary.Write
			fo
			binary.LittleEndian
			rec_out))
	     )
	    
	    (do0
	     ,(lprint :msg "read")
	     (do0
	      ,(panic `(:var fi
			:cmd (os.Open (string ,fn))))
	      (defer ((lambda ()
			,(panic0 `(fi.Close)))))
	      (assign r (bufio.NewReader fi)))
	     "var rec Record"
	     "var buf [4]byte"
	     ,(panic `(:cmd (io.ReadFull r (aref buf ":"))))
	     (setf rec.len (aref buf 0)
		   #+nil(dot binary
				   BigEndian
				   (Uint16 (aref buf (slice 0 2))))
		   rec.dtype (aref buf 2)
		   rec.sub (aref buf 3))
	     (setf rec.data (make "[]byte" rec.len)
		   )
	     ,(panic `(:cmd (io.ReadFull r rec.data)))
	     ,(tprint :vars `(rec)))))))))
