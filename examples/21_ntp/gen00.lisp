(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/21_ntp"
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
    (format s "module main~%")
    (format s "go 1.18~%"))
  (write-go
   "main"
   `(do0
     (package main)
     (import
      ;io
      ;bufio
      fmt
      time
      ;io/ioutil
      os
      flag
      encoding/binary
      ;log
      net
      
					;github.com/samber/lo
					;github.com/schollz/progressbar/v3
      runtime/pprof
      )
     ,(lprint-init)

     ,(let ((l `((:field Settings :type uint8)
			     (:field Stratum :type uint8)
			     (:field Poll :type int8)
			     (:field Precision :type int8)
			     RootDelay RootDispersion ReferenceID
			     RefTimeSec RefTimeFrac
			     OrigTimeSec OrigTimeFrac
			     RxTimeSec RxTimeFrac
			     TxTimeSec TxTimeFrac)))
	`(defstruct0 packet
	     ,@(loop for e in l
		     collect
		     (let ((ftype 'uint32)
			   (fname e))
		       (when (listp e)
			 (destructuring-bind (&key field type) e
			   (setf ftype type
				 fname field)))
		       `(,fname ,ftype)))))
     
     (const ntpEpochOffset 2208988800)
     (defun main ()
       ,(lprint :msg "main")

       (do0
	"var host string"
	(flag.StringVar &host (string "e")
			(string "us.pool.ntp.org:123")
			(string "NTP host"))
	(flag.Parse))
       
       (let ((prof_fn (string "ntp.prof")))

	 ;; go tool pprof satpla satplan.prof
	 ,(lprint :msg "start profiling" :vars `(prof_fn))
	 ,(lprint :msg "you can view the profile with: go tool pprof satplan ntp.prof")
	 ,(panic `(:var prof_f
		   :cmd (os.Create prof_fn)))
	 (pprof.StartCPUProfile prof_f)
	 (defer (pprof.StopCPUProfile)))

       (do0
	,(panic `(:var conn
		  :cmd (net.Dial (string "udp")
				 host)))
	(defer (conn.Close))
	,(panic0 `(conn.SetDeadline
		  (dot time
		       (Now)
		       (Add (* 15 time.Second))))))

       (do0
	(assign request (curly &packet
			       :Settings #x1b))
	,(panic0 `(binary.Write
		  conn
		  binary.BigEndian
		  request)))
       ))))
