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
		 RootDelay RootDispersion
		 ReferenceID
		 RefTimeSec RefTimeFrac
		 OrigTimeSec OrigTimeFrac
		 RxTimeSec RxTimeFrac
		 TxTimeSec TxTimeFrac)))
	`(do0 (defstruct0 packet
		  ,@(loop for e in l
			  collect
			  (let ((ftype 'uint32)
				(fname e))
			    (when (listp e)
			      (destructuring-bind (&key field type) e
				(setf ftype type
				      fname field)))
			    `(,fname ,ftype))))

	      (const ntpEpochOffset 2208988800)
	      (defun main ()
		,(lprint :msg "main")

		(do0
		 "var host string"
		 (flag.StringVar &host (string "e")
				 (string "nl.pool.ntp.org:123")
				 (string "NTP host"))
		 (flag.Parse))

		(let ((prof_fn (string "ntp.prof")))

		  ;; go tool pprof satpla satplan.prof
		  ,(lprint :msg "start profiling" :vars `(prof_fn))
		  ,(lprint :msg "you can view the profile with: go tool pprof main ntp.prof")
		  ,(panic `(:var prof_f
				 :cmd (os.Create prof_fn)))
		  (pprof.StartCPUProfile prof_f)
		  (defer (pprof.StopCPUProfile)))

		(do0
		 ,(lprint :msg "open connection")
		 ,(panic `(:var conn
				:cmd (net.Dial (string "udp")
					       host)))
		 (defer (conn.Close))
		 )

		(for ()
		     (do0
		      (do0
		       (assign request (curly &packet
					      :Settings (hex #x1b)))
		       ,(panic0 `(conn.SetDeadline
				  (dot time
				       (Now)
				       (Add (* 15 time.Second)))))
		       (assign ctx0 (time.Now))
		       ,(panic0 `(binary.Write
				  conn
				  binary.BigEndian
				  request))
		       (assign ctx1 (time.Now)))

		      (do0
		       (assign response (curly &packet))
		       (assign crx0 (time.Now))
		       ,(panic0 `(binary.Read conn
					      binary.BigEndian
					      response))
		       (assign crx1 (time.Now)))

		      (do0
		       (do0
			(assign secs (- (float64 response.TxTimeSec)
					ntpEpochOffset)
				nanos (>> (* (int64 response.TxTimeFrac)
					     1e9)
					  32))
			(assign tx (time.Unix (int64 secs)
					      nanos)))
		       (do0
			(setf secs (- (float64 response.RxTimeSec)
				      ntpEpochOffset)
			      nanos (>> (* (int64 response.RxTimeFrac)
					   1e9)
					32))
			(assign rx (time.Unix (int64 secs)
					      nanos)))
		       ,@(loop for e in `(ctx0 ctx1 crx0 crx1)
			       collect
			       `(assign ,(format nil "~a_" e)
					(dot ,e (Format
						 (string
						  "2006-01-02 15:04:05.000")))))
		       ,(lprint :vars `(ctx0_ ctx1_ rx crx0_ tx crx1_))
		       #+nil ,@(loop for e in `(
						RefTimeSec RefTimeFrac
							   OrigTimeSec OrigTimeFrac
							   RxTimeSec RxTimeFrac
							   TxTimeSec TxTimeFrac)
				     collect
				     `(do0
				       ,(lprint :vars `((dot response ,e)))))
		       (time.Sleep (* 1 time.Second)))))

		))))))
