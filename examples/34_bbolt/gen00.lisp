(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/34_bbolt"
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
    (defun panic-init ()
      `(defun checkAndPanic (msg err)
	 (declare (type error err)
		  (type string msg))
	 (unless (== err "nil")
	   ,(lprint
	     :vars `(msg err))
	   (panic err))

	 ))
    (defun panic (var-cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd)
	    var-cmd
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(checkAndPanic (string ,(substitute #\' #\" (emit-go :code cmd)))
			       ,err))
	    (incf err-nr)))))
    (defun panic0 (cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (checkAndPanic (string ,(substitute #\' #\" (emit-go :code cmd)))
			     ,err))
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

  (let ((name "use_bbolt"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
					;math
	time
	runtime
	runtime/debug
					;"github.com/samber/lo"
	(bolt "go.etcd.io/bbolt")
	)
       ,(lprint-init)
       ,(panic-init)
 
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
	  code_repository (string ,(format nil "https://github.com/plops/cl-golang-generator/tree/master/examples/34_bbolt"))
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

       ;; https://pkg.go.dev/go.etcd.io/bbolt#section-readme

       (defun main ()
	 ,(lprint :msg (format nil "program ~a starts" name))
	 (reportGenerator)
	 ,(lprint :msg "Go version:" :vars `((runtime.Version)))
	 (reportDependencies)

	 (do0
	  (do0
	   (assign db_path (string "data.db"))
	   ,(lprint :msg "open database" :vars `(db_path))
	   ,(panic `(:var db
			  :cmd (bolt.Open db_path "0666" "nil"))))
	  (defer ((lambda ()
		    ,(lprint :msg "close database" :vars `(db_path db))
		    ,(panic0 `(db.Close))))))






	 )))))
