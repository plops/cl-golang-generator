(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/41_letsencrypt"
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
    (defun when-err (var-cmd-code)
      "check second return value and execute code if error occured"
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd (code `(comments "no error handling code specified")) )
	    var-cmd-code
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(unless (== ,err "nil")
		  ,code))
	    (incf err-nr)))))
    (defun when-err0 (cmd-code)
      "check return value and execute code if error occured"
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key cmd (code `(comments "no error handling code specified")) )
	    cmd-code
	  (prog1
	      `(do0
		(assign ,err
			,cmd)
		(unless (== ,err "nil")
		  ,code))
	    (incf err-nr)))))
    (defun panic (var-cmd)
      "check second return value and panic with an error message if necessary"
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd)
	    var-cmd
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(checkAndPanic (string-raw ,(substitute #\' #\` (emit-go :code cmd)))
			       ,err))
	    (incf err-nr)))))
    (defun panic0 (cmd)
      "check an error value that is returned as the only return value. panic with error message."
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (checkAndPanic (string-raw ,(substitute #\' #\` (emit-go :code cmd)))
			     ,err))
	  (incf err-nr))))
    )

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
	      (format s "module mymain~%")
	      (format s "go 1.18~%"))
	    (write-source
	     (format nil "~a/source~2,'0d/~a"
		     *path* file-count name)
	     code))
					;(incf file-count)
	)))

  (let ((name (format nil "~2,'0d_mymain" *idx*)))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
	net/http
					;log
	context
	crypto/tls
	flag
	io
					;math
					;encoding/binary
	time
	runtime
	runtime/debug
					;path/filepath
					;"github.com/samber/lo"
	"golang.org/x/crypto/acme/autocert"

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
	  code_repository (string ,(format nil "https://github.com/plops/cl-golang-generator/tree/master/examples/35_rest"))
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


       "const htmlIndex = `<htm><body>WElcome!</body></html>`"
       "const httpPort = \"127.0.0.1:8080\""

       (let ((flagProduction false)
	     (flagRedirectHTTPToHTTPS false)))

       (defun handleIndex (w r)
	 (declare (type http.ResponseWriter w)
		  (type *http.Request r))
	 (io.WriteString w
			 htmlIndex))

       (defun makeServerFromMux (mux)
	 (declare (values *http.Server)
		  (type *http.ServerMux mux))
	 (return (curly &http.Server
			:ReadTimeout (* 5 time.Second)
			:WriteTimeout (* 5 time.Second)
			:IdleTimeout (* 120 time.Second)
			:Handler mux)))
       (defun makeHTTPServer ()
	 (declare (values *http.Server))
	 (assign mux (curly &http.ServeMux))
	 (mux.HandleFunc (string "/")
			 handleIndex)
	 (return (makeServerFromMux mux)))

       (defun makeHTTPToHTTPSRedirectServer ()
	 (declare (values *http.Server))
	 (assign handleRedirect
		 (lambda (w r)
		   (declare (type http.ResponseWriter w)
			    (type *http.Request r))
		   (assign newURI (+ (string "https://")
				     r.Host
				     (r.URL.String)))
		   (http.Redirect w r newURI http.StatusFound)
		   ))
	 (assign mux (curly &http.ServeMux))
	 (mux.handleFunc (string "/")
			 handleRedirect)
	 (return (makeServerFromMux mux)))

       (defun parseFlags ()
	 (flag.BoolVar &flagProduction
		       (string "production")
		       false
		       (string "if true, we start https server"))
	 (flag.BoolVar &flagRedirectHTTPToHTTPS
		       (string "redirect-to-https")
		       false
		       (string "if true, we redirect HTTP to HTTPS")
		       )
	 (flag.Parse))
       
       (defun main ()
	 ,(lprint :msg (format nil "program ~a starts" name))
	 (reportGenerator)
	 ,(lprint :msg "Go version:" :vars `((runtime.Version)))
	 (reportDependencies)
	 (parseFlags)

	 (do0
	  "var m *autocert.Manager")

	 

	 ))))

  )
