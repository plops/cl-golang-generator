(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/38_rest_with_gin"
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
    (defun check-err (var-cmd)
      "checks second return value and returns fmt.Errorf. This is used in closures of the bbolt library."
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd)
	    var-cmd
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(unless (== ,err "nil")
		  ,(let ((msg (substitute #\' #\" (emit-go :code cmd))))
		     `(do0
		       #+nil
		       ,(lprint :msg msg
				:vars `( ,err))
		       (return (fmt.Errorf (string ,(format nil "~a %s" msg)) ,err))))))
	    (incf err-nr)))))
    (defun check-err0 (cmd &key (use-err-number err-nr
						))
      "check return value and returns fmt.Errorf if not nil. This is used in closures of the bbolt library. if use-err-number is nil don't append a digit"
      (let ((err (if use-err-number
		     (format nil "err~2,'0d" err-nr)
		     (format nil "err")))
	    )
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (unless (== ,err "nil")
		,(let ((msg (substitute #\' #\" (emit-go :code cmd))))
		   `(do0
		     (return (fmt.Errorf (string ,(format nil "~a %s" msg)) ,err))))))
	  (when use-err-number
	    (incf err-nr)
	    ))))
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
	      (format s "module mymain~%")
	      (format s "go 1.18~%"))
	    (write-source
	     (format nil "~a/source~2,'0d/~a"
		     *path* file-count name)
	     code))
					;(incf file-count)
	)))

  (let ((record-def `((:name ID :type string)
		      (:name Title :type string)
		      (:name Artist :type string)
		      (:name Price :type float64))))
    (let ((name (format nil "~2,'0d_mymain" *idx*)))
      (write-go
       name
       `(do0
	 (package mymain)
	 (import
	  fmt
	  net/http
					;log
	  github.com/gin-gonic/gin

					;math
					;encoding/binary
	  time
	  runtime
	  runtime/debug
					;path/filepath
					;"github.com/samber/lo"


	  )

	 (defstruct0 album
					;(ID "string `json:\"id\"`")
	     ,@(loop for e in record-def
		     collect
		     (destructuring-bind (&key name type) e
		       `(,name ,(format nil "~a `json:\"~a\"`"
					type
					(string-downcase (format nil "~a" name))))))
	   )

	 (setf "var albums"
	       (curly []album
		      ,@(loop for (title artist price) in `(("blue train" "john coltrane" "54.99")
							    ("jeru" "eryy muliiang" "17.99")
							    ("vaun and brown" "vaaughn" "39.99"))
			      and id from 1
			      collect
			      `(curly ""
				      :ID (string ,id)
				      :Title (string ,title)
				      :Artist (string ,artist)
				      :Price ,price))))

	 ,(lprint-init)
	 ,(panic-init)


	 (defun getAlbums (c)
	   (declare (type *gin.Context c))
	   (comments "gin.Context carries request details, validates and serializes JSON"
		     "note: Context.JSON would be more compact")
	   (c.IndentedJSON http.StatusOK
			   albums))


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


	 (defun main ()
	   ,(lprint :msg (format nil "program ~a starts" name))
	   (reportGenerator)
	   ,(lprint :msg "Go version:" :vars `((runtime.Version)))
	   (reportDependencies)

	   (do0
	    (assign router (gin.Default))
	    (router.GET (string "/albums")
			getAlbums)
	    (router.Run (string "localhost:8080")))

	   ))))
    (let ((name (format nil "~2,'0d_mymain_unit_test" 1)))
      (write-go
       name
       `(do0
	 ;; if the package is called main then
	 ;; `go test` doesnt work
	 ;; `go test *.go` works
	 ;; This is some arcane behaviour of test driver: https://appliedgo.net/testmain/

	 (package mymain)
	 (import
	  testing
	  net/http
	  github.com/stretchr/testify/assert
					;io/ioutil
	  encoding/json
	  net/http/httptest
	  github.com/gin-gonic/gin

	  
	  )
	 (comments "a test file must end with _test.go. Each test method must start with prefix Test")
	 #+nil (do0
	  (import fmt)
	  ;,(lprint-init)
	  )
	 (defun SetUpRouter ()
	   (declare (values *gin.Engine))
	   (assign router (gin.Default))
	   (return router))
	 (defun Test_getAlbums (tt)
	   (declare (type *testing.T tt))
	   (assign r (SetUpRouter))
	   (r.GET (string "/albums")
		  getAlbums)

	   (assign (ntuple req _) (http.NewRequest (string "GET")
						   (string "/albums")
						   "nil"))
	   (assign w (httptest.NewRecorder))
	   (r.ServeHTTP w req)
	   #+nil (assign (ntuple responseData _)
			 (ioutil.ReadAll w.Body))
	    (setf "var albumsOrig"
	       (curly []album
		      ,@(loop for (title artist price) in `(("blue train" "john coltrane" "54.99")
							    ("jeru" "eryy muliiang" "17.99")
							    ("vaun and brown" "vaaughn" "39.91"))
			      and id from 1
			      collect
			      `(curly ""
				      :ID (string ,id)
				      :Title (string ,title)
				      :Artist (string ,artist)
				      :Price ,price))))
	   "var albums []album"
	   (json.Unmarshal (w.Body.Bytes)
			   &albums)
	   (assert.Equal tt http.StatusOK w.Code)
	   (assert.NotEmpty tt albums)
	   (assert.Equal tt albums albumsOrig)
	   #+nil ,(lprint :vars `(albums))
	   )

	 )))
    ))
