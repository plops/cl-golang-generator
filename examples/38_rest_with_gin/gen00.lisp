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

  (let ((record-def `((:name ID :type string :example "1" :binding required)
		      (:name Title :type string :example "This is a song" :binding required)
		      (:name Artist :type string :example "Art Ista" :binding required)
		      (:name Price :type float64 :example "12.23" :binding required))))
    (let ((name (format nil "~2,'0d_mymain" *idx*)))
      (write-go
       name
       `(do0
	 (package main)
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
	  (ginSwagger github.com/swaggo/gin-swagger)
	  (swaggerfiles github.com/swaggo/files)
	  (docs "mymain/docs")

	  )
	 ;; install swag
	 ;; go get github.com/swaggo/swag/cmd/swag
	 ;; go install github.com/swaggo/swag/cmd/swag
	 ;; create documentation:
	 ;; ~/go/bin/swag init -g 00_mymain.go
	 ;; format the comments:
	 ;; ~/go/bin/swag fmt
	 ;; look at API documentation while mymain is running:
	 ;; http://localhost:8080/swagger/index.html
	 (comments "@title  Music Album service"
		   "@version   1.0"
		   "@description Sample server to manage music albums."
		   "@license.name Apache 2.0"
		   "@host localhost:8080"
		   "@BasePath /")

	 (defstruct0 Album
					;(ID "string `json:\"id\"`")
	     ,@(loop for e in record-def
		     collect
		     (destructuring-bind (&key name type example binding) e
		       `(,name ,(format nil "~a `json:\"~a\" binding:\"~a\" example:\"~a\"`"
					type
					(string-downcase (format nil "~a" name))
					binding
					example))))
	   )

	 (setf "var albums"
	       (curly []Album
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


	 (comments "getAlbums godoc"
		   "@Summary List existing albums"
		   "@Schemes"
		   "@Description Get all the albums"
		   "@Tags albums,list"
		   "@Accept json"
		   "@Produce json"
		   "@Success 200 {array} Album"
		   "@Router /albums [get]")
	 (defun getAlbums (c)
	   (declare (type *gin.Context c))
	   (comments "gin.Context carries request details, validates and serializes JSON"
		     "note: Context.JSON would be more compact")
	   (c.IndentedJSON http.StatusOK
			   albums))

	 (defun postAlbums (c)
	   (declare (type *gin.Context c))
	   "var newAlbum Album"
	   ,(when-err0 `(:cmd (c.BindJSON &newAlbum)
			      :code return))
	   (comments "add new Album to slice")
	   (setf albums (append albums newAlbum))
	   (c.IndentedJSON http.StatusCreated ;; 201 status code with
			   ;; json of the new Album
			   newAlbum))

	 (defun getAlbumByID (c)
	   (declare (type *gin.Context c))
	   (assign id (c.Param (string "id")))
	   (comments "locate Album whose ID matches parameter")
	   #+nil ,(lprint :msg "locate album with"
			  :vars `(id))
	   (foreach ((ntuple _ a)
		     (range albums))
		    (when (== a.ID id)
		      (c.IndentedJSON http.StatusOK
				      a)
		      (return)))
	   (c.IndentedJSON http.StatusNotFound
			   (curly gin.H
				  ,(make-keyword (string-upcase "\"message\""))
				  (string "Album not found"))))


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
	    ,@(loop for e in `((Title (string "Music Albums API"))
			       (Description (string "Music Albums API "))
			       (Version (string "1.0"))
			       (Host (string "localhost:8080"))
			       (BasePath (string "/api/v1"))
			       (Schemes (curly []string (string "http"))))
		    collect
		    (destructuring-bind (name value) e
		     `(setf (dot
			     docs
			     SwaggerInfo
			     ,name )
			    ,value))))
	   
	   (do0
	    (assign router (gin.Default))
	    ,@(loop for e in `((:name Albums :type get)
			       (:name Albums :type post)
			       (:name AlbumByID :type get :url (string "/albums/:id"))
			       )
		    appending
		    (destructuring-bind (&key name type url) e
		      (let* ((small-name (string-downcase (format nil "~a" name)))
			     )
			(unless url
			  (setf url `(string ,(format nil "/~a" small-name))))
			(let ((fun (format nil "~a~a" type name))
			      (GET (string-upcase (format nil "~a" type))))
			  `((dot router (,GET ,url
					      ,fun)))))))
	    (router.GET (string "/swagger/*any")
			(dot ginSwagger
			     (WrapHandler
			      swaggerfiles.Handler)))
	    (router.Run (string "localhost:8080")))

	   ))))
    (let ((name (format nil "~2,'0d_mymain_unit_test" 1)))
      (write-go
       name
       `(do0
	 ;; how to write tests with gin: https://circleci.com/blog/gin-gonic-testing/


	 ;; if the package is called main then
	 ;; `go test` doesnt work
	 ;; `go test *.go` works
	 ;; This is some arcane behaviour of test driver: https://appliedgo.net/testmain/

	 (package main)
	 (import
	  testing
	  net/http
	  github.com/stretchr/testify/assert
					;io/ioutil
	  encoding/json
	  net/http/httptest
	  github.com/gin-gonic/gin
	  github.com/rs/xid
	  bytes
					;fmt

	  )
	 (comments "run with `go test` or `GIN_MODE=release go test -v`")
	 (comments "a test file must end with _test.go. Each test method must start with prefix Test")

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
		 (curly []Album
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

	   (assert.Equal tt http.StatusOK w.Code)
	   (do0 "var albums []Album"
		(json.Unmarshal (w.Body.Bytes)
				&albums)
		(assert.NotEmpty tt albums)
		(assert.Equal tt albums albumsOrig))
	   #+nil ,(lprint :vars `(albums))
	   )

	 (defun Test_postAlbums (tt)
	   (declare (type *testing.T tt))
	   (assign r (SetUpRouter))
	   (r.POST (string "/albums")
		   postAlbums)
	   (assign albumId (dot xid
				(New)
				(String)))
	   (assign albumOrig (curly Album :ID albumId
				    :Title (string "bla")
				    :Artist (string "blub")
				    :Price "32.12"))
					;,(lprint :vars `(albumOrig))
	   (assign (ntuple jsonValue _) (json.Marshal albumOrig))
	   (assign (ntuple req _) (http.NewRequest (string "POST")
						   (string "/albums")
						   (bytes.NewBuffer jsonValue)))
	   (assign w (httptest.NewRecorder))
	   (r.ServeHTTP w req)

	   (assert.Equal tt http.StatusCreated w.Code)

	   (do0 "var album Album"
		(json.Unmarshal (w.Body.Bytes)
				&album)
					;,(lprint :vars `(album))
		(assert.NotEmpty tt album)
		(assert.Equal tt album albumOrig))
	   )

	 ;; fuzz testing described here
	 ;; https://universalglue.dev/posts/gin-fuzzing/

	 )))
    ))
