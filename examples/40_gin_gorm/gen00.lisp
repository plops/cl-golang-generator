(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/40_gin_gorm"
	    (user-homedir-pathname)))
  (defparameter *idx* "00")
  (defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))
  (defun lprint-init ()
    `(do0
      (comments "timeNow godoc")
      (comments "standardized timestamp for logging")
      (defun timeNow ()
	(declare (values string))
	(return
	  (dot time
	       (Now)
	       (Format
		(string
		 "2006-01-02 15:04:05.000")))))))
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
      `(do0
	(comments "checkAndPanic godoc")
	(comments "I use this for many functions that return errors to bail with an error message")
	(defun checkAndPanic (msg err)
	  (declare (type error err)
		   (type string msg))
	  (unless (== err "nil")
	    ,(lprint
	      :vars `(msg err))
	    (panic err))

	  )))
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

  (let ((record-def `(
		      (:name ID :type int :example 1 :gorm "primaryKey;autoIncrement" ;:binding required
			     ;; if the ID is set to <n> in the POST command then subsequent posts with empty ID will assign <n+1>, <n+2> ...
			     )
		      (:name GivenName :type string :example "Heuss" :gorm "not null;default:null;type:varchar(100)" :binding required)
		      (:name LastName :type string :example "Karl" :gorm "not null" :binding required)
		      (:name CreatedAt :type time.Time :example "2022-09-18T14:11:36.527738191+02:00")
		      (:name UpdatedAt :type time.Time :example "2022-09-18T14:11:36.527738191+02:00")
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
	  github.com/gin-gonic/gin
	  github.com/jinzhu/gorm
	  (_ "github.com/mattn/go-sqlite3")

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
	 ;; ~/go/bin/swag init --parseDependency -g 00_mymain.go
	 ;; format the comments:
	 ;; ~/go/bin/swag fmt
	 ;; look at API documentation while mymain is running, you can also test it:
	 ;; http://localhost:8080/swagger/index.html
	 (comments "GORM uses ID as primary key by default"
		   "@title  User table service"
		   "@version   1.0"
		   "@description Information about users"
		   "@license.name Apache 2.0"
		   "@host localhost:8080"
		   "@BasePath /api/v1")
	 ;; gorm pluralizes the struct name by default to the table name using snake_case
	 (defstruct0 Users
					;(ID "string `json:\"id\"`")
	     ;("" gorm.Model)
	     ,@(loop for e in record-def
		     collect
		     (destructuring-bind (&key name type example binding gorm) e
		       `(,name ,(format
				 nil
				 "~a `json:\"~a\" form:\"~a\" gorm:\"~a\" ~@[binding:\"~a\"~]example:\"~a\"`"
				 type
				 (string-downcase (format nil "~a" name)) ;; json
				 (string-downcase (format nil "~a" name)) ;; form
				 gorm
				 binding
				 example)))))

	 (defstruct0 UsersNoID
					;(ID "string `json:\"id\"`")
	     ,@(loop for e in (rest record-def)
		     collect
		     (destructuring-bind (&key name type example binding gorm) e
		       `(,name ,(format
				 nil
				 "~a `json:\"~a\" form:\"~a\" gorm:\"~a\" ~@[binding:\"~a\"~]example:\"~a\"`"
				 type
				 (string-downcase (format nil "~a" name)) ;; json
				 (string-downcase (format nil "~a" name)) ;; form
				 gorm
				 binding
				 example)))))

	 ,(lprint-init)
	 ,(panic-init)

	 (do0
	  (comments "Create SQL database table from Go structure definition")
	  (defun InitDb ()
	    (declare (values *gorm.DB))
	    ,(panic `(:var db
			   :cmd (gorm.Open (string "sqlite3")
					   (string "./data.db"))))
	    (db.AutoMigrate (curly &Users))
	    (db.LogMode true)

	    (unless (db.HasTable (curly &Users))
	      (db.CreateTable (curly &Users))
	      #+nil (dot db
			 (Set (string "gorm:table_options")
			      (string "ENGINE=InnoDB"))
			 (CreateTable (curly &Users))))
	    (return db)))

	 (do0
	  (comments "list version and checksums of the go dependencies")
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
		  ,(lprint :msg "failed to read build info"))))))

	 (do0
	  (comments "list git commit and date of when the go code was generated that was used to compile the binary")
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
		      ,(lprint :vars `(,e))))))
	 ,(let ((route-def `(
			     (:name User :type post
				    :url (string "/users")
				    :doc
				    (comments
				     "@Summary Add new user"
				     "@Schemes"
				     "@Description Add a new user to the table. The input can contain an id, but it will be ignored. The database chooses an id automatically. The chosen id is returned in the response."
				     "@Tags users"
				     "@Accept json"
				     "@Produce json"
				     "@Param user body UsersNoID true \"Create User\""
				     "@Success 201 {object} Users"
				     "@Failure 422 {object} string \"Fields are empty\""
				     "@Router /users [post]")

				    :code
				    (do0
				     #+nil
				     (do0
				      (assign db (InitDb))
				      (defer (db.Close)))
				     "var user UsersNoID"
				     (do0
				      ,(when-err0 `(:cmd (c.ShouldBindJSON &user)
							 :code
							 (do0
							  (c.IndentedJSON http.StatusBadRequest ;; 400
									  (curly gin.H
										 ,(make-keyword "\"ERROR\"")
										 (dot err01 Error)))

							  ))))
				     #+nil
				     (c.Bind &user)
				     ,(lprint :vars `(user))
				     (comments "definition of http status code https://go.dev/src/net/http/status.go ")
				     (if (logand (!= user.GivenName (string ""))
						 (!= user.LastName (string "")))
					 (do0
					  (comments "insert into users (name) values user.Name)")
					  (assign userFromInput (curly Users
								       :GivenName user.GivenName
								       :LastName user.LastName))
					  (db.Create &userFromInput)
					  (c.IndentedJSON http.StatusCreated ;; 201
							  userFromInput)
					  )
					 (do0

					  (c.IndentedJSON http.StatusUnprocessableEntity ;; 422
							  (curly gin.H ,(make-keyword "\"ERROR\"")
								 (string "Fields are empty"))
							  )
					  ))))
			     (:name Users :type get
				    :doc (comments
					  "@Summary List existing users"
					  "@Schemes"
					  "@Description Get all the users"
					  "@Tags users"
					  "@Accept json"
					  "@Produce json"
					  "@Success 200 {array} Users"
					  "@Router /users [get]")
				    :code
				    (do0
				     "var users []Users"
				     (comments "select * from users")
				     (db.Find &users)
				     (c.IndentedJSON http.StatusOK ;; 200
						     users)))
			     (:name UserByID :type get :url (string "/users/:id")
				    :doc
				    (comments
				     "@Summary Get single user"
				     "@Schemes"
				     "@Description Get a single user from list"
				     "@Tags users"
				     "@Accept json"
				     "@Produce json"
				     "@Param id path int true \"Get User\""
				     "@Success 200 {object} Users"
				     "@Failure 404 {object} string \"User not Found\""
				     "@Router /users/{id} [get]")
				    :code
				    (do0
				     (assign id (c.Params.ByName (string "id")))
				     (comments "select * from users where id=<id>")
				     "var user Users"
				     (db.First &user id)
				     (if (== user.ID 0)
					 (c.IndentedJSON http.StatusNotFound ;; 404
							 (curly gin.H
								,(make-keyword "\"ERROR\"")
								(string "User not found"))
							 )
					 (c.IndentedJSON http.StatusOK ;; 200
							 user))))
			     ;; type H map[string]any in gin-gonic utils.go
			     (:name UserByID :type put :url (string "/users/:id")
				    :doc
				    (comments
				     "@Summary Change single user"
				     "@Schemes"
				     "@Description Change a single user that already exists in list"
				     "@Tags users"
				     "@Accept json"
				     "@Produce json"
				     "@Param id path int true \"Put User\""
				     "@Param user body Users true \"Put User\""
				     "@Success 200 {object} Users"
				     "@Failure 404 {object} string \"User not found\"" ;;  {\"code\": 404, \"error\":\"User not found\"}
				     "@Failure 422 {object} string \"Fields are empty\"" ;; {\"code\": 422, \"error\":\"Fields are empty\"}
				     "@Router /users/{id} [put]")
				    :code
				    (do0
				     (assign id (c.Params.ByName (string "id")))
				     (comments "select * from users where id=<id>")
				     "var user Users"
				     (db.First &user id)
				     (if (logand (!= user.GivenName (string ""))
						 (!= user.LastName (string "")))
					 (do0 (if (== user.ID 0)
						  (do0
						   (c.IndentedJSON http.StatusNotFound ;; 404
								   (curly gin.H
									  ,(make-keyword "\"ERROR\"")
									  (string "User not found"))
								   ))
						  (do0
						   "var newUser Users"
						   (c.Bind &newUser)
						   (assign result (curly Users
									 :ID user.ID
									 :GivenName newUser.GivenName
									 :LastName newUser.LastName))
						   (comments "// update users set givenname='newuser.GivenName', lastname='newUser.LastName' where id=user.id;")
						   (db.Save &result)
						   (c.IndentedJSON http.StatusOK ;; 200
								   result))

						  ))
					 (do0
					  (c.IndentedJSON http.StatusUnprocessableEntity ;; 422
							  (curly gin.H ,(make-keyword "\"ERROR\"")
								 (string "Fields are empty"))
							  )))))
			     (:name UserByID :type delete :url (string "/users/:id")
				    :doc
				    (comments
				     "@Summary Delete single user"
				     "@Schemes"
				     "@Description Delete a single user that already exists in list"
				     "@Tags users"
				     "@Accept json"
				     "@Produce json"
				     "@Param id path int true \"Delete User\""
				     "@Success 200 {object} string \"User #{id} deleted\""
				     "@Failure 404 {object} string \"User not found\""
				     "@Router /users/{id} [delete]")
				    :code
				    (do0
				     (assign id (c.Params.ByName (string "id")))
				     (comments "select * from users where id=<id>")
				     "var user Users"
				     (db.First &user id)
				     (if (== user.ID 0)
					 (do0
					  (c.IndentedJSON http.StatusNotFound ;; 404
							  (curly gin.H ,(make-keyword "\"ERROR\"")
								 (string "User not found"))
							  ))
					 (do0
					  (comments "delete from users where id = user.ID")
					  (db.Delete &user)
					  (c.IndentedJSON http.StatusOK ;; 200
							  (curly gin.H ,(make-keyword "\"SUCCESS\"")
								 (+ (string "User #")
								    id
								    (string " deleted")))))))))))
	    `(do0
	      ,@(loop for e in route-def
		      collect
		      (destructuring-bind (&key name type url doc code) e
			(let* ((small-name (string-downcase (format nil "~a" name))))
			  (unless url
			    (setf url `(string ,(format nil "/~a" small-name))))
			  (let ((fun (format nil "~a~a" type name))
				(GET (string-upcase (format nil "~a" type))))
			    `(do0
			      (comments ,(format nil "~a handler function for gin-gonic godoc" fun))
			      ,doc
			      (defun ,fun (c)
				(declare (type *gin.Context c))
				(do0
				 (assign db (InitDb))
				 (defer (db.Close)))
				,code))))))

	      (comments "Cross Origin Resource Sharing (untested)")
	      (defun Cors ()
		(declare (values gin.HandlerFunc))
		(return (lambda (c)
			  (declare (type *gin.Context c))
			  (dot c
			       Writer (Header)
			       (Set (string "Access-Control-Allow-Origin")
				    (string "*")))
			  (c.Next))))

	      (comments "Set up headers for for XMLHttpRequest or Fetch from Javascript with CORS (untested)")
	      (defun OptionsUser (c)
		(declare (type *gin.Context c))
		(dot c
		     Writer (Header)
		     (Set (string "Access-Control-Allow-Methods")
			  (string "DELETE,POST,PUT")))
		(dot c
		     Writer (Header)
		     (Set (string "Access-Control-Allow-Headers")
			  (string "Content-Type")))
		(c.Next))

	      (defun main ()
		,(lprint :msg (format nil "program ~a starts" name))
		(reportGenerator)
		,(lprint :msg "Go version:" :vars `((runtime.Version)))
		(reportDependencies)

		#+nil (gin.SetMode gin.ReleaseMode)

		(do0
		 ,@(loop for e in `((Title (string "Users API"))
				    (Description (string "Users API "))
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
		 (router.Use (Cors))
		 (router.SetTrustedProxies "nil")

		 (assign v1 (router.Group (string "api/v1")))
		 (progn
		   ,@(loop for e in route-def
			   appending
			   (destructuring-bind (&key name type url doc code) e
			     (let* ((small-name (string-downcase (format nil "~a" name))))
			       (unless url
				 (setf url `(string ,(format nil "/~a" small-name))))
			       (let ((fun (format nil "~a~a" type name))
				     (GET (string-upcase (format nil "~a" type))))
				 `((dot v1 (,GET ,url
						 ,fun)))))))
		   (comments "POST:")
		   (v1.OPTIONS (string "/users")
			       OptionsUser)
		   (comments "PUT,DELETE")
		   (v1.OPTIONS (string "/users/:id")
			       OptionsUser)
		   )
		 (router.GET (string "/swagger/*any")
			     (dot ginSwagger
				  (WrapHandler
				   swaggerfiles.Handler)))
		 (router.Run (string "localhost:8080")))

		))))))
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
					;github.com/rs/xid
	  bytes
	  os
	  fmt
					;strconv
	  )
	 (comments "run with `go test` or `GIN_MODE=release go test -v`")
	 (comments "a test file must end with _test.go. Each test method must start with prefix Test")

	 (defun SetUpRouter ()
	   (declare (values *gin.Engine))
	   (do0
	    (comments "remove database file")
	    (assign dbFilename (string "data.db"))
	    (assign (ntuple _ err) (os.Stat dbFilename))
	    (when (== err "nil")
	      ,(panic0 `(os.Remove dbFilename))))
	   (assign router (gin.Default))
	   (router.SetTrustedProxies "nil")
	   (return router))

	 (defun Test_00_getUsers_empty (tt)
	   (declare (type *testing.T tt))
	   (assign r (SetUpRouter))
	   (do0
	    (do0
	     (comments "submit get request to obtain entire user list")
	     (r.GET (string "/users")
		    getUsers)

	     (assign (ntuple req _) (http.NewRequest (string "GET")
						     (string "/users")
						     "nil")))

	    (do0
	     (comments "validate response")
	     (assign w (httptest.NewRecorder))
	     (r.ServeHTTP w req)
	     (setf "var usersOrig"
		   (curly []Users
			  ))

	     (assert.Equal tt http.StatusOK w.Code)
	     (do0 "var users []Users"
		  (json.Unmarshal (w.Body.Bytes)
				  &users)
		  (assert.Empty tt users)
		  (assert.Equal tt users usersOrig)))))



	 (defun Test_01_postUser_once (tt)
	   (declare (type *testing.T tt))
	   (assign r (SetUpRouter))

	   (do0
	    (do0
	     (comments "submit post request to add one user")
	     (r.POST (string "/users")
		     postUser)
	     (comments "let the database choose the ID")
	     (assign userOrig (curly UsersNoID
				     :GivenName (string "Bella")
				     :LastName (string "Hound")))
	     (assign (ntuple jsonValue _) (json.Marshal userOrig))
	     ,(lprint :vars `(("string" jsonValue)))
	     (assign (ntuple req _) (http.NewRequest (string "POST")
						     (string "/users")
						     (bytes.NewBuffer jsonValue))))

	    (do0
	     (comments "verify response for the post request")
	     (assign w (httptest.NewRecorder))
	     (r.ServeHTTP w req)

	     (assert.Equal tt http.StatusCreated w.Code)

	     (do0 "var user Users "
		  (json.Unmarshal (w.Body.Bytes)
				  &user)
		  (assert.NotEmpty tt user)
		  ,(lprint :vars `(user userOrig))
		  ,@(loop for e in `(GivenName LastName)
			  collect
			  `(assert.Equal tt (dot user ,e) (dot userOrig ,e)))))))

	 ,(let ((three-names `((john coltrane)
			       (jery mulijang)
			       (vanes vaughn))))
	    `(do0
	      (defun Test_02_postUser_three_times_without_ID (tt)
		(declare (type *testing.T tt))
		(assign r (SetUpRouter))
		(r.POST (string "/users")
			postUser)
		,@(loop for (given-name last-name) in three-names
			and e-i from 10
			and count in `(first second third)
			collect
			`(progn
			   (do0
			    (do0
			     (comments ,(format nil "submit post request to add the ~a user (out of three)" count))

			     (comments "It is acceptable to submit an ID but the the database will choose the ID.")
			     (assign userOrig (curly UsersNoID

						     :GivenName (string ,given-name)
						     :LastName (string ,last-name)))
			     (assign (ntuple jsonValue _) (json.Marshal userOrig))
			     ,(lprint :vars `(("string" jsonValue)))
			     (assign (ntuple req _) (http.NewRequest (string "POST")
								     (string "/users")
								     (bytes.NewBuffer jsonValue))))

			    (do0
			     (comments "verify response for the post request")
			     (assign w (httptest.NewRecorder))
			     (r.ServeHTTP w req)

			     (assert.Equal tt http.StatusCreated w.Code)

			     (do0 "var user Users "
				  (json.Unmarshal (w.Body.Bytes)
						  &user)
				  (assert.NotEmpty tt user)
				  ,(lprint :vars `(user userOrig))

				  ,@(loop for e in `(GivenName LastName)
					  collect
					  `(assert.Equal tt (dot user ,e) (dot userOrig ,e))))))))


		(do0
		 (do0
		  (comments "submit get request to obtain entire user list")
		  (r.GET (string "/users")
			 getUsers)

		  (assign (ntuple req _) (http.NewRequest (string "GET")
							  (string "/users")
							  "nil")))

		 (do0
		  (comments "validate response")
		  (assign w (httptest.NewRecorder))
		  (r.ServeHTTP w req)



		  (assert.Equal tt http.StatusOK w.Code)
		  (do0 "var users []Users"
		       (json.Unmarshal (w.Body.Bytes)
				       &users)
		       (assert.NotEmpty tt users)
		       ,@(loop for (given-name last-name) in three-names
			       and e-i from 0
			       collect
			       `(do0
				 (assert.Equal tt
					       (dot (aref users ,e-i) GivenName)
					       (string ,given-name))
				 (assert.Equal tt
					       (dot (aref users ,e-i) LastName)
					       (string ,last-name)))
			       )
		       ))))
	      (defun Test_03_postUser_three_times_with_ID (tt)
		(declare (type *testing.T tt))
		(do0 (assign r (SetUpRouter))
		     (r.POST (string "/users")
			     postUser))
		,@(loop for (given-name last-name) in three-names
			and e-i from 10
			and count in `(first second third)
			collect
			`(progn
			   (do0
			    (do0
			     (comments ,(format nil "submit post request to add the ~a user (out of three)" count))

			     (comments "It is acceptable to submit an ID but the the database will choose the ID.")
			     (assign userToSubmit (curly Users
							 :ID ,e-i
							 :GivenName (string ,given-name)
							 :LastName (string ,last-name)))
			     (assign (ntuple jsonValue _) (json.Marshal userToSubmit))
			     ,(lprint :vars `(("string" jsonValue)))
			     (assign (ntuple req _) (http.NewRequest (string "POST")
								     (string "/users")
								     (bytes.NewBuffer jsonValue))))

			    (do0
			     (comments "verify response for the post request")
			     (assign w (httptest.NewRecorder))
			     (r.ServeHTTP w req)

			     (assert.Equal tt http.StatusCreated w.Code)

			     (do0 "var userReadBack Users "
				  (json.Unmarshal (w.Body.Bytes)
						  &userReadBack)
				  ,(lprint :vars `(userToSubmit userReadBack))
				  (assert.NotEmpty tt userReadBack)
				  ,@(loop for e in `(ID)
					  collect
					  `(assert.NotEqual tt (dot userReadBack ,e) (dot userToSubmit ,e)))
				  ,@(loop for e in `(GivenName LastName)
					  collect
					  `(assert.Equal tt (dot userReadBack ,e) (dot userToSubmit ,e))))))))


		(do0
		 (do0
		  (comments "submit get request to obtain entire user list")
		  (r.GET (string "/users")
			 getUsers)

		  (assign (ntuple req _) (http.NewRequest (string "GET")
							  (string "/users")
							  "nil")))

		 (do0
		  (comments "validate response")
		  (assign w (httptest.NewRecorder))
		  (r.ServeHTTP w req)



		  (assert.Equal tt http.StatusOK w.Code)

		  (do0 "var users []Users"
		       (json.Unmarshal (w.Body.Bytes)
				       &users)
		       (assert.NotEmpty tt users)
		       (comments "we only add three users with ids starting from 10. the database ignores those ids and uses 1, 2, 3 instead")
		       ,@(loop for (given-name last-name) in three-names
			       and e-i from 0
			       and id from 10
			       collect
			       `(do0
				 (assert.NotEqual tt
						  (dot (aref users ,e-i) ID)
						  ,id)
				 (assert.Equal tt
					       (dot (aref users ,e-i) ID)
					       ,(+ 1 e-i))
				 (assert.Equal tt
					       (dot (aref users ,e-i) GivenName)
					       (string ,given-name))
				 (assert.Equal tt
					       (dot (aref users ,e-i) LastName)
					       (string ,last-name)))
			       )
		       ))))))





	 ;; fuzz testing described here
	 ;; https://universalglue.dev/posts/gin-fuzzing/

	#+nil (defun FuzzEntries (f)
	   (declare (type *testing.F f))
	   (comments "run this test with `go test -fuzz=. -fuzztime=5s .`"
		     "fuzzing can run out of memory, be careful when using it in CI environment")
	   (do0 (assign r (SetUpRouter))
		(r.POST (string "/users")
			postUser)
		(r.GET (string "/users/:id")
		       getUserByID)
		(r.DELETE (string "/users/:id")
			  deleteUserByID))
	   (f.Fuzz
	    (lambda (tt givenName lastName id)
	      (declare (type string givenName lastName )
		       (type int id)
		       (type *testing.T tt) )
	      (do0
	       (do0
		(comments ,(format nil "submit post request to add a new user"))
		(assign userToSubmit (curly Users
					    :ID id
					    :GivenName givenName
					    :LastName lastName))
		(assign (ntuple jsonValue _) (json.Marshal userToSubmit))
					;,(lprint :vars `(("string" jsonValue)))
		(assign (ntuple req _) (http.NewRequest (string "POST")
							(string "/users")
							(bytes.NewBuffer jsonValue)))
		(do0
		 (comments "verify response for the post request")
		 (assign w (httptest.NewRecorder))
		 (r.ServeHTTP w req)
		 (assert.Equal tt http.StatusCreated w.Code)
		 (do0 "var userReadBack Users "
		      (json.Unmarshal (w.Body.Bytes)
				      &userReadBack)
		      ,(lprint :vars `(userToSubmit userReadBack))
		      (assert.NotEmpty tt userReadBack)
		      ,@(loop for e in `(GivenName LastName)
			      collect
			      `(assert.Equal tt (dot userReadBack ,e) (dot userToSubmit ,e)))))

		#+nil(do0
		      (comments "delete the user")
		      (assign (ntuple req2 _) (http.NewRequest (string "DELETE")
							       (+ (string "/users/")
								  (strconv.Itoa (dot userReadBack ID)))
							       (bytes.NewBuffer jsonValue)))
		      (assign w2 (httptest.NewRecorder))
		      (r.ServeHTTP w2 req2)
		      (assert.Equal tt http.StatusOK w2.Code)
		      )
		)
	       )
	      )))

	 )))
    ))
