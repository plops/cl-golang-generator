(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/18_kml"
	    (user-homedir-pathname)))
  (defparameter *idx* "02")
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
	    (&key var cmd (pre "") (post ""))
	    var-cmd
	  (prog1	
	      `(do0
		,pre
		(assign (ntuple ,var ,err)
			,cmd)
		(unless (== ,err "nil")
		  ,(lprint :vars `(,err))
		  (panic ,err))
		,post)
	    (incf err-nr)))))
    (defun panic0 (cmddef)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	    (&key cmd (pre "") (post "")) cmddef
	  (prog1	
	      `(do0
		,pre
		(assign ,err
			,cmd)
		(unless (== ,err "nil")
		  (panic ,err))
		,post)
	    (incf err-nr))))))
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
  (defun xml-struct (name-defs)
    (destructuring-bind (struct-name defs) name-defs
     `(defstruct0
	  ,struct-name
	  ,@(loop for e in
		  defs
		  collect
		  (destructuring-bind
		      (&key name (type name)
			 (xml name)) e
		    `(,name ,(format
			      nil
			      "~a `xml:\"~a\"`"
			      type xml)))))))
  (write-go
   "main"
   `(do0
     (package main)
     (import fmt
	     time
	     
	     io/ioutil
	     os
	     golang.org/x/net/html/charset
	     encoding/xml
	     bytes
	     database/sql
	     github.com/mattn/go-sqlite3
	     github.com/samber/lo
	     github.com/schollz/progressbar/v3
	     runtime/pprof
	     )

     ;; i have to define the following structures
     ;; then i shall be able to read the sentinel kml

     ;; kml Document Folder Placemark
     ;; ExtendedData Data value
     ;; LinearRing altitudeMode coordinates tesselate
     ;; TimeSpan
     ;; styleUrl
     ;; NetworkLink
     ;; Style LineStyle color width

     
     ,(xml-struct
       `(KML ((:name XMLName
	       :type xml.Name
	       :xml kml)
	      (:name Document ))))
     ,(xml-struct
       `(Document ((:name Id
		    :type string
		    :xml "id,attr")
		   ;(:name Schema )
		   (:name Folder ))))
     ,(xml-struct
       `(Folder
	 ((:name Name
	   :type string
	   :xml "name")
	  (:name Placemarks
	   :type "[]Placemark"
	   :xml "Placemark")
	  (:name Folders
	   :type "[]Folder"
	   :xml "Folder")
	  )))

     ,(xml-struct
       `(Placemark
	 ((:name DateTime
	   :type string
	   :xml "name")
	  (:name Style)
	  (:name styleUrl
	   :type string)
	  (:name TimeSpan)
	  (:name ExtendedData)
	  )))

     ,(xml-struct
       `(Style
	 ((:name LineStyle
	   )
	  )))
     
     
     ,(xml-struct
       `(TimeSpan
	 ((:name Begin
	   :type string
	   :xml begin)
	  (:name End
	   :type string
	   :xml end)
	  )))
     
     ,(xml-struct
       `(LineStyle
	      ((:name Color
		:type string
		:xml color))))
     ,(xml-struct
       `(LinearRing
	      ((:name StringCoords
		:type string
		:xml coordinates)
	       (:name altitudeMode
		:type string)
	       (:name tessellate
		:type string)
	       )))

     ,(xml-struct
       `(ExtendedData
	 ((:name Data
	   :type "[]Data"))))
     ,(xml-struct
       `(Data
	 ((:name Key
	   :type string
	   :xml "name,attr")
	  (:name Value
	   :type string
	   :xml "value")
	  )))

     
     ,(lprint-init)
     

     (defun main ()
       ,(lprint :msg "main")

       (let ((prof_fn (string "satplan.prof")))

	 ;; go tool pprof satplan satplan.prof
	 ,(lprint :msg "start profiling" :vars `(prof_fn))
	 ,(lprint :msg "you can view the profile with: go tool pprof satplan satplan.prof")
	 ,(panic `(:var prof_f
		   :cmd (os.Create prof_fn)))
	 (pprof.StartCPUProfile prof_f)
	 (defer (pprof.StopCPUProfile)))
       
       (let ((dbfn (string "plan.db")))
	 ,(panic `(:var db
		   :cmd (sql.Open (string "sqlite3") dbfn)))
	 ,(tprint :msg "open database" :vars `(dbfn db))
	 ,(panic `(:var exec_res_create
		   :cmd (db.Exec (string "CREATE TABLE IF NOT EXISTS activities (id INTEGER NOT NULL PRIMARY KEY, time DATETIME NOT NULL, description TEXT, value TEXT);"))))
	 (assign (ntuple version version_nr src_id)
		 (sqlite3.Version))
	 ,(lprint :vars `(exec_res_create version version_nr src_id)))
       
       (let ((fn
	       (string 
		"../source00/S1A_MP_USER_20220715T160000_20220804T180000.kml"
		)))
	 ,(panic
	   `(:var kml
	     :pre ,(lprint :msg "open KML file"
			   :vars `(fn))
	     :cmd (os.Open fn)
	     :post (defer (kml.Close))))
	 ,(panic `(:var kmlbytes
		   :pre
		   ,(lprint
		     :msg "read KML file as bytes")
		   :cmd (ioutil.ReadAll kml)
		   ))

	 (do0
	  ,(lprint :msg "read file with CharsetReader because file is in iso-8859-1 (instead of utf-8)")
	  ;(assign kmlbuf (bytes.NewBuffer kmlbytes))
	  (assign reader
		  (bytes.NewReader kmlbytes))
	  (assign decoder
		  (xml.NewDecoder reader))
	  (setf decoder.Strict false)
	  (setf decoder.CharsetReader
		charset.NewReaderLabel))
	 (do0
	  ,(lprint :msg "unmarshall KML with manually constructed go types")
	  "var kmldoc KML"
	  ,(panic0 `(:cmd (decoder.Decode &kmldoc)))
	  )
	 #+nil


	 ,(lprint :msg "result" :vars `(kmldoc ))
					;,(lprint :msg "result" :vars `( kmldoc.Document.Folder))
	 (foreach ;(f kmldoc.Document.Folder.Folders)
		  ((ntuple idx_folder f) (range kmldoc.Document.Folder.Folders))
		  (when (== idx_folder 3)
		    break)
		  ,(lprint :vars `(idx_folder f.Name))
		  
		  (assign bar (progressbar.Default (int64 (len f.Placemarks))))
		  (foreach ;(p f.Placemarks) ;
			   ((ntuple jdx p) (range f.Placemarks))
			   (bar.Add 1)
			   ;,(lprint :vars `(p))
			   (;foreach ((ntuple kdx d) (range (dot p ExtendedData Data)))
			    lo.ForEach (dot p ExtendedData Data)
				       (lambda (d i)
				         (declare (type Data d)
						  (type int i))
					(let ((k (dot d
						      Key))
					      (v (dot d
						      Value)))

					  ;; batch insert could be done but maybe not necessary
					  ;; https://stackoverflow.com/questions/12486436/how-do-i-batch-sql-statements-with-package-database-sql
					  ,(panic `(:var exec_res_insert
						    :cmd (db.Exec (string "INSERT INTO activities VALUES(NULL,?,?,?);")
								  p.DateTime k v)))
					  (when false
					    ,(lprint :vars `(exec_res_insert jdx p.DateTime k v)))
					  
					  ))))
		  )
	 ;,(lprint :msg "result" :vars `( kmldoc.Document.Folder.Placemarks))
	 
	 
	 
	 	 
	 
	 )))))
