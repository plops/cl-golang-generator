(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/24_fuel_scrape"
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

  (let ((name "getFuelPrices"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
					;math
	time
	"github.com/gocolly/colly"
					;"github.com/mattn/go-sqlite3"
	database/sql
					;modernc.org/sqlite
	(_ github.com/mattn/go-sqlite3)
	strings
	)
       ,(lprint-init)

       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 (assign c (colly.NewCollector
		    (colly.UserAgent (string "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))))
	 (c.Limit
	  (curly &colly.LimitRule
		 :DomainGlob (string "www.makro.nl/*")
		 :Delay (* 3 time.Second)
		 :RandomDelay (* 1 time.Second))
	  )

	 (do0
	  (assign cityName (string "None"))

	  ,@(loop for e in `(#+nil (:name OnRequest :cb-types (*colly.Request) :vars (p0.URL))
				   #+nil (:name OnHTML :params ((string "a[href]"))
						:cb-types (*colly.HTMLElement)
					; :cb-code (p0.Request.Visit (p0.Attr (string "href")))
						)
				   #+nil (:name OnHTML :params ((string "div.field-price"))
						:cb-types (*colly.HTMLElement)
						:vars (p0.Text))
				   #+nil (:name OnHTML :params ((string "div.field-name"))
						:cb-types (*colly.HTMLElement)
						:vars (p0.Text))
				   (:name OnHTML :params ((string "div.price.slide.element-position"))
					  :cb-types (*colly.HTMLElement)
					  :cb-code (do0
						    (assign spl (strings.Split p0.Text (string "€"))

							    )
						    (if (<= 3 (len spl))
							(do0
							 (assign name (aref spl 0)
								 price (aref spl 2))
							 ,(lprint :vars `(cityName name price)))
							(do0
							 (assign spl (strings.Split p0.Text
										    (string "€ / liter")))
							 (if (<= 2 (len spl))
							     (do0
							      (assign name (aref spl 0)
								      price (aref spl 1))
							      ,(lprint :vars `(cityName name price)))
							     (do0
							      ,(lprint :msg "cant parse"
								       :vars `(cityName p0.Text))))
							 ))))
				   #+nil (:name OnError :params ()
						:cb-types (*colly.Response error)
						:vars (p0.Request.URL p1))
				   #+nil (:name OnResponseHeaders :params ()
						:cb-types (*colly.Response)
						:vars (p0.Request.URL)
						)
				   #+nil (:name OnResponse :params ()
						:cb-types (*colly.Response)
						:vars (p0.Request.URL)
						)
				   #+nil (:name OnScraped :params ()
						:cb-types (*colly.Response)
						:vars (p0.Request.URL)
						))
		  collect
		  (destructuring-bind (&key name params cb-types cb-code vars)
		      e
		    `(dot c
			  (,name
			   ,@params
			   (lambda ,(loop for f in cb-types
					  and f-i from 0
					  collect (intern (string-upcase (format nil "p~a" f-i))))
			     ,@(loop for f in cb-types
				     and f-i from 0
				     collect
				     `(declare (type ,f ,(intern (string-upcase (format nil "p~a" f-i))))))
			     ,(lprint :msg (format nil "~a ~{~a~}" name (mapcar #'(lambda (x) (substitute #\' #\" (emit-go :code x))) params))
				      :vars vars)
			     ,cb-code))))

		  ))

	 (do0
	  ,(panic `(:var db
			 :cmd (sql.Open (string "sqlite3")
					(string "fuel.db"))))
	  ,(panic `(:var _
			 :cmd (db.Exec (string "CREATE TABLE IF NOT EXISTS fuel ( id INTEGER NOT NULL PRIMARY KEY, time DATETIME NOT NULL, description TEXT );")))))

	 
	 (do0
	  (assign makros_with_gas_station
		  (curly []string

			 ,@(loop for e in `(
					    amsterdam
					    best
					    breda
					;barendrecht
					;beverwijk
					    delft
					;dordrecht
					    duiven
					    groningen
					; hengelo
					; leeuwarden
					    nuth
					; nijmegen
					; nieuwegein
					; vianen
					; wateringen
					; s-hertogenbosch
					    )
				 collect
				 `(string ,e)
				 )))
	  (foreach ((ntuple _ name) (range makros_with_gas_station))
		   (setf cityName name)
		   (c.Visit (+ (string ,(format nil "https://www.makro.nl/vestigingen/"))
			       name))))
	 )))))