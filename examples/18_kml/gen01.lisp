(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/18_kml"
	    (user-homedir-pathname)))
  (defparameter *idx* "01")
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
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v~^ ~}\\n"
		msg vars))
      (timeNow)
      ,@vars
      ))
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
  (write-go
   "main"
   `(do0
     (package main)
     (import fmt
	     time
	     
	     io/ioutil
	     os
	     ;golang.org/x/net/html/charset
	     ;encoding/xml
	     bytes
	     github.com/amundsentech/kml-decode
	     )
     ,(lprint-init)
     (defun main ()
       ,(lprint :msg "main")
       (let ((fn
	       (string ;;"../source00/KML_Samples.kml" i think the
		;; following file isn't decoded properly because it is
		;; not in UTF-8:
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
	 (assign kmlbuf (bytes.NewBuffer kmlbytes))
	 	 
	 (do0
	  "var kmlstruct kmldecode.KML"
	  (kmldecode.KMLDecode kmlbuf 
			       &kmlstruct)
	  #+nil
	  ,(panic0 `(:cmd (kmldecode.KMLDecode kmlbuf 
					       &kmlstruct))))
	 ,(lprint :vars `(kmlstruct))
	 )))))
