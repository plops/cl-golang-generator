(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/02_gopl_ch1_fetchall")
  (defparameter *code-file* "fetchall")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import fmt io io/ioutil net/http os
		    time)
	    (defun main ()
	      (assign start (time.Now)
		      ch (make (chan string)))
	      (foreach ((ntuple _ url) (range (aref os.Args "1:")))
		   (go (fetch url ch)))
	      (foreach ((range (aref os.Args "1:")))
		       (fmt.Println <-ch))
	      (fmt.Printf (string "%.2fs elapsed\\n") (dot (time.Since start)
							   (Seconds))))
	    (defun fetch (url ch)
	      (declare (type string url)
		       (type "chan<- string" ch))
	      (assign start (time.Now)
		      (ntuple resp err) (http.Get url))
	      (if (!= err "nil" )
		  (do0
		   (<- ch (fmt.Sprint err))
		   return))
	      (assign (ntuple nbytes err) (io.Copy ioutil.Discard resp.Body))
	      (resp.Body.Close)
	      (if (!= err "nil")
		  (do0
		   (<- ch (fmt.Sprintf (string "while reading %s: %v" url err)))
		   return))
	      (assign secs (dot (time.Since start)
				(Seconds)))
	      (<- ch (fmt.Sprintf (string "%.2fs %7d %s" secs nbytes url)))))))
    (write-source *source* code)))


;; go build echo.go
