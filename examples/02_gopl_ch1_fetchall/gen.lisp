(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/01_gopl_ch1_fetchall")
  (defparameter *code-file* "fetchall")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import fmt io io/ioutil net/http os time)
	    (defun main ()
	      (assign start (time.Now)
		      ch (make (chan string)))))))
    (write-source *source* code)))


;; go build echo.go
