(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/09_gopl_ch9_concurrent_memoization")
  (defparameter *code-file* "memo")
  
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))


  
  
  (let* ((code
	  `(do0
	    (package main)
	    "// concurrent memoization using shared variables and locks:"
	    "// 1) call to Get acquires mutex lock on the cache"
	    "// 2) is there an existing entry?"
	    "// 3a) yes: wait for ready condition"
	    "// 3b) no: write 'not ready' into the map, when result arrives replace with result"
	    
	    
	     
	    (import sync)
	    
	    (defstruct0 entry
		(res result)
	      (ready "chan struct{}")))))
    (write-source *source* code)))


