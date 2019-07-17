(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/10_gopl_ch9_concurrent_memoization_channel")
  (defparameter *code-file* "memo")
  
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))


  
  
  (let* ((code
	  `(do0
	    (package main)
	    "// concurrent memoization using communicating sequential processes"
	    "// 1) monitor goroutine synchronizes access to map"
	    "// 2) get sends message with key and response channel"
	    "// 3a) "
	    "// 3b) "
	    
	    (import sync)

	    (deftype Func ()
	      (defun-declaration func (key)
		(declare (type string key)
			 (values "interface{}"
				 error))))

	    (defstruct0 result
		(value "interface{}")
	      (err error))

	    
	    
	    (defstruct0 entry
		(res result)
	      (ready "chan struct{}"))

	    (defstruct0 request
		(key string)
	      (response "chan<- result"))
	    
	    
	    (defstruct0 Memo
		(requests "chan request"))

	    (defun New (f)
	      (declare (type Func f)
		       (values *Memo))
	      "// clients have to subsequently call close"
	      (assign memo 
		      (curly &Memo
			     :requests (make "chan request")))
	      (go (memo.server f))
	      (return memo))
	    
	    (defmethod Get ((memo *Memo) key)
	      (declare (type string key)
		       (values "interface{}" error))
	      (assign response (make "chan resul"))
	      (<- memo.requests (curly request key response))
	      (assign res (<- response))
	      (resturn (ntuple res.value
			       res.err)))
	    (defmethod Close ((memo *Memo))
	      (close memo.requests))

	    (defmethod server ((memo *Memo) f)
	      (declare (type Func f))
	      (assign cache (make "map[string]*entry"))
	      (foreach (req (range memo.requests))
		       (assign e (aref cache req.key))
		       (when (== e "nil")
			 "// first request for key"
			 (setf e (curly &entry :ready (make "chan struct{}"))
			       (aref cache req.key) e)
			 (go (e.call f req.key)))
		       (go (e.deliver req.response))))

	    (defmethod call ((e *entry) f key)
	      (declare (type Func f)
		       (type string key))
	      (setf (ntuple e.res.value
			    e.res.err
			    )
		    (f key))
	      (close e.ready))
	    (defmethod deliver ((e *entry) response)
	      (declare (type "chan<- result" response))
	      (<- e.ready)
	      (<- response e.res)))))
    (write-source *source* code)))


