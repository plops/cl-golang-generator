(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/09_gopl_ch9_concurrent_memoization_mutex")
  (defparameter *code-file* "memo")
  
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))


  
  
  (let* ((code
	  `(do0
	    (package main)
	    "// concurrent memoization using shared variables and locks:"
	    "// 1) call to Get acquires mutex lock on the cache"
	    "// 2) is there an existing entry?"
	    "// 3a) yes: wait for ready condition"
	    "// 3b) no: write 'not ready' into the map, when result arrives"
	    "//     replace with result and broadcast ready"
	    
	    
	     
	    (import sync)
	    
	    (defstruct0 entry
		(res result)
	      (ready "chan struct{}"))
	    (defun New (f)
	      (declare (type Func f)
		       (values *Memo))
	      (return (curly &Memo
			     :f f
			     :cache (make "map[string]*entry"))))
	    (defstruct0 Memo
		(f Func)
	      (mu sync.Mutex)
	      (cache "map[string]*entry"))

	    (defmethod Get ((memo *Memo) key)
	      (declare (type string key)
		       (values "interface{}" error))
	      (memo.mu.Lock)
	      (assign e (aref memo.cache key))
	      (if (== e "nil")
		  (do0
		   "// 3b) first request for key"
		   (setf e (curly &entry :ready (make "chan struct{}"))
			 (aref memo.cache key) e
			 )
		   (memo.mu.Unlock)
		   (setf (ntuple e.res.value
				 e.res.err)
			 (memo.f key))
		   (close e.ready))
		  (do0
		   "// 3a) repeat request"
		   (memo.mu.Unlock)
		   (<- e.ready)))
	      (return (ntuple e.res.value
			      e.res.err))))))
    (write-source *source* code)))


