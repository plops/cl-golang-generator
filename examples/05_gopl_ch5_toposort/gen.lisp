(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/05_gopl_ch5_toposort")
  (defparameter *code-file* "toposort")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    (import fmt sort)
	    (let ((prereqs (cast "map[string][]string"
				 (dict
				  ,@(loop for e in `((algorithm (data-structures))
						     (calculus (linear-algebra))
						     (compilers (data-structures
								 formal-languages
								 computer-organization))
						     (data-structures (discrete-math))
						     (databases (data-structures))
						     (discrete-math (intro-to-programming))
						     (formal-languages (discrete-math))
						     (networks (operating-systems))
						     (operating-systems (data-structures
									 computer-organization))
						     (programming-languages (data-structures
									     computer-organization)))
				       collect
					 (destructuring-bind (name components) e
					   `((string ,name) (braces ,@(mapcar
								       (lambda (x)
									 `(string ,x))
								       components)))))))))
	      (defun main ()
		(foreach ((ntuple i course) (range (topoSort prereqs)))
			 (fmt.Printf (string "%d:\\t%s\\n") (+ i 1) course)))
	      (defun topoSort (m)
		(declare (type "map[string][]string" m)
			 (values "[]string" &optional))
		(let (order
		      keys
		      visitAll ;; needs to be explicitly defined for recursion
		      )
		  (declare (type "[]string" order keys)
			   (type "func(items []string)" visitAll)
			   )
		  (assign seen (make "map[string]bool"))
		  
		  (setf visitAll (lambda (items)
				     (declare (type "[]string" items))
				     (foreach ((ntuple _ item)
					       (range items))
					      (if (not (aref seen item))
						  (do0
						   (setf (aref seen item) true)
						   (visitAll (aref m item))
						   (setf order (append order item)))))))
		  (foreach (key (range m))
			   (setf keys (append keys key)))
		  (sort.Strings keys)
		  (visitAll keys)
		  (return order)))))))
    (write-source *source* code)))


