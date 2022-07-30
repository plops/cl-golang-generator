(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/22_leet"
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
	     code)
	    )
	(incf file-count))))

  (let ((name "main674longestSubsequence"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
	time
	)
       ,(lprint-init)

       (defun traverse (nums)
	 (declare (type []int nums)
		  (values int))
	 (when (== 0 (len nums))
	   (return 0))

	 (let ((res 1)
	       (length 1)
	       (old (aref nums 0)))
	   (foreach ((ntuple idx el) (range (aref nums (slice 1 ""))))
		    (if (< old el)
			(incf length)
			(do0
			 ,(lprint :msg "sequence broken" :vars `(old el idx res length))
			 (setf res (max res length)
			       length 1)))
		    (setf old el))
	   (return (max res length))))
       (defun max (a b)
	 (declare (type int a b)
		  (values int))
	 (if (< b a)
	     (return a))
	 (return b))

       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 ,(lprint :msg "should be 3" :vars `((traverse (curly []int 1 3 5 4 7))))
	 ,(lprint :msg "should be 1" :vars `((traverse (curly []int  2 2 2 2 2
							      ))))

	 ))))
  (let ((name "main946validateStack"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
	time
	)
       ,(lprint-init)

       (defun validate (pushed popped)
	 (declare (type []int pushed popped)
		  (values bool))


	 (let ((stack (curly []int))
	       (j 0)
	       (N (len pushed)))
	   (foreach ((ntuple _ el) (range pushed))
		    (setf stack (append stack el))
		    (while (and
			    (!= (len stack) 0)
			    (< j N)
			    (== (aref stack
				      (- (len stack)
					 1))
				(aref popped j)))
		      (setf stack (aref stack (slice 0 (- (len stack)
							  1))))
		      (incf j)))
	   (return (== j N))))


       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 ,(lprint :msg "should be true" :vars `((validate (curly []int 1 2 3 4 5)
							  (curly []int 5 4 3 2 1))))
	 ,(lprint :msg "should be false" :vars `((validate (curly []int 1 2 3 4 5)
							   (curly []int 4 3 5 1 2))))))))


  (let ((name "main004median"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
	time
	github.com/montanaflynn/stats
	github.com/kycklingar/MinMax
	)
       ,(lprint-init)

       (comments "median of two sorted arrays")

       (defun median_slow (a b)
	 (declare (type []float64 a b)
		  (values float64))
	 (assign c (append a b...))
	 ,(panic `(:var med
			:cmd (stats.Median c)))
	 (return med ))


       (const

	,@(loop for (name code )
		in `((red 31)
		     (green 32)
		     (yellow 33)
		     (blue 34)
		     (magenta 35)
		     (cyan 36)
		     (white 37))
		appending
		`(,(string-upcase (format nil "~a" name))
		   (string ,(format nil "\\033[1;~am%02d\\033[0m" code))))
	)

       ,(flet ((show (&key color (a "nums1Mid") (b "nums2Mid"))
		 (if color
		     `(progn
			(foreach ((ntuple idx v) (range nums1))
				 (if (== idx ,a)
				     (fmt.Printf ,color v)
				     (fmt.Printf WHITE v)))
			(fmt.Printf (string "|"))
			(foreach ((ntuple idx v) (range nums2))
				 (if (== idx ,b)
				     (fmt.Printf ,color v)
				     (fmt.Printf WHITE v)))
			(fmt.Printf (string "\\n")))
		     `(progn
			(foreach ((ntuple _ v) (range nums1))
				 (fmt.Printf WHITE v))
			(fmt.Printf (string "|"))
			(foreach ((ntuple _ v) (range nums2))
				 (fmt.Printf WHITE v))
			(fmt.Printf (string "\\n"))))
		 ))
	  `(defun median (nums1 nums2)
	     (declare (type []int nums1 nums2)
		      (values float64))

					;,(show :color nil)


	     (when (< (len nums2)
		      (len nums1))
	       (return (median nums2 nums1)))
	     (assign low 0
 		     high (len nums1)
		     k (/ (+ 1
			     (len nums1)
			     (len nums2))
			  2)
		     nums1Mid 0
		     nums2Mid 0)
	     (while
		 (<= low high)
		 
	       (setf nums1Mid (+ low
				 (/ (- high
				       low)
				    2)))
	       (setf nums2Mid (- k
				 nums1Mid))
	       ,(lprint :msg "l264" :vars `(low high k nums1Mid nums2Mid))
	       ,(show :color 'RED :a `(- nums1Mid 1)
		      :b `(- nums2Mid 1))
	       (if (logand (< 0 nums1Mid)
			   (< (aref nums2 nums2Mid)
			      (aref nums1 (- nums1Mid 1))))
		   (setf high (- nums1Mid
				 1))
		   (if (logand (!= nums1Mid
				   (len nums1))
			       (< (aref nums1 nums1Mid)
				  (aref nums2 (- nums2Mid
						 1))))
		       (setf low (+ 1 nums1Mid))
		       break)))
	     (assign midLeft 0
		     midRight 0)
	     (if (== 0 nums1Mid)
		 (do0 (setf midLeft (aref nums2 (- nums2Mid 1)))
		      ,(show :color 'GREEN :a 0 :b `(- nums2Mid 1)))
		 (if (== 0 nums2Mid)
		     (do0
		      (setf midLeft (aref nums1 (- nums1Mid 1)))
		      ,(show :color 'YELLOW :a `(- nums1Mid 1) :b 0))
		     (do0
		      (setf midLeft (mm.Max (aref nums1 (- nums1Mid 1))
					    (aref nums2 (- nums2Mid 1))
					    ))
		      ,(show :color 'MAGENTA :a `(- nums1Mid 1) :b `(- nums2Mid 1))
		      )))
	     ,(lprint :msg "L294" :vars `(midLeft))
	     (when (== 1 (and (+ (len nums1)
				 (len nums2))
			      1))
	       (return (float64 midLeft)))
	     (if (== nums1Mid
		     (len nums1))
		 (setf midRight (aref nums2 nums2Mid))
		 (if (== (len nums2)
			 nums2Mid)
		     (setf midRight (aref nums1 nums1Mid))
		     (setf midRight (mm.Min (aref nums1 nums1Mid)
					    (aref nums2 nums2Mid)))))
	     ,(lprint :msg "L307" :vars `(midRight))
	     (return (* .5
			(float64 (+ midLeft
				    midRight))))))

       #+nil
       (defun median (a b)
	 (declare (type []float64 a b)
		  (values float64))

	 (assign A a
		 B b)
	 (when (< (len B) (len A))
	   ,(lprint :msg "swap arrays so that A is the shorter one")
	   (setf B a
		 A b))

	 (do0

	  (assign midA (/ (len A) 2)
		  midB (/ (len B) 2))
	  (assign condition
		  (logand
		   (<= (aref A (- midA 1))
		       (aref B midB)
		       )
		   (<= (aref B (- midB 1))
		       (aref A midA)
		       )))
	  ,(lprint :msg "search for split line in both arrays"
		   :vars `(midA midB condition)))

	 (return 0.0))



       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 ,(let ((l `((:result 2.5 :A (1 2) :B (3 4))
		     (:result 2 :A (1 3) :B (2))
		     (:result 5 :A (1 3 4 5 6 7 8 ) :B (2 3 5 7 8 9))
		     (:result 8.5 :A (1 3 4 5 6 7 8 9 10 11 12 17 90 91 92 93 99) :B (1 2 3 4 5 5 5 5 20 30 33 34 45)))))
	    `(do0
	      ,@(loop for e in l
		      collect
		      (destructuring-bind (&key result A B) e
			`(do0
			  (progn
			    (assign res (median_slow (curly []float64 ,@A)
						     (curly []float64 ,@B)
						     )
				    )
			    ,(lprint :msg (format nil "should be ~a" result)
				     :vars `((median_slow (curly []float64 ,@A)
							  (curly []float64 ,@B)
							  )
					     (== res ,result))))
			  (progn
			    (assign res (median (curly []int ,@A)
						(curly []int ,@B)
						)
				    )
			    ,(lprint :msg (format nil "should be ~a" result)
				     :vars `(#+nil (median (curly []int ,@A)
						     (curly []int ,@B)
						     )
					     (== res ,result))))))))))))))
