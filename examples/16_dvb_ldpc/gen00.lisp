(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

;; call
;; go mod tidy
;; on first run


(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/16_dvb_ldpc"
	    (user-homedir-pathname)))
  (defun lprint (&key (msg "") vars)
    `(fmt.Printf (string ,(format nil "%v ~a ~{~a=%v~^ ~}\\n" msg vars))
		 (dot time
		      (Now)
		      (Format
		       (string "2017-09-07 17:06:04.000000")))
		 
		 ,@vars
		 ))
  (let ((file-count 0))

    (defun write-go (name code)
      (prog1
	  (write-source (format nil "~a/source/~2,'0d_~a" *path* file-count name)
			code)
	(incf file-count))))

  ;; overwriting this file makes it necessary to call setup01_tidy again
  (with-open-file (s (format nil "~a/source/go.mod"
			     *path*)
		     :direction :output
		     :if-exists nil
		     :if-does-not-exist :create)
    (format s "module ldpc~%")
    (format s "go 1.18~%"))
  (let* (;; parity check matrix (n-k) x m
	 ;; length N=8, m=4 check nodes
	 (H `((1 0 0 1 1 0 0 1)
	      (0 1 1 0 1 0 1 0)
	      (1 0 1 0 0 1 0 1)
	      (0 1 0 1 0 1 1 0)))
	 (H-N (length (elt H 0)))
	 (H-M (length H)))
    (write-go
     "main"
     `(do0
       (package main)
       (import fmt
	       time
	       math/rand
	       math/big)
       
       (defun main ()
	 (rand.Seed (int64 0))
	 (do0
	  "var bit_nodes big.Int"
	  "var check_nodes big.Int"
	  ,@(loop for i in `(0 3 4 7)
		  collect
		  (progn
		    (assert (< i H-N))
		    `(bit_nodes.SetBit
		      &bit_nodes
		      ,i
		      1)))
	  ,@(loop
	      for m below H-M
	      collect
	      (let ((parity
		      (remove-if
		       #'null
		       (loop
			 for val
			   in (elt H m)
			   and val-i from 0
			 collect
			 (when (eq val 1)
			   `(dot bit_nodes
				 (Bit
				  ,val-i)))))))
		   `(dot check_nodes
			 (SetBit
			  &check_nodes
			  ,m
			  (logior ,@parity)))))
	  )
	 ,(lprint :vars `(bit_nodes (time.Now)))
	 ,(lprint :vars `(check_nodes))
	 )
       ))))
