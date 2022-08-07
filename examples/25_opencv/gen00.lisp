(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))
;; https://gocv.io/writing-code/hello-video/

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/25_opencv"
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

  (let ((name "getVideo"))
    (write-go
     name
     `(do0
       (package main)
       (import
	fmt
					;math
	time
	image
	gocv.io/x/gocv
	)
       ,(lprint-init)

       (defun main ()
	 ,(lprint :msg (format nil "~a" name))
	 ,(panic `(:var cam
			:cmd (gocv.VideoCaptureDevice 0)))
	 ,@(loop for (e f) in `((VideoCaptureFrameWidth 320)
				(VideoCaptureFrameHeight 240))
		 collect
		 `(cam.Set (dot gocv ,e)
			   ,f))
	 (assign win (gocv.NewWindow (string "hello"))
		 img0 (gocv.NewMat)
		 img1 (gocv.NewMat)
		 img2 (gocv.NewMat)
		 clahe ;(gocv.NewCLAHE)
		 (gocv.NewCLAHEWithParams 13.0
					  (curly image.Point
						 8 8
					;16 16
					;32 24
						 ))
		 )
	 (for ()
	      (cam.Read &img0)
	      (gocv.CvtColor img0
			     &img1
			     gocv.ColorBGRToGray)
	      (clahe.Apply img1 &img2)
	      (win.IMShow img2)
	      (win.WaitKey 1))
	 (clahe.Close)
	 ,(panic0 `(cam.Close))

	 )))))
