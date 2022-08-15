(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

;; try to use pure go implementation of imgui

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/31_pure_imgui"
	    (user-homedir-pathname)))
  (defparameter *idx* "00")
  (defun lprint-init ()
    `(defun TimeNow ()
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
      (TimeNow)
      ,@vars
      ))
  (defun tprint (&key (msg "") vars)
    "generate go code to print variables in log output with their types"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v (%T)~^ ~}\\n"
		msg vars))
      (TimeNow)
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
	  (incf err-nr))))
    (defun properr (cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (unless (== ,err "nil")
		(return ,err))
	      )
	  (incf err-nr)))))

  (let ((file-count 0))
    (defun write-go (&key name code folder)
      (prog1
	  (progn
     	    (let ((dir-name (format nil "~a/source~a/~@[~a/~]"
				    *path*
				    *idx*
				    folder)))
	      (format t "ensure dir-name exists ~a" dir-name)
	      (ensure-directories-exist
	       dir-name))
	    (unless folder
	      (with-open-file (s (format nil "~a/source~a/go.mod"
					 *path*
					 *idx*
					 )
				 :direction :output
				 :if-exists nil
				 :if-does-not-exist :create)
		(format s "module puregoexample~%")
		(format s "go 1.18~%")))
	    (write-source
	     (format nil "~a/source~a/~@[~a/~]g~2,'0d_~a"
		     *path* *idx*
		     folder file-count name)
	     code))
	(incf file-count))))

  (let ((name "cltimelog")
	(folder "cltimelog"))
    (write-go
     :name name
     :folder folder
     :code
     `(do0
       (package cltimelog)
       (import
	time
					;fmt
	)
       ,(lprint-init)
       )))

  (let ((name "main")
	(folder nil))
    (write-go
     :name name
     :folder folder
     :code
     `(do0
       (package main)
       (import
					;log
					;time
	fmt
	time
	;encoding/binary
	("." puregoexample/cltimelog)
	github.com/splizard/imgui
	(platforms github.com/splizard/imgui/example/platforms/glfw)
	github.com/splizard/imgui/example/renderers)
       
       (definterface Renderer
	   (defmethod-interface PreRender ((v Var) clearColor)
	     (declare (type [3]float32 clearColor)))
	 (defmethod-interface Render ((v Var)
				      displaySize
				      framebufferSize
				      drawData)
	   (declare (type [2]float32 displaySize
			  framebufferSize)
		    (type imgui.ImDrawData drawData))))
       (defun main ()
	 ,(lprint :msg (format nil "~@[~a/~]~a" folder name))
	 (imgui.CreateContext "nil")
	 (assign io (imgui.GetIO))
	 ,(panic `(:var p
		   :cmd (platforms.NewGLFW io
					   platforms.GLFWClientAPIOpenGL3)))
	 (defer (p.Dispose))
	 ,(panic `(:var r
		   :cmd (renderers.NewOpenGL3 io)))
	 (defer (r.Dispose))
	 (while
	  (not (p.ShouldStop))
	  (p.ProcessEvents)
	  (p.NewFrame)
	  (imgui.NewFrame)
	  (imgui.ShowMetricsWindow "nil")
	  (imgui.ShowDemoWindow "nil")
	  (dotimes (i 5)
	    (imgui.Text (string "the quick brown fox jumped over the lazy dog")))

	  (imgui.Render)
	  (r.PreRender (curly [3]float32 .45 .55 .6))
	  (r.Render (p.DisplaySize)
		    (p.FramebufferSize)
		    (imgui.GetDrawData))
	  (p.PostRender)
	  (<- (time.After (* time.Millisecond 10))))
	 
	 )))))
