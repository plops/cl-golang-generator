(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)
;; sudo dnf install libX11-devel libXcursor-devel libXrandr-devel libXinerama-devel libXi-devel libGL-devel libXxf86vm-devel
(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/30_gomobile"
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
		(format s "module androidexample~%")
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
	encoding/binary
	("." androidexample/cltimelog)
	,@(loop for e in `(app event/lifecycle
			       event/paint
			       event/size
			       event/touch
			       exp/app/debug
			       exp/f32
			       exp/gl/glutil
			       gl)
		collect
		(format nil "golang.org/x/mobile/~a" e))

	)
       ,@(loop for (e f)
	       in `((images *glutil.Images)
		    (fps *debug.FPS)
		    (program gl.Program)
		    (position gl.Attrib)
		    (offset gl.Uniform)
		    (color gl.Uniform)
		    (buf gl.Buffer)
		    (green float32)
		    (touchX float32)
		    (touchY float32))
	       collect
	       (format nil "var ~a ~a" e f))
       (const coordsPerVertex 3
	      vertexCount 3)
       (setf "var triangleData"
	     (f32.Bytes
	      binary.LittleEndian
	      .0 .2 .0 ;; top left
	      .0 .0 .0 ;; btm left
	      .2 .0 .0 ;; btm right
	      ))
       (defun onStop (glctx)
	 (declare (type gl.Context glctx))
	 ,(lprint :msg "onStop")
	 (glctx.DeleteProgram program)
	 (glctx.DeleteBuffer buf)
	 (fps.Release)
	 (images.Release)
	 )
       "var frameCount = 0"
       (defun onPaint (glctx sz)
	 (declare (type gl.Context glctx)
		  (type size.Event sz))



	 
	 (incf green .01)
	 (when (< 1 green)
	   (setf green 0))
					;,(lprint :msg "onPaint" :vars `(green))


	 ,@(loop for e in `((ClearColor .2 .3 .4 1)
			    (Clear gl.COLOR_BUFFER_BIT)
			    (:cmd (UseProgram program)
				  :vars (program))
			    (:cmd (Uniform4f color 0 green 0 1)
				  :vars (green))
			    (:cmd (Uniform2f offset
					     (/ touchX
						(float32 sz.WidthPx))
					     (/ touchY
						(float32 sz.HeightPx)))
				  :vars (touchX touchY
						sz.WidthPx sz.HeightPx))

			    (BindBuffer gl.ARRAY_BUFFER buf)
			    (EnableVertexAttribArray position)
			    (:cmd (VertexAttribPointer
				   position
				   coordsPerVertex
				   gl.FLOAT
				   false
				   0 0)
				  :vars (coordsPerVertex))
			    (:cmd (DrawArrays gl.TRIANGLES
					      0
					      vertexCount)
				  :vars (vertexCount))
			    (DisableVertexAttribArray position))
		 collect
		 (let ((ncmd e)
		       (nvars nil))
		   (when (eq (first e) :cmd)
		     (destructuring-bind (&key cmd vars) e
		       (setf ncmd cmd
			     nvars vars)))
		   `(do0
		     (dot glctx ,ncmd)
		     (when (== frameCount 0)
		       ,(lprint :msg (format nil "onPaint ~a" (emit-go :code ncmd))
				:vars nvars)))))
	 (fps.Draw sz)
	 (incf frameCount)
	 )

       (defun onStart (glctx)
	 (declare (type gl.Context glctx))
	 ,(lprint :msg "onStart")

	 (let ((vert (string-raw "#version 100
uniform vec2 offset;
attribute vec4 position;
void main() {
	// offset comes in with x/y values between 0 and 1.
	// position bounds are -1 to 1.
	vec4 offset4 = vec4(2.0*offset.x-1.0, 1.0-2.0*offset.y, 0, 0);
	gl_Position = position + offset4;
}"))

	       (frag (string-raw
		      "#version 100
precision mediump float;
uniform vec4 color;
void main() {
	gl_FragColor = color;
}")))
	   ,(panic `(:var program
			  :cmd (glutil.CreateProgram
				glctx
				vert frag
				)))
	   (do0

	    (setf buf (glctx.CreateBuffer))
	    ,@(loop for e in `((:cmd (BindBuffer gl.ARRAY_BUFFER buf)
				     :vars (buf))
			       (:cmd (BufferData gl.ARRAY_BUFFER
						 triangleData
						 gl.STATIC_DRAW)
				     :vars (triangleData)))
		 collect
		 (let ((ncmd e)
		       (nvars nil))
		   (when (eq (first e) :cmd)
		     (destructuring-bind (&key cmd vars) e
		       (setf ncmd cmd
			     nvars vars)))
		   `(do0
		     (dot glctx ,ncmd)
		     (when (== frameCount 0)
		       ,(lprint :msg (format nil "onStart ~a" (emit-go :code ncmd))
				:vars nvars)))))
	    
	    
	    
	    
	    ,@(loop for e in `((:name position :type attrib)
			       (:name color :type uniform)
			       (:name offset :type uniform))
		    collect
		    (destructuring-bind (&key name type) e
		      `(setf ,name
			     (dot
			      glctx
			      (,(format nil "Get~aLocation"
					(string-capitalize
					 (format nil "~a" type)))
				program
				(string ,name))))))
	    (setf images (glutil.NewImages glctx)
		  fps (debug.NewFPS images)))))

       (defun main ()
	 ,(lprint :msg (format nil "~@[~a/~]~a" folder name))
	 (dot app
	      (Main (lambda (a)
		      (declare (type app.App a))
		      "var glctx gl.Context"
		      "var sz size.Event"
		      (foreach (ae (range (a.Events)))
			       (assign e (dot a
					      (Filter ae)
					      ))
			       (typecase e
				 (lifecycle.Event
				  ;,(lprint :msg "lifecycle.Event")
				  (case (dot e
					     (Crosses
					      lifecycle.StageVisible))
				    (lifecycle.CrossOn
				     ,(lprint :msg "lifecycle.CrossOn")
				     (setf (ntuple glctx
						   _)
					   (dot e
						(DrawContext.
						 gl.Context)))
				     (onStart glctx)
				     (a.Send (curly paint.Event))
				     )
				    (lifecycle.CrossOff
				     ,(lprint :msg "lifecycle.CrossOff")
				     (onStop glctx)
				     (setf glctx "nil")
				     )))
				 (size.Event
				  ,(lprint :msg "size.Event"
					   :vars `(sz.WidthPx
						   sz.HeightPx))
				  (setf sz e
					touchX (* .5 (float32 sz.WidthPx))
					touchY (* .5 (float32 sz.HeightPx))))
				 (paint.Event
				  (when (logior (== glctx "nil")
						e.External)
				    (comments "skip paint events sent by the system. we already paint as fast as possible")
				    continue)
				  (onPaint glctx sz)
				  (a.Publish)
				  (a.Send (curly paint.Event)))
				 (touch.Event
				  ,(lprint :msg "touch.Event"
					   :vars `(e.X e.Y))
				  (setf touchX e.X
					touchY e.Y)))))))
	 )))))
