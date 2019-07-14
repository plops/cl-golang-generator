(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/06_gopl_ch7_eval")
  (defparameter *code-file* "eval")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (let* ((code
	  `(do0
	    (package main)
	    
	    ;(import fmt sort)
	    (deftype Var ()
	      string)
	    (deftype literal ()
	      float64)
	    (defstruct0 unary
		(op rune)
	      (x Expr)
	      )
	    (defstruct0 binary
		(op rune)
	      (x Expr)
	      (y Expr))
	    (defstruct0 call
		(fn string)
	      (args "[]Expr"))
	    (deftype Env ()
	      "map[Var]float64")
	    (definterface Expr
		(defmethod-interface Eval ((v Var) env)
		  (declare (type Env env)
			   (values float64 &opional))))
	    (defmethod Eval ((v Var) env)
	      (declare (type Env env)
		       (values float64 &opional))
	      (return (aref env v)))
	    (defmethod Eval ((l literal) _)
	      (declare (type Env _)
		       (values float64 &opional))
	      (return (float64 l)))
	    (defmethod Eval ((u unary) env)
	      (declare (type Env env)
		       (values float64 &opional))
	      (ecase u.op
		((char +)
		 (return (u.x.Eval env)))
		((char -)
		 (return (- (u.x.Eval env)))))
	      (panic (fmt.Sprintf
		      (string "unsupported unary operator: %q")
		      u.op)))
	    
	    (defmethod Eval ((b binary) env)
	      (declare (type Env env)
		       (values float64 &opional))
	      (ecase b.op
		,@(loop for op in `(+ - * /) collect
		       `((char ,op)
			 (return (,op (u.x.Eval env)
				      (u.x.Eval env))))))
	      (panic (fmt.Sprintf
		      (string "unsupported binary operator: %q")
		      b.op)))

	    (defmethod Eval ((c call) env)
	      (declare (type Env env)
		       (values float64 &opional))
	      (ecase c.fn
		,@(loop for (name nargs gofun)
		     in `((pow 2 math.Pow)
			  (sin 1 math.Sin)
			  (sqrt 1 math.Sin))
		     collect
		       `((string ,name)
			 (return (,gofun
				  ,@(loop for i below nargs
				       collect
					 `(dot c
					      (aref args ,i)
					      (Eval env))))))))
	      (panic (fmt.Sprintf
		      (string "unsupported function call: %s")
		      c.fn)))
	    
	    )))
    (write-source *source* code)))



