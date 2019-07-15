(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator"))

(in-package :cl-golang-generator)

(progn
  (defparameter *path* "/home/martin/quicklisp/local-projects/cl-golang-generator/examples/06_gopl_ch7_eval")
  (defparameter *code-file* "eval")
  (defparameter *code-test-file* "eval_test")
  (defparameter *source* (format nil "~a/source/~a" *path*  *code-file*))
  (defparameter *source-test* (format nil "~a/source/~a" *path*  *code-test-file*))
  (let* ((code
	  `(do0
	    (package eval)
	    
	    (import fmt math
		    strconv
		    strings
		    text/scanner)
	    (deftype Var ()
	      string)
	    (deftype literal ()
	      float64)
	    (defstruct0 unary
		(op rune)
	      (x Expr))
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
			 (return (,op (b.x.Eval env)
				      (b.x.Eval env))))))
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

	    
	    (defstruct0 lexer
		(scan scanner.Scanner)
	      (token rune))

	    (defmethod next ((lex *lexer))
	      (setf lex.token
		    (lex.scan.Scan)))
	    (defmethod text ((lex *lexer))
	      (declare (values string &optional))
	      (return (lex.scan.TokenText)))
	    (deftype lexPanic ()
	      string)
	    (defun precedence (op)
	      (declare (type rune op)
		       (values int))
	      (ecase op
		((ntuple (char *)
			 (char /))
		 (return 2))
		((ntuple (char +)
			 (char -))
		 (return 1)))
	      (return 0))
	    ;; expr ::= num | id | id '(' exp ',' ... ')'| '-' expr | expr '+' expr
	    (defun Parse (input)
	      (declare (type string input)
		       (values "_ Expr" "err error"))
	      (defer
		  ((lambda ()
		     (case (:= x
				   (dot (recover)
					"(type)")) ;; ch 7.10 type assertion
		       ("nil"
			)
		       (lexPanic
			(setf err (fmt.Errorf (string "%s") x)))
		       (t (panic x))))))
	      (assign lex (new lexer))
	      (lex.scan.Init (strings.NewReader input))
	      (setf lex.scan.Mode
		    (logior scanner.ScanIdents
			    scanner.ScanInts
			    scanner.ScanFloats))
	      (lex.next)
	      (assign e (parseExpr lex))
	      (if (!= lex.token
		      scanner.EOF)
		  (return (ntuple
			   "nil"
			   (fmt.Errorf (string "unexpected")))))
	      (return (ntuple e "nil")))

	    (defun parseExpr (lex)
	      (declare (type *lexer lex)
		       (values Expr))
	      (return (parseBinary lex 1)))
	    ;; binary = unary ('+' binary)*
	    ;; stops when operator of lower precedence than prec1 
	    (defun parseBinary (lex prec1)
	      (declare (type *lexer lex)
		       (type int prec1)
		       (values Expr))
	      (assign lhs (parseUnary lex))
	      (for ((:= prec
			(precedence lex.token))
		    (<= prec1 prec)
		    (decf prec))
		   (while (== (precedence lex.token)
			      prec)
		     (assign op lex.token)
		     (lex.next)
		     (assign rhs (parseBinary lex (+ prec 1)))
		     (setf lhs (curly binary op lhs rhs))))
	      (return lhs))

	    ;; unary = '+' expr | primary
	    (defun parseUnary (lex)
	      (declare (type *lexer lex)
		       (values Expr))
	      (if (or (== lex.token (char +))
		      (== lex.token (char -)))
		  (do0
		   (assign op lex.token)
		   (lex.next) ;; consume operator char
		   (return (curly unary op (parseUnary lex)))))
	      (return (parsePrimary lex)))

	    ;; primary = id|id( expr, ...)| num| (expr)
	    (defun parsePrimary (lex)
	      (declare (type *lexer lex)
		       (values Expr))
	      (ecase lex.token
		(scanner.Ident
		 (assign id (lex.text))
		 (lex.next) ;; consume id
		 (unless (== lex.token (char "("))
		   (return (Var id)))
		 (lex.next) ;; consume (
		 (let (args)
		   (declare (type "[]Expr" args))
		   (unless (== lex.token (char ")"))
		     (while true
		       (setf args (append args (parseExpr lex)))
		       (unless (== lex.token (char ","))
			 break)
		       (lex.next) ;; consume ,
		       )
		     (unless (== lex.token (char ")"))
		       (assign msg (fmt.Sprintf
				    (string "got %q, want )")
				    lex.token))
		       (panic (lexPanic msg))))
		   (lex.next) ;; consume )
		   (return (curly call id args))
		   )
		 )
		((ntuple scanner.Int
			 scanner.Float)
		 
		 (assign (ntuple f err)
			 (strconv.ParseFloat (lex.text) 64))
		 (unless (== err "nil")
		   (panic (lexPanic (err.Error))))
		 (lex.next) ;; consume number
		 (return (literal f))
		 )
		((char "(")
		 (lex.next) ;; consume (
		 (assign e (parseExpr lex))
		 (unless (== lex.token (char ")"))
		   (assign msg (fmt.Sprintf (string "got <?>, want )")))
		   (panic (lexPanic msg)))
		 (lex.next) ;; consume )
		 (return e)
		 ))
	      (assign msg (fmt.Sprintf (string "unexpected at end <?>")))
	      (panic (lexPanic msg)))))
	 (code-test
	  `(do0
	    (package eval)
	    
	    (import fmt math testing
		    )
	    

	    (defstruct0 TestDefinition
		(expr string)
	      (env Env)
	      (want string))
	    (defun TestEval (intest)
	      (declare (type "*testing.T" intest))
	      
	      (assign
	       tests
	       (cast "[]TestDefinition"
		     (braces
		      (braces (string "sqrt(A / pi)")
			      (cast Env (dict
					 ((string "A")
					  87616)
					 ((string "pi")
					  math.Pi)))
			      (string "167"))
		      (braces (string "pow(x,3)+pow(y,3)")
			      (cast Env (dict
					 ((string "x")
					  12)
					 ((string "y")
					  1)))
			      (string "1729")))))
	      (let (prevExpr)
		(declare (type string prevExpr))
		(foreach ((ntuple _ test)
			  (range tests))
			 (if (!= test.expr
				 prevExpr) 
			     (do0
			      (fmt.Printf (string "\\n%s\\n"
						  )
					  test.expr)
			      (setf prevExpr test.expr)))
			 (assign (ntuple expr err)
				 (Parse test.expr))
			 (if (!= err "nil")
			     (do0
			      (intest.Error err)
			      continue))
			 (assign got
				 (fmt.Sprintf
				  (string "%.6g")
				  (expr.Eval test.env)))
			 (fmt.Printf
			  (string "\\t%v => %s\\n")
			  test.env got)
			 (if (!= got test.want)
			     (intest.Errorf
			      (string "%s.Eval() in <env>=%q, want %q")
			      test.expr
			      ;test.env
			      got
			      test.want)))))

	    )))
    (write-source *source* code)
    (write-source *source-test* code-test)))


