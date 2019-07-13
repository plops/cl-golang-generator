(ql:quickload "alexandria")

(defpackage :trial
  (:use :cl
	;:optima
	:alexandria))
(in-package :trial)

(setf (readtable-case *readtable*) :invert)

;; http://clhs.lisp.se/Body/s_declar.htm
;; http://clhs.lisp.se/Body/d_type.htm

;; go through the body until no declare anymore

(defun consume-declare (body)
  "take a list of instructions (body), parse type declarations,
return the body without them and a hash table with an environment. the
entry return-values contains a list of return values"

  ;; (declare (type int a b))
  ;; (declare (values int &optional))
  ;; (declare (values int float &optional))
  (let ((env (make-hash-table))
	(looking-p t)
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (progn
		      (when (and (listp (second e))
				 (eq (first (second e)) 'type))
			(destructuring-bind (type-symb type &rest vars) (second e)
			  (loop for var in vars do
			       (setf (gethash var env) type))))
		      (when (and (listp (second e))
				 (eq (first (second e)) 'values))
			(destructuring-bind (symb &rest types-opt) (second e)
			  (let ((types nil))
			    ;; only collect types until occurrance of &optional
			    (loop for type in types-opt do
				 (unless (eq #\& (aref (format nil "~a" type) 0))
				   (push type types)))
			    (setf (gethash 'return-values env) (reverse types))))))
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values (reverse new-body) env)))

(defun lookup-type (name &key env)
  "get the type of a variable from an environment"
  (gethash name env))

(defun parse-let (code emit)
  "let ({var | (var [init-form])}*) declaration* form*"
  (destructuring-bind (decls &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (with-output-to-string (s)
	(format s "~a"
		(funcall emit
			`(do0
			  ,@(loop for decl in decls collect
			       (destructuring-bind (name &optional value) decl
				 (format nil "var ~a~@[ ~a~]~@[ = ~a~]"
					 name
					 (lookup-type name :env env)
					 (funcall emit value))))
			  ,@body)))))))



(defun parse-setf (code emit)
  "setf {pair}*"
  (let ((args (cdr code)))
   (format nil "~a"
	   (funcall emit
		    `(do0 
		      ,@(loop for i below (length args) by 2 collect
			     (let ((a (elt args i))
				   (b (elt args (+ 1 i))))
			       `(= ,a ,b))))))))

(progn
  (defun emit-go (&key code (str nil)  (level 0))
    (flet ((emit (code &optional (dl 0))
	     "change the indentation level. this is used in do"
	     (emit-go :code code :level (+ dl level))))
      (if code
	  (if (listp code)
	      (case (car code)
		(paren
		 ;; paren {args}*
		 (let ((args (cdr code)))
			 (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		(indent
		 ;; indent form
		 (format nil "~{~a~}~a"
				;; print indentation characters
			      (loop for i below level collect "    ")
			      (emit (cadr code))))
		(do (with-output-to-string (s)
		      ;; do {form}*
		      ;; print each form on a new line with one more indentation.
		      (format s "~{~&~a~}" (mapcar #'(lambda (x) (emit `(indent ,x) 1)) (cdr code)))))
		(do0 (with-output-to-string (s)
		       ;; do0 {form}*
		       ;; write each form into a newline, keep current indentation level
		     (format s "~&~a~{~&~a~}"
			     (emit (cadr code))
			     (mapcar #'(lambda (x) (emit `(indent ,x) 0)) (cddr code)))))
		(let (parse-let code #'emit))
		(defun
		    ;;  defun function-name lambda-list [declaration*] form*
		    ;; https://golang.org/ref/spec#Function_declarations
		    (destructuring-bind (name lambda-list &rest body) (cdr code)
		      (multiple-value-bind (req-param opt-param res-param
						      key-param other-key-p
						      aux-param key-exist-p)
			 (parse-ordinary-lambda-list lambda-list)
		       (declare (ignorable req-param opt-param res-param
					   key-param other-key-p aux-param key-exist-p))
		       (with-output-to-string (s)
			 (format s "func ~a~a:~%"
				 name
				 (emit `(paren ,@(append req-param
							 (loop for e in key-param collect 
							      (destructuring-bind ((keyword-name name) init suppliedp)
								  e
								(declare (ignorable keyword-name suppliedp))
								(if init
								    `(= ,name ,init)
								    `(= ,name "None"))))))))
			 (format s "~a" (emit `(do ,@body)))))))
		(setf (parse-setf code #'emit))
		(+ (let ((args (cdr code)))
		     ;; + {summands}*
		     (format nil "(~{(~a)~^+~})" (mapcar #'emit args))))
		(- (let ((args (cdr code)))
		     (if (eq 1 (length args))
			 (format nil "(-(~a))" (emit (car args)))
			 (format nil "(~{(~a)~^-~})" (mapcar #'emit args)))))
		(* (let ((args (cdr code)))
		     (format nil "(~{(~a)~^*~})" (mapcar #'emit args))))
		(/ (let ((args (cdr code)))
		     (if (eq 1 (length args))
			 (format nil "(1.0/(~a))" (emit (car args)))
			 (format nil "(~{(~a)~^/~})" (mapcar #'emit args)))))
		(logior (let ((args (cdr code)))
			  (format nil "(~{(~a)~^|~})" (mapcar #'emit args))))
		(logand (let ((args (cdr code)))
			  (format nil "(~{(~a)~^&~})" (mapcar #'emit args))))
		(or (let ((args (cdr code)))
		      (format nil "(~{(~a)~^||~})" (mapcar #'emit args))))
		(and (let ((args (cdr code)))
		      (format nil "(~{(~a)~^&&~})" (mapcar #'emit args))))
		(= (destructuring-bind (a b) (cdr code)
		     ;; = pair
		     (format nil "~a=~a" (emit a) (emit b))))
		(:= (destructuring-bind (a b) (cdr code)
		      (format nil "~a:=~a" (emit a) (emit b))))
		(/= (destructuring-bind (a b) (cdr code)
		      (format nil "~a/=~a" (emit a) (emit b))))
		(^= (destructuring-bind (a b) (cdr code)
		      (format nil "~a^=~a" (emit a) (emit b))))
		(<= (destructuring-bind (a b) (cdr code)
		      (format nil "~a<=~a" (emit a) (emit b))))
		(< (destructuring-bind (a b) (cdr code)
		     (format nil "~a<~a" (emit a) (emit b))))
		(<< (destructuring-bind (a b) (cdr code)
		     (format nil "~a<<~a" (emit a) (emit b))))
		(>> (destructuring-bind (a b) (cdr code)
		      (format nil "~a>>~a" (emit a) (emit b))))
		(incf (destructuring-bind (a &optional b) (cdr code)
			(if b
			    (format nil "(~a)+=(~a)" (emit a) (emit b))
			    (format nil "(~a)++" (emit a)))))
		(decf (destructuring-bind (a &optional b) (cdr code)
			(if b
			    (format nil "(~a)-=(~a)" (emit a) (emit b))
			    (format nil "(~a)--" (emit a)))))
		(string (format nil "\"~a\"" (cadr code)))
		(slice (let ((args (cdr code)))
		       (if (null args)
			   (format nil ":")
			   (format nil "~{~a~^:~}" (mapcar #'emit args)))))
		(aref (destructuring-bind (name &rest indices) (cdr code)
		      (format nil "~a[~{~a~^,~}]" (emit name) (mapcar #'emit indices))))
		#+nil (-> (let ((forms (cdr code)))
		      ;; clojure's thread first macro, thrush operator
		      ;; http://blog.fogus.me/2010/09/28/thrush-in-clojure-redux/
		      ;; -> {form}*
		      (emit (reduce #'(lambda (x y) (list (emit x) (emit y))) forms))))
		(t (destructuring-bind (name &rest args) code
		     (progn ;if
		       #+nil(and
			(= 1 (length args))
			(eq (aref (format nil "~a" (car args)) 0) #\.))
		       #+nil (format nil "~a~a" name
			       (emit args))
		       (format nil "~a~a" name
			       (emit `(paren ,@args)))))))
	      (cond
		((or (symbolp code)
		     (stringp code)) ;; print variable
		 (format nil "~a" code))
		((numberp code) ;; print constants
		 (cond ((integerp code) (format str "~a" code))
		       ))))
	  "")))
  (emit-go :code `(let ((a (+ 40 2))
			(b 3))
		    (declare (type int64 a))
		    
		    (setf a1 3
			  b2 (logior a 5))
		    (:= stai (string "hello"))
		    (incf a 4)
		    (incf b)
		    (/= a (- b))
		    (/ q)
		    (setf q (sin (atan (aref a (slice 3 4))))
			  ; f
			  #+nil (-> sin
			      (atan a)))
		    #+nil (->
		     
			(string "bla")
			(.strip)
			(.split (string " "))
			(aref 0)))))

;; "var a int64 = 3"

#+nil
(logand #x0f #xf0)
