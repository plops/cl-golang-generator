(in-package :cl-golang-generator)

(setf (readtable-case *readtable*) :invert)

(defparameter *file-hashes* (make-hash-table))

(defun write-source (name code &optional (dir (user-homedir-pathname))
				 ignore-hash)
  (let* ((fn (merge-pathnames (format nil "~a.go" name)
			      dir))
	 (code-str (emit-go
		    :code code))
	 (fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
      (when (or (not exists) ignore-hash (/= code-hash old-code-hash))
	;; store the sxhash of the c source in the hash table
	;; *file-hashes* with the key formed by the sxhash of the full
	;; pathname
	(setf (gethash fn-hash *file-hashes*) code-hash)
	(with-open-file (s fn
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	  (write-sequence code-str s))
	#+sbcl
	(sb-ext:run-program
					;"/usr/local/go/bin/go"
	 "/usr/bin/go"
	 (list "fmt" (namestring fn))
	 )))))


;; http://clhs.lisp.se/Body/s_declar.htm
;; http://clhs.lisp.se/Body/d_type.htm

;; go through the body until no declare anymore

(defun consume-declare (body)
  "take a list of instructions (body), parse type declarations,
return the body without them and a hash table with an environment. the
entry return-values contains a list of return values"

  ;; (declare (type int a b) (type float c)
  ;; (declare (values int &optional))
  ;; (declare (values int float &optional))

  ;; FIXME doesnt handle documentation strings
  (let ((env (make-hash-table))
	(looking-p t)
	(new-body nil))
    (loop for e in body do
	  (if looking-p
	      (if (listp e)
		  (if (eq (car e) 'declare)
		      (loop for declaration in (cdr e) do
			    (when (eq (first declaration) 'type)
			      (destructuring-bind (symb type &rest vars) declaration
				(declare (ignorable symb))
				(loop for var in vars do
				      (setf (gethash var env) type))))
			    (when (eq (first declaration) 'values)
			      (destructuring-bind (symb &rest types-opt) declaration
				(declare (ignorable symb))
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
				   (if (listp decl) ;; split into name and initform
				       (destructuring-bind (name &optional value) decl
					 (format nil "var ~a~@[ ~a~]~@[ = ~a~]"
						 name
						 (lookup-type name :env env)
						 (funcall emit value)))
				       (format nil "var ~a ~a"
					       decl
					       (let ((type (lookup-type decl :env env)))
						 (if type
						     type
						     (break "type ~a not defined." decl))))))
			   ,@body)))))))

(defun parse-defun (code emit)
  ;;  defun function-name lambda-list [declaration*] form*
  ;; https://golang.org/ref/spec#Function_declarations
  ;; func(x float, y int) int {
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body) ;; py
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "func ~a~a ~@[~a ~]"
		  name
		  (funcall emit `(paren
				  ,@(loop for p in req-param collect
					   (let ((type (gethash p env)))
					    (if type
					     (format nil "~a ~a"
						     p type)
					     (format nil "~a"
						     p)))
					   )))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r))))
	  (format s "~a" (funcall emit `(progn ,@body))))))))

(defun parse-defun-declaration (code emit)
  ;; only emit the declaration of the function
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "~a~a ~@[~a ~]"
		  name
		  (funcall emit `(paren
				  ,@(loop for p in req-param collect
					  (let ((type (gethash p env)))
					    (if type
					     (format nil "~a ~a"
						     p type)
					     (format nil "~a"
						     p))))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r)))))))))

(defun parse-defmethod (code emit)
  ;;  defmethod function-name specialized-lambda-list [declaration*] form*
  ;; specialized-lambda-list::= ({var | (var parameter-specializer-name)}*
  ;; first element of lambda-list declares the object (the methods receiver)
  ;; (defmethod Distance ((p Point) q) ...
  ;; => func (p Point) Distance(q Point) float64 { ...

  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (destructuring-bind (receiver-name receiver-type) (car lambda-list)
	(multiple-value-bind (req-param opt-param res-param
					key-param other-key-p
					aux-param key-exist-p)
	    (parse-ordinary-lambda-list (cdr lambda-list))
	  (declare (ignorable req-param opt-param res-param
			      key-param other-key-p aux-param key-exist-p))
	  (with-output-to-string (s)
	    (format s "func (~a ~a) ~a~a ~@[~a ~]"
		    receiver-name
		    receiver-type
		    name
		    (funcall emit `(paren
				    ,@(loop for p in req-param
					    collect
					    (let ((type (gethash p env)))
					    (if type
					     (format nil "~a ~a"
						     p type)
					     (format nil "~a"
						     p)))
					    #+nil (format nil "~a ~a"
						  p
						  (let ((type (gethash p env)))
						    (if type
							type
							(break "can't find type for ~a in defun"
							       p))))
							     )))
		    (let ((r (gethash 'return-values env)))
		      (if (< 1 (length r))
			  (funcall emit `(paren ,@r))
			  (car r))))
	    (format s "~a" (funcall emit `(progn ,@body)))))))))

(defun parse-defmethod-declaration (code emit)
  ;; only emit declaration
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (destructuring-bind (receiver-name receiver-type) (car lambda-list)
	(multiple-value-bind (req-param opt-param res-param
					key-param other-key-p
					aux-param key-exist-p)
	    (parse-ordinary-lambda-list (cdr lambda-list))
	  (declare (ignorable req-param opt-param res-param
			      key-param other-key-p aux-param key-exist-p))
	  (with-output-to-string (s)
	    (format s "func (~a ~a) ~a~a ~@[~a ~]"
		    receiver-name
		    receiver-type
		    name
		    (funcall emit `(paren
				    ,@(loop for p in req-param collect
					    (let ((type (gethash p env)))
					    (if type
					     (format nil "~a ~a"
						     p type)
					     (format nil "~a"
						     p)))
					    #+nil
					    (format nil "~a ~a"
						    p
						    (let ((type (gethash p env)))
						      (if type
							  type
							  (break "can't find type for ~a in defun"
								 p)))))))
		    (let ((r (gethash 'return-values env)))
		      (if (< 1 (length r))
			  (funcall emit `(paren ,@r))
			  (car r))))))))))

(defun parse-defmethod-interface (code emit)
  ;; only emit declaration that can be used in an interface
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (destructuring-bind (receiver-name receiver-type) (car lambda-list)
	(multiple-value-bind (req-param opt-param res-param
					key-param other-key-p
					aux-param key-exist-p)
	    (parse-ordinary-lambda-list (cdr lambda-list))
	  (declare (ignorable req-param opt-param res-param
			      key-param other-key-p aux-param key-exist-p))
	  (with-output-to-string (s)
	    (format s "~a~a ~@[~a ~]"
		    name
		    (funcall emit `(paren
				    ,@(loop for p in req-param collect
					    (let ((type (gethash p env)))
					    (if type
					     (format nil "~a ~a"
						     p type)
					     (format nil "~a"
						     p)))
					    #+nil
					    (format nil "~a ~a"
						    p
						    (let ((type (gethash p env)))
						      (if type
							  type
							  (break "can't find type for ~a in defun"
								 p)))))))
		    (let ((r (gethash 'return-values env)))
		      (if (< 1 (length r))
			  (funcall emit `(paren ,@r))
			  (car r))))))))))


(defun parse-lambda (code emit)
  ;;  lambda lambda-list [declaration*] form*
  (destructuring-bind (lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "func ~a ~@[~a ~]"
		  (funcall emit `(paren
				  ,@(loop for p in req-param
					  collect
					  (let ((type (gethash p env)))
					    (if type
					     (format nil "~a ~a"
						     p type)
					     (format nil "~a"
						     p))))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r))))
	  (format s "~a" (funcall emit `(progn ,@body))))))))




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

(defun parse-const (code emit)
  "const {pair}*"
  (let ((args (cdr code)))
    (with-output-to-string (s)
      (format s "const (")
      (format s "~a~%"
	      (funcall emit
		       `(do0
			 ,@(loop for i below (length args) by 2 collect
				 (let ((a (elt args i))
				       (b (elt args (+ 1 i))))
				   `(= ,a ,b))))))
      (format s ")"))))



(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'double-float))
	 (s (format nil "~E" ff)))
    #+nil (assert (= 0d0 (- ff
			    (read-from-string s))))
    (assert (< (abs (- ff
		       (read-from-string s)))
	       1d-12))
    (substitute #\e #\d s)))

(progn
  (defun emit-go (&key code (str nil)  (level 0))
    (flet ((emit (code &optional (dl 0))
	     "change the indentation level. this is used in do"
	     (emit-go :code code :level (+ dl level))))
      (if code
	  (if (listp code)
	      (case (car code)
		(ntuple (let ((args (cdr code)))
			  (format nil "~{~a~^, ~}" (mapcar #'emit args))))
		(space
		 ;; space {args}*
		 (let ((args (cdr code)))
		   (format nil "~{~a~^ ~}" (mapcar #'emit args))))
		(paren
		 ;; paren {args}*
		 (let ((args (cdr code)))
		   (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		(braces
		 ;; braces {args}*
		 (let ((args (cdr code)))
		   (format nil "{~{~a~^, ~}}" (mapcar #'emit args))))
		(curly ;; name{arg1, args}
		 ;; or name{key1:arg1, key2:arg2}
		 (destructuring-bind (name &rest args) (cdr code)
		   (emit `(cast ,name
				(braces
				 ,@(if (keywordp (car args))
				       (loop for i below (length args) by 2 collect
					     (let ((a (elt args i))
						   (b (elt args (+ 1 i))))
					       (format nil "~a: ~a" (emit a) (emit b))))
				       args))))))
		(cast ;; cast type value
		 (destructuring-bind (type value) (cdr code)
		   (format nil "~a ~a" (emit type) (emit value)))
		 )
		(comment (format nil "// ~a~%" (cadr code)))
		(comments (let ((args (cdr code)))
			    (format nil "~{// ~a~%~}" args)))
	      	(dict
		 ;; dict {pair}*
		 (let* ((args (cdr code)))
		   (let ((str (with-output-to-string (s)
				(loop for (e f) in args
				      do
				      (format s "~a: ~a," (emit e) (emit f))))))
		     (format nil "{~a}" ;; remove trailing comma
			     (subseq str 0 (- (length str) 1))))))
		(go (format nil "go ~a" (emit (car (cdr code)))))
		(range (format nil "range ~a" (emit (car (cdr code)))))
		(chan (format nil "chan ~a" (emit (car (cdr code)))))
		(defer (format nil "defer ~a" (emit (car (cdr code)))))
		(return (format nil "return ~a" (emit (car (cdr code)))))
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
		(progn (with-output-to-string (s)
			 ;; progrn {form}*
			 ;; like do but surrounds forms with braces.
			 (format s "{~{~&~a~}~&}" (mapcar #'(lambda (x) (emit `(indent ,x) 1)) (cdr code)))))
		(do0 (with-output-to-string (s)
		       ;; do0 {form}*
		       ;; write each form into a newline, keep current indentation level
		       (format s "~&~a~{~&~a~}"
			       (emit (cadr code))
			       (mapcar #'(lambda (x) (emit `(indent ,x) 0)) (cddr code)))))
		(let (parse-let code #'emit))

		(defun (parse-defun code #'emit))
		(defun-declaration (parse-defun-declaration code #'emit))
		(lambda (parse-lambda code #'emit))
		(defmethod (parse-defmethod code #'emit))
		(defmethod-interface (parse-defmethod-interface code #'emit))
		(defmethod-declaration (parse-defmethod-declaration code #'emit))
		#+nil (defstruct
			  ;;  defstruct name {slot-description}*
			  ;; slot-description::= slot-name | (slot-name [slot-initform [[slot-option]]])
			  ;; slot-option::= :type slot-type
			  (destructuring-bind (name &rest slot-descriptions) (cdr code)
			    (format
			     nil "type ~a struct ~a"
			     name
			     (emit
			      `(progn
				 ,@(loop for desc in slot-descriptions collect
					 (destructuring-bind (slot-name ;; &optional init
							      ;; init doesnt really fit into go semantics
							      &key type) desc
					   (format nil "~a~@[ ~a~]" slot-name type))))))))
		(deftype
		    ;; deftype name lambda-list {form}*
		    ;; only the first form of the body is used, lambda list is ignored
		    (destructuring-bind (name lambda-list &rest body) (cdr code)
		      (declare (ignore lambda-list))
		      (format nil "type ~a ~a" name (emit (car body)))))
		(defstruct0
		 ;; defstruct without init-form
		 ;; defstruct name {slot-description}*
		 ;; slot-description::= slot-name | (slot-name [slot-type])

		 ;; a slot-name without type can be used to create a
		 ;; composed type with a struct embedding
		 (destructuring-bind (name &rest slot-descriptions) (cdr code)
		   (format nil "type ~a struct ~a"
			   name
			   (emit
			    `(progn
			       ,@(loop for desc in slot-descriptions collect
				       (destructuring-bind (slot-name &optional type) desc
					 (format nil "~a~@[ ~a~]" slot-name type))))))))

		(definterface

		 ;; definterface name {slot-description}*
		 ;; slot-description::= other-interface-name | method-interface-declaration

		 (destructuring-bind (name &rest slot-descriptions) (cdr code)
		   (format nil "type ~a interface ~a"
			   name
			   (emit
			    `(progn
			       ,@(mapcar #'emit slot-descriptions))))))
		(setf (parse-setf code #'emit))
		(const (parse-const code #'emit))
		(assign
		 ;; assign {pair}*
		 (let ((args (cdr code)))
		   (format nil "~a~%"
			   (emit
			    `(do0
			      ,@(loop for i below (length args) by 2 collect
				      (let ((a (elt args i))
					    (b (elt args (+ 1 i))))
					`(:= ,a ,b))))))))
		(if (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
		      (with-output-to-string (s)
			(format s "if ( ~a ) ~a"
				(emit condition)
				(emit `(progn ,true-statement)))
			(when false-statement
			  (format s " else ~a"
				  (emit `(progn ,false-statement)))))))
		(when (destructuring-bind (condition &rest forms) (cdr code)
			(emit `(if ,condition
				   (do0
				    ,@forms)))))
		(unless (destructuring-bind (condition &rest forms) (cdr code)
			  (emit `(if (not ,condition)
				     (do0
				      ,@forms)))))
		(ecase
		    ;; ecase keyform {normal-clause}*
		    ;; normal-clause::= (keys form*)
		    (destructuring-bind (keyform &rest clauses)
			(cdr code)
		      (format
		       nil "switch ~a ~a"
		       (emit keyform)
		       (emit
			`(progn
			   ,@(loop for c in clauses collect
				   (destructuring-bind (key &rest forms) c
				     (format nil "case ~a:~&~a"
					     (emit key)
					     (emit
					      `(do0
						,@(mapcar #'emit
							  forms)))))))))))

		(case
		    ;; case keyform {normal-clause}* [otherwise-clause]
		    ;; normal-clause::= (keys form*)
		    ;; otherwise-clause::= (t form*)

		    (destructuring-bind (keyform &rest clauses)
			(cdr code)
		      (format
		       nil "switch ~a ~a"
		       (emit keyform)
		       (emit
			`(progn
			   ,@(loop for c in clauses collect
				   (destructuring-bind (key &rest forms) c
				     (if (eq key t)
					 (format nil "default:~&~a"
						 (emit
						  `(do0
						    ,@(mapcar #'emit
							      forms))))
					 (format nil "case ~a:~&~a"
						 (emit key)
						 (emit
						  `(do0
						    ,@(mapcar #'emit
							      forms))))))))))))
		(typecase
		    ;; typecase keyform {normal-clause}* [otherwise-clause]
		    ;; normal-clause::= (keys form*)
		    ;; otherwise-clause::= (t form*)

		    (destructuring-bind (keyform &rest clauses)
			(cdr code)
		      (format
		       nil "switch ~a := ~a.(type) ~a"
		       (emit keyform)
		       (emit keyform)
		       (emit
			`(progn
			   ,@(loop for c in clauses collect
				   (destructuring-bind (key &rest forms) c
				     (if (eq key t)
					 (format nil "default:~&~a"
						 (emit
						  `(do0
						    ,@(mapcar #'emit
							      forms))))
					 (format nil "case ~a:~&~a"
						 (emit key)
						 (emit
						  `(do0
						    ,@(mapcar #'emit
							      forms))))))))))))
		(for
		 ;; for init condition update {forms}*  .. for loop
		 ;; for                                 .. infinite loop (for () ...)
		 (let* ((args (cdr code))
			(params (first args))
			(body (cdr args)))
		   (with-output-to-string (s)
		     (cond ((eq 0 (length params))
			    (format s "for "))
			   ((eq 3 (length params))
			    (destructuring-bind (init condition update) params
			      (format s "for ~a ; ~a; ~a "
				      (emit init)
				      (emit condition)
				      (emit update))))
			   (t (break "unsupported number of arguments in for loop")))
		     (format s "~a" (emit `(progn ,@body))))
		   ))
		(foreach
		 ;; foreach [var] range {forms}*
		 ;; foreach range {forms}*
		 (destructuring-bind ((&rest decl) &rest body) (cdr code)
		   (with-output-to-string (s)
		     (format s "for ~a "
			     (if (< 1 (length decl))
				 (destructuring-bind (var range) decl
				   (emit `(:= ,var ,range)))
				 (emit (car decl))))
		     (format s "~a" (emit `(progn ,@body))))))

		(while
		    ;; while condition {forms}*

		    (destructuring-bind (condition &rest body) (cdr code)
		      (with-output-to-string (s)
			(format s "for ~a "
				(emit condition))
			(format s "~a" (emit `(progn ,@body))))))
		(dotimes (destructuring-bind ((var end) &rest body) (cdr code)
			   (emit `(for ((:= ,var 0)
					(< ,var ,end)
					(incf ,var))
				       ,@body))))
		(not (format nil "(!(~a))" (emit (car (cdr code)))))
		(ref (format nil "(&(~a))" (emit (car (cdr code)))))
		(deref (format nil "(*(~a))" (emit (car (cdr code)))))
		(package (format nil "package ~a" (car (cdr code))))
		(import (let ((args (cdr code)))
			  ;; import {(name|pair)}*
			  ;; pair := nickname name
			  (with-output-to-string (s)
			    (format s "import (")
			    (loop for e in args do
				  (if (listp e)
				      (destructuring-bind (nick name) e
					(format s "~&~a \"~a\""
						nick name))
				      (format s "~&\"~a\"" e))
				  )
			    (format s "~&)"))))

		(+ (let ((args (cdr code)))
		     ;; + {summands}*
		     (format nil "(~{(~a)~^+~})" (mapcar #'emit args))))
		(- (let ((args (cdr code)))
		     (if (eq 1 (length args))
			 (format nil "(-(~a))" (emit (car args))) ;; py
			 (format nil "(~{(~a)~^-~})" (mapcar #'emit args)))))
		(* (let ((args (cdr code)))
		     (format nil "(~{(~a)~^*~})" (mapcar #'emit args))))
		(/ (let ((args (cdr code)))
		     (if (eq 1 (length args))
			 (format nil "(1.0/(~a))" (emit (car args))) ;; py
			 (format nil "(~{(~a)~^/~})" (mapcar #'emit args)))))
		(logior (let ((args (cdr code))) ;; py
			  (format nil "(~{(~a)~^|~})" (mapcar #'emit args))))
		(logand (let ((args (cdr code))) ;; py
			  (format nil "(~{(~a)~^&~})" (mapcar #'emit args))))
		(or (let ((args (cdr code)))
		      (format nil "(~{(~a)~^||~})" (mapcar #'emit args))))
		(and (let ((args (cdr code)))
		       (format nil "(~{(~a)~^&&~})" (mapcar #'emit args))))
		(= (destructuring-bind (a b) (cdr code)
		     ;; = pair
		     (format nil "~a=~a" (emit a) (emit b))))
		(% (destructuring-bind (a b) (cdr code)
		     (format nil "((~a)%(~a))" (emit a) (emit b))))
		(:= (destructuring-bind (a b) (cdr code)
		      (format nil "~a:=~a" (emit a) (emit b))))
		(/= (destructuring-bind (a b) (cdr code)
		      (format nil "~a/=(~a)" (emit a) (emit b))))
		(*= (destructuring-bind (a b) (cdr code)
		      (format nil "~a*=(~a)" (emit a) (emit b))))
		(^= (destructuring-bind (a b) (cdr code)
		      (format nil "(~a)^=(~a)" (emit a) (emit b))))
		(<= (destructuring-bind (a b) (cdr code)
		      (format nil "(~a)<=(~a)" (emit a) (emit b))))
		(!= (destructuring-bind (a b) (cdr code)
		      (format nil "(~a)!=(~a)" (emit a) (emit b))))
		(== (destructuring-bind (a b) (cdr code)
		      (format nil "(~a)==(~a)" (emit a) (emit b))))
		(<- ;; send to channel channel
		 (destructuring-bind (a &optional b) (cdr code)
		   (if b
		       (format nil "~a<-~a" (emit a) (emit b))
		       (format nil "<-~a" (emit a)))))
		(< (destructuring-bind (a b) (cdr code)
		     (format nil "~a<~a" (emit a) (emit b))))
		(<< (destructuring-bind (a b) (cdr code)
		      (format nil "~a<<~a" (emit a) (emit b))))
		(>> (destructuring-bind (a b) (cdr code)
		      (format nil "~a>>~a" (emit a) (emit b))))
		(incf (destructuring-bind (a &optional b) (cdr code) ;; py
			(if b
			    (format nil "(~a)+=(~a)" (emit a) (emit b))
			    (format nil "(~a)++" (emit a)))))
		(decf (destructuring-bind (a &optional b) (cdr code)
			(if b
			    (format nil "(~a)-=(~a)" (emit a) (emit b))
			    (format nil "(~a)--" (emit a)))))
		(string (format nil "\"~a\"" (cadr code)))
		(char (format nil "'~a'" (cadr code)))
		(slice (let ((args (cdr code)))
			 (if (null args)
			     (format nil ":")
			     (format nil "~{~a~^:~}" (mapcar #'emit args)))))
		(aref (destructuring-bind (name &rest indices) (cdr code)
			(format nil "~a[~{~a~^,~}]" (emit name) (mapcar #'emit indices))))
		(dot (let ((args (cdr code)))
		       (format nil "~{~a~^.~}" (mapcar #'emit args))))
		(hex (let ((arg (car (cdr code))))
		       (format nil "0x~x" (emit arg))))
		(bin (let ((arg (car (cdr code))))
		       (format nil "0b~b" (emit arg))))
		(oct (let ((arg (car (cdr code))))
		       (format nil "0o~o" (emit arg))))



		#+nil (-> (let ((forms (cdr code)))
			    ;; clojure's thread first macro, thrush operator
			    ;; http://blog.fogus.me/2010/09/28/thrush-in-clojure-redux/
			    ;; -> {form}*
			    (emit (reduce #'(lambda (x y) (list (emit x) (emit y))) forms))))
		(t (destructuring-bind (name &rest args) code

		     (if (listp name)
			 ;; lambda call and similar complex constructs
			 (format nil "(~a)(~a)"
				 (emit name)
				 (if args
				     (emit `(paren ,@args))
				     ""))
			 ;; function call


			 (progn ;if
			   #+nil(and
				 (= 1 (length args))
				 (eq (aref (format nil "~a" (car args)) 0) #\.))
			   #+nil (format nil "~a~a" name
					 (emit args))




			   (format nil "~a~a" name
				   (emit `(paren ,@args))))))))
	      (cond
		((or (symbolp code)
		     (stringp code)) ;; print variable
		 (format nil "~a" code))
		((numberp code) ;; print constants
		 (cond ((integerp code) (format str "~a" code))
		       ((floatp code) ;; FIXME arbitrary precision?
			(format str "(~a)" (print-sufficient-digits-f64 code)))))))
	  "")))
  #-nil
  (defparameter *bla*
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
		      (defun bla (a b c)
			(declare (type int a b)
				 (type float c)
				 (values float &optional))
			(if a (return c)
			    v))

		      #+nil (->

			     (string "bla")
			     (.strip)
			     (.split (string " "))
			     (aref 0))
		      (defstruct0 Employee
			  (ID int)
			(Name string))
		      (lambda (items)
			(declare (type "[]string" items))
			)
		      (defstruct0 Point
			  (X float64)
			(Y float64))
		      (defmethod Distance ((p Point) q)
			(declare (type Point q)
				 (values float64 &optional))
			(return (math.Hypot (- q.X p.X)
					    (- q.Y p.Y))))))))
