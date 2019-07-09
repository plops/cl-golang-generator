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
return the body without them and a hash table with an environment"
  (let ((env (make-hash-table))
	(looking-p t)
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (when (and (listp (second e))
				(eq (first (second e)) 'type))
		       (destructuring-bind (type-symb type &rest vars) (second e)
			 (loop for var in vars do
			      (setf (gethash var env) type))))
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values new-body env)))

(defun lookup-type (key &key env)
  (gethash key env))

(defun parse-let (code emit)
  (destructuring-bind (decls &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (with-output-to-string (s)
	(loop for decl in decls collect
	     (destructuring-bind (name &optional value) decl
	       (format s "var ~a~@[ ~a~]~@[ = ~a~]"
		       name (lookup-type name :env env) (funcall emit value))))))))

(defun parse-setf (code emit)
 (let ((args (cdr code)))
   (format nil "~a"
	   (emit `(do0 
		   ,@(loop for i below (length args) by 2 collect
			  (let ((a (elt args i))
				(b (elt args (+ 1 i))))
			    `(= ,a ,b))))))))

(defun parse-+ (code emit)
  (let ((args (cdr code)))
    (format nil "(~{(~a)~^+~})" (mapcar emit args))))

(progn
  (defun emit-go (&key code (str nil)  (level 0))
    (flet ((emit (code &optional (dl 0))
	     (emit-go :code code :level (+ dl level))))
      (if code
	  (if (listp code)
	      (case (car code)
		(paren (let ((args (cdr code)))
			 (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		(let (parse-let code #'emit))
		(setf (parse-setf code #'emit))
		(+ (parse-+ code #'emit))
		(t (destructuring-bind (name &rest args) code
		     (format nil "~a~a" name
			     (emit `(paren ,@args)))
		     )))
	      (cond
		((or (symbolp code)
		     (stringp code)) ;; print variable
		 (format nil "~a" code))
		((numberp code) ;; print constants
		 (cond ((integerp code) (format str "~a" code))
		       ))))
	  "")))
  (emit-go :code `(let ((a (+ 40 2)))
		    (declare (type int64 a)))))

;; "var a int64 = 3"
