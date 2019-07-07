(ql:quickload "alexandria")

(defpackage :trial
  (:use :cl
	;:optima
	:alexandria))
(in-package :trial)

(setf (readtable-case *readtable*) :invert)

(format nil "var ~a ~@[~a ~]= ~@[~a~]" 'name nil 3) ;; => "var name = 3"
(format nil "var ~a ~@[~a ~]= ~@[~a~]" 'name 'int 3) ;; => "var name int = 3"

(progn
  
  (defun emit-go (&key code (str nil)  (level 0))
    (flet ((emit (code &optional (dl 0))
	     (emit-go :code code :level (+ dl level))))
      (if code
	  (if (listp code)
	      (case (car code)
		(paren (let ((args (cdr code)))
			 (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		(let (destructuring-bind (decls &rest body) (cdr code)
		       (loop for e in body )
		       (with-output-to-string (s)
			(loop for decl in decls collect
			     (destructuring-bind (name &optional value) decl
			       (format s "var ~a"))))
		       (format t "let ~a" (list decls body))))
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
  (emit-go :code `(let ((a 3)))))
