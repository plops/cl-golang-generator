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
(declare (type int64 a b))

(defparameter *a* (make-hash-table))
(gethash "bla" *a*)

(defun consume-declare (body)
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

(consume-declare `((declare (type int64 a b))
		   (setf a 3)
		   (setf b 4)))

;; ((setf b 4) (setf a 3))
;; #<hash-table :TEST eql :COUNT 2 {10031E1F03}>


(multiple-value-bind (body env)
    (consume-declare `((declare (type int64 a b))
		       (setf a 3)
		       (setf b 4)))
  (lookup-type 'b :env env))
;; int64
;; t

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
		       (multiple-value-bind (body env) (consume-declare body)
			 (with-output-to-string (s)
			   (loop for decl in decls collect
				(destructuring-bind (name &optional value) decl
				  (format s "var ~a~@[ ~a~]~@[ = ~a~]"
					  name (lookup-type name :env env) value)))))))
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
  (emit-go :code `(let ((a 3))
		    (declare (type int64 a)))))

;; "var a int64 = 3"
