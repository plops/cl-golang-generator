(ql:quickload "alexandria")

(defpackage :trial
  (:use :cl
	;:optima
	:alexandria))
(in-package :trial)

(setf (readtable-case *readtable*) :invert)

(progn
  (defparameter *env-functions* nil)
  (defparameter *env-macros* nil)
  (defun emit-go (&key code (str nil) (clear-env nil) (level 0))
					;(format t "emit ~a ~a~%" level code)
    (when clear-env
      (setf *env-functions* nil
	    *env-macros* nil))
    (flet ((emit (code &optional (dl 0))
	     (emit-go :code code :clear-env nil :level (+ dl level))))
      (if code
	  (if (listp code)
	      (case (car code)
		(paren (let ((args (cdr code)))
			 (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		(let (destructuring-bind (decls &rest body) (cdr code)
		       (loop for e in body )
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
