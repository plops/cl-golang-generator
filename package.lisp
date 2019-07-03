;(ql:quickload "optima")
;(ql:quickload "alexandria")
(defpackage :cl-golang-generator
  (:use :cl
	;:optima
	:alexandria)
  (:export
   #:write-source))
