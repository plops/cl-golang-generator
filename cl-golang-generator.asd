(asdf:defsystem cl-golang-generator
    :version "0"
    :description "Emit Go Language code"
    :maintainer " <kielhorn.martin@gmail.com>"
    :author " <kielhorn.martin@gmail.com>"
    :licence "GPL"
    :depends-on ("alexandria")
    :serial t
    :components ((:file "package")
		 (:file "go")
		 ) )
