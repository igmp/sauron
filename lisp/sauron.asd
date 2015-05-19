(in-package :common-lisp-user)
(use-package :asdf)

(defsystem :sauron
  :author "Igor Plekhov <penguin@shtraf.net.ru>"

  :depends-on (cl-ppcre clsql-postgresql-socket html-template hunchentoot
			uiop xmls)
  :serial t
  :components ((:file "package")
	       (:file "server")
	       (:file "config")
	       (:file "evaluate")
	       (:file "execute")
	       (:file "registry")))

;;;;
