(in-package :common-lisp-user)
(use-package :asdf)

(defsystem :sauron
  :author "Igor Plekhov <penguin@shtraf.net.ru>"
  :version "0.22"

  :depends-on (cl-base64 cl-ppcre clsql-postgresql-socket drakma html-template hunchentoot
			 sb-concurrency uiop xmls)
  :serial t
  :components ((:file "package")
	       (:file "generic")
	       (:file "config")
	       (:file "evaluate")
	       (:file "execute")
	       (:file "load")
	       (:file "realm")
	       (:file "main")))

;;;;
