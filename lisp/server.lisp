(in-package :sauron)
(file-enable-sql-reader-syntax)

(defparameter *sauron-version* "0.1")
(defparameter *default-database-type* :postgresql-socket)
(defparameter *default-caching* nil)
(defparameter *db-host* "postgres.usernet")
(defparameter *db-port* 5432)
(defparameter *db-name* "sauron")
(defparameter *db-user* "sauron")
(defparameter *db-password* "D%nl`$xg&7_XnBD%")

(defmacro define-database-configured-variable (var)
  `(let (,var)
     (makunbound ',var)
     (defun ,var ()
       (if (boundp ',var)
	   ,var
	   (setq ,var (caar (select [value]
				    :from [config]
				    :where [= [key] (symbol-name ',var)])))))

     (defun (setf ,var) (id)
       (update-records [config]
		       :av-pairs `(([value] ,id))
		       :where [= [key] (symbol-name ',var)])
       (setf ,var id))))

(define-database-configured-variable active-sauron)
(define-database-configured-variable bird-init.d)
(define-database-configured-variable bird-protocol)
(define-database-configured-variable block-url)
(define-database-configured-variable home-directory)
(define-database-configured-variable nginx-init.d)
(define-database-configured-variable root-means-domain)
(define-database-configured-variable working-registry-id)

(defun last-registry-id ()
  (caar (select [id]
		:from [registry]
		:where [= [-update-time] [select [max [-update-time]]
						 :from [registry]]])))

(defmacro define-home-variable (var param)
  `(defun ,var ()
     (concatenate 'string (home-directory) ,param)))

(define-home-variable etc-directory  "data/etc/")
(define-home-variable bird-conf      "data/etc/bird.conf")
(define-home-variable nginx-conf     "data/etc/nginx.conf")
(define-home-variable registry-csv   "data/etc/registry.csv")
(define-home-variable tmp-directory  "data/tmp/")
(define-home-variable tmp-registry   "data/tmp/dump.xml")
(define-home-variable tmpl-directory "tmpl/")
(define-home-variable zip-directory  "data/zip/")

(defparameter *sauron-dispatch-table*
  (list (create-regex-dispatcher "^/$" 'root-redirect)
	(create-regex-dispatcher "^/config/$"        'config/)
	(create-regex-dispatcher "^/config/set/$"    'config/set/)
	(create-regex-dispatcher "^/registry/$"      'registry/)
	(create-regex-dispatcher "^/registry/come/$" 'registry/come/)
	(create-regex-dispatcher "^/registry/del/$"  'registry/del/)
	(create-regex-dispatcher "^/registry/exec/$" 'registry/exec/)
	(create-regex-dispatcher "^/registry/load/$" 'registry/load/)
	(create-regex-dispatcher "^/status/$"        'status/)
        (create-regex-dispatcher "\\.(css)$" 'static-file)))

(defclass sauron-acceptor (acceptor)
  ()
  (:default-initargs
   :address "127.0.0.1"
   :port 8004
   :access-log-destination (concatenate 'string (home-directory) "log/access.log")
   :message-log-destination (concatenate 'string (home-directory) "log/error.log")))

(defmethod acceptor-dispatch-request ((acceptor sauron-acceptor) request)
  (in-package :sauron)
  (mapc (lambda (dispatcher)
	  (let ((handler (funcall dispatcher request)))
	    (when handler
	      (return-from acceptor-dispatch-request
                (with-database (*default-database* (list *db-host* *db-name* *db-user* *db-password* *db-port*)
						   :pool t)
		  (let ((*tmp-directory* (tmp-directory)))
		    (funcall handler)))))))
	*sauron-dispatch-table*)
  (call-next-method))

(defvar *server* nil)

(defun start-sauron ()
  (with-database (*default-database* (list *db-host* *db-name* *db-user* *db-password* *db-port*)
				     :pool t)
    (start (setf *server* (make-instance 'sauron-acceptor))))
  ;; start cron
  )

(defun stop-sauron ()
  (stop *server*))

(defun restart-sauron ()
  (stop-sauron)
  (sleep 1)
  (start-sauron))

(defun static-file ()
  (handle-static-file (concatenate 'string (home-directory) (script-name*))))

(defun htmpl (tmpl plist)
  (let ((*default-template-pathname* (concatenate 'string (tmpl-directory)))
        (*template-start-marker* "<")
        (*template-end-marker* ">")
        (*warn-on-creation* nil)
        (*force-default* t)
        (*string-modifier* #'identity))
    (with-output-to-string (*default-template-output*)
      (fill-and-print-template tmpl plist))))

(defun string-integer (string)
  (when string
    (parse-integer string :junk-allowed t)))

(defun escape-sql-integer (string)
  (register-groups-bind (digit)
      ("(\\d+)" string)
    (format nil "~a" digit)))

(defun escape-sql-string (string)
  (if string
      (concatenate 'string "'" (regex-replace "'" string "''") "'")
      "''"))

(defun root-redirect ()
  (redirect "/status/"))

(defun general-plist ()
  (append (list :sauron-version *sauron-version*)
	  (prog1 (apply #'append (session-value :motd))
	    (setf (session-value :motd) nil))))

;;;;
