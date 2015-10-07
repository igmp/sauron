(in-package :sauron)
(file-enable-sql-reader-syntax)

(defparameter *sauron-version* #.(asdf:component-version (asdf:find-system :sauron)))
(defparameter *default-database-type* :postgresql-socket)
(defparameter *default-caching* nil)
(defparameter *db-host* (or (sb-posix:getenv "DB_HOST") "localhost"))
(defparameter *db-port* (parse-integer (or (sb-posix:getenv "DB_PORT") "5432") :junk-allowed t))
(defparameter *db-name* (or (sb-posix:getenv "DB_NAME") "sauron"))
(defparameter *db-user* (or (sb-posix:getenv "DB_USER") "sauron"))
(defparameter *db-password* (or (sb-posix:getenv "DB_PASSWORD") ""))

(defmacro with-sauron-db (() &rest body)
  `(with-database (*default-database* (list *db-host* *db-name* *db-user* *db-password* *db-port*)
				      :pool t)
     ,@body))

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
(define-database-configured-variable add-black)
(define-database-configured-variable bird-protocol)
(define-database-configured-variable bird-reload)
(define-database-configured-variable block-url)
(define-database-configured-variable del-black)
(define-database-configured-variable home-directory)
(define-database-configured-variable list-black)
(define-database-configured-variable nginx-port)
(define-database-configured-variable nginx-reload)
(define-database-configured-variable nginx-resolver)
(define-database-configured-variable root-means-domain)
(define-database-configured-variable routers)
(define-database-configured-variable store-days)
(define-database-configured-variable working-registry-id)

(defun last-registry-id ()
  (caar (select [id]
		:from [registry]
		:where [= [-update-time] [select [max [-update-time]]
						 :from [registry]]])))

(defmacro define-home-variable (var param)
  `(defun ,var ()
     (concatenate 'string (home-directory) ,param)))

(define-home-variable etc-directory    "data/etc/")
(define-home-variable bird-conf        "data/etc/bird.conf")
(define-home-variable nginx-black-conf "data/etc/nginx-black.conf")
(define-home-variable nginx-rkn-conf   "data/etc/nginx-rkn.conf")
(define-home-variable registry-csv     "data/etc/registry.csv")
(define-home-variable tmp-directory    "data/tmp/")
(define-home-variable tmp-registry     "data/tmp/dump.xml")
(define-home-variable tmpl-directory   "tmpl/")
(define-home-variable zip-directory    "data/zip/")

(defparameter *sauron-dispatch-table*
  (list (create-regex-dispatcher "^/$" 'root-redirect)
	(create-regex-dispatcher "^/config/$"         'config/)
	(create-regex-dispatcher "^/config/set/$"     'config/set/)
	(create-regex-dispatcher "^/realm/$"          'realm/)
	(create-regex-dispatcher "^/realm/new/$"      'realm/new/)
	(create-regex-dispatcher "^/realm/set/$"      'realm/set/)
	(create-regex-dispatcher "^/realm/time/add/$" 'realm/time/add/)
	(create-regex-dispatcher "^/realm/time/del/$" 'realm/time/del/)
	(create-regex-dispatcher "^/registry/$"       'registry/)
	(create-regex-dispatcher "^/registry/come/$"  'registry/come/)
	(create-regex-dispatcher "^/registry/del/$"   'registry/del/)
	(create-regex-dispatcher "^/registry/exec/$"  'registry/exec/)
	(create-regex-dispatcher "^/registry/load/$"  'registry/load/)
	(create-regex-dispatcher "^/status/$"         'status/)
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
                (with-sauron-db ()
		  (let ((*tmp-directory* (tmp-directory)))
		    (funcall handler)))))))
	*sauron-dispatch-table*)
  (call-next-method))

(defvar *server* nil)

(defparameter *black-time-switcher*
  (make-timer #'(lambda ()
		  (with-sauron-db ()
		    (generate-nginx-conf :black :file (nginx-black-conf))
		    (sb-ext:run-program "/bin/sh" `("-c" ,(nginx-reload)))))
	      :name "black time switcher"
	      :thread t)
  "Next time nginx-black.conf should be regenerated.")

(defun start-sauron ()
  (with-sauron-db ()
    (start (setf *server* (make-instance 'sauron-acceptor)))
    (schedule-timer *black-time-switcher* (next-black-time-switch)))
  (sb-thread:make-thread #'(lambda ()
			     (with-sauron-db ()
			       (execute-registry :id (working-registry-id))))
			 :name (format nil "exec-registry ~a" (working-registry-id))))

(defun stop-sauron ()
  (unschedule-timer *black-time-switcher*)
  (stop *server*))

(defun restart-sauron ()
  (stop-sauron)
  (sleep 1)
  (start-sauron))

(defun static-file ()
  (handle-static-file (concatenate 'string (home-directory) (script-name*))))

(defun root-redirect ()
  (redirect "/status/"))

(defun sauron-plist ()
  (append (list :sauron-version *sauron-version*)
	  (prog1 (apply #'append (session-value :motd))
	    (setf (session-value :motd) nil))))

(defun ftmpl (tmpl plist)
  (let ((*default-template-pathname* (concatenate 'string (tmpl-directory)))
        (*template-start-marker* "<")
        (*template-end-marker* ">")
        (*warn-on-creation* nil)
        (*force-default* t)
	(*ignore-empty-lines* t)
        (*string-modifier* #'identity))
    (fill-and-print-template tmpl plist)))

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

;; modified Hunchentoot's version
(defun url-encode* (string &optional (external-format *hunchentoot-default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT. The default
for EXTERNAL-FORMAT is the value of *HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT*."
  (when string
    (with-output-to-string (s)
      (loop for c across string
	 for index from 0
	 do (cond ((or (char<= #\0 c #\9)
		       (char<= #\a c #\z)
		       (char<= #\A c #\Z)
		       (find c "/?:@&-_.+!*'()," :test #'char=))
		   (write-char c s))
		  (t (loop for octet across
			  (flexi-streams:string-to-octets string
							  :start index
							  :end (1+ index)
							  :external-format external-format)
			do (format s "%~2,'0x" octet))))))))

;;;;
