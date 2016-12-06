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

(defparameter *wday-arr* #("пн" "вт" "ср" "чт" "пт" "сб" "вс"))

(defvar clsql-sys::*retry-on-socket-error* nil
  "Whether retry to connect in case of some network failure.")

(defmethod clsql-sys:database-connect :around (connection-spec database-type)
  (if clsql-sys::*retry-on-socket-error*
      (handler-case (call-next-method)
	(sb-bsd-sockets:socket-error ()
	  (sleep 1)
	  (clsql-sys:database-connect connection-spec database-type)))
      (call-next-method)))

(defmacro with-sauron-db ((&key (retry t)) &rest body)
  `(let ((clsql-sys::*retry-on-socket-error* ,retry))
     (with-database (*default-database* (list *db-host* *db-name* *db-user* *db-password* *db-port*)
					:pool t)
       ,@body)))

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
(define-database-configured-variable check-period)
(define-database-configured-variable del-black)
(define-database-configured-variable download-period)
(define-database-configured-variable dump-format)
(define-database-configured-variable home-directory)
(define-database-configured-variable list-black)
(define-database-configured-variable nginx-port)
(define-database-configured-variable nginx-reload)
(define-database-configured-variable nginx-resolver)
(define-database-configured-variable root-means-domain)
(define-database-configured-variable routers)
(define-database-configured-variable store-interval)
(define-database-configured-variable working-registry-id)

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

(defvar *http-server* nil
  "Sauron's HTTP server.")

(defvar *download-semaphore*
  (make-semaphore :name "download semaphore")
  "If it went up then it's now time to download a new registry.")

(defvar *download-registry* nil
  "Download RKN's registries.")

(defvar *process-mailbox*
  (sb-concurrency:make-mailbox :name "process mailbox")
  "Messages about registries to be processed are sent here.  Each message is
a property list with :id and :exec properties.  It can serve as an argument
list to #'process-registry.")

(defvar *process-registry* nil
  "Process RKN's registries.")

(defvar *execute-mailbox*
  (sb-concurrency:make-mailbox :name "execute mailbox")
  "Messages about registries to be executeed are sent here.  Each message
is a property list with :id property.  It can serve as an argument list to
#'execute-registry.")

(defvar *execute-registry* nil
  "Execute RKN's registries.")

(defvar *check-semaphore*
  (make-semaphore :name "check semaphore")
  "If it went up then it's now time to check for a registry renewal.")

(defvar *check-registry* nil
  "Check for RKN's registry renewal.")

(defvar *black-semaphore*
  (make-semaphore :name "black semaphore")
  "If it went up then it is needed to renew limits implied by black lists.")

(defvar *black-switch* nil
  "Imply black lists.")

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

(defun escape-nginx-parameter (string)
  (when string
    (regex-replace-all "\\$" string "%24")))

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
		       (find c "/?:;@&-_.+!*'(),=~%" :test #'char=))
		   (write-char c s))
		  (t (loop for octet across
			  (flexi-streams:string-to-octets string
							  :start index
							  :end (1+ index)
							  :external-format external-format)
			do (format s "%~2,'0x" octet))))))))

(defun file-base64 (filename)
  (with-open-file (stream filename
			  :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream)
			   :element-type '(unsigned-byte 8)
			   :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      (usb8-array-to-base64-string seq))))

(defparameter *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun unix-time-string (time)
  (multiple-value-bind (second minute hour mday month year wday daylight-p zone)
      (decode-universal-time (unix-to-universal-time time))
    (declare (ignore wday daylight-p))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~@d"
	    year month mday hour minute second (- zone))))

(defun read-file (filename)
  (with-open-file (stream filename
			  :element-type 'character)
    (let ((seq (make-array (file-length stream)
			   :element-type 'character
			   :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

;;;;
