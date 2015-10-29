(in-package :sauron)
(file-enable-sql-reader-syntax)

(defparameter *sauron-dispatch-table*
  (list (create-regex-dispatcher "^/$" 'root-redirect)
	(create-regex-dispatcher "^/config/$"         'config/)
	(create-regex-dispatcher "^/config/set/$"     'config/set/)
	(create-regex-dispatcher "^/realm/$"          'realm/)
	(create-regex-dispatcher "^/realm/del/$"      'realm/del/)
	(create-regex-dispatcher "^/realm/new/$"      'realm/new/)
	(create-regex-dispatcher "^/realm/set/$"      'realm/set/)
	(create-regex-dispatcher "^/realm/time/add/$" 'realm/time/add/)
	(create-regex-dispatcher "^/realm/time/del/$" 'realm/time/del/)
	(create-regex-dispatcher "^/registry/$"       'registry/)
	(create-regex-dispatcher "^/registry/check/$" 'registry/check/)
	(create-regex-dispatcher "^/registry/come/$"  'registry/come/)
	(create-regex-dispatcher "^/registry/del/$"   'registry/del/)
	(create-regex-dispatcher "^/registry/exec/$"  'registry/exec/)
	(create-regex-dispatcher "^/status/$"         'status/)
        (create-regex-dispatcher "\\.(css)$" 'static-file)))

(defun static-file ()
  (handle-static-file (concatenate 'string (home-directory) (script-name*))))

(defun root-redirect ()
  (redirect "/status/"))

(defclass sauron-acceptor (acceptor)
  ()
  (:default-initargs
   :address "127.0.0.1"
   :port 8004
   :access-log-destination (concatenate 'string (home-directory) "log/access.log")
   :message-log-destination (concatenate 'string (home-directory) "log/error.log")))

(defmethod acceptor-dispatch-request ((acceptor sauron-acceptor) request)
  (in-package :sauron)
  (mapc #'(lambda (dispatcher)
	    (let ((handler (funcall dispatcher request)))
	      (when handler
		(return-from acceptor-dispatch-request
		  (with-sauron-db ()
		    (let ((*session-max-time* 7200)
			  (*tmp-directory* (tmp-directory)))
		      (funcall handler)))))))
	*sauron-dispatch-table*)
  (call-next-method))

(defgeneric init-sauron (action system)
  (:documentation "Start or stop various Sauron's subsystems.")
  (:method :around ((action (eql :start)) system)
    (with-sauron-db ()
      (call-next-method)))

  (:method ((action (eql :start)) system)
    (init-sauron :start :http)
    (init-sauron :start :download)
    (init-sauron :start :process)
    (init-sauron :start :execute)
    (init-sauron :start :check)
    (init-sauron :start :black))
  (:method ((action (eql :stop)) system)
    (init-sauron :stop :black)
    (init-sauron :stop :check)
    (init-sauron :stop :execute)
    (init-sauron :stop :process)
    (init-sauron :stop :download)
    (init-sauron :stop :http))
  (:method ((action (eql :restart)) system)
    (init-sauron :stop t)
    (sleep 1)
    (init-sauron :start t))

  (:method ((action (eql :start)) (system (eql :http)))
    (start (setf *http-server* (make-instance 'sauron-acceptor))))
  (:method ((action (eql :stop)) (system (eql :http)))
    (stop *http-server*))

  (:method ((action (eql :start)) (system (eql :download)))
    (setq *download-registry*
	  (make-thread #'(lambda ()
			   (with-sauron-db ()
			     (loop (handler-case (download-registry)
				     (t () nil))
				(wait-on-semaphore *download-semaphore*
						   :timeout (* 60 60 (parse-integer (download-period)))))))
		       :name "download registry")))
  (:method ((action (eql :stop)) (system (eql :download)))
    (terminate-thread *download-registry*))

  (:method ((action (eql :start)) (system (eql :process)))
    (setq *process-registry*
	  (make-thread #'(lambda ()
			   (with-sauron-db ()
			     (loop (let ((to-process (receive-message *process-mailbox*)))
				     (handler-case (apply #'process-registry to-process)
				       (file-error () (apply #'annihilate-registry to-process))
				       (t () nil))))))
		       :name "process registry")))
  (:method ((action (eql :stop)) (system (eql :process)))
    (terminate-thread *process-registry*))

  (:method ((action (eql :start)) (system (eql :execute)))
    (setq *execute-registry*
	  (make-thread #'(lambda ()
			   (with-sauron-db ()
			     (loop (let ((to-execute (receive-message *execute-mailbox*)))
				     (handler-case (apply #'execute-registry to-execute)
				       (t () nil))))))
		       :name "execute registry")))
  (:method ((action (eql :stop)) (system (eql :execute)))
    (terminate-thread *execute-registry*))

  (:method ((action (eql :start)) (system (eql :check)))
    (setq *check-registry*
	  (make-thread #'(lambda ()
			   (sleep 300) ; wait 5 minutes for initial download to complete
			   (with-sauron-db ()
			     (loop (handler-case (check-registry)
				     (t () nil))
				(wait-on-semaphore *check-semaphore*
						   :timeout (* 60 (parse-integer (check-period)))))))
		       :name "check registry")))
  (:method ((action (eql :stop)) (system (eql :check)))
    (terminate-thread *check-registry*))

  (:method ((action (eql :start)) (system (eql :black)))
    (setq *black-switch*
	  (make-thread #'(lambda ()
			   (with-sauron-db ()
			     (loop (handler-case (black-switch)
				     (t () nil))
				(wait-on-semaphore *black-semaphore*
						   :timeout (black-time-interval)))))
		       :name (format nil "switch black lists"))))
  (:method ((action (eql :stop)) (system (eql :black)))
    (terminate-thread *black-switch*)))

;;;;
