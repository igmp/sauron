(in-package :sauron)
(file-enable-sql-reader-syntax)

;;; - Если идентификатор реестра установлен, то всё в порядке.
;;; - Если идентификатор отсутствует, то его надо выставить по времени.
;;; - Если время не задано, то интересует самый поздний по времени реестр.
;;; - Выставляем идентификатор реестра по времени.
(defmacro normalize-registry-id-by-time (id time)
  `(unless ,id
     (unless ,time
       (setq ,time (caar (select [max [-update-time]]
				 :from [registry]))))
     (setq ,id (caar (select [id]
			     :from [registry]
			     :where [= [-update-time] ,time])))))

(defun generate-registry-csv (&key (id (working-registry-id)) (file (registry-csv)))
  (let ((tmp (uiop/stream::get-temporary-file :directory (tmp-directory))))
    (with-open-file (stream tmp
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (loop for (content-id content-id* domain location query anchor) in
	   (select [content id] [-id] [-domain] [-location] [-query] [-anchor]
		   :from '([content] [resource])
		   :where [and [= [content id] [content-id]]
			       [= [content registry-id] id]]
		   :order-by '([-id] [-domain] [-location] [-query] [-anchor]))
	 do (format stream "~a; \"~a\"; \"~a\"; ~{~a~^, ~}~%"
		    content-id*
		    domain
		    (if location
			(format nil "~a~@[?~a~]~@[#~a~]"
				location query anchor)
			"")
		    (select [-address]
			    :from [rkn-ip-address]
			    :where [= [content-id] content-id]
			    :flatp t))))
    (rename-file tmp file)))

(defgeneric generate-bird-conf (type &key id file)
  (:method :around (type &key (file (bird-conf)) &allow-other-keys)
    (let ((tmp (uiop/stream::get-temporary-file :directory (tmp-directory))))
      (with-open-file (*default-template-output* tmp
						 :direction :output
						 :if-exists :supersede
						 :if-does-not-exist :create)
	(call-next-method))
      (rename-file tmp file)))
  (:method ((type (eql :static)) &key (id (working-registry-id)) &allow-other-keys)
    (ftmpl #p"conf/bird-static.conf"
	   (list :bird-protocol (bird-protocol)
		 :address-list (loop for (address) in (select [distinct [-address]]
							      :from [rkn-ip-address]
							      :where [and [= [registry-id] id]
									  [is [-subnet] nil]]
							      :order-by [-address])
				  collect (list :ip address))))))

(defgeneric generate-nginx-conf (type &key id file)
  (:method :around (type &key file &allow-other-keys)
    (let ((tmp (uiop/stream::get-temporary-file :directory (tmp-directory))))
      (with-open-file (*default-template-output* tmp
						 :direction :output
						 :if-exists :supersede
						 :if-does-not-exist :create)
	(call-next-method))
      (rename-file tmp file)))
  (:method ((type (eql :black)) &key id file)
    (declare (ignore id file))
    (ftmpl #p"conf/nginx-black.conf"
	   (append (list :nginx-port     (nginx-port)
			 :nginx-resolver (nginx-resolver))
		   (black-sheet))))
  (:method ((type (eql :rkn)) &key (id (working-registry-id)) file)
    (declare (ignore file))
    ;; Get a list of all domains.
    ;; For each domain get a list of different locations.
    ;; For each location get a list of different query strings.
    (labels ((location-query-list (domain location)
	       (loop for (query) in
		    (select [distinct [-query]]
			    :from [resource]
			    :where [and [= [-location] location]
					[= [-domain] domain]
					[= [registry-id] id]]
			    :order-by [-query])
		  collect (list :query (escape-nginx-parameter query))))
	     (domain-location-list (domain)
	       (loop for (location) in
		    (select [distinct [-location]]
			    :from [resource]
			    :where [and [= [-domain] domain]
					[= [registry-id] id]]
			    :order-by [-location])
		  collect (list :location (escape-nginx-parameter location)
				:root (or (not location)
					  (and (eql location "/")
					       (root-means-domain)))
				:query-list (location-query-list domain location))))
	     (registry-domain-list ()
	       (loop for (domain) in
		    (select [distinct [-domain]]
			    :from [resource]
			    :where [and [is [ssl] nil]
					[is [-url] nil]
					[= [registry-id] id]]
			    :order-by [-domain])
		  collect (list :domain domain
				:location-list (domain-location-list domain)))))
      (ftmpl #p"conf/nginx-rkn.conf"
	     (list :nginx-port     (nginx-port)
		   :nginx-resolver (nginx-resolver)
		   :block-url      (block-url)
		   :domain-list    (registry-domain-list))))))

(defun execute-registry (&key (id (working-registry-id)))
  "Generate bird.conf and nginx-rkn.conf and then reload both daemons."
  (setf (working-registry-id) id)
  (generate-registry-csv :id (and (active-sauron) id))
  (generate-bird-conf :static :id (and (active-sauron) id))
  (run-program "/bin/sh" `("-c" ,(bird-reload)))
  (generate-nginx-conf :rkn :file (nginx-rkn-conf) :id (and (active-sauron) id))
  (run-program "/bin/sh" `("-c" ,(nginx-reload))))

(defun registry-plist* (tuple)
  (let ((working-registry-id (parse-integer (or (working-registry-id) "0"))))
    (destructuring-bind (resource-count id up-time up-time-urg completed) tuple
      (list :id             id
	    :up-time        up-time
	    :up-time-urg    up-time-urg
	    :completed      completed
	    :resource-count resource-count
	    :workingp       (equal working-registry-id id)))))

(defun registry-plist (&key id)
  (if id
      (registry-plist* (first (select [registry id] [-update-time] [-update-time-urgently] [completed] [count [resource id]]
				      :from '([registry] [resource])
				      :where [and [= [registry id] id]
						  [= [registry-id] [registry id]]]
						  :group-by '([registry id] [-update-time] [-update-time-urgently] [completed]))))
      (list :registry-list
	    (loop for tuple in (query "select resource_count, id, _update_time, _update_time_urgently, completed
					  from registry
					  order by _time desc, _update_time desc, id desc ")
	       do (unless (nth 0 tuple)
		    (setf (nth 0 tuple) (caar (select [count [*]]
						      :from [resource]
						      :where [= [registry-id] (nth 1 tuple)]))))
	       collect (registry-plist* tuple)))))

(defun registry/ ()
  (with-output-to-string (*default-template-output*)
    (let ((id (string-integer (get-parameter "id"))))
      (if id
	  (ftmpl #p"registry/one.html" (registry-plist :id id))
	  (ftmpl #p"registry/list.html" (append (registry-plist)
						(list :registry-tab t)
						(sauron-plist)))))))

(defun registry/exec/ ()
  (setf (active-sauron) "exec")
  (let ((id (string-integer (get-parameter "id"))))
    (send-message *execute-mailbox* (list :id id))
    (push '(:motd-registry-executed t) (session-value :motd)))
  (redirect "/status/"))

;;;;
