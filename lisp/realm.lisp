(in-package :sauron)
(file-enable-sql-reader-syntax)

(defun realm-plist* (tuple)
  (destructuring-bind (id name block-url active) tuple
    (list :id        id
	  :name      name
	  :block-url block-url
	  :active    active)))

(defun realm-plist (&key id)
  (if id
      (realm-plist* (first (select [id] [name] [block-url] [active]
				   :from [realm]
				   :where [= [id] id])))
      (list :realm-list
	    (mapcar #'realm-plist*
		    (select [id] [name] [block-url] [active]
			    :from [realm]
			    :order-by [name])))))

(defun black-time-plist* (tuple)
  (destructuring-bind (id realm-id start stop) tuple
    (list :id       id
	  :realm-id realm-id
	  :start    start
	  :stop     stop)))

(defun black-time-plist (&key realm-id)
  (list :black-time
	(mapcar #'black-time-plist*
		(select [id] [realm-id]
			(sql-operation 'function "to_char" [start] "HH24:MI")
			(sql-operation 'function "to_char" [stop] "HH24:MI")
			:from [black-time]
			:where [= [realm-id] realm-id]
			:order-by '([start] [stop])))))

(defun schedule-black-timer
    (&optional (wait (first (query "select extract(epoch from min(switch - now()))
				    from (select current_date + start as switch
					  from black_time
					  union
					  select current_date + stop
					  from black_time) as schedule
				    where switch > now()"
				   :flatp t))))
  (if wait
      (schedule-timer *black-time-switcher* wait)
      (unschedule-timer *black-time-switcher*)))

(defun realm/time/add/ ()
  (let ((realm-id (post-parameter "realm-id"))
	(start    (post-parameter "start"))
	(stop     (post-parameter "stop")))
    (insert-records :into [black-time]
		    :av-pairs `(([realm-id] ,realm-id)
				([start]    ,start)
				([stop]     ,stop)))
    (sb-thread:make-thread #'(lambda ()
			       (with-sauron-db ()
				 (generate-nginx-conf :black :file (nginx-black-conf))
				 (sb-ext:run-program "/bin/sh" `("-c" ,(nginx-reload)))
				 (schedule-black-timer)))
			   :name (format nil "generate ~a" (nginx-black-conf)))
    (redirect (format nil "/realm/?id=~a" realm-id))))

(defun realm/time/del/ ()
  (let ((id       (get-parameter "id"))
	(realm-id (get-parameter "realm-id")))
    (delete-records :from [black-time]
		    :where [= [id] id])
    (sb-thread:make-thread #'(lambda ()
			       (with-sauron-db ()
				 (generate-nginx-conf :black :file (nginx-black-conf))
				 (sb-ext:run-program "/bin/sh" `("-c" ,(nginx-reload)))
				 (schedule-black-timer)))
			   :name (format nil "generate ~a" (nginx-black-conf)))
    (redirect (format nil "/realm/?id=~a" realm-id))))

(defun realm-internal-plist (&key realm-id)
  (list :realm-internal-list
	(format nil "~{~a~^~%~}" (select [address]
					 :from [realm-internal-address]
					 :where [= [realm-id] realm-id]
					 :order-by [address]
					 :flatp t))))

(defun realm-external-plist (&key realm-id)
  (list :realm-external-list
	(format nil "~{~a~^~%~}" (select [address]
					 :from [realm-external-address]
					 :where [= [realm-id] realm-id]
					 :order-by [address]
					 :flatp t))))

(defun black-list-plist (&key realm-id)
  (list :black-list
	(format nil "~{~a~^~%~}" (select [domain]
					 :from [black-list]
					 :where [= [realm-id] realm-id]
					 :order-by [domain]
					 :flatp t))))

(defun black-sheet ()
  (list :blackness (format nil "~{~a~^ ~}"
			   (select [domain]
				   :from '([black-list] [realm])
				   :where [and [= [realm id] [realm-id]]
					       [= [active] "true"]]
				   :order-by [domain]
				   :flatp t))
	:black-sheet (loop for (block-url domain address) in
			  (select [block-url] [domain] [address]
				  :from '([realm] [black-list] [realm-external-address] [black-time])
				  :where [and [= [realm id] [black-list realm-id]]
					      [= [realm id] [realm-external-address realm-id]]
					      [= [realm id] [black-time realm-id]]
					      (sql-expression :string "now() - current_date between start and stop")
					      [= [active] "true"]]
				  :order-by '([address] [domain]))
			collect (list :block-url block-url
				      :domain    domain
				      :address   address))))

(defun realm/ ()
  (with-output-to-string (*default-template-output*)
    (let ((id (string-integer (get-parameter "id"))))
      (if id
	  (ftmpl #p"realm/one.html" (append (realm-plist :id id)
					    (black-time-plist     :realm-id id)
					    (realm-internal-plist :realm-id id)
					    (realm-external-plist :realm-id id)
					    (black-list-plist     :realm-id id)
					    (sauron-plist)))
	  (ftmpl #p"realm/list.html" (append (realm-plist)
					     (list :realm-tab t)
					     (sauron-plist)))))))

(defun realm/new/ ()
  (with-output-to-string (*default-template-output*)
    (ftmpl #p"realm/one.html" (append (list :active    t
					    :block-url (block-url))
				      (sauron-plist)))))

(defun realm/set/ ()
  (let ((id        (post-parameter "id"))
	(name      (post-parameter "name"))
	(block-url (post-parameter "block-url"))
	(active    (equal (post-parameter "active") "yes"))
	(realm-internal-list (post-parameter "realm-internal-list"))
	(realm-external-list (post-parameter "realm-external-list"))
	(black-list          (post-parameter "black-list")))
    (cond ((> (length id) 0)
	   (update-records [realm]
			   :av-pairs `(([name]      ,name)
				       ([block-url] ,block-url)
				       ([active]    ,active))
			   :where [= [id] id])
	   (push '(:motd-realm-set t) (session-value :motd))
	   (with-transaction ()
	     (delete-records :from [realm-internal-address]
			     :where [= [realm-id] id])
	     (do-matches-as-strings (address "\\d+\\.\\d+\\.\\d+\\.\\d+" realm-internal-list nil :sharedp t)
	       (insert-records :into [realm-internal-address]
			       :av-pairs `(([realm-id] ,id)
					   ([address]  ,address)))))
	   (with-transaction ()
	     (delete-records :from [realm-external-address]
			     :where [= [realm-id] id])
	     (do-matches-as-strings (address "\\d+\\.\\d+\\.\\d+\\.\\d+" realm-external-list nil :sharedp t)
	       (insert-records :into [realm-external-address]
			       :av-pairs `(([realm-id] ,id)
					   ([address]  ,address)))))
	   (with-transaction ()
	     (delete-records :from [black-list]
			     :where [= [realm-id] id])
	     (do-matches-as-strings (domain "[^\\s]+" black-list nil :sharedp t)
	       (insert-records :into [black-list]
			       :av-pairs `(([realm-id] ,id)
					   ([domain] ,domain))))))
	  (t (setq id (sequence-next [realm-seq]))
	     (insert-records :into [realm]
			     :av-pairs `(([id]        ,id)
					 ([name]      ,name)
					 ([block-url] ,block-url)
					 ([active]    ,active)))
	     (push '(:motd-realm-added t) (session-value :motd))))
    (sb-thread:make-thread #'(lambda ()
			       (with-sauron-db ()
				 (generate-nginx-conf :black :file (nginx-black-conf))
				 (sb-ext:run-program "/bin/sh" `("-c" ,(nginx-reload)))
				 (schedule-black-timer)
				 (propagate-realm)))
			   :name (format nil "generate ~a" (nginx-black-conf)))
    (redirect (format nil "/realm/?id=~a" id))))

;;;;
