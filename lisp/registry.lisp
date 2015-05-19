(in-package :sauron)
(file-enable-sql-reader-syntax)

(defun registry-plist* (tuple)
  (let ((working-registry-id (parse-integer (or (working-registry-id) "0"))))
    (destructuring-bind (id up-time up-time-urg completed resource-count) tuple
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
	    (mapcar #'registry-plist*
		    (query (concatenate 'string
					"select registry.id, _update_time, _update_time_urgently, completed, count(resource.id) "
					"from registry "
					"left join resource on registry_id = registry.id "
					"group by registry.id, _update_time, _update_time_urgently, completed "
					"order by _update_time desc, registry.id desc "))))))

(defun registry/ ()
  (let ((id (string-integer (get-parameter "id"))))
    (if id
	(htmpl #p"registry/one.html" (registry-plist :id id))
	(htmpl #p"registry/list.html" (append (registry-plist)
					      (list :registry-tab t)
					      (general-plist))))))

(defun registry/del/ ()
  (let ((id (string-integer (get-parameter "id"))))
    (annihilate-registry :id id))
  (redirect "/registry/"))

(defun registry/come/ ()
  (let* ((id (sequence-next [registry-seq]))
	 (file (post-parameter "file"))
	 (exec (post-parameter "exec"))
	 (zip (format nil "~a~a" (zip-directory) id)))
    (with-transaction ()
      (insert-records :into [registry]
		      :av-pairs `(([id] ,id)))
      (rename-file (first file) zip))
    (sb-thread:make-thread #'(lambda ()
			       (with-database (*default-database* (list *db-host* *db-name* *db-user* *db-password* *db-port*)
								  :pool t)
				 (extract-registry zip)
				 (evaluate-registry :id id)
				 (when (equal exec "yes")
				   (execute-registry :id id))))
			   :name (format nil "process-registry ~a" id))
    (push '(:motd-registry-loaded t) (session-value :motd))
    (when (equal exec "yes")
      (push '(:motd-registry-executed t) (session-value :motd)))
    (redirect (format nil "/registry/#~a" id))))

(defun registry/exec/ ()
  (setf (active-sauron) "exec")
  (let ((id (string-integer (get-parameter "id"))))
    (sb-thread:make-thread #'(lambda ()
			       (with-database (*default-database* (list *db-host* *db-name* *db-user* *db-password* *db-port*)
								  :pool t)
				 (execute-registry :id id)))
			   :name (format nil "exec-registry ~a" id))
    (push '(:motd-registry-executed t) (session-value :motd)))
  (redirect "/status/"))

(defun registry/load/ ()
  (htmpl #p"registry/load.html" (append (list :load-tab t)
					(general-plist))))

;;;;
