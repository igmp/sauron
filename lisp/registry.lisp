(in-package :sauron)
(file-enable-sql-reader-syntax)

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
			       (with-sauron-db ()
				 (extract-registry zip)
				 (evaluate-registry :id id)
				 (when (equal exec "yes")
				   (execute-registry :id id))))
			   :name (format nil "process registry ~a" id))
    (push '(:motd-registry-loaded t) (session-value :motd))
    (when (equal exec "yes")
      (push '(:motd-registry-executed t) (session-value :motd)))
    (clear-outdated)
    (redirect (format nil "/registry/#~a" id))))

(defun registry/exec/ ()
  (setf (active-sauron) "exec")
  (let ((id (string-integer (get-parameter "id"))))
    (sb-thread:make-thread #'(lambda ()
			       (with-sauron-db ()
				 (execute-registry :id id)))
			   :name (format nil "exec-registry ~a" id))
    (push '(:motd-registry-executed t) (session-value :motd)))
  (redirect "/status/"))

(defun registry/load/ ()
  (with-output-to-string (*default-template-output*)
    (ftmpl #p"registry/load.html" (append (list :load-tab t)
					  (sauron-plist)))))

;;;;
