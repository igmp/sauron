(in-package :sauron)
(file-enable-sql-reader-syntax)

(defun config-plist ()
  (list :active-sauron     (active-sauron)
	:home-directory    (home-directory)
	:bird-reload       (bird-reload)
	:bird-protocol     (bird-protocol)
	:block-url         (block-url)
	:nginx-reload      (nginx-reload)
	:root-means-domain (root-means-domain)))

(defun config/ ()
  (htmpl #p"config.html"
	 (append (config-plist)
		 (list :config-tab t)
		 (general-plist))))

(defun config/set/ ()
  (setf (active-sauron)     (equal (post-parameter "active-sauron")     "yes")
	(root-means-domain) (equal (post-parameter "root-means-domain") "yes")
	(bird-reload)       (post-parameter "bird-reload")
	(bird-protocol)     (post-parameter "bird-protocol")
	(block-url)         (post-parameter "block-url")
	(nginx-reload)      (post-parameter "nginx-reload"))
  (when (and (active-sauron)
	     (not (working-registry-id)))
    (setf (working-registry-id) (last-registry-id)))
  (sb-thread:make-thread #'(lambda ()
			     (with-sauron-db ()
			       (execute-registry :id (working-registry-id))))
			 :name (format nil "exec-registry ~a" (working-registry-id)))
  (push '(:motd-config-set t) (session-value :motd))
  (redirect "/config/"))

(defun status-plist ()
  (append (list :last-info-date (caar (query "select to_timestamp(max(_date / 1000)) from last_info"))
		:last-registry-time (caar (select [max [-update-time]]
						  :from [registry]))
		:working-registry-time (caar (select [-update-time]
						     :from [registry]
						     :where [= [id] (working-registry-id)]))
		:working-ip (caar (select [count [distinct [-address]]]
					  :from [ip-address]
					  :where [= [registry-id] (working-registry-id)])))
	  (destructuring-bind (http https bad total)
	      (first (query (concatenate 'string
					 "select count(case when ssl is NULL then 1 end) as http, "
					 "  count(ssl) as https, "
					 "  count(_url) as bad, "
					 "  count(*) as total "
					 "from resource "
					 "where registry_id = '" (or (working-registry-id) "0") "' ")))
	    (list :working-http  http
		  :working-https https
		  :working-bad   bad
		  :working-total total))))

(defun status/ ()
  (htmpl #p"status.html"
	 (append (status-plist)
		 (list :status-tab t)
		 (general-plist))))

;;;;
