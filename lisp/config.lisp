(in-package :sauron)
(file-enable-sql-reader-syntax)

(defun config-plist ()
  (list :active-sauron     (active-sauron)
	:add-black         (add-black)
	:del-black         (del-black)
	:bird-reload       (bird-reload)
	:bird-protocol     (bird-protocol)
	:block-url         (block-url)
	:home-directory    (home-directory)
	:list-black        (list-black)
	:nginx-port        (nginx-port)
	:nginx-reload      (nginx-reload)
	:nginx-resolver    (nginx-resolver)
	:root-means-domain (root-means-domain)
	:routers           (routers)
	:store-days        (store-days)))

(defun config/ ()
  (with-output-to-string (*default-template-output*)
    (ftmpl #p"config.html"
	   (append (config-plist)
		   (list :config-tab t)
		   (sauron-plist)))))

(defun config/set/ ()
  (setf (active-sauron)     (equal (post-parameter "active-sauron")     "yes")
	(root-means-domain) (equal (post-parameter "root-means-domain") "yes")
	(add-black)      (post-parameter "add-black")
	(bird-reload)    (post-parameter "bird-reload")
	(bird-protocol)  (post-parameter "bird-protocol")
	(block-url)      (post-parameter "block-url")
	(del-black)      (post-parameter "del-black")
	(list-black)     (post-parameter "list-black")
	(nginx-port)     (post-parameter "nginx-port")
	(nginx-reload)   (post-parameter "nginx-reload")
	(nginx-resolver) (post-parameter "nginx-resolver")
	(routers)        (post-parameter "routers")
	(store-days)     (post-parameter "store-days"))
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
					  :from [rkn-ip-address]
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
  (with-output-to-string (*default-template-output*)
    (ftmpl #p"status.html"
	   (append (status-plist)
		   (list :status-tab t)
		   (sauron-plist)))))

;;;;
