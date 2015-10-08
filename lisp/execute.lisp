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
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (loop for (content-id content-id* domain location) in
	 (select [content id] [-id] [-domain] [-location]
		 :from '([content] [resource])
		 :where [and [= [content id] [content-id]]
			     [= [content registry-id] id]]
		 :order-by '([-id] [-domain] [-location]))
       do (format stream "~a; \"~a\"; \"~a\"; ~{~a~^, ~}~%"
		  content-id* domain (or location "") (select [-address]
							      :from [rkn-ip-address]
							      :where [= [content-id] content-id]
							      :flatp t)))))

(defgeneric generate-bird-conf (type &key id file)
  (:method :around (type &key (file (bird-conf)) &allow-other-keys)
    (with-open-file (*default-template-output* file
					       :direction :output
					       :if-exists :supersede
					       :if-does-not-exist :create)
      (call-next-method)))
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
    (with-open-file (*default-template-output* file
					       :direction :output
					       :if-exists :supersede
					       :if-does-not-exist :create)
      (call-next-method)))
  (:method ((type (eql :black)) &key id file)
    (declare (ignore id file))
    (ftmpl #p"conf/nginx-black.conf"
	   (append (list :nginx-port     (nginx-port)
			 :nginx-resolver (nginx-resolver))
		   (black-sheet))))
  (:method ((type (eql :rkn)) &key (id (working-registry-id)) file)
    (declare (ignore file))
    ;; Nginx doesn't allow duplicate locations within one domain.
    ;; So we collect all locations into a hash tables (one hash table per domain).
    ;; Then we write a config file from that unique hashes.
    (let ((dmnhash (make-hash-table :test 'equal)) ; all known domains
	  (lcthash nil) ; all known locations for a current domain
	  (dmn0 ""))    ; previous domain
      (loop for (content-id domain location) in
	   (select [-id] [-domain] [-location]
		   :from '([content] [resource])
		   :where [and [= [content id] [content-id]]
			       [is [ssl] nil]
			       [is [-url] nil]
			       [= [content registry-id] id]]
		   :order-by '([-domain] [-location]))
	 do (let ((location* (url-encode* location)))
	      (unless (string-equal domain dmn0)
		(setq lcthash (make-hash-table :test 'equal)))
	      (setf (gethash location* lcthash)
		    (append (list :content-id content-id)
			    (let ((root (or (not location*)
					    (and (eql location* "/")
						 (root-means-domain)))))
			      (when root
				(list :root root)))))
	      (unless (string-equal domain dmn0)
		(setf (gethash domain dmnhash) lcthash))
	      (setq dmn0 domain))
	 finally (setf (gethash domain dmnhash) lcthash)) ; last domain's locations
      (ftmpl #p"conf/nginx-rkn.conf"
	     (list :nginx-port     (nginx-port)
		   :nginx-resolver (nginx-resolver)
		   :block-url      (block-url)
		   :domain-list (loop for domain being each hash-key in dmnhash using (hash-value lcthash)
				   collect (list :domain domain
						 :resource-list (if (> (hash-table-count lcthash) 0)
								    (loop for location being each hash-key in lcthash using (hash-value lctadd)
								       collect (append (list :location (or location "/"))
										       lctadd))
								    (list (list :location "/"
										:root (root-means-domain)))))))))))

(defun execute-registry (&key (id (working-registry-id)))
  (setf (working-registry-id) id)
  (generate-registry-csv :id (and (active-sauron) id))
  (generate-bird-conf :static :id (and (active-sauron) id))
  (sb-ext:run-program "/bin/sh" `("-c" ,(bird-reload)))
  (generate-nginx-conf :rkn :file (nginx-rkn-conf) :id (and (active-sauron) id))
  (sb-ext:run-program "/bin/sh" `("-c" ,(nginx-reload))))

;;;;
