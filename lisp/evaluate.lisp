(in-package :sauron)
(file-enable-sql-reader-syntax)

(defun extract-registry (zip)
  (sb-posix:chdir (tmp-directory))
  (run-program "unzip" `(,zip "dump.xml")
	       :search t))

(defun parse-rkn-time (string)
  (register-groups-bind (date time) ("^([-0-9]*)T([:0-9]*)$" string)
    (values date time)))

(defun parse-rkn-tz-time (string)
  (register-groups-bind (date time tz)
      ("([-0-9]*)T([:0-9]*)(\\+[:0-9]+)?$" string)
    (values date time tz)))

;;; - Разбираем переданный XML файл.
;;; - Читаем данные относительно дампа в целом: время обновления,
;;;   версию формата etc.
;;; - Временная зона есть только во времени последнего обновления.
;;;   Во всех других временах внутри дампа зоны нет.  Так что будем
;;;   использовать зону из времени обновления дампа.
;;; - Читаем список всех требований блокировки.  Часть информации
;;;   содержится в описании самого требования, а информация о решении
;;;   находится глубже -- внутри требования.  Так что сначала добавляем в
;;;   БД строку с требованием, а потом обновляем в ней информацию о решении.
;;; - В требовании может быть список доменов, список URL-ов, список
;;;   IP адресов или сетей:
;;;   - Если списка URL-ов нет, то записываются только домены.
;;;   - Если список URL-ов есть, то записывюатся все домены и для
;;;     каждого домена все URL-ы.
;;;   - IP адреса используются независимо от доменов и URL-ов.
(defun evaluate-registry (&key id (file (tmp-registry)))
  (with-open-file (stream file :external-format '(:cp1251))
    (delete-file stream) ; prevent any external intrusion
    (let ((registry (xmls:parse stream))
	  (resource-count 0)
	  up-date up-time up-tz up-date-urg up-time-urg format-version)
      ;; "header" of the registry
      (dolist (av-pair (cadr registry))
	(cond ((equal (first av-pair) "updateTime")
	       (multiple-value-setq (up-date up-time up-tz)
		 (parse-rkn-tz-time (second av-pair))))
	      ((equal (first av-pair) "updateTimeUrgently")
	       (multiple-value-setq (up-date-urg up-time-urg)
		 (parse-rkn-tz-time (second av-pair))))
	      ((equal (first av-pair) "formatVersion")
	       (setq format-version (second av-pair)))))
      (acceptor-log-message *http-server* :info "evaluating registry: ~a ~a~a" up-date up-time up-tz)
      (update-records [registry]
		      :av-pairs `(([id]                    ,id)
				  ([-update-time]          ,(format nil "~a ~a ~a" up-date up-time up-tz))
				  ([-update-time-urgently] ,(when up-date-urg
							      (format nil "~a ~a ~a" up-date-urg up-time-urg up-tz)))
				  ([-format-version]       ,format-version))
		      :where [= [id] id])
      ;; contents of the registry
      (dolist (content (cddr registry))
	(let ((content-id (sequence-next [content-seq]))
	      -id inc-date inc-time urgency entry-type block-domain
	      dcs-date dcs-number dcs-org domain-list url-list)
	  ;; "header" of an individual content
	  (dolist (av-pair (cadr content))
	    (cond ((equal (first av-pair) "id")
		   (setq -id (parse-integer (second av-pair))))
		  ((equal (first av-pair) "includeTime")
		   (multiple-value-setq (inc-date inc-time)
		     (parse-rkn-time (second av-pair))))
		  ((equal (first av-pair) "urgencyType")
		   (setq urgency (equal (second av-pair) "1")))
		  ((equal (first av-pair) "entryType")
		   (setq entry-type (parse-integer (second av-pair))))
		  ((equal (first av-pair) "blockType")
		   (setq block-domain (equal (second av-pair) "domain")))))
	  (insert-records :into [content]
			  :av-pairs `(([id]             ,content-id)
				      ([registry-id]    ,id)
				      ([-id]            ,-id)
				      ([-include-time]  ,(format nil "~a ~a ~a" inc-date inc-time up-tz))
				      ([-urgency]       ,urgency)
				      ([-entry-type-id] ,entry-type)
				      ([-block-domain]  ,block-domain)))
	  ;; contents of the content
	  (dolist (resource (cddr content))
	    (cond ((equal (first resource) "decision")
		   (dolist (av-pair (second resource))
		     (cond ((equal (first av-pair) "date")   (setq dcs-date   (second av-pair)))
			   ((equal (first av-pair) "number") (setq dcs-number (second av-pair)))
			   ((equal (first av-pair) "org")    (setq dcs-org    (second av-pair)))))
		   (update-records [content]
				   :av-pairs `(([-decision-date]   ,dcs-date)
					       ([-decision-number] ,dcs-number)
					       ([-decision-org]    ,dcs-org))
				   :where [= [id] content-id]))
		  ((equal (first resource) "domain")
		   (push (third resource) domain-list))
		  ((equal (first resource) "url")
		   (push (third resource) url-list))
		  ((equal (first resource) "ip")
		   (insert-records :into [rkn-ip-address]
				   :av-pairs `(([registry-id] ,id)
					       ([content-id]  ,content-id)
					       ([-address]    ,(third resource)))))
		  ((equal (first resource) "ipSubnet")
		   (insert-records :into [rkn-ip-address]
				   :av-pairs `(([registry-id] ,id)
					       ([content-id]  ,content-id)
					       ([-address]    ,(third resource))
					       ([-subnet]     ,t))))))
	  (dolist (domain domain-list)
	    (cond (url-list
		   (dolist (url url-list)
		     (incf resource-count)
		     (register-groups-bind ((#'string-downcase proto domain) location entire)
			 ("(?:(http|https)://([^/]+)(/.*)?)|^(.*)$" url)
		       (if entire
			   (insert-records :into [resource]
					   :av-pairs `(([registry-id] ,id)
						       ([content-id]  ,content-id)
						       ([-url]        ,url)))
			   (insert-records :into [resource]
					   :av-pairs `(([registry-id] ,id)
						       ([content-id]  ,content-id)
						       ([ssl]         ,(not (equal proto "http")))
						       ([-domain]     ,domain)
						       ([-location]   ,location)))))))
		  (t (incf resource-count)
		     (insert-records :into [resource]
				     :av-pairs `(([registry-id] ,id)
						 ([content-id]  ,content-id)
						 ([-domain]     ,(string-downcase domain)))))))))
      (update-records [registry]
		      :av-pairs `(([completed]      t)
				  ([resource-count] ,resource-count))
		      :where [= [id] id]))))

(defun annihilate-registry (&key id time (op '=))
  (with-transaction ()
    (execute-command (concatenate 'string
				  "create temp table delete_registry on commit drop "
				  "as select id from registry "
				  "where " (format nil "~a ~a '~a' "
						   (if id "id" "_time")
						   op
						   (if id id time))))
    (let* ((delete (select [id]
			   :from [delete-registry]
			   :flatp t))
	   (delete* (or delete (sql-expression :string "(0)"))))
      (delete-records :from [rkn-ip-address]
		      :where [in [registry-id] delete*])
      (delete-records :from [resource]
		      :where [in [registry-id] delete*])
      (delete-records :from [content]
		      :where [in [registry-id] delete*])
      (delete-records :from [registry]
		      :where [in [id] delete*])
      (dolist (id delete)
	(ignore-errors (delete-file (format nil "~a~a" (zip-directory) id)))))))

(defun clear-outdated (&key (days (store-interval)))
  (dolist (id (query (concatenate 'string
				  "select id "
				  "from registry "
				  "where _update_time + interval '" days " days' < now() ")
		     :flatp t))
    (annihilate-registry :id id)
    (delete-records :from [request]
		    :where [< [time] (sql-expression :string (format nil "now() - interval '~a days'" (store-interval)))])
    (delete-records :from [last-info]
		    :where [< [time] (sql-expression :string (format nil "now() - interval '~a days'" (store-interval)))])))

(defun registry/del/ ()
  (let ((id (string-integer (get-parameter "id"))))
    (annihilate-registry :id id))
  (redirect "/registry/"))

;;;;
