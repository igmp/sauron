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
							      :from [ip-address]
							      :where [= [content-id] content-id]
							      :flatp t)))))

(defun generate-bird-conf (&key (id (working-registry-id)) (file (bird-conf)))
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "protocol static ~a {~%" (bird-protocol))
    (loop for (address) in (select [distinct [-address]]
				   :from [ip-address]
				   :where [and [= [registry-id] id]
					       [is [-subnet] nil]]
				   :order-by [-address])
       do (format stream "~aroute       ~a/32~areject;~%" #\Tab address #\Tab))
    (format stream "}~%")))

(defun generate-nginx-conf (&key (id (working-registry-id)) (file (nginx-conf)))
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (let ((dm0 nil))
      (loop for (content-id domain location) in
	   (select [-id] [-domain] [-location]
		   :from '([content] [resource])
		   :where [and [= [content id] [content-id]]
			       [is [ssl] nil]
			       [= [content registry-id] id]]
		   :order-by '([-domain] [-location]))
	 do (progn (when (string-not-equal domain dm0)
		     (when dm0
		       (format stream "~alocation / {~%" #\Tab)
		       (format stream "~a~aproxy_pass $scheme://$host$request_uri;~%" #\Tab #\Tab)
		       (format stream "~a~aproxy_set_header Host $http_host;~%" #\Tab #\Tab)
		       (format stream "~a~aproxy_buffering off;~%" #\Tab #\Tab)
		       (format stream "~a}~%" #\Tab)
		       (format stream "}~%~%"))
		     (format stream "server {~%~aserver_name ~a;~%~%" #\Tab domain))
		   (if (or (not location)
			   (and (eql location "/") (root-means-domain)))
		       (format stream "~alocation ~~ . { # ~a~%" #\Tab content-id)
		       (format stream "~alocation = ~a { # ~a~%" #\Tab location content-id))
		   (format stream "~a~areturn 301 ~a~%" #\Tab #\Tab (block-url))
		   (format stream "~a}~%" #\Tab)
		   (setq dm0 domain)))
      (when dm0
	(format stream "}~%")))))

(defun execute-registry (&key (id (working-registry-id)))
  (setf (working-registry-id) id)
  (generate-registry-csv :id id)
  (generate-bird-conf    :id id)
  (generate-nginx-conf   :id id)
  (sb-ext:run-program (bird-init.d)  '("reload"))
  (sb-ext:run-program (nginx-init.d) '("reload")))

;;;;
