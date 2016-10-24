(in-package :sauron)
(file-enable-sql-reader-syntax)

(defparameter *rkn-web-service* "http://vigruzki.rkn.gov.ru/services/OperatorRequest/")

(defparameter *max-request-number* 3
  "Maximum number of request before giving up.")

(defun last-registry-date ()
  (apply #'values (or (first (query "select extract(epoch from _update_time_urgently)::integer,
				       extract(epoch from _update_time)::integer
				     from registry
				     order by _update_time_urgently desc, _update_time desc
				     limit 1"))
		      '(0 0))))

(defun get-last-dump-date ()
  (dotimes (i *max-request-number*)
    (multiple-value-bind (body status)
	(http-request *rkn-web-service*
		      :method :post
		      :content (with-output-to-string (*default-template-output*)
				 (ftmpl #p"xml/getLastDumpDate.xml" nil)))
      (when (eq status 200) ;; got OK
	(let* ((reply (cddr (third (third (xmls:parse body)))))
	       (timestamp (parse-integer (third (find "lastDumpDate" reply :key #'first :test #'equal)))))
	  (return timestamp))))
    (sleep 1)))

(defun get-last-dump-date-ex ()
  (dotimes (i *max-request-number*)
    (multiple-value-bind (body status)
	(http-request *rkn-web-service*
		      :method :post
		      :content (with-output-to-string (*default-template-output*)
				 (ftmpl #p"xml/getLastDumpDateEx.xml" nil)))
      (when (eq status 200) ;; got OK
	(let* ((reply (cddr (third (third (xmls:parse body)))))
	       (timestamp   (/ (parse-integer (third (find "lastDumpDate"         reply :key #'first :test #'equal))) 1000))
	       (urgently    (/ (parse-integer (third (find "lastDumpDateUrgently" reply :key #'first :test #'equal))) 1000))
	       (web-service (third (find "webServiceVersion"    reply :key #'first :test #'equal)))
	       (dump-format (third (find "dumpFormatVersion"    reply :key #'first :test #'equal)))
	       (doc         (third (find "docVersion"           reply :key #'first :test #'equal)))
	       (id          (sequence-next [last-info-seq])))
	  (insert-records :into [last-info]
			  :av-pairs `(([id]           ,id)
				      ([-date]        ,timestamp)
				      ([-urgently]    ,urgently)
				      ([-web-service] ,web-service)
				      ([-dump-format] ,dump-format)
				      ([-doc]         ,doc)))
	  (return (values urgently timestamp id)))))
    (sleep 1)))

(defun send-request (&optional (request-file (format nil "~adata/xml/request.xml" (home-directory)))
		       (signature-file (format nil "~adata/xml/request.xml.sig" (home-directory)))
		       (dump-format (dump-format)))
  (dotimes (i *max-request-number*)
    (multiple-value-bind (body status)
	(http-request *rkn-web-service*
		      :method :post
		      :content (with-output-to-string (*default-template-output*)
				 (ftmpl #p"xml/sendRequest.xml"
					(list :request-file (file-base64 request-file)
					      :signature-file (file-base64 signature-file)
					      :dump-format dump-format))))
      (when (eq status 200) ;; got OK
	(let* ((reply (cddr (third (third (xmls:parse body)))))
	       (result  (third (find "result"        reply :key #'first :test #'equal)))
	       (comment (third (find "resultComment" reply :key #'first :test #'equal)))
	       (code    (third (find "code"          reply :key #'first :test #'equal)))
	       (id (sequence-next [request-seq])))
	  (insert-records :into [request]
			  :av-pairs `(([id]       ,id)
				      ([-result]  ,result)
				      ([-comment] ,comment)
				      ([-code]    ,code)))
	  (return (values code comment)))))
    (sleep 1)))

(defun get-result (code)
  (dotimes (i *max-request-number*)
    (multiple-value-bind (body status)
	(http-request *rkn-web-service*
		      :method :post
		      :content (with-output-to-string (*default-template-output*)
				 (ftmpl #p"xml/getResult.xml"
					(list :code code))))
      (when (eq status 200) ;; got OK
	(let* ((reply (cddr (third (third (xmls:parse body)))))
	       (code* (parse-integer (third (find "resultCode" reply :key #'first :test #'equal))))
	       (result  (third (find "result"        reply :key #'first :test #'equal)))
	       (comment (third (find "resultComment" reply :key #'first :test #'equal)))
	       (request-id (query (concatenate 'string
					       "update request "
					       "set time = now(), "
					       (format nil "  _result = '~a', " result)
					       (format nil "  _comment = '~a' " (or comment ""))
					       (format nil "where _code = '~a' " code)
					       "returning id "))))
	  (if (equal result "true")
	      (let ((zip (third (find "registerZipArchive" reply :key #'first :test #'equal)))
		    (id (sequence-next [registry-seq])))
		(with-transaction ()
		  (with-open-file (stream (format nil "~a~a" (zip-directory) id)
					  :direction :output
					  :element-type '(unsigned-byte 8))
		    (write-sequence (base64-string-to-usb8-array zip)
				    stream))
		  (insert-records :into [registry]
				  :av-pairs `(([id]         ,id)
					      ([request-id] ,request-id))))
		(return (values id code*)))
	      (return (values nil code* comment))))))
    (sleep 1)))

(defun process-registry (&key id exec)
  (extract-registry (format nil "~a~a" (zip-directory) id))
  (evaluate-registry :id id)
  (when exec
    (send-message *execute-mailbox* (list :id id)))
  (clear-outdated))

(defun check-registry ()
  (multiple-value-bind (urgently date) (get-last-dump-date-ex)
    (acceptor-log-message *http-server* :info "last dump: date ~a, urgently ~a"
			  (unix-time-string date)
			  (unix-time-string urgently))
    (multiple-value-bind (last-urg last-date) (last-registry-date)
      (when (or (> urgently last-urg)
		(> (- date last-date) (* 60 60 (parse-integer (download-period)))))
	(signal-semaphore *download-semaphore*)))))

(defun download-registry ()
  (multiple-value-bind (code comment) (send-request)
    (unless code
      (acceptor-log-message *http-server* :error "while sending request: ~a" comment)
      (return-from download-registry))
    (acceptor-log-message *http-server* :info "waiting for an answer from RKN")
    (sleep 120) ; wait 2 minutes while RKN is preparing an answer
    (loop (multiple-value-bind (id code* comment) (get-result code)
	    (cond ((= code* 0)
		   (sleep 60)) ; wait 1 minute before next try
		  ((= code* 1)
		   (send-message *process-mailbox* (list :id id :exec t))
		   (return id))
		  ((< code* 0)
		   (acceptor-log-message *http-server* :error "while getting result of ~a: ~a" code comment)
		   (return)))))))

(defun registry/check/ ()
  (signal-semaphore *check-semaphore*)
  (push '(:motd-registry-checked t) (session-value :motd))
  (redirect (format nil "/status/")))

(defun registry/come/ ()
  (let* ((from (post-parameter "from"))
	 (file (post-parameter "file"))
	 (exec (equal (post-parameter "exec") "yes"))
	 (id (sequence-next [registry-seq]))
	 (zip (format nil "~a~a" (zip-directory) id)))
    (cond ((equal from "rkn")
	   (signal-semaphore *download-semaphore*)
	   (push '(:motd-registry-loading t) (session-value :motd)))
	  ((equal from "file")
	   (insert-records :into [registry]
			   :av-pairs `(([id] ,id)))
	   (rename-file (first file) zip)
	   (send-message *process-mailbox* (list :id id :exec exec))
	   (push '(:motd-registry-loaded t) (session-value :motd))
	   (when exec
	     (push '(:motd-registry-executed t) (session-value :motd)))))
    (redirect (format nil "/registry/#~a" id))))

;;;;
