(defpackage :sauron
  (:use :cl-base64 :cl-ppcre :clsql :common-lisp :html-template :hunchentoot
	:sb-concurrency :sb-ext :sb-thread)
  (:import-from :drakma
		#:http-request)
  (:export #:*black-semaphore*
	   #:*check-semaphore*
	   #:*db-host*
	   #:*db-name*
	   #:*db-password*
	   #:*db-port*
	   #:*db-user*
	   #:*download-semaphore*
	   #:*execute-mailbox*
	   #:*process-mailbox*
	   #:*sauron-version*

	   #:annihilate-registry
	   #:black-switch
	   #:black-time-interval
	   #:check-registry
	   #:clear-outdated
	   #:download-registry
	   #:evaluate-registry
	   #:execute-registry
	   #:extract-registry
	   #:generate-bird-conf
	   #:generate-nginx-conf
	   #:generate-registry-csv
	   #:get-last-dump-date
	   #:get-last-dump-date-ex
	   #:get-result
	   #:init-sauron
	   #:process-registry
	   #:schedule-black-timer
	   #:send-request))

;;;;
