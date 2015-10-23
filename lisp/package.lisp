(defpackage :sauron
  (:use :cl-base64 :cl-ppcre :clsql :common-lisp :html-template :hunchentoot :sb-ext :sb-thread)
  (:import-from :drakma
		#:http-request)
  (:export #:*db-host*
	   #:*db-name*
	   #:*db-password*
	   #:*db-port*
	   #:*db-user*
	   #:*download-semaphore*
	   #:*sauron-version*

	   #:annihilate-registry
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
	   #:process-registry
	   #:restart-sauron
	   #:schedule-black-timer
	   #:send-request
	   #:start-sauron
	   #:stop-sauron))

;;;;
