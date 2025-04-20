(defpackage #:microcms
  (:nicknames #:microcms/main)
  (:use #:cl)
  (:import-from #:alexandria
                #:remove-from-plist
                #:symbolicate
                #:plist-hash-table)
  (:import-from #:com.inuoe.jzon
                #:stringify
                #:parse)
  (:import-from #:dexador
                #:request
                #:http-request-failed
                #:response-status)
  (:import-from #:quri
                #:make-uri
                #:render-uri)
  (:import-from #:kebab
                #:to-camel-case)
  (:export #:*api-key*
           #:*service-domain*
           #:define-list-client
           #:define-object-client))
(in-package #:microcms)

(defparameter *api-key* nil)
(defparameter *service-domain* nil)

(defun %request (method endpoint &key path query content)
  (or *service-domain* (error "microcms:*service-domain* is not configured."))
  (or *api-key* (error "microcms:*api-key* is not configured."))
  (handler-case
      (multiple-value-bind (res-body status res-headers)
          (request (%build-uri endpoint :path path :query query)
                   :method method
                   :headers `(("X-MICROCMS-API-KEY" . ,*api-key*)
                              ("Content-Type" . "application/json"))
                   :content (%build-content content)
                   :force-binary nil)
        (format t "microCMS status: ~D~%" status)
        (when (and (stringp res-body)
                   (search "application/json" (gethash "content-type" res-headers)))
          (parse res-body)))
    (http-request-failed (e)
      (format *error-output* "microCMS status: ~D~%" (response-status e)))))

(defun %build-uri (endpoint &key path query)
  (render-uri (make-uri :scheme "https"
                        :host (format nil "~A.microcms.io" *service-domain*)
                        :path (format nil "/api/v1/~A~@[/~A~]" endpoint path)
                        :query (%build-query query))))

(defun %build-query (query)
  (loop :for (key val) :on query :by #'cddr
        :collect (cons (to-camel-case (symbol-name key)) val)))

(defun %build-content (content)
  (and content (stringify (plist-hash-table content))))

(defmacro define-list-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint)))
        (get-list (symbolicate 'get- endpoint '-list))
        (get-detail (symbolicate 'get- endpoint '-detail))
        (create (symbolicate 'create- endpoint))
        (update (symbolicate 'update- endpoint))
        (delete (symbolicate 'delete- endpoint)))
    `(list
       (defun ,get-list (&key query)
         (%request :get ,str-endpoint :query query))
       (defun ,get-detail (id &key query)
         (%request :get ,str-endpoint :path id :query query))
       (defun ,create (content &key query)
         (let ((id (getf content :|id|)))
           (%request (if id :put :post)
                     ,str-endpoint
                     :path id
                     :query query
                     :content (remove-from-plist content :|id|))))
       (defun ,update (id content)
         (%request :patch ,str-endpoint :path id :content content))
       (defun ,delete (id)
         (%request :delete ,str-endpoint :path id)))))

(defmacro define-object-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint)))
        (get-object (symbolicate 'get- endpoint '-object))
        (update (symbolicate 'update- endpoint)))
    `(list
       (defun ,get-object (&key query)
         (%request :get ,str-endpoint :query query))
       (defun ,update (content)
         (%request :patch ,str-endpoint :content content)))))
