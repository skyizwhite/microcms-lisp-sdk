(defpackage #:microcms
  (:nicknames #:microcms/main)
  (:use #:cl)
  (:import-from #:alexandria
                #:remove-from-plist
                #:symbolicate)
  (:import-from #:jonathan
                #:to-json
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
  (let* ((url (%build-uri endpoint :path path :query query))
         (req-headers `(("X-MICROCMS-API-KEY" . ,*api-key*)
                        ("Content-Type" . "application/json"))))
    (handler-case
        (multiple-value-bind (res-body status res-headers)
            (request url
                     :method method
                     :headers req-headers
                     :content (and content (to-json content))
                     :force-binary nil)
          (format t "microCMS status: ~D~%" status)
          (when (and (stringp res-body)
                     (search "application/json" (gethash "content-type" res-headers)))
            (parse res-body)))
      (http-request-failed (e)
        (format *error-output* "microCMS status: ~D~%" (response-status e))))))

(defun %build-uri (endpoint &key path query)
  (let ((uri (make-uri
              :scheme "https"
              :host (format nil "~A.microcms.io" *service-domain*)
              :path (format nil "/api/v1/~A~@[/~A~]" endpoint path)
              :query (%build-query query))))
    (render-uri uri)))

(defun %build-query (query)
  (loop :for (key val) :on query :by #'cddr
        :collect (cons (to-camel-case (symbol-name key)) val)))

(defmacro define-list-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint))))
    `(progn
       (defun ,(symbolicate 'get- endpoint '-list) (&key query)
         (%request :get ,str-endpoint :query query))
       (defun ,(symbolicate 'get- endpoint '-list-detail) (id &key query)
         (%request :get ,str-endpoint :path id :query query))
       (defun ,(symbolicate 'create- endpoint) (content &key query)
         (let ((id (getf content :|id|)))
           (%request (if id :put :post)
                     ,str-endpoint
                     :path id
                     :query query
                     :content (remove-from-plist content :|id|))))
       (defun ,(symbolicate 'update- endpoint) (id content)
         (%request :patch ,str-endpoint :path id :content content))
       (defun ,(symbolicate 'delete- endpoint) (id)
         (%request :delete ,str-endpoint :path id))
       nil)))

(defmacro define-object-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint))))
    `(progn
       (defun ,(symbolicate 'get- endpoint '-object) (&key query)
         (%request :get ,str-endpoint :query query))
       (defun ,(symbolicate 'update- endpoint) (content)
         (%request :patch ,str-endpoint :content content))
       nil)))