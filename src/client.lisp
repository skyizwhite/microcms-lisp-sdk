(defpackage #:microcms/client
  (:use #:cl)
  (:import-from #:alexandria
                #:remove-from-plist
                #:symbolicate
                #:alist-hash-table
                #:hash-table-alist
                #:make-keyword)
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
                #:to-camel-case
                #:to-kebab-case)
  (:export #:*api-key*
           #:*service-domain*
           #:get-list
           #:get-item
           #:create-item
           #:update-item
           #:delete-item
           #:get-object
           #:update-object
           #:%build-uri
           #:%build-query
           #:%build-content
           #:%parse-response))
(in-package #:microcms/client)

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
          (%parse-response res-body)))
    (http-request-failed (e)
      (format *error-output* "microCMS status: ~D~%" (response-status e)))))

(defun %build-uri (endpoint &key path query)
  (render-uri (make-uri :scheme "https"
                        :host (format nil "~A.microcms.io" *service-domain*)
                        :path (format nil "/api/v1/~A~@[/~A~]" endpoint path)
                        :query (%build-query query))))

(defun %kebab-case-plist->camel-case-alist (plist)
  (loop :for (key val) :on plist :by #'cddr
        :collect (cons (to-camel-case (symbol-name key)) val)))

(defun %jzon-object->kebab-case-plist (obj)
  (typecase obj
    (simple-vector (mapcar #'%jzon-object->kebab-case-plist
                           (coerce obj 'list)))
    (hash-table (loop :for (key . val) :in (hash-table-alist obj)
                      :append (list (make-keyword (string-upcase (to-kebab-case key)))
                                    (%jzon-object->kebab-case-plist val))))
    (t obj)))

(defun %build-query (query)
  (%kebab-case-plist->camel-case-alist query))

(defun %build-content (content)
  (and content (stringify (alist-hash-table (%kebab-case-plist->camel-case-alist content)))))

(defun %parse-response (body)
  (%jzon-object->kebab-case-plist (parse body)))

(defun get-list (endpoint &key query)
  (%request :get endpoint :query query))

(defun get-item (endpoint id &key query)
  (%request :get endpoint :path id :query query))

(defun create-item (endpoint content &key query)
  (let ((id (getf content :id)))
    (%request (if id :put :post)
              endpoint
              :path id
              :query query
              :content (remove-from-plist content :id))))

(defun update-item (endpoint id content)
  (%request :patch endpoint :path id :content content))

(defun delete-item (endpoint id)
  (%request :delete endpoint :path id))

(defun get-object (endpoint &key query)
  (%request :get endpoint :query query))

(defun update-object (endpoint content)
  (%request :patch endpoint :content content))
