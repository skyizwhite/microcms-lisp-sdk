(defpackage #:microcms
  (:nicknames #:microcms/main)
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-plist
                #:assoc-value
                #:remove-from-plist
                #:symbolicate)
  (:import-from #:jonathan
                #:to-json
                #:parse)
  (:import-from #:dexador
                #:request
                #:http-request-failed)
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

(defun %request (method endpoint &optional (path "") (query nil) (content nil))
  (let* ((url (%build-uri endpoint path query))
         (headers `(("X-MICROCMS-API-KEY" . ,*api-key*)
                    ("Content-Type" . "application/json"))))
    (format t "API request url: ~a~%" url)
    (handler-case
        (multiple-value-bind (res-body status resp-headers)
            (request url
                     :method method
                     :headers headers
                     :content (and content (to-json content))
                     :force-binary nil)
          (format t "API response status: ~a~%" status)
          (when (and (stringp res-body)
                     (search "application/json" (gethash "content-type" resp-headers)))
            (parse res-body)))
      (http-request-failed ()
        '(:|error| "API request failed")))))

(defun %build-uri (endpoint &optional (path "") (query nil))
  (let ((uri (make-uri
              :scheme "https"
              :host (format nil "~A.microcms.io" *service-domain*)
              :path (format nil "/api/v1/~A/~A" endpoint path)
              :query (%build-query query))))
    (render-uri uri)))

(defun %build-query (query)
  (loop :for (key val) :on query :by #'cddr
        :collect (cons (to-camel-case (symbol-name key)) val)))

(defmacro define-list-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint))))
    `(progn
       (defun ,(symbolicate 'get- endpoint '-list) (&optional query)
         (%request :get ,str-endpoint nil query))
       (defun ,(symbolicate 'get- endpoint '-list-detail) (id &optional query)
         (%request :get ,str-endpoint id query))
       (defun ,(symbolicate 'create- endpoint) (content &optional query)
         (let ((id (getf content :|id|)))
           (%request (if id :put :post)
                     ,str-endpoint
                     id
                     query
                     (remove-from-plist content :|id|))))
       (defun ,(symbolicate 'update- endpoint) (id content)
         (%request :patch ,str-endpoint id nil content))
       (defun ,(symbolicate 'delete- endpoint) (id)
         (%request :delete ,str-endpoint id))
       nil)))

(defmacro define-object-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint))))
    `(progn
       (defun ,(symbolicate 'get- endpoint '-object) ()
         (%request :get ,str-endpoint))
       (defun ,(symbolicate 'update- endpoint) (content)
         (%request :patch ,str-endpoint nil nil content))
       nil)))
