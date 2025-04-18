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

(defun %get-list (endpoint &optional query)
  (%request :get endpoint "" query))

(defun %get-list-detail (endpoint id &optional (query nil))
  (%request :get endpoint id query))

(defun %create (endpoint content &optional query)
  (let ((id (getf content :|id|))
        (pure-content (remove-from-plist content :|id|)))
    (if id
        (%put endpoint id pure-content)
        (%post endpoint pure-content query))))

(defun %put (endpoint id content &optional query)
  (%request :put endpoint id query content))

(defun %post (endpoint content &optional query)
  (%request :post endpoint "" query content))

(defun %update (endpoint id content)
  (%request :patch endpoint id nil content))

(defun %delete (endpoint id)
  (%request :delete endpoint id))

(defun %get-object (endpoint)
  (%request :get endpoint))

(defun %request (method endpoint &optional (path "") (query nil) (body nil))
  (let* ((url (%build-uri endpoint path query))
         (headers `(("X-MICROCMS-API-KEY" . ,*api-key*)
                    ("Content-Type" . "application/json"))))
    (format t "API request url: ~a~%" url)
    (handler-case
        (multiple-value-bind (resp-body status resp-headers)
            (request url
                     :method method
                     :headers headers
                     :content (and body (to-json body))
                     :force-binary nil)
          (format t "API response status: ~a~%" status)
          (when (and (stringp resp-body)
                     (search "application/json" (gethash "content-type" resp-headers)))
            (parse resp-body)))
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
         (%get-list ,str-endpoint query))
       (defun ,(symbolicate 'get- endpoint '-list-detail) (id &optional query)
         (%get-list-detail ,str-endpoint id query))
       (defun ,(symbolicate 'create- endpoint) (content &optional query)
         (%create ,str-endpoint content query))
       (defun ,(symbolicate 'update- endpoint) (id content)
         (%update ,str-endpoint id content))
       (defun ,(symbolicate 'delete- endpoint) (id)
         (%delete ,str-endpoint id))
       nil)))

(defmacro define-object-client (endpoint)
  (let ((str-endpoint (string-downcase (string endpoint))))
    `(progn
       (defun ,(symbolicate 'get- endpoint '-object) ()
         (%get-object ,str-endpoint))
       (defun ,(symbolicate 'update- endpoint) (content)
         (%update ,str-endpoint nil content))
       nil)))
