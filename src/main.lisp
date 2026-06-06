(uiop:define-package :microcms
  (:nicknames #:microcms/main)
  (:use #:cl
        #:microcms/client)
  (:export #:*api-key*
           #:*service-domain*
           #:get-list
           #:get-item
           #:create-item
           #:update-item
           #:delete-item
           #:get-object
           #:update-object
           #:microcms-error
           #:microcms-error-status
           #:microcms-error-body))
(in-package :microcms)
