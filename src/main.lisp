(uiop:define-package :microcms
  (:nicknames #:microcms/main)
  (:use #:cl
        #:microcms/client)
  (:export #:*api-key*
           #:*service-domain*
           #:define-list-client
           #:define-object-client))
(in-package :microcms)
