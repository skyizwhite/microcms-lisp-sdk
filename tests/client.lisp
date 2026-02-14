(defpackage #:microcms-test/client
  (:use #:cl
        #:rove
        #:microcms/client))
(in-package #:microcms-test/client)

(deftest build-uri-test
  (testing "build-uri constructs correct endpoint URLs"
    (let ((*service-domain* "my-service"))
      (ok (string= (%build-uri "article")
                   "https://my-service.microcms.io/api/v1/article"))
      (ok (string= (%build-uri "article" :path "abc123")
                   "https://my-service.microcms.io/api/v1/article/abc123"))
      (ok (string= (%build-uri "article" :query '(:limit 10 :offset 5))
                   "https://my-service.microcms.io/api/v1/article?limit=10&offset=5")))))

(deftest build-query-test
  (testing "build-query converts kebab-case plist to camelCase alist"
    (let ((result (%build-query '(:foo 1 :bar-baz 2))))
      (ok (equal result '(("foo" . 1) ("barBaz" . 2)))))))

(deftest build-content-test
  (testing "build-content converts kebab-case plist to camelCase JSON"
    (let ((json (%build-content '(:title "Hello" :created-at "2025-04-01"))))
      (ok (search "\"title\":\"Hello\"" json))
      (ok (search "\"createdAt\":\"2025-04-01\"" json)))))

(deftest parse-response-test
  (testing "parse-response converts camelCase JSON to kebab-case plist"
    (let ((result (%parse-response "{\"myTitle\": \"Hello\", \"createdAt\": \"2025-04-01\"}")))
      (ok (equal (getf result :my-title) "Hello"))
      (ok (equal (getf result :created-at) "2025-04-01")))))
