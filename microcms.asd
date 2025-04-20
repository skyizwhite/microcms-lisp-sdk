(defsystem "microcms"
  :version "0.1.0"
  :description "microCMS Common Lisp SDK."
  :author "Akira Tempaku"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("microcms/main")
  :in-order-to ((test-op (test-op "microcms-test"))))
