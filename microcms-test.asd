(defsystem "microcms-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "microcms-test/client")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
