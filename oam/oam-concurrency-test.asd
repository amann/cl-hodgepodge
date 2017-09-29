
(asdf:defsystem oam-concurrency-test
  :name "OAM-CONCURRENCY-TEST"
  :depends-on (#:oam-unit-test-framework #:oam-concurrency)
  :components
  ((:file "oam-concurrency-test"))
  :serial t)