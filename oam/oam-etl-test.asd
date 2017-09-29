(in-package #:cl)

(pushnew :ch.amann-wolowyk.oam-etl-system-test *features*)

(asdf:defsystem oam-etl-test
  :name "OAM-ETL-TEST"
  :author "me"
  :components
  ((:file "oam-etl"))
  :depends-on ("oam-unit-test-framework" "oam-cursor")
  :serial t)

  