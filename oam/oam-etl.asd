(in-package #:cl)

(setq *features* (remove :ch.amann-wolowyk.oam-etl-system-test *features*))

(asdf:defsystem oam-etl
  :name "OAM-ETL"
  :author "me"
  :components
  ((:file "oam-etl"))
  :depends-on ("oam-cursor")
  :serial t)

  