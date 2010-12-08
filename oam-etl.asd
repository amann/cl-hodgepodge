(in-package #:cl)

(asdf:defsystem oam-etl
  :name "OAM-ETL"
  :author "me"
  :components
  ((:file "oam-etl"))
  :depends-on ("oam-cursor")
  :serial t)

  