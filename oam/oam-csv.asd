(in-package #:cl-user)

(asdf:defsystem oam-csv
  :name "OAM-CSV"
  :author "Olivier Amann"
  :depends-on (#:oam-util)
  :components ((:file "oam-csv"))
  :serial t)

  