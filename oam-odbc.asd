(in-package #:cl-user)

(asdf:defsystem oam-odbc
  :name "OAM-ODBC"
  :author "Olivier Amann"
  :depends-on (#:oam-date #:plain-odbc)
  :components
  ((:file "oam-odbc"))
  :serial t)

  