(in-package #:cl-user)

(asdf:defsystem adaptiv-odbc
  :name "ADAPTIV-ODBC"
  :author "me"
  :depends-on (#:oam-util #:oam-odbc)
  :components
  ((:file "adaptiv-odbc"))
  :serial t)

  