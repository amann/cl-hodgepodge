(in-package #:cl-user)

(asdf:defsystem blueprint-odbc
  :name "BLUEPRINT-ODBC"
  :author "me"
  :depends-on (#:oam-base #:oam-odbc)
  :components
  ((:file "blueprint-connection-base")
   (:file "blueprint-connection-odbc"))
  :serial t)

  