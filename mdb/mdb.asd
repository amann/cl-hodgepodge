(in-package #:cl-user)

(asdf:defsystem mdb
  :name "MDB"
  :author "Olivier Amann"
  :depends-on ("oam-db" "usocket" )
  :components ((:file "mdb")
               (:file "oam-server"))
  :serial t)

  