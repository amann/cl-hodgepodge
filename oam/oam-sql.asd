(in-package #:cl-user)

(asdf:defsystem oam-sql
  :name "OAM-SQL"
  :author "Olivier Amann"
  :depends-on (#:oam-util #:oam-date)
  :components
  ((:file "oam-sql"))
  :serial t)

  