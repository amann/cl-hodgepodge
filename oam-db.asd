(in-package #:cl-user)

(asdf:defsystem oam-db
  :name "OAM-DB"
  :author "Olivier Amann"
  :depends-on (#:oam-util #:oam-date #:oam-clos #:oam-csv #:bordeaux-threads)
  :components ((:file "oam-db"))
  :serial t)

  