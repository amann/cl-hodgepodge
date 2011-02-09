(in-package #:cl-user)


(asdf:defsystem oam-concurrency
  :name "OAM-CONCURRENCY"
  :author "Olivier Amann"
  :depends-on (#:bordeaux-threads #:oam-util)
  :author "me"
  :components
  ((:file "oam-concurrency"))
  :serial t)


