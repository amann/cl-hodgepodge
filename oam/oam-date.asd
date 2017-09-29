(in-package #:cl-user)

(asdf:defsystem oam-date
  :name "OAM-DATE"
  :author "me"
  :components
  ((:file "oam-date"))
  :depends-on ("oam-util")
  :serial t)

  