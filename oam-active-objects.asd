(in-package #:cl-user)

(asdf:defsystem oam-active-objects
  :name "OAM-ACTIVE-OBJECTS"
  :author "me"
  :components
  ((:file "oam-active-objects"))
  :depends-on ("bordeaux-threads" "closer-mop" "oam-util")
  :serial t)