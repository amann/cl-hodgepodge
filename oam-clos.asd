(in-package #:cl-user)

(asdf:defsystem oam-clos
  :name "OAM-CLOS"
  :author "me"
  :depends-on (#:oam-util #:closer-mop)
  :components
  ((:file "oam-clos"))
  :serial t)

  