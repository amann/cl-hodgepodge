(in-package #:cl-user)

(asdf:defsystem oam-util
  :name "OAM-UTIL"
  :author "Olivier Amann"
  :depends-on (#:oam-base #:closer-mop #:bordeaux-threads)
  :components ((:file "oam-util"))
  :serial t)

  