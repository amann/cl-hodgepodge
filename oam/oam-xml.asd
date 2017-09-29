(in-package #:cl-user)

(asdf:defsystem oam-xml
  :name "OAM-XML"
  :author "Olivier Amann"
  :depends-on (#:oam-util #:s-xml)
  :components
  ((:file "oam-xml"))
  :serial t)

  