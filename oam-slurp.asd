(in-package #:cl-user)

(asdf:defsystem oam-slurp
  :name "OAM-SLURP"
  :author "Olivier Amann"
  :depends-on (#:oam-util)
  :components ((:file "oam-slurp"))
  :serial t)

  