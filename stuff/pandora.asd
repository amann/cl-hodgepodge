(in-package #:cl-user)

(asdf:defsystem pandora
  :name "PANDORA"
  :author "Olivier Amann"
  :depends-on (#:oam-util)
  :components ((:file "pandora"))
  :serial t)

  