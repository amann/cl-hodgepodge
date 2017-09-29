(in-package #:cl-user)

(asdf:defsystem financial-instruments
  :name "FINANCIAL-INSTRUMENTS"
  :author "Olivier Amann"
  :depends-on (#:oam-util)
  :components
  ((:file "instruments"))
  :serial t)

  