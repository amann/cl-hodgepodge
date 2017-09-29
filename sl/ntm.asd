(in-package #:cl-user)

(asdf:defsystem ntm
  :name "NTM"
  :author "Olivier Amann"
  :depends-on (#:oam-base #:oam-date #:oam-xml)
  :components
  ((:file "ntm-init")
   (:file "ntm-config"))
  :serial t)

  