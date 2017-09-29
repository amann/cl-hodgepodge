(in-package #:cl-user)

(asdf:defsystem analytics-deals
  :name "ANALYTICS-DEALS"
  :author "Olivier Amann"
  :depends-on (#:oam-util)
  :components
  ((:file "analytics-deals"))
  :serial t)

  