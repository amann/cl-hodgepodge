(in-package #:cl-user)
(defpackage #:util-system (:use #:asdf #:cl))
(in-package #:util-system)



(defsystem oam-util
  :name "OAM-UTIL"
  :author "me"
  :components
  ((:file "oam-util"))
  :serial t)

  