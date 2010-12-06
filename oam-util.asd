(in-package #:cl-user)
(defpackage #:util-system (:use #:asdf #:cl))
(in-package #:util-system)



(defsystem oam-util
  :name "OAM-UTIL"
  :author "me"
  :components
  ((:file "oam-util")
   (:file "oam-clos")
   (:file "oam-date")
   #+(or) (:file "oam-etl")
   #+(or) (:file "oam-sxml"))
  :serial t)

  