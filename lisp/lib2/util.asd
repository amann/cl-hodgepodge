(in-package #:cl-user)
(defpackage #:util-system (:use #:asdf #:cl))
(in-package #:util-system)



(defsystem util
  :name "UTIL"
  :author "me"
  :components
  ((:file "packages")
   (:file "utils")
   (:file "connection")
   (:file "stored-procedures")
   (:file "codes")
   (:file "adaptiv"))
  :serial t)

  