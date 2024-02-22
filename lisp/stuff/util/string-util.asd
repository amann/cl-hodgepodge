(in-package #:cl-user)
(defpackage #:string-util-system (:use #:asdf #:cl))
(in-package #:string-util-system)



(defsystem string-util
  :name "STRING-UTIL"
  :author "me"
  :components
  ((:file "string-util-packages")
   (:file "string-utils"))
  :serial t)

  