(in-package #:cl-user)
(defpackage #:adaptiv-system (:use #:asdf #:cl))
(in-package #:adaptiv-system)

#-clsql
(asdf:operate 'asdf:load-op 'clsql-odbc)


(defsystem adaptiv
  :name "ADAPTIV"
  :author "me"
  :components
  ((:file "packages")
   (:file "adaptiv"))
  :serial t)

  