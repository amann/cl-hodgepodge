(in-package #:cl-user)
(defpackage #:adaptiv-system (:use #:asdf #:cl))
(in-package #:adaptiv-system)

#-clsql-odbc
(asdf:operate 'asdf:load-op 'clsql-odbc)
#-clsql-oracle
(asdf:operate 'asdf:load-op  'clsql-oracle)


(defsystem adaptiv
  :name "ADAPTIV"
  :author "me"
  :components
  ((:file "packages")
   (:file "utils")
   (:file "connection")
   (:file "stored-procedures")
   (:file "codes")
   (:file "adaptiv"))
  :serial t)

  