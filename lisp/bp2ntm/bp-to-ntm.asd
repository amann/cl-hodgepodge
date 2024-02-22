(in-package #:cl-user)
(defpackage #:bp-to-ntm-system
  (:use #:asdf #:cl))
(in-package #:bp-to-ntm-system)

#-clsql-odbc
(asdf:operate 'asdf:load-op 'clsql-odbc)
#-clsql-oracle
(asdf:operate 'asdf:load-op  'clsql-oracle)


(defsystem BP-TO-NTM
  :name "BP-TO-NTM"
  :author "amao"
  :components
  ((:file "packages")
   (:file "utils")
   (:file "connection")
#+Q   (:file "stored-procedures")
#+Q   (:file "codes")
#+Q   (:file "adaptiv"))
  :serial t)

  