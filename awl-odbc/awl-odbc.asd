(in-package #:cl-user)

(asdf:defsystem awl-odbc
  :name "AWL-ODBC"
  :author "Olivier Amann"
  :depends-on (#:oam-util #:cffi #:trivial-garbage #:babel)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "odbc-package")
                         (:file "odbc-ffi")
                         (:file "odbc-sql-constants")
                         (:file "odbc-sql-type-aliases")
                         (:file "odbc-sql-functions")
                         (:file "odbc-base"))))
  :serial t)

  
