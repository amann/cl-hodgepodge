(in-package #:cl-user)
(defpackage #:blueprint-system (:use #:asdf #:cl))
(in-package #:blueprint-system)

#-clsql
(asdf:operate 'asdf:load-op 'clsql-oracle)


(defsystem blueprint
  :name "BLUEPRINT"
  :author "me"
  :components
  ((:file "clsql-ext")
   (:file "packages")
   (:file "blueprint"))
  :serial t)

  