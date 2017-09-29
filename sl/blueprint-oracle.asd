(in-package #:cl-user)

(pushnew :win32 *features*)

(asdf:defsystem blueprint-oracle
  :name "blueprint-oracle"
  :author "me"
  :depends-on (#:oam-base #:clsql-cffi #:clsql-oracle)
  :components
  ((:file "blueprint-connection-base")
   (:file "blueprint-connection-oracle"))
  :serial t)

  