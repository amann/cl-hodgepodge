(in-package #:cl-user)

(defpackage "CH.AMANN-WOLOWYK.AWL-SYSTEM"
  (:use "COMMON-LISP"))
(defpackage "CH.AMANN-WOLOWYK.AWL"
  (:use)
  (:nicknames "AWL"))

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


(asdf:defsystem #:awl
  :name "AWL"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:closer-mop #:bordeaux-threads)
  :components
  ((:module "src"
            :components
            ((:file "awl-cltl2")
             (:file "awl-util")
             (:file "awl-date"))))
  :serial t)
