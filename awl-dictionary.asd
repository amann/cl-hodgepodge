(in-package #:cl-user)

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

(asdf:defsystem #:awl-dictionary
  :name "AWL-DICTIONARY"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl-base)
  :components
  ((:module "src"
            :components
            ((:file "awl-dictionary"))))
  :serial t)
