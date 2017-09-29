(in-package #:cl-user)

(asdf:defsystem #:awl-structures
  :name "AWL-STRUCTURES"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl-mop)
  :components
  ((:module "src"
            :components
            ((:file "awl-structures"))))
  :serial t)
