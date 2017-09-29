(in-package #:cl-user)

(asdf:defsystem #:awl-mop
  :name "AWL-MOP"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl-base)
  :components
  ((:module "src"
            :components
            ((:file "awl-mop"))))
  :serial t)
