(in-package #:cl-user)

(asdf:defsystem #:awl
  :name "AWL"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl-base)
  :components
  ((:module "src"
            :components
            ((:file "awl-sequences")
             (:file "awl-generators")
             (:file "awl-date"))))
  :serial t)
