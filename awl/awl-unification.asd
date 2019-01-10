(in-package #:cl-user)



(asdf:defsystem #:awl-unification
  :name "AWL-UNIFICATION"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl)
  :components
  ((:module "src"
            :components
            ((:file "awl-unification")
             (:file "awl-treesearch"))))
  :serial t)
