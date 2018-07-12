(in-package #:cl-user)



(asdf:defsystem #:awl-expressions
  :name "AWL-EXPRESSIONS"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl)
  :components
  ((:module "src"
            :components
            ((:file "awl-treesearch")
             (:file "awl-expressions"))))
  :serial t)
