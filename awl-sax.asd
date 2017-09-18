(in-package #:cl-user)

(asdf:defsystem #:awl-sax
  :name "AWL-SAX"
  :author "Olivier Amann <olivier.e.amann@gmail.com>"
  :depends-on (#:awl-base)
  :components
  ((:module "src"
            :components
            ((:file "awl-package-registry")
             (:file "awl-sax"))))
  :serial t)
