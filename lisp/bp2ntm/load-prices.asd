(in-package #:cl-user)
(defpackage #:sl.load-prices
  (:use #:plain-odbc #:cl))



(in-package #:sl.load-prices)



(asdf:defsystem load-prices
  :name "LOAD-PRICES"
  :author "me"
  :components
  ((:module "oam-util")
   (:file "load-prices"))
  :serial t)