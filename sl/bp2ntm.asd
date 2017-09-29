(in-package #:cl-user)

(asdf:defsystem bp2ntm
  :name "BP2NTM"
  :author "me"
  :components
  ((:file "bp2ntm"))
  :depends-on ("oam-etl" "oam-date" "plain-odbc")
  :serial t)


