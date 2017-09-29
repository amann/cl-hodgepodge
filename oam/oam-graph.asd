(in-package #:cl-user)

(asdf:defsystem oam-graph
  :name "OAM-GRAPH"
  :author "me"
  :depends-on (#:oam-util)
  :components
  ((:file "oam-graph"))
  :serial t)

  