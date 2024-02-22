;;;; $Id: bp2ntm.asd,v 1.2 2010/03/26 12:14:11 amao Exp $
;;;;; The BluePrint to NTM Mapping Program
(in-package #:cl-user)
(defpackage #:bp2ntm-system
  (:use #:cl #:asdf))
(in-package #:bp2ntm-system)

(defsystem BP2NTM
  :name "BP2NTM"
  :author "amao"
  :components
  ((:file "bp2ntm")
#+Q   (:file "stored-procedures")
#+Q   (:file "codes")
#+Q   (:file "adaptiv"))
  :serial t
  :depends-on (clsql))

  