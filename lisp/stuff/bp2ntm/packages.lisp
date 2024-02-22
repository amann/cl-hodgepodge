(in-package #:cl-user)
(defpackage #:bp-to-ntm-system
  (:use #:cl #:clsql))
(defpackage #:bp-to-ntm
  (:use #:cl #:clsql #:bp-to-ntm-system))
(defpackage #:bp-to-ntm-user
  (:use #:cl #:clsql #:bp-to-ntm))
