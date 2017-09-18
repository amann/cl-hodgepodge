;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
(in-package #:cl-user)
;;;;============================================================================
;;;;* Package definitions
(defpackage "CH" (:use))
(defpackage "CH.AMANN-WOLOWYK" (:use "COMMON-LISP"))
(defpackage "CH.AMANN-WOLOWYK.AWL"
  (:use)
  (:nicknames #-relative-package-names #:.awl #:awl))

(defpackage "CH.AMANN-WOLOWYK.AWL-SYSTEM"
  (:use "COMMON-LISP"))
(defpackage "CH.AMANN-WOLOWYK.AWL-ALIEN-SYSTEM"
  (:use "COMMON-LISP"))

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

(defun awl::export-interface (interface-nickname)
  "Export all symbols from package INTERFACE-NICKNAME."
  (let ((package (find-package interface-nickname)))
    (do-symbols (symbol package)
      (export symbol package))))

(awl::export-interface "AWL")
