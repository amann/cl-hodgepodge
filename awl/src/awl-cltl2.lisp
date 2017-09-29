;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; Time-stamp: <>

;;;; $ID: awl-cltl2.lisp,v 1.2 2010/08/25 13:36:23 amao Exp $

(in-package "CL-USER")


;;;;* CLTL2 API
#+sbcl
(let ((awl-pkg (find-package '#:awl)))
  (do-external-symbols (symb (find-package '#:sb-cltl2))
    (shadowing-import symb awl-pkg)
    (export symb awl-pkg)))


