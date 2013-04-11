;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
(in-package #:cl-user)

(defpackage "CH" (:use))
(defpackage "CH.AMANN-WOLOWYK" (:use "COMMON-LISP"))
(defpackage "CH.AMANN-WOLOWYK.AWL"
  (:use)
  (:nicknames #-relative-package-names #:.awl #:awl))

(defpackage "CH.AMANN-WOLOWYK.AWL-SYSTEM"
  (:use "COMMON-LISP"))


(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;* Package Utilities

(defmacro awl::define-project-package (project-name interface-nickname)
  "Define a package whose name is the concatenation of the PROJECT-NAME and \"-SYSTEM\" and which uses the COMMON-LISP package, and a package of name PROJECT-NAME and nickname INTERFACE-NICKNAME which uses no other symbols. Symbols from latter package can be exported using awl:export-interface."
  (let ((common-lisp "COMMON-LISP")
        (project-name (string project-name))
        (interface-nickname (string interface-nickname)))`(progn
      (in-package ,common-lisp)
      (defpackage ,(concatenate 'string project-name "-SYSTEM")
        (:use ,common-lisp))
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (unless (find-package ,interface-nickname)
          (make-package ,project-name
                        :use nil
                        :nicknames '(,interface-nickname)))))))
(defun awl::make-project-package (project-name interface-nickname)
  (make-package project-name
                :use nil
                :nicknames (list interface-nickname))
  (make-package (concatenate 'string (string project-name) "-SYSTEM")
                :use '(#:common-lisp)))
(defun awl::export-interface (interface-nickname)
  "Export all symbols from package INTERFACE-NICKNAME."
  (let ((package (find-package interface-nickname)))
    (do-symbols (symbol package)
      (export symbol package))))
#+ (or)
(defun awl::export-interface (interface-nickname)
  (let ((package (find-package interface-nickname)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (export symbol package)))))

(defun awl::package-add-nickname (package nickname)
  "Add nickname NICKNAME to the package PACKAGE."
  (let ((package (find-package package)))
    (loop :for package-with-nickname := (find-package nickname)
       :do (cond
             ((eq package-with-nickname package) (return package))
             (package-with-nickname
              (restart-case
                  (if (string= (package-name package-with-nickname) nickname)
                      (error "Nickname ~A cannot be given; There is already a package with such name."
                             nickname)
                      (restart-case
                          (error "Nickname ~A is already taken by package ~A."
                                 nickname (package-name package-with-nickname))
                        (force ()
                          :report (lambda (stream)
                                    (format stream "Force adding nickname ~A to package ~A and remove nickname ~0@*~A from package ~2@*~A."
                                            nickname (package-name package) (package-name package-with-nickname))))))
                (use-value (value &optional condition)
                  :report "Use another nickname."
                  :interactive (lambda ()
                                 (princ "Enter a new nickname: ")
                                 (multiple-value-list (read)))
                  (declare (ignore condition))
                  (setq nickname value))))
             (t (return (rename-package package (package-name package)
                                        (cons nickname (package-nicknames package)))))))))

(defun awl::package-remove-nickname (package nickname)
  "Remove the nickname NICKNAME from the package PACKAGE."
  (let ((package (find-package package)))
    (rename-package package (package-name package)
                    (delete (string nickname) (package-nicknames package)
                            :test #'string=))))
(defun awl::set-nickname (package nicknames)
  "Remove all nicknames from PACKAGE and set NICKNAMES instead."
  (let ((package (find-package package)))
    (rename-package package (package-name package) nicknames)))


(defun get-new-pkg-name (&optional (prefix "PKG"))
  (let ((taken-names (remove-duplicates (mapcan (let ((len-prf (length prefix)))
                                                  (lambda (name)
                                                    (when (and (<= len-prf (length name))
                                                               (string= prefix name :end2 len-prf))
                                                      (list name))))
                                                (mapcar (lambda (pkg)
                                                          (list* (package-name pkg)
                                                                 (package-nicknames pkg)))
                                                        (list-all-packages)))
                                        :test 'string=)))
    (do ((cnt 0 (1+ cnt))
         (name prefix (format nil "~A~A" prefix cnt)))
        ((not (member name taken-names :test 'string=))
         name))))

(defmacro awl::with-nicknames (bindings &body body)
  (awl::with-gensyms (g!pkge g!original-state g!new-state)
    (reduce
     (lambda (bind r)
       `(let* ((,g!pkge (find-package ,(first bind)))
               (,g!original-state
                (remove-duplicates
                 (list* (list* ,g!pkge
                               (package-name ,g!pkge)
                               (package-nicknames ,g!pkge))
                        (mapcar (lambda (nick &aux (pkg (find-package nick)))
                                  (list* pkg
                                         (package-name pkg)
                                         (package-nicknames pkg)))
                                ',(rest bind)))
                 :key 'car))
               (,g!new-state
                (append (mapcar (lambda (state)
                                  (list* (first state)
                                         (or (set-difference (rest state)
                                                             ',(rest bind))
                                             (list (get-new-pkg-name)))))
                                (rest ,g!original-state))
                        (list (list* (first (first ,g!original-state))
                                     (union (rest (first ,g!original-state))
                                            ',(rest bind)))))))
          (unwind-protect
               (progn
                 (dolist (pkge ,g!new-state)
                   (rename-package (first pkge) (second pkge)
                                   (cddr pkge)))
                 ,r)
            (dolist (pkge ,g!original-state)
              (rename-package (first pkge) (second pkge)
                              (cddr pkge))))))
     bindings
     :from-end t
     :initial-value `(progn ,@body))))


;;;;** Trial for forcing namespaces
;;;;
;;;; The idea is to find a safe and easy mechanism to load data using READ without polluting
;;;; existing packages with symbols interned by this loading.  E.g. in normal case, if one
;;;; wants to load some data in an SXML-structure like (tag1 (@ (key value)) (namespace::tag2 ....)),
;;;; tag1 will be interned into the current package as indicated in *PACKAGE* which is easy to
;;;; have control on; the tag2 however will be interned into the package NAMESPACE, which can
;;;; potentially be any, even COMMON-LISP. This i want to avoid by forcing the reader to intern
;;;; the symbols into packages which i chose.
;;;;
;;;; One idea to achieve this is to locally replace the package registry by one containing specific
;;;; packages under specific names. In SBCL the package registry is bound to SB-IMPL::*PACKAGE-NAMES*.
;;;;
;;;; One problem arises when printing symbols from packages in those replaced registries: the
;;;; package name printed is the one from symbol-package...

#+sbcl
(progn
  (defstruct package-registry
    (pkg<-name (make-hash-table :test #'equal))
    (name<-pkg (make-hash-table :test #'eq :weakness :key)))
  (defvar *pkg-name<-pkg*)
  (defun package-%name (pkg)
    (gethash pkg *pkg-name<-pkg*))
  (defmacro awl::with-package-registry (registry &body body)
    (let ((orig-package-list (list-all-packages))
          registry-intersection)
      (unwind-protect
           (let ((sb-impl::*package-names* (package-registry-pkg<-name registry))
                 (*pkg-name<-pkg* (package-registry-name<-pkg registry)))
             (dolist (pkg (setf registry-intersection (intersection (list-all-packages) orig-package-list)))
               (rename-package pkg (package-%name pkg)
                               (cons (package-name pkg) (package-nicknames pkg))))
             ,@body)
        (dolist (pkg registry-intersection)
          (rename-package pkg (first (package-nicknames pkg))
                          (rest (package-nicknames pkg)))))))
  (defun awl::make-package-registry (bindings)
    (sb-impl::package-%name
     (map nil (lambda (binding)
                (let ((package (let ((candidate (first (last binding))))
                                 (if (packagep candidate)
                                     candidate
                                     (make-package (string (first binding))
                                                   :nicknames (rest (butlast binding)))))))))))))


#||
#+ccl
(let ((orig-find-pkg #'ccl::%find-pkg)
      (new-find-pkg (lambda (name &optional (len (length name)))
                      (declare (fixnum len)
                               (special awl::prefix))
                      (if (and (boundp 'awl::prefix) awl::prefix)
                          (funcall orig-find-pkg (concatenate 'string awl::prefix name)
                                   (the fixnum (+ (the fixnum (length awl::prefix)) len)))
                          (funcall orig-find-pkg name len))))
      amendedp)
  (defun awl::amend-find-package (&optional test-only)
    "Change the definition of CL:FIND-PACKAGE so that when the special variable ccl::prefix is bound to a string <prefix> the call of (find-package <name>) makes a lookup for the package with name <prefix><name>. If ccl::prefix in unbound, find-package keeps the original behaviour. This amendmend works only for Clozure CL. When called a second time, undo the amendment so that AMEND-FIND-PACKAGE effectively toggles between the original and the changed function definition. The return values are T if the resulting function definition is the changed one and nil if the call result in the original function definition. When called with the optional parameter TEST-ONLY non nil, do not change the definition and return only the current state of FIND-PACKAGE."
    (if test-only
        amendedp
        (setf (symbol-function 'ccl::%find-pkg) (if amendedp orig-find-pkg new-find-pkg)
              amendedp (not amendedp)))))

;;;; In SBCL the package registry is a hashtable hold in sb-impl::*package-names*. By locally binding this
;;;; variable to a modified registry, one can create perfect sandboxes for loading code from users.

(let ((cl-pkg (find-package "COMMON-LISP"))
      (sb-impl::*package-names* (make-hash-table :test #'equal :synchronized t)))
  (setf (gethash "FOO" sb-impl::*package-names*) cl-pkg)
  (eval (read-from-string "(foo:car '(1 2 3 4))")))
||#



(awl::export-interface '#:awl)
