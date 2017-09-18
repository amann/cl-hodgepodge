;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")
;;;;
;;;;* Package Registry
;;;;
;;;; TODO: Works only for SBCL!!!
(defstruct package-registry
  (packages (make-hash-table :test 'equal :weakness :value :synchronized t) :read-only t)
  (names (make-hash-table :test 'eq :weakness :key) :read-only t))
(define-condition awl::package-registry-error (simple-error) ())

(defvar awl::*package-registry*)
(defun awl::registry-find-package (package-designator &optional (registry awl::*package-registry*))
  "Return the package corresponding to PACKAGE-DESIGNATOR in the registry REGISTRY if it is contained there and NIL else."
  (typecase package-designator
    (package (when (gethash package-designator (package-registry-names registry))
               package-designator))
    (t (gethash (string package-designator) (package-registry-packages registry)))))
(defun awl::registry-package-name (package-designator &optional (registry awl::*package-registry*))
  "Return the package name associated to PACKAGE-DESIGNATOR in the registry REGISTRY when the package is in that registry and NIL else."
  (first (gethash (awl::registry-find-package package-designator) (package-registry-names registry))))
(defun awl::registry-package-nicknames (package-designator &optional (registry awl::*package-registry*))
  "Return a fresh list of the package nicknames associated to PACKAGE-DESIGNATOR in the registry REGISTRY when the package is in that registry and NIL else."
  (copy-list (rest (gethash (awl::registry-find-package package-designator) (package-registry-names registry)))))

(defun awl::registry-remove-package (package &optional (registry awl::*package-registry*))
  "Remove the package from the registry if it exists. In this case the package is returned, else NIL is returned."
  (let ((packages (package-registry-packages registry)))
    (dolist (name (gethash package (package-registry-names registry)))
      (remhash name packages))
    (remhash package (package-registry-names registry)))
  package)

(defun awl::registry-remove-package-name (string-designator &optional (total-remove-error-p t) (registry awl::*package-registry*)
                                          &aux (name (string string-designator)))
  "Remove from the registry REGISTRY the name indicated by STRING-DESIGNATOR if this name is in REGISTRY. In that case return the associated package, else return NIL. If the removing of the name implies the total removal of the package from REGISTRY and if TOTAL-REMOVE-ERROR-P is true (the default), a simple continuable error is signalled. If the associated CONTINUE restart is invoked, the package is removed from the registry. If name is the primary name of package (the one returned by (registry-package-name STRING-DESIGNATOR)), the primary name will be replaced by the first nickname in the list (registry-package-nicknames STRING-DESIGNATOR)."
  (let ((package (awl::registry-find-package name registry)))
    (when package
      (let ((remaining-names (remove name (gethash package (package-registry-names registry)))))
        (if remaining-names
            (setf (gethash package (package-registry-names registry)) remaining-names)
            (progn
              (when total-remove-error-p
                (cerror "Delete package ~4*~{~A~} from the registry."
                        'awl::package-registry-error
                        :format-control "Deleting package ~A form the registry."
                        :format-arguments (list package)))
              (remhash package (package-registry-names registry))))
        (remhash name (package-registry-packages registry))))
    package))

(defun awl::registry-set-package-name (package string-designator &optional (registry awl::*package-registry*)
                                       &aux (name (string string-designator)))
  "Set the primary name in REGISTRY of the package PACKAGE to STRING-DESIGNATOR. If this name is already used by this or another package, a continuable error is signalled. If invoking the CONTINUE restart, the name is first removed from the registry as with REGISTRY-REMOVE-PACKAGE-NAME (which might trigger another error if this implies the total removal of the other package) before beeing associated as primary name to the package associated to PACKAGE. When PACKAGE-DESIGNATOR does not designate any package in the global package namespace, nothing is done. The return value is PACKAGE."
  (etypecase package
    (package (unless (string= name (registry-package-name package registry))
               (let ((other-package (registry-find-package name registry)))
                 (when other-package
                   (cerror "Remove name ~4*~{~A for package ~A~} and assign it to package ~*~A as primary name."
                           'awl::package-registry-error
                           :format-control "Name ~A is already used by package ~A."
                           :format-arguments (list name other-package)
                           :ignore package
                           :allow-other-keys t)
                   (awl::registry-remove-package-name name registry)
                   (setf (gethash name (package-registry-packages registry)) package)
                   (push name (gethash package (package-registry-names registry))))))))
  package)

(defun awl::registry-set-package-nicknames (package nicknames &optional (registry awl::*package-registry*)
                                            &aux (nicknames (mapcar 'string nicknames)))
  "Replace the nicknames of PACKAGE by those given in NICKNAMES. If a name is already used for a package (including this one) a continuable error is signalled, allowing either to insist taking this name, in which case the name is removed from the other package, or to skip the use of this name."
  (etypecase package
    (package (let* ((package-names (gethash package (package-registry-names registry)))
                    (nicknames (mapcan (lambda (name)
                                         (restart-case
                                             (progn
                                               (unless (member name (rest package-names))
                                                 (let ((other-package (awl::registry-find-package name registry)))
                                                   (when other-package 
                                                     (cerror "Remove name ~4*~{~A for package ~A~} and use it for package ~A as a nickname."
                                                             'awl::package-registry-error
                                                             :format-control "Name ~A is already used by package ~A."
                                                             :format-arguments (list name other-package)
                                                             :ignore package
                                                             :allow-other-keys t)
                                                     (awl::registry-remove-package-name name registry)
                                                     (setf (gethash name (package-registry-packages registry)) package))))
                                               (list name))
                                           (skip ()
                                             :report (lambda (stream)
                                                       (format stream "Skip nickname ~A." name)))))
                                       nicknames))
                    (registry-packages (package-registry-packages registry)))
               (dolist (name (set-difference (rest package-names) nicknames))
                 (remhash name registry-packages))
               (setf (rest package-names) nicknames)))))

(defun (setf awl::registry-find-package) (package package-designator &optional (registry awl::*package-registry*))
  (typecase package-designator
    (package (unless (null package)
               (error 'awl::package-registry-error
                      :format-control "Registry-Find-Package with a package agument can only be SETFed to NIL which means remove the package form the registry."))
             (awl::registry-remove-package package-designator registry))
    (t (etypecase package
         (package (awl::registry-set-package-name package package-designator registry))
         (null (awl::registry-remove-package-name package-designator t registry))))))

(defvar *package->name*)
#+sbcl
(defmacro awl::with-package-registry (registry &body body)
  (let ((g!registry (gensym "REGISTRY")))
    `(let ((,g!registry ,registry))
       (let ((sb-impl::*package-names* (package-registry-packages ,g!registry))
             (*package->name* (package-registry-names ,g!registry))
             (awl::*package-registry* ,g!registry))
         ,@body))))

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

#+ (or)
(progn
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
    (eval (read-from-string "(foo:car '(1 2 3 4))"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))
