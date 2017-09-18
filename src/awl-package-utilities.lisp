;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")
;;;;
;;;;* Package Utilities
;;;; Utilities for managing names and nicknames of packages

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

(awl::export-interface "AWL")