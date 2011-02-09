;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.oam
  (:use)
  (:nicknames #:oam))
(defpackage #:ch.amann-wolowyk.oam-system
  (:use #:common-lisp))

(in-package #:ch.amann-wolowyk.oam-system)


;;;;* Package Utilities

(defmacro oam::define-project-package (project-name interface-nickname)
  `(progn
     (in-package #:common-lisp)
     (defpackage ,project-name
       (:use)
       (:nicknames ,interface-nickname))
     (defpackage ,(concatenate 'string (string project-name) "-SYSTEM")
       (:use #:common-lisp))))
(defun oam::export-interface (interface-nickname)
  (let ((package (find-package interface-nickname)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (export symbol package)))))

(defun oam::package-add-nickname (package nickname)
  "Add nickname NICKNAME to the package PACKAGE."
  (let ((package (find-package package)))
    (loop for nickname-taken-p = (find-package nickname)
       do (cond
            ((eq nickname-taken-p package) (return package))
            (nickname-taken-p
             (restart-case
                 (error "Nickname ~A is already taken by package ~A."
                        nickname (package-name nickname-taken-p))
               (use-value (value &optional condition)
                 :report "Use another nickname."
                 :interactive (lambda ()
                                (princ "Enter a new nickname: ")
                                (multiple-value-list (read)))
                 (declare (ignore condition))
                 (setq nickname value))))
            (t (return (rename-package package (package-name package)
                                       (cons nickname (package-nicknames package)))))))))

(defun oam::package-remove-nickname (package nickname)
  "Remove the nickname NICKNAME from the package PACKAGE."
  (let ((package (find-package package)))
    (rename-package package (package-name package)
                    (delete (string nickname) (package-nicknames package)
                            :test #'string=))))

;;;;* Macros


;;;;** Binding Forms
;;;;
;;;; Locally bind functions to to symbols via `flet'. The rationale for this
;;;; macro is to locally rename functions or to easily give names to functions
;;;; stored in symbol-value. For this `flet' is enough.

(defmacro oam::fbind ((&rest bindings) &body body)
  "Locally bind functions to to symbols via `flet'. The rationale for this macro is to locally rename functions or to easily give names to functions stored in symbol-value thus avoiding the use of funcall. The syntax is as in let; if a binding consists of only one symbol s the function in (symbol-value s) is flet to the symbol s."
  (let* ((vars (mapcar #'(lambda (b)
                           (etypecase b
                             (cons (first b))
                             (symbol b)))
                       bindings))
         (fcts (mapcar #'(lambda (b)
                           (etypecase b
                             (cons (second b))
                             (symbol b)))
                       bindings))
         (evaled-fcts (mapcar #'(lambda (v f)
                                  (list (gensym (string-upcase (string v)))
                                        f))
                              vars fcts)))
    `(let (,@evaled-fcts)
       (flet (,@(mapcar #'(lambda (v ef)
                            (list v '(&rest args)
                                  `(apply ,(first ef) args)))
                        vars evaled-fcts))
         ,@body))))



(defmacro oam::once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defmacro oam::with-gensyms ((&rest names) &body body)
  `(let (,@(mapcar #'(lambda (name)
                      `(,name (gensym ,(string name))))
                  names))
     ,@body))


#+(or)
(defun oam::expand-list (list)
  (list* 'list*
         (if (listp (car list))
             (oam::expand-list (car list))
             (car list))
         (when (cdr list)
           (list (oam::expand-list (cdr list))))))

#+(or)
(defmacro with-actual-macros (names &body body &environment env)
  (let ((gensyms (mapcar #'(lambda (name) (gensym (string name))) names)))
    (labels
        ((make-macro-def-code (names)
           (mapcar #'(lambda (gensym name)
                       `(,gensym ,(function-arg-list name)
                                 (macroexpand
                                  (list* ',name
                                         ,(expand-list
                                           (get-parameter-list
                                            (function-arg-list name))))
                                              ,env)))
                   gensyms names)))
      `(let (,@(mapcar #'list names gensyms))
         `(macrolet (,,@(make-macro-def-code names))
            ,,@body)))))

(oam::export-interface '#:oam)