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
(defun oam::make-project-package (project-name interface-nickname)
  (make-package project-name
                :use nil
                :nicknames (list interface-nickname))
  (make-package (concatenate 'string (string project-name) "-SYSTEM")
                :use '(#:common-lisp)))
(defun oam::export-interface (interface-nickname)
  (let ((package (find-package interface-nickname)))
    (do-symbols (symbol package)
      (export symbol package))))
#+ (or)
(defun oam::export-interface (interface-nickname)
  (let ((package (find-package interface-nickname)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (export symbol package)))))

(defun oam::package-add-nickname (package nickname)
  "Add nickname NICKNAME to the package PACKAGE."
  (let ((package (find-package package)))
    (loop :for nickname-taken-p := (find-package nickname)
       :do (cond
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
(defun oam::set-nickname (package nickname)
  "Remove all nicknames from PACKAGE and set NICKNAMES instead."
  (let ((package (find-package package)))
    (rename-package package (package-name package) nickname)))

;;;;* Macros

;;;;* Functions
(defun oam::function-arg-list (fn)
  "return the arg list of `fn'."
  #+clisp (ext:arglist fn)
  #+sbcl (sb-introspect:function-lambda-list fn)
  #+lucid (system::arglist fn)
  #+allegro (excl::arglist fn)
  #+openmcl (ccl:arglist fn)
  ;; GCLisp 1.1 version
  #+gclisp (if (macro-function fn)
               '(&rest "Form =")
               (lambda-list fn))
  #+kcl (let ((x (symbol-function fn)))
          (cond ((atom x) nil)
                ((eq (first x) 'macro) (list '&rest "Form ="))
                (t (third x)))))

;;; CMU Common Lisp version.  This version looks in a symbol's
;;; function cell and knows how to take apart lexical closures
;;; and compiled code objects found there.
#+cmu
(defun oam::function-arg-list (x &optional original-x)
  (typecase x
    (symbol (function-arg-list (symbol-function x) x))
    (compiled-function (read-from-string
                        (lisp::%primitive header-ref x
                                          lisp::%function-arg-names-slot)))
    (list (case (first x)
            (lambda (second x))
            (lisp::%lexical-closure% (function-arg-list (second x)))
            (system:macro '(&rest "Form ="))
            (t '(&rest "Arglist:"))))
    (t (cerror (format nil
                       "Use a reasonable default argument list for ~S"
                       original-x)
               "Unkown object in function cell of ~S:  ~S" original-x x)
       '())))

#+ (or) ;;;; TODO: Correct bug showing a nil element after a &rest variable.
(defun oam::get-parameter-list (lambda-list)
  "Return the argument list used to call a function whose arg list is `lambda-list'."
  (let (rest)
    (append
     (mapcan (let ((state '&ordinary))
               (lambda (x)
                 (if (member x lambda-list-keywords)
                     (progn (setq state x) nil)
                     (case state
                       (&ordinary (etypecase x
                                    (list (list (oam::get-parameter-list x)))
                                    (symbol (list x))))
                       (&optional (list (or (and (consp x) (car x)) x)))
                       ((&rest &body) (progn (setq rest x) nil))
                       ((&aux &whole &environment)
                        (progn (setq state '&ordinary) nil))
                       (&key (unless rest
                               (if (consp x)
                                   (let ((x (car x)))
                                     (if (consp x)
                                         x
                                         (list (intern (symbol-name x)
                                                       :keyword) x)))
                                   (list (intern (symbol-name x)
                                                 :keyword) x))))))))
             lambda-list)
     (list rest))))

(defun oam::get-lambda-variables (lambda-list)
  "Return a list containing the variable names defined in the ordinary or macro lambda list LAMBDA-LIST."
  (mapcan (let ((state '&ordinary))
            (lambda (x)
              (if (member x lambda-list-keywords)
                  (progn
                    (setq state x)
                    nil)
                  (case state
                    (&ordinary (etypecase x
                                 (list (oam::get-lambda-variables x))
                                 (symbol (list x))))
                    ((&rest &body) (list x))
                    ((&whole &environment) (progn
                                             (setq state '&ordinary)
                                             (list x)))
                    ((&optional &aux &key) (if (consp x)
                                               (let ((x (car x))
                                                     (y (third x)))
                                                 (list* (if (consp x)
                                                            (cadr x)
                                                            x)
                                                        (when y (list y))))
                                               (list x)))))))
          lambda-list))

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
                                  `(typecase ,(first ef)
                                     (function (apply ,(first ef) args))
                                     (t ,(first ef)))))
                        vars evaled-fcts))
         ,@body))))

(defmacro oam::fbind* ((&rest bindings) &body body)
  "Locally bind functions to to symbols via `labels'. The rationale for this macro is to locally rename functions or to easily give names to functions stored in symbol-value thus avoiding the use of funcall. The syntax is as in let; if a binding consists of only one symbol s the function in (symbol-value s) is flet to the symbol s."
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
       (labels (,@(mapcar #'(lambda (v ef)
                              (list v '(&rest args)
                                    `(apply ,(first ef) args)))
                          vars evaled-fcts))
         ,@body))))

(defmacro oam::mbind ((&rest bindings) &body body &environment environment)
  "Lexically bind macros to an alias using CL:MACROLET. BINDINGS is a symbol or a pair of symbols of the form new-name=orig-name or (new-name orig-name). Using it in a macro definition, insures that macro definitions are used which where valid in the lexical environment of the macro definition itself and not in the lexical environment of the expansion of the macro definition; To avoid shadowing macro definitions in the lexical environment of macro expansion, the new macro names can be given as gensyms."
  (flet ((make-macrolet-entry (binding)
           (destructuring-bind (new-macro-name orig-macro-name &rest ignore)
               (etypecase binding
                 (symbol (list binding binding))
                 (cons (append binding binding)))
             (declare (ignore ignore))
             (let* ((whole (gensym "WHOLE"))
                    (env (gensym "ENV"))
                    (macro-expander (macro-function orig-macro-name environment))
                    (lambda-list (oam::function-arg-list orig-macro-name))
                    (parameters (oam::get-lambda-variables lambda-list)))
               `(,new-macro-name (&whole ,whole &environment ,env ,@lambda-list)
                                 (declare (ignore ,@parameters))
                                 (setf (first ,whole) ',orig-macro-name)
                                 (funcall ,macro-expander ,whole ,env))))))
    `(macrolet ,(mapcar #'make-macrolet-entry bindings)
       ,@body)))

(defun oam::mkstr (&rest args)
  "Return a string which is a concatenation of the printed representations via CL:PRINC of ARGS."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun oam::symb (&rest args)
  "Intern into the current package and return the symbol whose name is a concatenation of the printed representations via CL:PRINC of ARGS."
  (values (intern (apply #'oam::mkstr args))))
(defun oam::group (source n)
  (if (zerop n) (error "Zero length."))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (when source
      (rec source nil))))
(defun oam::flatten (x)
  "Return a list of all atoms contained in the tree-structures cons X in the depth-first search order."
  (labels ((rec (x acc)
             (typecase x
               (null acc)
               (atom (cons x acc))
               (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;;;** From Let over Lambda 
;;;;*** Defmacro/g!
(defun g!-symbol-p (s)
  (and (symbolp s)
       (< 2 (length (symbol-name s)))
       (string= (symbol-name s) "G!" :start1 0 :end1 2)))

(defmacro defmacro/g! (name args docstring declarations &rest body)
  (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p (oam::flatten body)))))
    `(defmacro ,name ,args
       ,@(when docstring (list docstring))
       ,@declarations
       (let ,(mapcar (lambda (s)
                       `(,s (gensym ,(subseq (symbol-name s) 2))))
                     syms)
         ,@body))))

;;;;*** Defmacro!
(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "O!" :start1 0 :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (oam::symb "G!" (subseq (symbol-name s) 2)))

(defmacro oam::defmacro! (name args &rest body)
  "As CL:DEFMACRO except that args prefixed with o! are evaluated once only and backquote-escaped variables in the body prefixed by g! are declared with gensyms."
  (let* ((os (remove-if-not #'o!-symbol-p (oam::flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os))
         (first-body (first body))
         (rest-body (rest body)) 
         (docstring (when (and rest-body (stringp first-body))
                      first-body))
         (body (if docstring rest-body body))
         (declarations (loop :while (eq 'declare (caar body))
                          :for form := (pop body)                          
                          :collect form)))
    `(defmacro/g! ,name ,args ,docstring ,declarations
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))


(defmacro oam::once-only ((&rest names) &body body)
  (let ((gensyms (loop :for n :in names :collect (gensym (string n)))))
    `(let (,@(loop :for g :in gensyms :collect `(,g (gensym))))
       `(let (,,@(loop :for g :in gensyms :for n :in names :collect ``(,,g ,,n)))
          ,(let (,@(loop :for n :in names :for g :in gensyms :collect `(,n ,g)))
                ,@body)))))

(defmacro oam::with-gensyms ((&rest names) &body body)
  "Lexically bind NAMES to gensyms."
  `(let ,(mapcar (lambda (name)
                   `(,name (gensym ,(string name))))
                 names)
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