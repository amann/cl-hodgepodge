;;;; EVAL-IN-CURRENT-ENVIRONMENT and COMPILE-IN-CURRENT-ENVIRONMENT
;;;; This package exports 2 macros which allow one to evaluate or
;;;; compile a form within the current lexical environment
;;;; of the EVAL/CE or COMPILE/CE form.
;;;; Passing other environments is possible if you call macroexpand yourself.
;;;; The form passed to EVAL/CE is supposed to be a quoted list, and will be
;;;; evaluated in the current environment (like EVAL), while the
;;;; form passed to COMPILE/CE is expected to be a constant lambda form.
;;;;
;;;; Current shortcomings of this implementation:
;;;; 1. Symbol macro and local macro definitions do not carry
;;;; to EVAL/CE or COMPILE/CE's `environment'.
;;;; See code comments for rationale.
;;;; A crippled form of SYMBOL-MACROLET and macrolet seems possible, but
;;;; I'm not sure if it's worth the extra effort of implementing it.
;;;; 2. Accessing the dynamic binding of a variable having the same name
;;;; as a closed over lexical variable will generate an error as
;;;; lexical variables are implemented using symbol macros, and
;;;; accessing the dynamic binding of a name which has a
;;;; symbol macro binding is an error according to CLHS:
;;;; http://www.lispworks.com/documentati...y/s_symbol.htm
;;;; ``If declaration contains a special declaration that names one of
;;;; the symbols being bound by SYMBOL-MACROLET, an error of
;;;; type PROGRAM-ERROR is signaled.''
;;;; 3. COMPILE/CE does not allow non-constant forms.
;;;; This shortcoming may be solvable, but I'm not sure how to go
;;;; about it yet, we need to be able to parse the lambda body in the macro,
;;;; yet we also need to evaluate it before parsing it.
;;;; 4. Implementation requires one to write non-portable
;;;; implementation-specific code to obtain the names of the
;;;; local functions and lexical variable names which are
;;;; present in the current environment.
;;;; Currently, I've only written code for SBCL, ClozureCL and CLISP,
;;;; as they're the only implementations I have installed.

;;;; Dependency loading and package definition

;;; The code requires support for locatives, you can find them here:
;;; http://article.gmane.org/gmane.lisp.sources.code/17

;;; This would likely be better done as an ASDF system:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:locatives)
    (load (compile-file "locatives.lisp"))))

(defpackage lexical-eval/compile
  (:use #:cl #:locatives)
  (:nicknames #:lexeval)
  (:export #:eval-in-current-environment
           #:compile-in-current-environment
           #:expand-eval/compile-in-environment
           ;; Synonyms
           #:eval/ce #:compile/ce))

(in-package #:lexical-eval/compile)

;;;; Portability layer

(eval-when (:compile-toplevel :execute)
  (defconstant +environment-variables-doc+
    "Obtain a list of names of the local lexical variables in the ENV
environment")
  (defconstant +environment-functions-doc+
    "Obtain a list of the names of local functions in the ENV environment"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  #+sbcl
  (defun environment-variables (env)
    #.+environment-variables-doc+
    (remove-duplicates
     (mapcar #'car
             (remove-if-not #'(lambda (x) (typep x 'sb-c::lambda-var))
                            (sb-c::lexenv-vars env)
                            :key #'cdr))))
  #+sbcl
  (defun environment-functions (env)
    #.+environment-functions-doc+
    (remove-duplicates (mapcar #'car (sb-c::lexenv-funs env))))
  
  #+ccl
  (labels ((get-functions-in-environment (env)
             (mapcar #'first (ccl::lexenv.functions env)))
           (get-lexical-variables-in-environment (env)
             (let ((vars (ccl::lexenv.variables env)))
               (unless (eql vars 'ccl::barrier)
                 (remove-if-not #'(lambda (x)
                                    (eql :lexical
                                         (ccl:variable-information x env)))
                                (mapcar #'(lambda (x) (ccl::var-name x))
                                        vars))))))
    
    (macrolet ((env-frob-collect-all (env how)
                 `(remove-duplicates
                   (loop
                      :for #1=#:current-env = ,env :then
                      (ccl::lexenv.parent-env #1#)
                      :while #1#
                      :appending (,how #1#)))))
      
      (defun environment-variables (env)
        #.+environment-variables-doc+
        (env-frob-collect-all env get-lexical-variables-in-environment))
      
      (defun environment-functions (env)
        #.+environment-functions-doc+
        (env-frob-collect-all env get-functions-in-environment))))
  
  #+clisp
  (labels ((lexical-variable-test (sub-env index env)
             (let ((item (svref sub-env index)))
               (and (symbolp item)
                    ;; global special variable test
                    (not (system:roclaimed-special-p item))
                    ;; local special variable test
                    (not (system::special-variable-p item env))
                    (not (system::symbol-macro-p (svref sub-env (1+ index))))
                    item)))
           (local-function-test (sub-env index env)
             (and (typep (svref sub-env (1+ index)) 'function env)
                  (svref sub-env index)))
           (collect-frobs (env pred &optional (sub-vector 0))
             (remove-duplicates
              (let ((sub-env (svref env sub-vector))
                    items)
                (loop :while sub-env :do
                   (let* ((len (length sub-env))
                          (last (1- len))
                          (next-env (svref sub-env last)))
                     (loop :for i :from 0 :upto (1- last) :by 2
                        :for item = (funcall pred sub-env i env)
                        :when item
                        :do (push item items)
                        :finally (setf sub-env next-env))))
                (nreverse items)))))
    
    (defun environment-variables (env)
      #.+environment-variables-doc+
      (collect-frobs env #'lexical-variable-test))
    
    (defun environment-functions (env)
      #.+environment-functions-doc+
      (collect-frobs env #'local-function-test 1)))
  
  #-(or sbcl ccl clisp)
  (defun environment-variables (env)
    #.+environment-variables-doc+
    (declare (ignore env))
    (error "ENVIRONMENT-VARIABLES is not implemented in your implementation."))
  
  #-(or sbcl ccl clisp)
  (defun environment-functions (env)
    #.+environment-functions-doc+
    (declare (ignore env))
    (error "ENVIRONMENT-FUNCTIONS is not implemented in your implementation."))
  
;;; END EVAL-WHEN
  )
;;; How to implement portability layer functions in other implementations:
;;;
;;; Define a macro like: (defmacro get-env (&environment env) `',env)
;;; Then generate an environment using let/flet/macrolet/symbol-macrolet...
;;; and obtain it using (... (get-env)).
;;; Inspect that environment using what functions the implementation provides
;;; (or just use SLIME's inspector) and see if you can write those functions.
;;; The tricky bits are obtaining only lexical variables/functions,
;;; as we don't want to rebind dynamic ones. There are multiple reasons why we
;;; wouldn't want to rebind them, but the major reason is that
;;; the following is an error according in CLHS:
;;;
;;; (let ((x 1))
;;; (declare (special x))
;;; (symbol-macrolet ((x 2))
;;; (declare (special x))
;;; x))
;;;
;;; One solution would be to use something like "Environments Access"
;;; ( http://www.lispwire.com/entry-proganal-envaccess-des ) which
;;; implements a variant of CLtL2's Environment access functions
;;; The reason I haven't used that instead of doing my own hacky non-portable
;;; environment access layer is that it doesn't seem to have a simple way of
;;; accessing /all/ the local lexical functions/variables.
;;; I've also noticed the "Environments Access" documentation provided an
;;; AllegroCL-only way of implementing eval in a user-specified environment,
;;; which is what this code is supposed to be doing.

;;;; Macro Utils

(defmacro with-gensyms (names &body body)
  `(let ,(loop :for name :in names
            :collect `(,name (gensym ,(string name)))) ,@body))

(defmacro def-synonym (target-name source-name)
  `(setf (macro-function ',target-name)
         (macro-function ',source-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun make-gensym-list (names)
    (loop :for name :in names :collect (gensym (string name))))
  
;;; PARSE-BODY was stolen from the Alexandria library.
;;; I did not add Alexandria as a dependency as
;;; I only need this single function.
  (defun parse-body (body &key documentation whole)
    "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
    (let ((doc nil)
          (decls nil)
          (current nil))
      (tagbody
       :declarations
         (setf current (car body))
         (when (and documentation (stringp current) (cdr body))
           (if doc
               (error "Too many documentation strings in ~S." (or whole body))
               (setf doc (pop body)))
           (go :declarations))
         (when (and (listp current) (eql (first current) 'declare))
           (push (pop body) decls)
           (go :declarations)))
      (values body (nreverse decls) doc)))
  
  (defun parse-lambda-form (form)
    "Parses a quoted lambda form and return its lambda list, body, declarations,
and the documentation string if one exists"
    (destructuring-bind (lambda lambdalist . body) form
      (unless (eql lambda 'lambda)
        (error "Not a lambda expression: ~A" form))
      (multiple-value-call #'values lambdalist
                           (parse-body body :documentation t))))
  
;;; END EVAL-WHEN
  )

;;;; Implementation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-eval/compile-in-environment (expr env &optional compile)
    "Generate a macro expansion as if expression EXPR would be evaluated
or compiled in the ENV environment. If COMPILE is nil, it will generate code
as if it would EVAL it, otherwise it will compile a LAMBDA expression.
quoted EXPR is evaluated in current environment (once-only-like) if
COMPILE is NIL, otherwise it's used as a literal(since the literal lambda
form needs to be parsed"
    (labels ((make-function-thunk (function &key
                                            (name nil name-supplied-p)
                                            special-function)
               (with-gensyms (args)
                 `(,(if name-supplied-p name 'lambda) (&rest ,args)
                    (declare (dynamic-extent ,args))
                    ,@(when special-function `((declare (special ,function))))
                    (apply ,function ,args)))))
      (with-gensyms (expr-gs)
        (let* ((vars (environment-variables env))
               (vars-gs (make-gensym-list vars))
               (funs (environment-functions env))
               (function-thunks (make-gensym-list funs))
               lambdalist body decls doc)
          (when compile
            ;; not using m-v-b here as we only
            ;; need these half the time.
            (multiple-value-setq (lambdalist body decls doc)
              (parse-lambda-form expr))
            (setq expr `'(progn ,@body)))
          (let* ((dyn-pairs
                  `(,@(mapcar #'(lambda (var gs) `(,gs (locatives:locf ,var)))
                              vars vars-gs)
                      .,(mapcar #'(lambda (fun gs)
                                    `(,gs ,(make-function-thunk`#',fun)))
                                funs function-thunks)))
                 (dyn-pair-names `(,@function-thunks .,vars-gs))
                 (dyn-decl `(declare (special .,dyn-pair-names))))
            `(progn
               ,@(when compile
                       `((locally ,dyn-decl
                           (psetq .,(reduce #'append dyn-pairs)))))
               (let ((,expr-gs ,expr)
                     ,@(unless compile dyn-pairs))
                 ,@(unless compile `(,dyn-decl))
                 (,@(if compile '(compile nil) '(eval))
                    `(,@',(cond
                           (compile `(lambda ,lambdalist ,@(when doc `(,doc))
                                             ,@decls))
                           (t `(progn)))
                          (locally ,',dyn-decl
                            (symbol-macrolet
                                ,',(mapcar #'(lambda (var loc)
                                               `(,var
                                                 (locatives:contents ,loc)))
                                           vars vars-gs)
                              (flet ,',(mapcar #'(lambda (fun thunk)
                                                   (make-function-thunk
                                                    thunk
                                                    :name fun
                                                    :special-function t))
                                               funs function-thunks)
                                ,,expr-gs)))))))))))))

(defmacro eval-in-current-environment (expr &environment env)
  "Evaluates EXPR in the environment."
  (expand-eval/compile-in-environment expr env))

(defmacro compile-in-current-environment (definition &environment env)
  "Compiles DEFINITION, a quoted lambda expression, in the environment."
  (expand-eval/compile-in-environment definition env t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-synonym eval/ce eval-in-current-environment)
  (def-synonym compile/ce compile-in-current-environment))

;;; Symbol macros and local macros are not implemented yet.
;;;
;;; I can't think of non-implementation dependent ways of implementing
;;; symbol macros, and it seems quite hairy overall.
;;; For example, in both SBCL and CCL, you can obtain the name and
;;; expansion body from the environment, which could just be duplicated
;;; in a symbol-macrolet of our own, but there would be a problem if
;;; we had a local lexical variable and a symbol macro which had the same name.
;;; We wouldn't know which one shadows which without
;;; digging deeper into the environment object.
;;;
;;; As for local macros, there's a confusing problem when it comes to the
;;; expansion environment. Let's say we write a compatility routine
;;; which obtains the names of all the local macros (like was done
;;; with lexical vars and local functions) then add a macrolet
;;; in the code passed to EVAL, that macrolet defines stubs like:
;;;
;;; `(local-macro-name (&whole whole &environment env)
;;; (declare (special ,old-environment))
;;; (macroexpand-1 whole ,old-environment))
;;;
;;; where OLD-ENVIRONMENT is a locally special gensym which points
;;; to the environment where EVAL/COMPILE-IN-CURRENT-ENVIRONMENT
;;; was expanded in. The problem seems to me that the ENV argument
;;; will get discarded and won't be passed to the original local macro.
;;; This is not a problem if the code from EVAL/COMPILE doesn't want to
;;; pass an environment to it, but if it does, it won't work correctly.
;;; For example, if the expression the user passed to
;;; EVAL/COMPILE-IN-CURRENT-ENVIRONMENT defines some local symbol macros
;;; or local macros, and the outer local macro (from the code which called
;;; EVAL/COMPILE-IN-CURRENT-ENVIRONMENT), does a CLTL2:MACROEXPAND-ALL on the
;;; body of code which it was passed to it, it won't be able to expand those
;;; local macros defined within the body
;;; of EVAL/COMPILE-IN-CURRENT-ENVIRONMENT.
;;; There may be other issues which I haven't considered.

;;;; Tests:

#+nil
(and
 
 (equal
  (multiple-value-list
   (let ((counter 0))
     (flet ((inc-counter () (incf counter)))
       (values
        counter
        (eval-in-current-environment '(list (inc-counter) counter))
        counter))));=> 0, (1 1), 1
  '(0 (1 1) 1)) ;=> T
 
 (let ((result
        (let ((a 1)
              (b 2)
              (op (nth (random 3) '(+ - *))))
          (list
           (eval-in-current-environment `(list (,op a b)
                                               (setf a 999
                                                     b (1+ a))))
           a b)))) ; => ((x 1000) 999 1000) where x is 3 or -1 or 2
   (not (null
         (member result
                 '(((3 1000) 999 1000)
                   ((2 1000) 999 1000)
                   ((-1 1000) 999 1000))
                 :test #'equal)))) ;=> T
 
 (equal
  (let ((a 1) (b 2))
    `((,a ,b)
      ,@(eval-in-current-environment
         '(let ((a b) (b a))
           `((,a ,b)
             ,(eval-in-current-environment
               '(let ((a b) (b a))
                 `(,a ,b)))))))) ;=> ((1 2) (2 1) (1 2))
  (let ((a 1) (b 2))
    `((,a ,b)
      ,@(let ((a b) (b a))
             `((,a ,b)
               ,(let ((a b) (b a))
                     `(,a ,b))))))) ;=> ((1 2) (2 1) (1 2))
                                        ;=> T
 
 (equal
  (let (next-fibs)
    (nconc
     (let* ((a 0)
            (b 1))
       ;; dummy function to test local functions too
       (flet ((my+ (x y) (+ x y)))
         (setf next-fibs
               (compile-in-current-environment
                (lambda (&optional (dummy 'dummy-val))
                  "This is a test lambda form, it contains a docstring
and a test declaration and some fibs"
                  (declare (ignore dummy))
                  (psetf a b
                         b (my+ a b))
                  a))))
       ;; works within lexical scope
       #1=(loop repeat 10 collect (funcall next-fibs)))
     ;; works outside of lexical scope (only for compile/ce!)
     #1#))
  (list 1 1 2 3 5 8 13 21 34 55 89 144 233 377
        610 987 1597 2584 4181 6765)) ;=> T
 
 ;; end AND
 ) ;=> T

