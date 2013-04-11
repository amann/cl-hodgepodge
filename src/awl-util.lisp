;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;============================================================================
;;;;* Type checks

(defun awl::type-spec-p (expr &optional env)
  "Return true if expr is a valid type specification in the environment ENV, NIL else."
  (ignore-errors (typep nil expr env) t))
(deftype awl::type-spec ()
  "Type of an expression which is a valid type specification."
  '(satisfies awl::type-spec-p))

(defun awl::lambda-expr-p (expr)
  "Return true if expr is a lambda expression, NIL else."
  (ignore-errors (destructuring-bind (lambda (&rest arg) &rest body)
                     expr
                   (declare (ignore arg body))
                   (eq 'lambda lambda))))
(deftype awl::lambda-expr () '(satisfies awl::lambda-expr-p))

(defun awl::lambda-expr-1-p (expr)
  "Return true if expr is a lambda expression with exactly one argument and this argument is mandatory, NIL else."
  (ignore-errors (destructuring-bind (lambda (arg) &rest body)
                     expr
                   (declare (ignore arg body))
                   (eq 'lambda lambda))))
(deftype awl::lambda-expr-1 () '(satisfies awl::lambda-expr-1-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awl::variablep (x)
    (and (symbolp x) (not (constantp x)))))
(deftype awl::variable () '(satisfies awl::variablep))



(defun globally-specialp (symbol)
  "Return true if symbol has been declaimed special. Useful for checking if a symbol can be used for a symbol-macro(let)."
  (check-type symbol symbol)
  (eq :special (awl::variable-information symbol)))
;;;; Other (buggy) tentatives:
;;;; On comp.lang.lisp:
;;;;
;;;;    From: "Pascal J. Bourguignon" <pjb@xxxxxxxxxxxxxxxxx>
;;;;    Date: Sun, 24 Apr 2011 08:33:53 +0200
;;;;
;;;;D Herring <dherring@xxxxxxxxxxxxxxxxxxx> writes:
;;;;
;;;;    Special Operator SYMBOL-MACROLET:
;;;;    "symbol-macrolet signals an error if a special declaration names one
;;;;    of the symbols being defined by symbol-macrolet"
;;;;
;;;;    Is there a better way not requiring the cltl2 environment interface?
;;;;
;;;;
;;;;
;;;;So far, we have:
;;;;
;;;;
;;;;(defun specialp (symbol)
;;;;"Detects whether the symbol has been declared special."
;;;;(and (symbolp symbol)
;;;;(cond
;;;;((constantp symbol) nil)
;;;;((not (eql symbol (macroexpand symbol))) nil)
;;;;(t (handler-bind ((warning (function muffle-warning)))
;;;;(eval `(flet ((f () ,symbol))
;;;;(let ((,symbol t))
;;;;(not (nth-value 1 (ignore-errors (f))))))))))))
;;;;
;;;;(defvar a 1)
;;;;(defconstant c 2)
;;;;(define-symbol-macro sa a)
;;;;(define-symbol-macro sb b)
;;;;(define-symbol-macro sc c)
;;;;(define-symbol-macro sd d)
;;;;(define-symbol-macro se 42)
;;;;
;;;;(values
;;;;(funcall (compile nil (lambda ()
;;;;(let ((b 1))
;;;;(declare (special b))
;;;;(list (specialp 'a) (specialp 'b)
;;;;(specialp 'c) (specialp 'd)
;;;;(specialp 'sa) (specialp 'sb)
;;;;(specialp 'sc) (specialp 'sd)
;;;;(specialp 'se))))))
;;;;(list (specialp 'a) (specialp 'b)
;;;;(specialp 'c) (specialp 'd)
;;;;(specialp 'sa) (specialp 'sb)
;;;;(specialp 'sc) (specialp 'sd)
;;;;(specialp 'se)))
;;;;
;;;;;; (T T NIL NIL NIL NIL NIL NIL NIL)
;;;;;; (T NIL NIL NIL NIL NIL NIL NIL NIL)
;;;;
;;;;
;;;;I claim that SPECIALP is conforming, and any implementation not giving
;;;;the same results on any symbol is not conforming.
;;;;
;;;;-- 
;;;;__Pascal Bourguignon__ http://www.informatimago.com/
;;;;A bad day in () is better than a good day in {}.
;;;;
;;;; This version doesn't work in following situations:
;;;; (1) when the dynamic symbol in SYMBOL has a declared type, it may not
;;;; be possible to assign T (or any other arbitrary value) to it.
;;;; (2) When the symbol in SYMBOL is locally declared special but unbound
;;;; (as it may be the case for free variables in a function definition,
;;;; this version of SPECIALP returns NIL.
#+ (or)
(defun specialp (symbol)
  "Detects whether the symbol has been declared special."
  (and (symbolp symbol)
       (cond
         ((constantp symbol) nil)
         ((not (eql symbol (macroexpand symbol))) nil)
         (t (handler-bind ((warning (function muffle-warning)))
              (eval `(flet ((f () ,symbol))
                       (let ((,symbol t))
                         (not (nth-value 1 (ignore-errors (f))))))))))))
#+ (or)
(defun specialp (symbol)
  "Detects whether the symbol has been declared special."
  (and (symbolp symbol)
       (cond
         ((constantp symbol) nil)
         ((not (eql symbol (macroexpand symbol))) nil)
         (t (handler-bind ((warning (function muffle-warning)))
              (funcall (compile nil `(lambda ()
                                       (declare (special foo))
                                       (flet ((f () ,symbol))
                                         (let ((,symbol foo))
                                           (ignore-errors (f) t)))))))))))
;;;;------------
(deftype awl::ntuple (&rest elt-types)
  "An ntuple is a list with n elements. For example, (ntuple * * *) is a triple of unspecified elements."
  (if elt-types
      `(cons ,(car elt-types) (awl::ntuple ,@(cdr elt-types)))
      'null))

(deftype awl::pair (&optional first (second first))
  "A pair is a list with two elements. The arguments specify the type of the elements. If only the first argument is given it means both elements are of that same type; for leaving unspecified the second element when specifying the first one, a * must be used, e.g. (awl:pair symbol *)."
  `(awl::ntuple ,first ,second))

(defun awl::pairp (x &rest types)
  (typep x `(awl::pair ,@types)))

(defun awl::elt-type-p (sequence type &optional env)
  "Return T if all elements in SEQUENCE are of type TYPE and NIL else."
  (declare (type sequence sequence))
  (every (lambda (elt) (typep elt type env)) sequence))
(deftype awl::list-of (elt-type &environment environment)
  "A list with elements of type TYPE."
  (setf (symbol-function '#1=#:check-elt-type)
        (lambda (list)
          (declare (type list list))
          (awl::elt-type-p list elt-type environment)))
  `(and list (satisfies #1#)))

(deftype awl::string-designator ()
  "A string designator."
  `(or character symbol string))


;;;;* Basic Macros
;;;;** Basic List Processing
(defun awl::ensure-list (o)
  "Return either the object O itself if O is a list or a list containing O otherwise."
  (typecase o
    (list o)
    (t (list o))))


(defun awl::flatten (x)
  "Return a list of all atoms contained in the tree-structures cons X in the depth-first search order."
  (labels ((rec (x acc)
             (typecase x
               (null acc)
               (atom (cons x acc))
               (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;;;** Some Utilities for Generating Code
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awl::var-binding-pair (pair)
    "Return (list PAIR PAIR) if PAIR is of type awl::variable, return PAIR if PAIR is a pair with first element of type awl::variable or signals an error of type type-error otherwise."
    (etypecase pair
      (awl::variable (list pair pair))
      ((awl::pair awl::variable *) pair))))

(defun awl::%call-fn (fn &rest args)
  "Auxilliary function generating a piece of code for calling function FN with arguments ARGS in generated code."
  (etypecase fn
    (function `(funcall ,fn ,@args))
    ((and symbol (not null)) `(,fn ,@args))))

(declaim (inline awl::mkstr awl::g!sym* awl::g!sym)
         (ftype (function (&rest *) string) awl::mkstr)
         (ftype (function (list) symbol) awl::g!sym*)
         (ftype (function (&rest *) symbol) awl::g!sym))

(defun awl::mkstr (&rest args)
  "Return a string which is a concatenation of the printed representations via CL:PRINC of ARGS."
  (declare (optimize (speed 3)))
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun awl::g!sym* (args)
  "Return a gensym whose name is a concatenation of the printed representations via CL:PRINC of ARGS."
  (declare (optimize (speed 3)))
  (gensym (apply #'awl::mkstr args)))
(defun awl::g!sym (&rest objs)
  "Return a gensym whose name is a concatenation of the printed representations via CL:PRINC of OBJS."
  (declare (optimize (speed 3)))
  (awl::g!sym* objs))

(defun awl::symb (&rest args)
  "Intern into the current package and return the symbol whose name is a concatenation of the printed representations via CL:PRINC of ARGS."
  (values (intern (apply #'awl::mkstr args))))


#+sbcl
(shadowing-import 'sb-int:parse-body "AWL")
#-sbcl
(defun awl::parse-body (body)
  (let* ((first-body (first body))
         (rest-body (rest body))
         (docstring (when (and rest-body (stringp first-body))
                      first-body))
         (body (if docstring rest-body body))
         (declarations (loop :while (eq 'declare (caar body))
                          :for form := (pop body)                          
                          :collect form)))
    (values body declarations docstring)))
(let ((parse-body-fn-symbol (find-symbol "PARSE-BODY" "AWL")))
  (when parse-body-fn-symbol
    (setf (documentation parse-body-fn-symbol 'function)
          "Parse the expression BODY as the body of a DEFUN-like macro and return as multiple values (1) the effective body, (2) the declaration part, (3) the documentation string.")))


;;;;** ONCE-ONLY and WITH-GENSYMS
(defmacro awl::once-only ((&rest names) &body body)
  "Generate code which ensures that variables used in a macro definition and listed in NAMES are expanded only once."
  (let ((gensyms (mapcar 'awl::g!sym names)))
    `(let ,(mapcar (lambda (g) `(,g (gensym))) gensyms)
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let ,(mapcar 'list names gensyms)
                ,@body)))))

(defmacro awl::with-gensyms ((&rest names) &body body)
  "Lexically bind NAMES to gensyms."
  `(let ,(mapcar (lambda (name)
                   `(,name (gensym ,(string name))))
                 names)
     ,@body))

;;;;** Binding Forms
;;;;
;;;; TODO: The interface is not satisfying: For the moment BODY, FORM and ENV are
;;;; implicitly bound to the &body, &whole resp. &environment variables of the
;;;; macro NAME being defined. Maybe one wants other args to be defined
;;;; in the arglist of the macro... the interface should not become too
;;;; complicated however.
(defmacro awl::define-recursive-macro (name (arg-destructuring &optional canonalizing-fn)
                                       &body body)
  "Define a macro with name NAME and lambda list of the form ((&rest args) &body body) where args is a list of forms which, after being applied to CANONALIZING-FN are of the form which can be destructured by PROTOTYPICAL-ARG."
  (multiple-value-bind (definition declarations docstring)
      body
    (let ((first-arg (if canonalizing-fn
                    `(funcall ,canonalizing-fn (first args))
                    `(first args)))
          (definition (let ((definition (if (= 1 (length definition))
                                            (first definition)
                                            ``(progn ,,@definition))))
                        (etypecase arg-destructuring
                          (symbol
                           `(let ((,arg-destructuring ,first-arg))
                              ,definition))
                          (list
                           `(destructuring-bind ,arg-destructuring
                                ,first-arg
                              ,definition))))))
      `(defmacro ,name (&whole form (&rest args) &body body &environment env)
         ,@(when docstring (list docstring))
         (declare (ignorable env))
         ,@declarations
         (let ((body (let ((rest-args (rest args)))
                       (if rest-args
                           `((,(first form) ,rest-args ,@body))
                           body))))
           (if args
               ,definition
               `(progn ,@body)))))))

;;;;*** Non recursive Macrolet
;;;; If one wants to create an environment in which a local macro is defined
;;;; using macrolet, one gets the problem that, when several such environments
;;;; are nested, the innermost macrolet shadows the outer macrolets with same
;;;; name and the inner macrolet cannot directly refer to the outer one, since
;;;; it will lead to an infinite loop. With awl:macrolet* one can refer to the
;;;; next outer macrolet or macro or even function by simply calling it.
;;;; For example (flet ((bar (x) (* 5 x)))
;;;;               (macrolet* ((bar (x) (typecase x
;;;;                                      (symbol `',x)
;;;;                                      (t `(bar ,x)))))
;;;;                 (values (bar 4) (bar a))))
;;;; returns the values 20, A
#+ (or) ;; In wait for finalizing AWL:DEFINE-RECURSIVE-MACRO.
(progn
  (defmacro #1=#:macrolet-wrapper (expr)
    expr)
  (define-setf-expander #1# (expr)
    (awl:with-gensyms (g!value)
      (values nil
              nil
              `(,g!value)
              `(setf ,(first (last expr)) ,g!value)
              `(#1# ,expr))))
  (awl::define-recursive-macro awl::macrolet* ((name arglist &body macro-body) #'identity)
    "Like CL:MACROLET, however invocations in the body of the local macro being defined of the macro of the same name as the local macro being defined are not recursive, but call the macro of function defined in the next outer lexical environment (if that macro or function exists)."
    (let ((macro-function (macro-function name env)))
      (if macro-function
          `(macrolet ((,name ,arglist
                        `(#1# (macrolet ((,',name (&whole form &rest args)
                                           (declare (ignore args))
                                           (funcall ,',macro-function form ,',env)))
                                ,,@macro-body))))
             ,@body)
          (let ((g!name (gensym (string name))))
            `(flet ((,g!name (&rest args)
                      (apply (function ,name) args)))
               (declare (inline ,g!name))
               (macrolet ((,name ,arglist
                            `(#1# (macrolet ((,',name (&rest args)
                                               `(,',',g!name ,@args)))
                                    ,,@macro-body))))
                 ,@body)))))))

;;;; #:macrolet-wrapper is a dummy macro wrapping a macrolet
;;;; and for which we define a setf-expander. A call to a
;;;; macrolet macro is expanded before setf dispaches its
;;;; own expanding mechanisms. Macrolets from awl::macrolet*
;;;; are all expanded into (macrolet (...) ...). However, for
;;;; macrolet -- at least in scbl -- no setf-expander is defined
;;;; and no such can be portably defined. Hence, we wrap the
;;;; above macrolet in a dummy macro for which we define a
;;;; setf-expander.
(progn
  (defmacro #1=#:macrolet-wrapper (expr)
    expr)
  (define-setf-expander #1# (expr)
    (awl:with-gensyms (g!value)
      (values nil
              nil
              `(,g!value)
              `(setf ,(first (last expr)) ,g!value)
              `(#1# ,expr))))
  (defmacro awl::macrolet* ((&rest bindings) &body body 
                            &environment env)
    "Like CL:MACROLET, however invocations in the body of the local macro being defined of the macro of the same name as the local macro being defined are not recursive, but call the macro of function defined in the next outer lexical environment (if that macro or function exists)."
    (let ((body (let ((rest (rest bindings)))
                  (if rest
                      `((awl::macrolet* ,rest
                          ,@body))
                      body))))
      (if bindings
          (destructuring-bind (name arglist &rest macro-body)
              (first bindings)
            (let ((macro-function (macro-function name env)))
              (if macro-function
                  `(macrolet
                       ((,name ,arglist
                          `(#1# (macrolet
                                    ((,',name (&whole form &rest args)
                                       (declare (ignore args))
                                       (funcall ,',macro-function form ,',env)))
                                  ,,@macro-body))))
                     ,@body)
                  (let ((g!name (gensym (string name))))
                    `(flet ((,g!name (&rest args)
                              (apply (function ,name) args)))
                       (declare (inline ,g!name))
                       (macrolet
                           ((,name ,arglist
                              `(#1# (macrolet ((,',name (&rest args)
                                                 `(,',',g!name ,@args)))
                                      ,,@macro-body))))
                         ,@body))))))
          `(progn ,@body)))))


#+ (or)
(defmacro awl::macrolet* ((&rest bindings) &body body 
                          &environment env)
  "Like CL:MACROLET, however invocations in the body of the local macro being defined of the macro of the same name as the local macro being defined are not recursive, but call the macro of function defined in the next outer lexical environment (if that macro or function exists)."
  (let ((body (let ((rest (rest bindings)))
                (if rest
                    `((awl::macrolet* ,rest
                        ,@body))
                    body))))
    (if bindings
        (destructuring-bind (name arglist &rest macro-body)
            (first bindings)
          (let ((macro-function (macro-function name env)))
            (if macro-function
                `(macrolet
                     ((,name ,arglist
                        `(macrolet
                             ((,',name (&whole form &rest args)
                                (declare (ignore args))
                                (funcall ,',macro-function form ,',env)))
                           ,,@macro-body)))
                   ,@body)
                (let ((g!name (gensym (string name))))
                  `(flet ((,g!name (&rest args)
                            (apply (function ,name) args)))
                     (declare (inline ,g!name))
                     (macrolet
                         ((,name ,arglist
                            `(macrolet ((,',name (&rest args)
                                          `(,',',g!name ,@args)))
                               ,,@macro-body)))
                       ,@body))))))
        `(progn ,@body))))
#+ (or) ;Original version
(defmacro awl::macrolet* ((&rest bindings) &body body
                          &environment env)
  "Like CL:MACROLET, however invocations in the body of the local macro being defined of the macro of the same name as the local macro being defined are not recursive, but call the macro defined in the outer lexical environment (if it exists)."
  (destructuring-bind (name arglist &rest macro-body)
      (first bindings)
    `(macrolet ((,name ,arglist
                  `(macrolet ((,',name (&whole form &rest args)
                                (declare (ignore args))
                                (macroexpand-1 form ,',env)))
                     ,,@macro-body)))
       ,@ (let ((rest (rest bindings)))
            (if rest
                `((awl::macrolet* ,rest
                    ,@body))
                body)))))

;;;;*** Locally Bind Functions
;;;; Locally bind functions to symbols via `flet'. The rationale for this
;;;; macro is to locally rename functions or to easily give names to functions
;;;; stored in symbol-value. For this `flet' is enough.
#+ (or)
(awl::define-recursive-macro fbind ((var fct-expr) #'awl::var-binding-pair)
  "Locally bind functions to symbols via `flet'. The rationale for this macro is to locally rename functions or to easily give names to functions stored in symbol-value thus avoiding the use of funcall. The syntax is as in let except if a binding consists of only one symbol s the function stored  in s in the lexical environment is flet to the symbol s."
  (awl::with-gensyms (g!fct)
    `(let ((,g!fct (compile nil ,fct-expr)))
       (declare (type function ,g!fct))
       (flet ((,var (&rest args) (apply (the function ,g!fct) args)))
         (declare (inline ,var))
         ,@body))))
(defmacro awl::fbind ((&rest bindings) &body body)
  "Locally bind functions to symbols via `flet'. The rationale for this macro is to locally rename functions or to easily give names to functions stored in symbol-value thus avoiding the use of funcall. The syntax is as in let except if a binding consists of only one symbol s the function stored  in s in the lexical environment is flet to the symbol s."
  (let ((body (let ((rest (rest bindings)))
                (if rest
                    `((awl::fbind ,rest ,@body))
                    body))))
    (if bindings
        (awl::with-gensyms (g!fct)
          (destructuring-bind (var fct-expr)
              (awl::var-binding-pair (first bindings))
            `(let ((,g!fct (compile nil ,fct-expr)))
               (declare (type function ,g!fct))
               (flet ((,var (&rest args) (apply (the function ,g!fct) args)))
                 (declare (inline ,var))
                 ,@body))))
        `(progn ,@body))))



;;;;*** Protect Macros used in Macro Definitions Against Overwriting or Shadowing
#+ (or)
(awl::define-recursive-macro awl::with-macros-protected
    ((var macroname) (lambda (pair)
                       (etypecase pair
                         ((awl::pair symbol symbol) pair)
                         (symbol `(,pair ,pair)))))
  "Ensure that when using macros whose name are in NAMES while defining a macro, the macro definitions when expanding those macros are effectively the expected ones. NAMES is a list of macro names or pairs (alias macro-name)."
  (let ((macro-function (macro-function macroname env)))
    (if macro-function
        (awl::with-gensyms (g!macroname)
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (setf (macro-function g!macroname) macro-function))
          ``(macrolet ((,',var (&rest args)
                         `(,',',g!macroname ,@args)))
              ,,@body))
        (error "Unknown macro ~S." macroname))))
(defmacro awl::with-actual-macros (names &body body &environment env)
  "Ensure that when using macros whose name are in NAMES while defining a macro, the macro definitions when expanding those macros are effectively the expected ones. NAMES is a list of macro names or pairs (alias macro-name)."
  (let ((body (let ((rest (rest names)))
                (if rest
                    `((awl::with-actual-macros ,rest ,@body))
                    body))))
    (if names
        (destructuring-bind (var macroname)
            (let ((pair (first names)))
              (etypecase pair
                ((awl::pair symbol symbol) pair)
                (symbol `(,pair ,pair))))
          (let ((macro-function (macro-function macroname env)))
            (if macro-function
                (awl::with-gensyms (g!macroname)
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (setf (macro-function g!macroname) macro-function))
                  ``(macrolet ((,',var (&rest args)
                                 `(,',',g!macroname ,@args)))
                      ,,@body))
                (error "Unknown macro ~S." macroname))))
        `(progn ,@body))))

;;;; Those versions have the problem that the macro-function
;;;; couldn't be dumped into the fasl file.
#+ (or)
(defmacro awl::with-actual-macros (names &body body &environment env)
  "Ensure that when using macros whose name are in NAMES while defining a macro, the macro definitions when expanding those macros are effectively the expected ones. NAMES is a list of macro names or pairs (alias macro-name)."
  (let ((body (let ((rest (rest names)))
                (if rest
                    `((awl::with-actual-macros ,rest ,@body))
                    body))))
    (if names
        (destructuring-bind (var macroname)
            (let ((pair (first names)))
              (etypecase pair
                ((awl::pair symbol symbol) pair)
                (symbol `(,pair ,pair))))
          (let ((macro-function (macro-function macroname env)))
            (if macro-function
                (awl::with-gensyms (g!macroname)
                  `(progn (unless (macro-function ',g!macroname)
                            (eval-when (:compile-toplevel :load-toplevel :execute)
                              (defmacro ,g!macroname (&whole form &rest args)
                                (declare (ignore args))
                                (funcall ,macro-function form ,env))))
                          `(macrolet ((,',var (&rest args)
                                        `(,',',g!macroname ,@args)))
                             ,,@body)))
                (error "Unknown macro ~S." macroname))))
        `(progn ,@body))))
#+ (or)
;;;; This version has following bug: the uninterned symbol #:macroname is the same for
;;;; all uses of the macro awl::with-actual-macros, hence the macro #:macroname is
;;;; overwritten at each use of awl::with-actual-macros.
(defmacro awl::with-actual-macros (names &body body &environment env)
  "Ensure that when using macros whose name are in NAMES while defining a macro, the macro definitions when expanding those macros are effectively the expected ones. NAMES is a list of macro names or pairs (alias macro-name)."
  (let ((body (let ((rest (rest names)))
                (if rest
                    `((awl::with-actual-macros ,rest ,@body))
                    body))))
    (if names
        (destructuring-bind (var macroname)
            (let ((pair (first names)))
              (etypecase pair
                ((awl::pair symbol symbol) pair)
                (symbol `(,pair ,pair))))
          (let ((macro-function (macro-function macroname env)))
            (if macro-function
                (progn (eval-when (:compile-toplevel :load-toplevel :execute)
                         (defmacro #1=#:macroname (&whole form &rest args)
                           (declare (ignore args))
                           (funcall macro-function form env)))
                       ``(macrolet ((,',var (&rest args)
                                      `(#1# ,@args)))
                           ,,@body))
                (error "Unknown macro ~S." macroname))))
        `(progn ,@body))))
#+ (or)
(defmacro awl::with-actual-macros (names &body body &environment env)
  "Ensure that when using macros whose name are in NAMES while defining a macro, the macro definitions when expanding those macros are effectively the expected ones. NAMES is a list of macro names or pairs (alias macro-name)."
  (let ((body (let ((rest (rest names)))
                (if rest
                    `((awl::with-actual-macros ,rest ,@body))
                    body))))
    (if names
        (destructuring-bind (var macroname)
            (let ((pair (first names)))
              (etypecase pair
                ((awl::pair symbol symbol) pair)
                (symbol `(,pair ,pair))))
          `(macrolet ((,var (form env)
                        (funcall ,(macro-function macroname env) form env)))
             ,@body))
        `(progn ,@body))))
#+ (or)
(defmacro awl::with-actual-macros (names &body body &environment env)
  "Ensure that when using macros whose name are in NAMES while defining a macro, the macro definitions when expanding those macros are effectively the expected ones. NAMES is a list of macro names or pairs (alias macro-name)."
  (let ((body (let ((rest (rest names)))
                (if rest
                    `((awl::with-actual-macros ,rest ,@body))
                    body))))
    (if names
        (destructuring-bind (var macroname)
            (let ((pair (first names)))
              (etypecase pair
                ((awl::pair symbol symbol) pair)
                (symbol `(,pair ,pair))))
          (awl::with-gensyms (g!macrofct)
            `(flet ((,g!macrofct (form env)
                      (funcall ,(macro-function macroname env) form env)))
               `(macrolet ((,',var (&whole form &rest args &environment env)
                             (declare (ignore args))
                             (,',g!macrofct form env)))
                  ,,@body))))
        `(progn ,@body))))


#+ (or)
(progn
  (defmacro define-recursive-macro (name (arg-destructuring canonalizing-fn) &body body)
    "Define a macro with name NAME and lambda list of the form ((&rest args) &body body) where args is a list of forms which, after being applied to CANONALIZING-FN are of the form which can be destructured by PROTOTYPICAL-ARG."
    (awl:with-gensyms (args g!body)
      `(defmacro ,name (&whole form (&rest ,args) &body ,g!body)
         (let ((,g!body (let ((rest (rest ,args)))
                          (if rest
                              `((,(first form) ,rest ,@g!body))
                              ,g!body))))
           (if ,args
               , (etypecase arg-destructuring
                   (awl::variable
                    `(let ((,arg-destructuring (first ,args)))
                       ,@body))
                   (cons `(destructuring-bind ,arg-destructuring
                              (funcall ,canonalizing-fn (first ,args))
                            ,@body)))
               `(progn ,@,g!body))))))

  (define-recursive-macro fbind ((var fct-expr) #'awl::var-binding-pair)
    `(let ((,g!fct (compile nil ,fct-expr)))
       (declare (type function ,g!fct))
       (flet ((,var (&rest args) (apply (the function ,g!fct) args)))
         (declare (inline ,var))
         ,@body))))



;;;;** From Let over Lambda 

(defun g!-symbol-p (s)
  (and (symbolp s)
       (< 2 (length (symbol-name s)))
       (string= (symbol-name s) "G!" :start1 0 :end1 2)))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "O!" :start1 0 :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (awl::symb "G!" (subseq (symbol-name s) 2)))

;;;;*** Defmacro!

(defmacro awl::defmacro! (name args &rest body)
  "As CL:DEFMACRO except that args prefixed with o! are evaluated once only and backquote-escaped variables in the body prefixed by g! are declared with gensyms."
  (let* ((os (remove-if-not #'o!-symbol-p (awl::flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
        (awl::parse-body body)
      (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p
                                                    (awl::flatten body)))))
       `(defmacro ,name ,args
          ,@(when docstring (list docstring))
          ,@declarations
          (let ,(mapcar (lambda (s)
                          `(,s (gensym ,(subseq (symbol-name s) 2))))
                        syms)
            `(let ,(mapcar #'list (list ,@gs) (list ,@os))
               ,(progn ,@body))))))))

#+ (or)
(progn
  (defmacro defmacro/g! (name args docstring declarations &rest body)
    (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p
                                                  (awl::flatten body)))))
      `(defmacro ,name ,args
         ,@(when docstring (list docstring))
         ,@declarations
         (let ,(mapcar (lambda (s)
                         `(,s (gensym ,(subseq (symbol-name s) 2))))
                       syms)
           ,@body))))
  (defmacro awl::defmacro! (name args &rest body)
    "As CL:DEFMACRO except that args prefixed with o! are evaluated once only and backquote-escaped variables in the body prefixed by g! are declared with gensyms."
    (let* ((os (remove-if-not #'o!-symbol-p (awl::flatten args)))
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
                       ,(progn ,@body))))))





;;;;* Sequences, Vectors and Lists
;;;;** Sequences
;;;; TODO review the algorithm: it is inefficient but should work as expected.
(defun awl::remove-if (test sequence &rest options
                       &key (key #'identity)
                         from-end start end count)
  "Same as cl:remove-if except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (let ((items-to-be-removed (apply #'cl:remove-if-not test sequence options))
        (resulting-sequence sequence))
    (map nil (lambda (item)
               (setq resulting-sequence
                     (apply #'cl:remove item resulting-sequence
                            :count 1 :test #'eq :key #'identity options)))
         items-to-be-removed)
    (values resulting-sequence items-to-be-removed)))

(defun awl::remove (item sequence &rest options &key (test #'eql) key from-end start end count)
  "Same as cl:remove except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (apply #'awl::remove-if (lambda (elt) (funcall test item elt))
         sequence options))

(defun awl::find-extrema (sequence predicate &key key (equality #'equal))
  "Return a list containing the extrema of SEQUENCE according to the preorder relation PREDICATE. An element e is extremal if for every element f in SEQUENCE which is not equal to e with respect to the equality predicate EQUALITY, PREDICATE(f e) = NIL. If KEY is given it is applied to the elements of sequence before being applied to PREDICATE and EQUALITY. The keyword argument EQUALITY defaults to CL:EQUAL."
  (declare ((or symbol (function (* *) boolean)) predicate equality)
           ((or symbol (function (*) *)) key)
           (optimize speed))
  (let* ((key-y&key-x (if key
                          `((funcall ,key y)
                            (funcall ,key x))
                          '(x y)))
         (y<x `(and (not (funcall ,equality ,@key-y&key-x))
                    (funcall ,predicate ,@key-y&key-x))))
    (remove-if (compile nil `(lambda (x)
                               (some (lambda (y) ,y<x)
                                     ',sequence)))
               sequence)))
(defun awl::insert (pos sequence &rest values)
  "Return a fresh sequence with VALUES inserted at POS. If POS is negative the position is relative to the end of SEQUENCE, -1 being after the last element."
  (let* ((type (let ((type (type-of sequence)))
                        (etypecase type
                          (cons (car type))
                          (null 'list)
                          (symbol (case type
                                    (null 'list)
                                    (t type))))))
         (sl (length sequence))
         (output-type (case type
                        (simple-vector (list type (+ sl (length values))))
                        (t type)))
         (sl+1 (1+ sl))
         (pos (mod (max (min pos sl) (- sl+1)) sl+1)))
    (concatenate output-type (subseq sequence 0 pos) values (subseq sequence pos))))


(defun awl::ninsert (pos sequence &rest values)
  "Return a sequence of same type as SEQUENCE with VALUES inserted at POS. NINSERT may destructively modify SEQUENCE by inserting values at position POS. If POS is negative the position is relative to the end of SEQUENCE, -1 being after the last element. It is unspecified and probably an error (or worse: e.g. infinite loop) if SEQUENCE is a unproper list."
  (etypecase sequence
    (null values)
    (cons (let* ((list sequence)
                 (ll (length list))
                 (ll+1 (1+ ll))
                 (pos (mod (max (min pos ll) (- ll+1)) ll+1))) 
            (if (zerop pos)
                (progn (awl::ninsert 1 list (car list))
                       (apply #'awl::ninsert 1 list (cdr values))
                       (rplaca list (car values)))
                (setf (cdr (nthcdr (1- pos) list))
                      (append values (nthcdr pos list))))
            list))
    (sequence (apply #'awl::insert pos sequence values))))


;;;;** Vectors

(defun awl::make-binary-search (preorder-fn sequence &key key)
  "Return a function taking a unique argument X and which returns the entities xl as primary, xu as secondary and i as third value satisfying either
-- xl = nil, x < KEY(xu), i = -1 and xu is the leftmost element of SEQUENCE,
-- not(x < KEY(xl)) and x < KEY(xu), 0 <= i < length(SEQUENCE), xl is the ith element and xu the i+1th element of SEQUENCE,
-- not(x < KEY(xl)), xu = nil, i = length(SEQUENCE) - 1 and xl is the rightmost element of SEQUENCE,
with respect to the preorder relation < given by PREORDER-FN and defined on the elements of SEQUENCE applied to KEY if KEY is given. Notice: if SEQUENCE contains elements which are indistinguible for < then the leftmost of those elements can only be an xu and the rightmost element can only be a xl; in particular if there are more than 2 elements which are indistinguible those inbetween will never be an output of the binary search."
  (declare (optimize speed)
           ((function (* *) *) preorder-fn)
           ((or null (function (*) *)) key))
  (let* ((seq (stable-sort (coerce sequence 'simple-vector) preorder-fn :key key))
         (< (compile nil `(lambda (a b)
                            (funcall ,preorder-fn a ,(if key `(funcall ,key b) 'b)))))
         (m (1- (length seq))))
    (declare (unsigned-byte m)
             (simple-vector seq)
             ((function (* *) *) <))
    (symbol-macrolet ((i* `(setq i (the unsigned-byte (+ lo (floor (the unsigned-byte (- up lo)) 2)))))
                      (x<xl `(funcall ,< x (setq xl (svref ,seq ,i*))))
                      (x<xu `(funcall ,< x (setq xu (svref ,seq (1+ i))))))
      (compile nil `(lambda (x &optional message)
                      (declare (optimize speed))
                      (if message
                          (case message
                            (:grid ,seq)
                            (:preorder ,<))
                          (let ((i 0) (lo 0) (up ,m) xl xu)
                            (declare (unsigned-byte i lo up))
                            (loop (cond (,x<xl (when (zerop i) (return (values nil xl -1)))
                                               (setq up i))
                                        ((= ,m i) (return (values xl nil ,m)))
                                        (,x<xu (return (values xl xu i)))
                                        (t (setq lo (the unsigned-byte (1+ i)))))))))))))
(defun awl::getgrid (binary-search)
  (funcall binary-search nil :grid))
(defun awl::getpreorder (binary-search)
  (funcall binary-search nil :preorder))

;;;;** Lists
;;;; Several utilities which might be more or less useful. I think
;;;; most of them are not useful... functions whose documentation
;;;; is more complicated than reprogram the equivalent function are
;;;; at least questionable.

(defun awl::group (list n)
  "Return a partition of the proper list LIST in form of a list sublists of lenght N from left to right; the last sublist is of length (mod (length LIST) N)."
  (check-type n (integer 1 #.(- most-positive-fixnum 2)))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                   (rec rest (cons (subseq list 0 n) acc))
                   (nreverse (cons list acc))))))
    (when list
      (rec list nil))))

(defun awl::filter (fn list &aux acc)
  "You give filter a function and a list, and get back a list of whatever non-nil values are returned by the function as it is applied to the elements of the list."
  (dolist (x list (nreverse acc))
    (let ((val (funcall fn x)))
      (if val (push val acc)))))

(defun awl::prune (test tree)
  "Every leaf for which the function returns true is removed."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun awl::classify (seq &key (test #'eql) (key #'identity))
  "Return a list of sublists of elements of SEQ; the elements in each sublist are, when applied to KEY, equivalent under TEST."
  (let* ((classes (make-hash-table :test test))
         keys result)
    (map nil (lambda (elt)
               (let ((key (funcall key elt))) 
                 (unless (nth-value 1 (gethash key classes))
                   (push key keys))
                 (push elt (gethash key classes))))
         seq)
    (dolist (key keys)
      (push (nreverse (gethash key classes)) result))
    result))

(defun awl::find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (awl::find2 fn (cdr lst))))))
(defun awl::before (x y lst &key (test #'eql) key)
  "Return the cdr beginning with the object given as the first argument if we encounter the first argument before encountering
the second. It will also return true if the second argument doesn't occur in the list at all."
  (let ((key (when (functionp key) key)))
    (labels ((before (lst)
               (and lst
                    (let ((first (if key
                                     (funcall key (car lst))
                                     (car lst))))
                      (cond ((funcall test y first) nil)
                            ((funcall test x first) lst)
                            (t (awl::before x y (cdr lst) :test test :key key)))))))
      (before lst))))
(defun awl::after (x y lst &key (test #'eql) key)
  "Return the cdr beginning with the object given as the first argument if we encounter the first argument after encountering
the second."
  (let ((rest (awl::before y x lst :test test :key key)))
    (and rest (member x rest :test test :key key))))
(defun awl::duplicate (obj lst &key (test #'eql) key)
  "Return the cdr beginning with the object given as the first argument if we encounter the first argument a second time in LST."
  (member obj (cdr (member obj lst :test test :key key))
          :test test))
(defun awl::split-if (fn lst)
  "Return as primary value a new list with the leftmost elements of the proper list LST for which the function FN, when applied to those elements, returns NIL. The secondary value is the rest of LST from the first element of LST on for which FN returns non NIL."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun awl::most (fn lst)
  "Return as primary value the first element of the proper list LST for whom the value returned by the function FN when applied to those elements is maximal. The secondary value is the value returned by FN."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))
(defun awl::best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))
(defun awl::mostn (fn lst)
  "Return as primary value a list of those elements in LST for which the value obtained by applying them to the function FN is maximal. The secondary value is this maximal value."
  (declare ((function (*) real) fn))
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (declare (real max))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (declare (real score))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defun awl::ncond-insert (item list test &optional termination)
  "Insert ITEM into LIST in front of the first element e (from the left) of LIST for which TEST(e) is true and return the resulting list; If TEST results to NIL fo all elements of LIST up to the tail TERMINATION, ITEM is appended to the front of TERMINATION. This operation destructively modifies LIST (except if ITEM is placed in front of LIST)."
  (declare ((function (*)) test)
           (optimize speed))
  (if (funcall test (car list))
      (cons item list)
      (do ((prev list curr)
           (curr (cdr list) (cdr curr)))
          ((or (eq curr termination) (funcall test (car curr)))
           (setf (cdr prev) (cons item curr))
           list))))

(defun awl::nordered-insert (item list order-pred &key key)
  "Insert ITEM into LIST in front of the first element e (from the left) of LIST for which ORDER-PRED(ITEM e) is true and return the resulting list. If KEY is given, ITEM and e are first applied to KEY before comparing with ORDER-PRED. This operation destructively modifies LIST (except if ITEM is placed in front of LIST)."
  (let ((test (compile nil
                       `(lambda (elt)
                          (funcall ,order-pred
                                   ,@(if key
                                         `((funcall ,key ',item) (funcall ,key elt))
                                         `(',item elt)))))))
    (awl::ncond-insert item list test)))

;;;;**** Mapping

(defun awl::map0-n (fn n)
  "Return a list resulting of mapping the function FN to all integers not below 0 and below N."
  (awl::mapa-b fn 0 n))
(defun awl::map1-n (fn n)
  (awl::mapa-b fn 1 n))
(defun awl::mapa-b (fn a b &optional (step 1))
  "Return a list resulting of mapping the function FN to the numbers from A inclusive to B exclusive by steps STEP."
  (declare (type (function (real) *) fn)
           (type real a b step)
           (optimize speed))
  (let ((test (cond ((< 0 step) (lambda (i) (not (< i b))))
                    ((< step 0) (lambda (i) (not (< b i))))
                    (t (constantly t)))))
    (declare (type (function (real) *) test))
    (do ((i a (+ i step))
         (result nil (cons (funcall fn i) result)))
        ((funcall test i) (nreverse result)))))
(defun awl::map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
(defun awl::mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
(defun awl::mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
(defun awl::rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
               (lambda (&rest args)
                 (apply #'awl::rmapcar fn args))
               args)))

(defun awl::map-tree (fn tree)
  "Map the function FN to each node of the cons structure TREE and return NIL. As node are considered the CARs of the CONSes."
  (declare (type (function (*)) fn))
  (let ((node (car tree)))
    (funcall fn node)
    (when (consp node)
      (awl::map-tree fn node))
    (awl::map-tree fn (cdr tree))))


;;;;***** Parallel Mapping Functions

(defun displace-split (vector count)
  "Return a list of COUNT displaced sub vectors of VECTOR."
  (let* ((length (length vector))
         (count-1 (1- count)))
    (if (zerop count)
        vector
        (multiple-value-bind (d r)
            (ceiling length count)
          (append (loop :for i :below count-1
                     :collect (make-array d :displaced-to vector :displaced-index-offset (* d i)))
                  (list (make-array (+ d r) :displaced-to vector :displaced-index-offset (* d count-1))))))))

(defvar *cpu-count* #+ccl (ccl:cpu-count) #-ccl 4)
(defvar *max-running-threads* 60)
(let ((thread-count 0)
      (lock (bt:make-lock)))
  (defun running-threads ()
    thread-count)
  (defun thread-count-inc-if (pred)
    (bt:with-lock-held (lock)
      (when (funcall pred thread-count) (incf thread-count)))))
(defun determine-split-factor (vects)
  "Return the number of sub vectors the vectors in VECTS should be split into for parallelization."
  (let ((min-size (reduce #'min (mapcar #'length vects))))
    (min (* 3/2 *cpu-count*) (ceiling min-size 1000))))
(defun awl::pmap-into (vector fn &rest vects &aux (count (determine-split-factor vects)))
    "Like cl:map-into but parallelize the process over several threads. Unlike with cl:map-into, the order in which the elements of VECTS are applied to FN is not known. The number of threads which are spawned is determined using the special variable *parallel-thread-count*."
    (when vects 
      (map nil #'bt:join-thread
           (apply #'mapcar
                  (lambda (sv &rest svs)
                    (bt:make-thread (compile nil
                                             `(lambda ()
                                                (map-into ,sv ,fn ,@svs)))))
                  (displace-split vector count)
                  (mapcar (lambda (vect)
                            (displace-split vect count))
                          vects))))
    vector)
(defun awl::pmap (fn &rest vects &aux (count (determine-split-factor vects)))
    "Like cl:map but parallelize the process over several threads and applies only to vectors. Unlike with cl:map, the order in which the elements of VECTS are applied to FN is not known. The number of threads which are spawned is hold in the special variable *parallel-thread-count*."
    (when vects 
      (map nil #'bt:join-thread
           (apply #'mapcar
                  (lambda (&rest svs)
                    (bt:make-thread (compile nil
                                             `(lambda ()
                                                (map nil ,fn ,@svs)))))
                  (mapcar (lambda (vect)
                            (displace-split vect count))
                          vects)))))

;;;;*** Circles

(defun awl::cyclic-p (cons)
  "Return T if the CONS contains cyclic references in the structure."
  (let ((hash (make-hash-table :test #'eq)))
    (labels ((seen-p (c)
               (and (consp c)
                    (or (gethash c hash)
                        (and (setf (gethash c hash) t)
                             (seen-p (car c))
                             (seen-p (cdr c)))))))
      (seen-p cons))))
(defun awl::circle-p (list)
  "Return T if the list LIST contains a circle and nil else. As second value return the length of the list resp. the circle. N.B. in opposition to CYCLIC-P, CIRCLE-P looks only in the CDR for cyclic references; we are interested in LIST as as list and not as a tree."
  (loop :with hash := (make-hash-table :test #'eq)
     :for l :from 0
     :for i :on list
     :for test := (gethash i hash)
     :do (if test
             (return (values t l test))
             (setf (gethash i hash) i))))
(defun awl::length* (seq)
  "Like cl:length but accepting circles."
  (etypecase seq
    (vector (length seq))
    (list (nth-value 1 (awl::circle-p seq)))))
(defun awl::last* (list &optional (n 1))
  "As cl:last but applicable on circles, in which case it returns (last (nth-value 2 (awl::circle-p LIST)) n)."
  (last (nth-value 2 (awl::circle-p LIST)) n))

(defun awl::proper-list-p (o)
  "Return T if o is a propoer list and nil else."
  (and (listp o)
       (not (awl::circle-p o))
       (null (cdr (last o)))))
(defun awl::make-circular (list)
  "Return LIST after having destructively modified it to form a circular list. LIST must be a proper list."
  (rplacd (last list) list)
  list)
(declaim (inline awl::make-circle*))
(defun awl::make-circle* (list)
  "Return a fresh circular list containing the elements of the proper list LIST in the same order with the head pointer pointing to the first element of LIST."
  (awl::make-circular (copy-list list)))
(declaim (inline awl::make-circle))
(defun awl::make-circle (&rest elts)
  "Return a fresh circular list containing the elements ELTS in the same order with the head pointer pointing to the first element of ELTS."
  (awl::make-circle* elts))

;;;;*** Alists and plists
(defun awl::alistp (o &key (key-type #'symbolp))
  "Return T if o is an association list and nil else. An association list is a propoer list of CONSes whose CAR satisfy KEY-TYPE."
  (and (listp o)
       (every (lambda (item)
                (and (consp item)
                     (funcall key-type (car item))))
              o)))
(defun awl::plistp (o &key (key-type #'symbolp))
  "Return T if o is a property list and nil else. A property list is a proper list with an even number of elements and whose elements on even position satisfy KEY-TYPE."
  (and (listp o)
       (evenp (length o))
       (every (let ((role :key))
                (awl::fbind
                    ((valid-key-p (cond
                                    ((functionp key-type)
                                     key-type)
                                    ((and (consp key-type)
                                          (eq 'lambda (first key-type)))
                                     (compile nil key-type))
                                    (t (lambda (x) (typep x key-type))))))
                  (lambda (item)
                    (case role
                      (:value (setq role :key)
                              t)
                      (:key (setq role :value)
                            (valid-key-p item))))))
              o)))

(defun awl::map-plist (fct plist)
  "Like cl:mapcar but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :collect (funcall fct k v)
       :until (null plist))))
(defun awl::map*-plist (fct plist)
  "Like MAP-PLIST but returning only NIL. This is used only for side effects."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :do (funcall fct k v)
       :until (null plist))))
(defun awl::mapcan-plist (fct plist)
  "Like cl:mapcan but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :append (funcall fct k v)
       :until (null plist))))
(defun awl::filter-plist (fct plist)
  "Return a list of non-nil results of FCT when applied on key-values of PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :for r := (funcall fct k v)
       :when r :collect r
       :until (null plist))))
(defun awl::keys+values<-plist (plist)
  "Return as primary value a list of all keys of the property list PLIST and as secondary value a list of all their corresponding values in the same order."
  (let (keys vals)
    (awl::map*-plist (lambda (k v)
                       (push k keys)
                       (push v vals))
                     plist)
    (values (nreverse keys) (nreverse vals))))

(defun awl::alist<-plist (plist)
  "Return an association list from a property list. Note: An association list is a proper list of CONSes and not of pairs."
  (awl::map-plist #'cons plist))
(defun awl::alist2<-plist (plist)
  "Return an association list from a property list. Note: Here an association list is a proper list of pairs and not of CONSes."
  (awl::map-plist #'list plist))

(defun awl::plist<-alist (alist)
  "Return a property list corresponding to the association list ALIST."
  (mapcan (lambda (x)
            (list (car x) (cdr x)))
          alist))
(defun awl::plist<-alist2 (alist2)
  "Return a property list made of the keys and the first element of each association in the association list ALIST2. This is especially used to map malformed association lists where the values of the associations are singletons."
  (mapcan (lambda (x)
            (list (car x) (cadr x)))
          alist2))

(defmacro awl::do-plist ((key value plist &optional result) &body body)
  "Like CL:DOLIST except itering KEY and VALUE over a plist."
  (awl::with-gensyms (g!plist)
    `(let ((,g!plist ,plist))
       (when ,g!plist
         (loop
            :for ,key := (pop ,g!plist)
            :for ,value := (pop ,g!plist)
            :do (progn ,@body)
            :finally (return ,result))))))








;;;;* Macros
;;;;** Inspect Arglists of Functions
(defun awl::function-arglist (fn)
  "Return the arg list of `fn'."
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
(defun awl::function-arglist (x &optional original-x)
  (typecase x
    (symbol (awl::function-arglist (symbol-function x) x))
    (compiled-function (read-from-string
                        (lisp::%primitive header-ref x
                                          lisp::%function-arg-names-slot)))
    (list (case (first x)
            (lambda (second x))
            (lisp::%lexical-closure% (awl::function-arglist (second x)))
            (system:macro '(&rest "Form ="))
            (t '(&rest "Arglist:"))))
    (t (cerror (format nil
                       "Use a reasonable default argument list for ~S"
                       original-x)
               "Unkown object in function cell of ~S:  ~S" original-x x)
       '())))

#+ (or) ;;;; TODO: Correct bug showing a nil element after a &rest variable.
(defun awl::get-parameter-list (lambda-list)
  "Return the argument list used to call a function whose arg list is `lambda-list'."
  (let (rest)
    (append
     (mapcan (let ((state '&ordinary))
               (lambda (x)
                 (if (member x lambda-list-keywords)
                     (progn (setq state x) nil)
                     (case state
                       (&ordinary (etypecase x
                                    (list (list (awl::get-parameter-list x)))
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
;;;;** Lambda Lists
;;;; Copied from Clozure source code
(defun awl::parse-lambda-list (list)
  "Parse the lambda list LIST and return if successful following values:
1. the required arguments,
2. the optional arguments,
3. T if a &rest arg is used, NIL else,
4. the &rest argument,
5. T if &key arguments are used, NIL else,
6. the &key arguments,
7. T if &allow-other-keys is used, NIL else,
8. the &aux arguments."
  (let ((required)
        (optional)
        (keys)
        (aux)
        (restp nil)
        (rest nil)
        (keyp nil)
        (allowp nil)
        (state :required))
    (dolist (arg list)
      (if (and (symbolp arg)
               (let ((name (symbol-name arg)))
                 (and (/= (length name) 0)
                      (char= (char name 0) #\&))))
          (case arg
            (&optional
             (unless (eq state :required)
               (error "Misplaced &optional in lambda-list: ~S." list))
             (setq state '&optional))
            ((&rest #+ccl ccl::&lexpr)
             (unless (member state '(:required &optional))
               (error "Misplaced &rest in lambda-list: ~S." list))
             (setq state '&rest))
            (&key
             (unless (member state '(:required &optional :post-rest))
               (error "Misplaced &key in lambda-list: ~S." list))
             (setq keyp t)
             (setq state '&key))
            (&allow-other-keys
             (unless (eq state '&key)
               (error "Misplaced &allow-other-keys in lambda-list: ~S." list))
             (setq allowp t  state '&allow-other-keys))
            (&aux
             (when (member state '(&rest))
               (error "Misplaced &aux in lambda-list: ~S." list))
             (setq state '&aux))
            (t
             (error "Unknown &keyword in lambda-list: ~S." arg)))
          (case state
            (:required (push arg required))
            (&optional (push arg optional))
            (&rest
             (setq restp t  rest arg  state :post-rest))
            (&key (push arg keys))
            (&aux (push arg aux))
            (t
             (error "Found garbage in lambda-list when expecting a keyword: ~S." arg))))) 
    (values (nreverse required) (nreverse optional)
            restp rest keyp (nreverse keys) allowp
            (nreverse aux))))



(defun get-lmbd-keydefs (lambda-list)
  "Return the part of the lambda list LAMBDA-LIST which define the keyword arguments."
  (let* ((aux (cdr (member '&key lambda-list)))
         (pos (position-if (lambda (item) (member item lambda-list-keywords))
                           aux)))
    (subseq aux 0 pos)))
(defun awl::get-lambda-keys (lambda-list)
  "Return the keyword names which are used by the keyword arguments in given LAMBDA-LIST."
  (mapcar (lambda (keydef)
            (etypecase keydef
              (symbol (intern (string keydef) :keyword))
              ((cons symbol *) (intern (string (car keydef)) :keyword))
              ((cons (awl::pair symbol symbol) *) (caar keydef))))
          (get-lmbd-keydefs lambda-list)))
(defun awl::get-lambda-key-variables (lambda-list)
  "Return the variable names which are assigned by the keyword arguments in given LAMBDA-LIST."
  (mapcar (lambda (keydef)
            (etypecase keydef
              (symbol keydef)
              ((cons symbol *) (car keydef))
              ((cons (awl::pair symbol symbol) *) (cadar keydef))))
          (get-lmbd-keydefs lambda-list)))

#+ (or)
(defun awl::make-lambda-list-from-arglist (arglist)
  (append (awl::get-mandatory-args  arglist)
          (let ((optional-args (awl::get-optional-args arglist)))
            (when optional-args (list* '&optional optional-args)))
          (let ((rest-arg (awl::get-rest-arg arglist)))
            (when rest-arg (list '&rest rest-arg)))
          (let* ((keys (awl::get-keys arglist))
                 (key-vars (loop :for key :in keys
                              :until (eq key '&allow-other-keys) :collect (make-symbol (string key)))))
            (when keys (append (list* '&key (mapcar (awl::f-comp #'list #'list) keys key-vars)) (member '&allow-other-keys keys))))))


(defun awl::get-lambda-variables (lambda-list)
  "Return a list containing the variable names defined in the ordinary or macro lambda list LAMBDA-LIST."
  (mapcan (let ((state '&ordinary))
            (lambda (x)
              (if (member x lambda-list-keywords)
                  (progn
                    (setq state x)
                    nil)
                  (case state
                    (&ordinary (etypecase x
                                 (list (awl::get-lambda-variables x))
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

;;;;*** Arglist

(defun awl::positional-args (arglist)
  "Return the positional arguments of an arglist of a function, that is the mandatory and &optional arguments."
  (let (result)
    (nreverse (dolist (arg arglist result)
                (case arg
                  ((&rest &key &aux) (return result))
                  (&optional)
                  (t (push arg result)))))))
(defun awl::rest-argp (arglist)
  "Return true if a rest arg exists in ARGLIST, NIL else."
  (not (null (member '&rest arglist))))
(defun awl::mandatory-args (arglist)
  "Return the mandatory arguments of the function arglist ARGLIST."
  (loop :for arg :in arglist
     :until (member arg '(&optional &rest &key)) :collect arg))
(defun awl::optional-args (arglist)
  "Return the optional argument specifications of the function arglist ARGLIST."
  (loop :for arg :in (rest (member '&optional arglist))
     :until (member arg '(&rest &key)) :collect arg))

(defun awl::key-args (arglist)
  "Return the key argument specifications of the function arglist ARGLIST."
  (rest (member '&key arglist)))
(defun awl::arglist-keys (arglist)
  "Return the keys defined in ARGLIST."
  (mapcar (lambda (key)
            (etypecase key
              (awl::variable (intern key :keyword))
              ((awl::pair awl::variable *) (intern (first key) :keyword))
              ((awl::pair (awl::pair symbol awl::variable) *) (first (first key)))))
          (awl::key-args arglist)))














#+(or)
(defun awl::expand-list (list)
  (list* 'list*
         (if (listp (car list))
             (awl::expand-list (car list))
             (car list))
         (when (cdr list)
           (list (awl::expand-list (cdr list))))))


;;;;* Lazy Evaluation
;;;;** Delay/Force
;;;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro awl::delay (&body exprs &environment env)
    "Return a delay for the expressions EXPRS. The expressions as a whole are guaranteed to be evaled once at most when forced. The result when forcing the delay is the result of the last expression of EXPRS as of with PROGN."
    (if (notevery (lambda (expr) (constantp expr env)) exprs)
        `(let* ((delay (cons '#1=#:delay nil))
                (funct (lambda () (setf (car delay) '#2=#:forced
                                        (cdr delay) (progn ,@exprs)))))
           (setf (cdr delay) funct)
           delay)
        `(cons '#2# (progn ,@exprs))))
  (declaim (inline awl::force))
  (defun awl::force (o)
    "Return the result of the expressions delayed with the macro DELAY."
    #+sbcl (declare (optimize speed))
    (if (consp o)
        (let ((car (car o)))
          (cond ((eq car '#2#) (cdr o))
                ((eq car '#1#) (funcall (the function (cdr o))))
                (t o)))
        o))
  (declaim (inline awl::force*)
           (ftype (function (cons))))
  (defun awl::force* (delay)
    "Return the result of the expressions delayed with the macro DELAY. The result is unspecified and most probably an error if the input is not a delay produced with the macro DELAY."
    (declare (optimize speed (safety 1)))
    (if (eq (car delay) '#2#) (cdr delay) (funcall (the function (cdr delay)))))
  (deftype awl::delay ()
    `(cons (or (eql #1#) (eql #2#)) *)))

;;;;** Pipes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro awl::make-pipe (head &rest tail)
    "Return a pipe (a lazy list) containing as first element HEAD and in which the elements in the tale are generated by successively evaluating (progn . tail) in the current lexical environment."
    `(cons ,head (cons (lambda () ,@tail) '#1=#:tail)))
  (defun awl::tail (pipe)
    "Return a pipe representing the tail of PIPE."
    (let ((tail (rest pipe)))
      (when (and (consp tail) (eq '#1# (cdr tail)))
        (let ((fn (car tail)))
          (setf (car tail) (funcall fn)
                (cdr tail) (cons '#1# fn))))
      tail)))
(setf (symbol-function 'awl::head) (symbol-function 'first)
      (documentation 'awl::head 'function) (documentation 'first 'function))
(defun awl::pipe-empty-p (pipe)
  (null pipe))




;;;;* Generators, Iterators and Cursors
;;;;
;;;; Generators, Iterators and Cursors are entities including an inner state and which, when invoked
;;;; (by means of the function `NEXT' for example) change the inner state return a value (called
;;;; henceforth "item") depending on this new state.
;;;; 

;;;;** Generators: Implementation and Interface
;;;;
;;;; In this library we take as convention that a generator is a (anonymous) function without arguments
;;;; which, when successively called, changes its internal state and returns an item according to this
;;;; new state. When no further items can be returned (eof), an error of type `NO-NEXT-ITEM-ERROR'
;;;; must be signalled or a customary eof object returned.
;;;;
;;;; As data abstraction, the function `NEXT' can be used instead of funcalling the generator.
;;;;
;;;;** Iterators
;;;;
;;;; An Iterator is like a generator but has a supplementary feature to peek ahead as far as possible for
;;;; fetching elements but without changing the inner state of the iterator. The Iterator must be implemented
;;;; as a function taking one optional integer argument (which may be restricted to be an UNSIGNED-BYTE).
;;;; If called without argument, it behaves exactly like a Generator. If called with 0, it returns the item
;;;; which corresponds to the current state of the Iterator; It is allowed for the Iterator to signal an error
;;;; of type `ITERATOR-NOT-INITIALIZED-ERROR' before the first item has been generated. It is however
;;;; also allowed for an Iterator to return a default initial value instead or to implicitly generate the
;;;; first item and to keep it ready to be fetched with the 0 argument.
;;;;
;;;; When called with a positive integer N, the Iterator returns the item which correspond to the state which
;;;; would be obtained when generating N times further items; the inner state however is left unchanged. If
;;;; there are less than N further states left, an error of type `NO-NEXT-ITEM-ERROR' may be signalled.
;;;;
;;;; The interfaces for Iterators are given by the function `NEXT' for changing the inner state to the next
;;;; one an returning the corresponding item, the function `CURRENT' for returning the item corresponding
;;;; to the current state, the function `PEEK' for peeking N items ahead without changing the state, and the
;;;; two error conditions mentioned above.
;;;;
;;;;** Cursors
;;;;
;;;; A Cursor is like an Iterator but has the additional feature to allow for changing the internal state
;;;; back to a previous one. The Cursor is implemented as a function taking three optional arguments:
;;;; position absolute state.
;;;;
;;;; The argument position is an integer (possibly restricted to a FIXNUM) which indicates the position
;;;; of the state in the chain of possible internal states of the Cursor; this position is either
;;;; absolute beginning with zero as the state prior first call of NEXT, if the argument absolute is non nil,
;;;; or relative to the position of the actual state, if the argument absolute is nil (the default).
;;;; When called without argument, the Cursor changes the internal state to the next one (c.f. Iterators)
;;;; and return the corresponding item. When called with one or two arguments, the Cursor returns the
;;;; item corresponding to the state as indicated by the positional arguments (see above) without changing
;;;; the internal state except when called with the third argument non nil, in which case the internal
;;;; state is changed as well.

;;;;** Implementation
;;;;*** Interface
;;;;**** Error Conditions
(define-condition awl::no-next-item-error (error)
  ((generator :initarg :generator :reader generator))
  (:report (lambda (condition stream)
             (format stream "There is no next element for Generator ~S."
                     (generator condition))))
  (:documentation "Condition of supertype `error' which is signaled by the Generator when no next element can be generated."))
(define-condition awl::iterator-not-initialized-error (error)
  ((iterator :initarg :iterator))
  (:report (lambda (condition stream)
             (format stream "The Iterator ~S has not been initialized."
                     (slot-value condition 'iterator))))
  (:documentation "Condition of supertype `error' which is signaled by the Iterator when no current element is defined, i.e. the Iterator is not initialized."))
;;;;**** Functions
(declaim (inline awl::next awl::back awl::current awl::peek awl::goto-item))

(declaim (ftype (function ((function) &optional fixnum)) awl::next))
(defun awl::next (generator &optional (n 1 np))
  "Change the internal state of the generator to the next following state or the state n positions ahead and return the corresponding item. If none such exists an error of type NO-NEXT-ITEM-ERROR may be signalled or a eof value returned. The optional value n cannot be used with simple generators."
  (declare (optimize speed)
           (fixnum n)
           ((function) generator))
  (if np
      (funcall generator n nil t)
      (funcall generator)))
(define-compiler-macro awl::next (generator &optional (n 1 np))
  "Change the internal state of the generator to the next following state or the state n positions ahead and return the corresponding item. If none such exists an error of type NO-NEXT-ITEM-ERROR may be signalled or a eof value returned. The optional value n cannot be used with simple generators."
  (declare (optimize speed))
  (if np
      `(funcall ,generator ,n nil t)
      `(funcall ,generator)))
(declaim (ftype (function ((function) &optional fixnum)) awl::back))
(defun awl::back (cursor &optional (n 1))
  "Change the internal state of the cursor to the next previous state or the state n positions back and return the corresponding item. If none such exists an error of type ITERATOR-NOT-INITIALIZED-ERROR may be signalled or a default initial value returned."
  (declare (optimize speed)
           (fixnum n)
           ((function) cursor))
  (funcall cursor n nil t))
(defun awl::current (iterator)
  "Return the item corresponding to the current internal state of the iterator. If iterator has never been used before, an error of type ITERATOR-NOT-INITIALIZED-ERROR may be signalled or a default initial value returned."
  (declare (optimize speed)
           ((function) iterator))
  (funcall iterator 0))
(defun awl::peek (iterator &optional (n 1) (absolute nil absp))
  "Return the item which is N positions ahead if N is positive or back if N is negative in ITERATOR but without changing the state of ITERATOR. If none such item exists, an error of type NO-NEXT-ITEM-ERROR or of type ITERATOR-NOT-INITIALIZED-ERROR may be is signalled or a default initial value or a eof value returned."
  (declare (optimize speed)
           (fixnum n)
           ((function) iterator))
  (if absp
      (funcall iterator n nil absolute)
      (funcall iterator n)))
(define-compiler-macro awl::peek (iterator &optional (n 1) (absolute nil absp))
  (declare (optimize speed)
           (fixnum n))
  (if absp
      `(funcall ,iterator ,n nil ,absolute)
      `(funcall ,iterator ,n)))
(defun awl::goto-item (cursor n)
  "Change the internal state of the cursor to the state which is N positions ahead if N is positive or back if N is negative and return the corresponding item. If none such item exists, an error of type NO-NEXT-ITEM-ERROR or of type ITERATOR-NOT-INITIALIZED-ERROR may be is signalled or a default initial value or a eof value returned."
  (declare (optimize speed)
           ((function) cursor))
  (funcall cursor n t t))
;;;;*** Constructors
;;;;
;;;;**** Iterators
(defclass awl::iterator () () (:metaclass c2mop:funcallable-standard-class))
(defmethod initialize-instance :after ((self awl::iterator) &key function)
  (c2mop:set-funcallable-instance-function self function))
(declaim (ftype (function (function &optional *)) make-iterator-from-generator))
(defun make-iterator-from-generator (generator &optional (initial-value nil initial-value-p))
  "Wrap the GENERATOR into an iterator. A generator is a thunk which returns a value at each call. An iterator can be applied to the functions NEXT, CURRENT and PEEK."
  (declare (optimize speed))
  (let ((cache (when initial-value-p (list initial-value))))
    (labels ((peek (n c)
               (declare (fixnum n))
               (if (< 0 n)
                   (peek (1- n) (or (rest c)
                                    (let ((current (list (awl::next generator))))
                                      (setq cache (append cache current))
                                      current)))
                   (if c
                       (car c)
                       (error 'awl::iterator-not-initialized-error :iterator n))))
             (next ()
               (prog1 (peek 1 cache) 
                 (pop cache))))
      (declare (inline next))
      (lambda (&optional (n 0 peekp))
        (declare (optimize speed) (fixnum n))
        (if peekp (peek (the fixnum n) cache) (next))))))

(defmacro awl::build-iterator (initial-value &body body)
  "Return an iterator defined by BODY."
  (declare (optimize speed))
  `(awl::make-iterator-from-generator (lambda () ,@body) ,initial-value))

(defgeneric compute-iterator-factory (object))

(defmethod compute-iterator-factory (object)
  (compute-iterator-factory (awl::make-generator object)))

(defmethod compute-iterator-factory ((object function))
  "Wrap the GENERATOR into an iterator. A generator is a thunk which returns a value at each call. An iterator can be applied to the functions NEXT, CURRENT and PEEK."
  #'make-iterator-from-generator)

(defmethod compute-iterator-factory ((object awl::iterator))
  #'make-iterator-from-generator)

(defun awl::make-iterator (object &optional (initial-value nil initial-value-p))
  (let ((iterator-factory (unless (and (typep object 'awl::iterator)
                                       (not initial-value-p))
                            (compute-iterator-factory object))))
    (if iterator-factory
        (make-instance 'awl::iterator
                       :function (apply iterator-factory
                                        object
                                        (when initial-value-p initial-value)))
        object)))



;;;;**** Cursors
(defclass awl::cursor (awl::iterator) () (:metaclass c2mop:funcallable-standard-class))

(declaim (ftype (function ((function) &optional *)) make-cursor-from-generator))
(defun make-cursor-from-generator (generator &optional (initial-value nil initial-value-p))
  (let ((vector (make-array 0 :adjustable t :fill-pointer t))
        (state -1))
    (declare (type fixnum state))
    (symbol-macrolet ((m (the fixnum (1- (length vector)))))
      (lambda (&optional (n 1 np) absolutep (change-state (not np)))
        (declare (optimize speed)
                 (type fixnum n)
                 (type boolean absolutep change-state))
        (let ((i (if absolutep n (+ state n))))
          (declare (type fixnum i))
          (cond
            ((< i 0) (if initial-value-p
                         (progn (when change-state (setq state -1))
                                initial-value)
                         (error 'awl::iterator-not-initialized-error :iterator n)))
            ((< m i) (loop :while (< m i)
                        :do (vector-push-extend (awl::next generator) vector)))
            (t (when change-state (setq state i)) (aref vector i))))))))

(defun make-cursor-from-vector (vector &optional (initial-value nil initial-value-p))
  (declare (type vector vector))
  (symbol-macrolet ((m (the fixnum (1- (length vector)))))
    (let ((state -1))
      (declare (type fixnum state))
      (lambda (&optional (n 1 np) absolutep (change-state (not np)))
        (declare (optimize speed)
                 (type fixnum n))
        (let ((i (if absolutep n (+ state n))))
          (declare (type fixnum i))
          (cond
            ((< i 0) (if initial-value-p
                         (progn (when change-state (setq state -1))
                                initial-value)
                         (error 'awl::iterator-not-initialized-error :iterator n)))
            ((< m i) (error 'awl::no-next-item-error :generator n))
            (t (when change-state (setq state i)) (aref vector i))))))))


(defgeneric compute-cursor-factory (object))

(defmethod compute-cursor-factory (object)
  (compute-cursor-factory (awl::make-generator object)))

(defmethod compute-cursor-factory ((object function))
  #'make-cursor-from-generator)

(defmethod compute-cursor-factory ((object awl::iterator))
  #'make-cursor-from-generator)

(defmethod compute-cursor-factory ((object vector))
  #'make-cursor-from-vector)

(defun awl::make-cursor (object &optional (initial-value nil initial-value-p))
  (let ((cursor-factory (unless (and (typep object 'awl::cursor)
                                       (not initial-value-p))
                            (compute-cursor-factory object))))
    (if cursor-factory
        (make-instance 'awl::cursor
                       :function (apply cursor-factory
                                        object
                                        (when initial-value-p initial-value)))
        object)))

;;;; TODO

;;;;*** Some Utilities
(defun awl::generator-cartesian-product* (preorder-p key generators)
  "Return a generator producing lists of items generated by each iterator in ITERATORS in following way: when (c1 ... cn) is the current item and n1, n2, ... nn are the next items respectively of the iterators 1 to n the next item is (m1 .. mn) with mi = ni if ni is a minimal element of n1 ... nn with respect to the preorder relation defined by PREORDER-P and mi = ci else."
  (let ((iterators (mapcar #'make-iterator-from-generator generators)))
    (compile nil
             `(lambda ()
                (mapc ,(awl::f-comp #'awl::next #'cdr)
                      (awl::find-extrema (mapcar ,(compile nil
                                                           `(lambda (g)
                                                              (cons ,(if key
                                                                         `(funcall ,key (awl::peek g))
                                                                         '(awl::peek g)) g))) iterators)
                                         ,preorder-p :key #'car))
                (mapcar ,#'awl::current ',iterators)))
    (mapcar #'awl::current iterators)))

(defun awl::combine-generators* (preorder-p key generators)
  "Return a generator producing at each invocation of NEXT an item of GENERATORS which is minimal with respect ot the preorder relation PREORDER-P."
  (let* ((key (compile nil `(lambda (x) ,(if (functionp key) `(funcall ,key (car x)) '(car x)))))
         (queue (stable-sort (mapcan (lambda (gen)
                                       (handler-case (list (cons (awl::next gen) gen))
                                         (awl::no-next-item-error ())))
                                     generators)
                             preorder-p :key key)))
    (lambda ()
      (destructuring-bind (item . gen)
          (pop queue)
        (handler-case
            (setq queue (awl::nordered-insert (cons (awl::next gen) gen) queue preorder-p :key key))
          (awl::no-next-item-error ()))
        item))))

(defun awl::map-generators (type fn generator &rest generators)
  "Map the output of GENERATORS to the function FN and return a sequence of the type TYPE containing the results of the mapping. If TYPE is nil, nil is returned. The loop terminates as soon as one of the generators signals a `no-next-item-error'. It is therefore expected that (at least one of) the generators throws a `no-next-item-error' to terminate the loop. TYPE is one of nil, list, vector, string or generator."
  (symbol-macrolet ((next (apply fn (mapcar #'awl::next (cons generator generators)))))
    (case type
      ((nil) (ignore-errors (loop :do (progn next))))
      (string
       (with-output-to-string (s)
         (ignore-errors (loop (write next :stream s)))))
      ((vector list)
       (let (collection)
         (handler-case
             (loop (push next collection))
           (awl::no-next-item-error ()
             (case type
               (list (nreverse collection))
               (t (make-array (list (length collection))
                              :initial-contents (nreverse collection))))))))
      (t (lambda () next)))))



(defgeneric awl::make-generator (object &key eof &allow-other-keys)
  (:documentation "Return a generator taking OBJECT as base and returning EOF if given or throwing a `no-next-item-error' if EOF is not given. A generator is a (anonymous) function which when successively called returns a new object. The specializer OUTPUT-TYPE may be used to specify the output type by implementing an adequate conversion."))

(defun awl::make-generator-from-list (list &key (step 1) key (eof nil eofp))
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(null list))
	 (eof-signal (if eofp eof '(error 'awl::no-next-item-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next (if (< 1 step)
                   `(dotimes (i ,step) (pop list))
                   `(pop list))))
    (funcall (compile nil `(lambda ()
                             (declare (optimize speed))
                             (let ((list ',list))
                               (lambda ()
                                 ,(if test
                                      `(if ,test
                                           ,eof-signal
                                           (prog1 ,get
                                             ,next))
                                      `(prog1 ,get
                                         ,next)))))))))

(defun awl::make-generator-from-circle (list &key (step 1) key (eof nil eofp))
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(or (gethash aux seen-conses) (null list)))
	 (eof-signal (if eofp eof '(error 'awl::no-next-item-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next `(progn
                  ,(if (< 1 step)
                      `(dotimes (i ,step) (pop list))
                      `(pop list))
                  (setf (gethash aux seen-conses) t))))
    (funcall (compile nil `(lambda (list)
                             (declare (optimize speed))
                             (let ((seen-conses (make-hash-table :test #'eq
                                                                 #+sbcl :weakness
                                                                 #+(or ccl clisp) :weak
                                                                 #+(or sbcl ccl clisp) :key)))
                               (lambda ()
                                 (let ((aux list))
                                   ,(if test
                                        `(if ,test
                                             ,eof-signal
                                             (prog1 ,get
                                               ,next))
                                        `(prog1 ,get
                                           ,next)))))))
             list)))


(defun awl::make-number-generator (to &key from (step 1) (eof nil eofp))
  "Return a generator producing numbers from FROM below TO by steps STEP. If TO <= FROM and 0 < STEP or FROM <= TO and STEP < 0 or if TO is not a number the generator never terminates. If STEP is not a number it is set to 1. A STEP of 0 produces an infinite sequence of the constant FROM (Not really useful). If FROM is not a number it is set to 0. If the key EOF is used it is returned when reaching the terminating condition instead of throwing a `no-next-item-error'."
  (let* ((n (if (numberp from)
		from
		0))
         (step (if (numberp step)
		   step
		   1))
	 (to (when (and (numberp to)
			(or (< (- n to) 0 step)
			    (< step 0 (- n to))))
	       to))
	 (test (when to `(<= ,to n)))
	 (eof-signal (if eofp eof '(error 'awl::no-next-item-error)))
	 (get 'n)
	 (next `(incf n ,step)))
    (funcall (compile nil `(lambda ()
                             (declare (optimize speed))
                             (let ((n ,n))
                               (lambda ()
                                 ,(if test
                                      `(if ,test
                                           ,eof-signal
                                           (prog1
                                               ,get
                                             ,next))
                                      `(prog1 ,get
                                         ,next)))))))))

#+(or)
(awl:defmacro! awl::make-generator-factory ((&rest lambdalist) &body body &key o!generator-form
                                            o!end-test o!eof-signal)
  (declare (ignore body))
  `(lambda (,@lambdalist)
     (funcall (compile nil `(lambda (,g!generator-form ,g!end-test)
                              (lambda ()
                                ,(if o!end-test
                                     `(if ,g!end-test
                                          ,g!eof-signal
                                          ,g!generator-form)
                                     g!generator-form)))))))



(defmethod awl::make-generator ((n number) &rest keys &key eof (step 1) to
                                &allow-other-keys)
  (declare (ignore eof step to))
  (apply #'awl::make-number-generator n keys))
(defmethod awl::make-generator ((list list) &rest keys &key eof (step 1) key
                                &allow-other-keys)
  (declare (ignore eof step key))
  (apply #'awl::make-generator-from-circle list keys))
(defmethod awl::make-generator ((vector vector) &key &allow-other-keys)
  (ch.amann-wolowyk.awl-system::make-cursor-from-vector vector))



;;;;* String Utilities
;;;;** Chunk Matcher
(defun awl::make-chunk-mapper (delimiters &optional escape-char)
  "Return a function taking a function FCT in one character and optionally an input character stream defaulting to *standard-input*. The returned function maps at each call the function FCT to each character of the next chunk of the stream. A chunk is the sequence of character between two occurences of DELIMITERS which are not escaped by ESCAPE-CHAR. DELIMITERS and ESCAPE-CHAR can be either a predicate function, a single character or a list of characters. In the latter case a delimiter or an escape-character is any of that list. The behaviour is unspecified if a character may be at the same time a delimiter and an escape-character (in fact the escape character wins)."
  (flet ((select-check-form (item)
           (etypecase item
             (function `(funcall ,item (the character char)))
             (character `(char= (the character char) ,item))
             (sequence (assert (every #'characterp (the sequence item)))
                   `(find (the character char) ',item :test #'char=)))))
    (compile nil
             `(lambda (fct &optional (stream *standard-input*))
                (declare (optimize speed)
                         (type (function (character)) fct))
                (handler-case
                    (loop ,@(when escape-char `(:with escapedp := nil))
                       :for char character := (read-char stream)
                       :do (cond
                             ,@ (when escape-char 
                                  `((escapedp (setq escapedp nil)
                                              (funcall fct (the character char)))
                                    (,(select-check-form escape-char) (setq escapedp t))))
                                (,(select-check-form delimiters) (return char))
                                (t (funcall fct (the character char)))))
                  (end-of-file ()))))))

;;;; old version; to be eliminated
#+ (or)
(defun awl::make-chunk-mapper (delimiters &optional escape-char)
  "Return a function taking a function FCT in one character and optionally an input character stream defaulting to *standard-input*. The returned function maps at each call the function FCT to each character of the next chunk of the stream. A chunk is the sequence of character between two occurences of DELIMITERS which are not escaped by ESCAPE-CHAR. DELIMITERS and ESCAPE-CHAR can be either a predicate function, a single character or a list of characters. In the latter case a delimiter or an escape-character is any of that list. The behaviour is unspecified if a character may be at the same time a delimiter and an escape-character."
  (declare (optimize speed))
  (flet ((select-check-form (item)
           (etypecase item
             (function `(funcall ,item (the character char)))
             (character `(char= (the character char) ,item))
             (cons `(find (the character char) ',item :test #'char=)))))
    (let ((delimiter-check-form (list (list (select-check-form delimiters)
                                            :delimiter)))
          (escape-check-form (when escape-char
                               (list (list (select-check-form escape-char)
                                           '(setq escapedp t)
                                           :escape)))))
      (eval `(macrolet
                 ((check (char)
                    (declare (ignorable char))
                    `(if escapedp
                         (progn (setq escapedp nil)
                                :take-it)
                         ,',(append '(cond)
                                    delimiter-check-form
                                    escape-check-form
                                    '((t :take-it))))))
               (lambda (fct &optional (stream *standard-input*))
                 (declare (optimize speed))
                 (handler-case
                     (let (escapedp)
                       (loop :for char character := (read-char stream)
                          :do (case (check char)
                                (:take-it (funcall fct (the character char)))
                                (:delimiter (return char))
                                (:escape))))
                   (end-of-file ()))))))))


(awl::fbind ((map-chunk (awl::make-chunk-mapper #'upper-case-p)))
  (defun awl::camel-style-to-lisp (string)
    "Return a string obtained from input STRING by inserting in front of each uppercase character of STRING, except the first one, the character #\- and then changeing to upper case all letters of STRING."
    (with-output-to-string (*standard-output*)
      (with-input-from-string (*standard-input* string)
        (write-char (char-upcase (read-char)))
        (loop
           :for upcase-char := (map-chunk (awl::f-comp #'write-char #'char-upcase))
           :while upcase-char
           :do (format t "~A~A" #\- upcase-char))))))

;;;;** String Matcher
(defun awl::make-string-matcher (targets &optional escape-char from-end)
         "Return a function taking a string and returning as primary value the position in STRING of the first character of any of targets from the beginning (from the end if FROM-END is t) in the substring of STRING delimited by START and END and which is not preceeded by a single ESCAPE-CHAR. The secondary value is the position + target-length and the third value is the target which is at that position."
         (etypecase targets
           (atom (make-string-matcher targets escape-char from-end))
           (cons (let ((matchers (mapcar (lambda (target)
                                           (make-string-matcher target escape-char from-end))
                                         targets)))
                   (eval `(lambda (string &optional (start 0) end)
                            ,(format nil "Return the position of the first match from the ~:[beginning~;end~] of ~v[~;~:;one of ~]~{~S~#[~; or ~:;, ~]~} in STRING between START and END, unless escaped with the character ~S."
                                     from-end (length targets) targets escape-char)
                            (values-list (reduce  ,(eval `(lambda (a b)
                                                            (awl::fbind ((following-p ,(if from-end #'> #'<)))
                                                              (if a
                                                                  (if (and b (following-p (car b) (car a)))
                                                                      b
                                                                      a)
                                                                  b))))
                                                  ',matchers
                                                  :key (lambda (m)
                                                         (multiple-value-list (funcall m string start end)))))))))))



(defun make-string-matcher (target &optional escape-char from-end)
  (let* ((target-length (etypecase target
                          (character 1)
                          (string (length target))))
         (no-escape-char-? (if escape-char
                               `(or (= 0 pos)
                                    (char/= ,escape-char (aref string (1- pos))))
                               t))
         (full-match-? (if (< 1 target-length)
                           `(when (<= pos max-pos)
                              (string= ,target (subseq string pos pos+target-length)))
                           t)))
    (compile nil
             `(lambda (string &optional (start 0) end)
                ,(format nil "Return the position of the first match from the ~:[beginning~;end~] of ~S in STRING between START and END, unless escaped with the character ~S."
                         from-end target escape-char)
                (let* ,(list*
                        `(pos ,(if from-end 'end '(1- start)))
                        (when (< 1 target-length)
                          `((string-length (length string))
                            (max-pos (- string-length ,target-length)))))
                  (loop (setq pos (position ,(etypecase target
                                                        (character target)
                                                        (string (aref target 0)))
                                            string
                                            :from-end ,from-end
                                            :start ,(if from-end
                                                        'start
                                                        '(1+ pos))
                                            :end ,(if from-end
                                                      'pos
                                                      'end)))
                     (if pos
                         (let ((pos+target-length (+ pos ,target-length)))
                           (when (and ,no-escape-char-? ,full-match-?)
                             (return (values pos pos+target-length ,target))))
                         (return (values)))))))))



(defun awl::make-string-splitter-factory (targets &key escape-char max-chunks from-end eof-signal-p)
  (let ((no-next-element (when eof-signal-p (make-condition 'awl::no-next-element-condition
                                                            :generator (format nil "#<string-splitter ~S, ~S, ~S, ~S>"
                                                                               targets escape-char max-chunks from-end))))
        (matcher (awl::make-string-matcher targets escape-char from-end)))
    (compile nil
             `(lambda (string &optional (start 0) end)
                (let ((status ,(if max-chunks
                                   (cond
                                     ((< 1 max-chunks)
                                      :continue)
                                     ((= 1 max-chunks)
                                      :single-element)
                                     ((= 0 max-chunks)
                                      :empty))
                                   :continue))
                      (s start)
                      (e end)
                      ,@(when max-chunks `((remaining-chunks ,max-chunks))))
                  (setq start 0 end nil)
                  (lambda ()
                    (case status
                      ((:continue)
                       ,(when max-chunks `(when (= 1 (decf remaining-chunks))
                                            (setq status :last-element)))
                       (multiple-value-bind ,(if from-end
                                                 '(next-end start)
                                                 '(end next-start))
                           (funcall ,matcher string s e)
                         (prog1
                             (subseq string start end)
                           ,(if from-end
                                `(if next-end
                                     (setq end next-end
                                           e next-end)
                                     (setq status :empty))
                                `(if next-start
                                     (setq start next-start
                                           s next-start)
                                     (setq status :empty))))))
                      (:last-element
                       (setq status :empty)
                       (subseq string ,(if from-end 0 'start) ,(when from-end 'end)))
                      (:single-element
                       (subseq string 0))
                      (:empty ,(when eof-signal-p `(signal ,no-next-element))))))))))

(defun awl::make-string-splitter (targets &optional escape-char max-chunks from-end)
  (awl::fbind ((make-string-splitter (awl::make-string-splitter-factory targets
                                                                       :escape-char escape-char
                                                                       :max-chunks max-chunks
                                                                       :from-end from-end :eof-signal-p nil)))
    (lambda (string &optional (start 0) end)
      (awl::fbind ((next-item (make-string-splitter string start end)))
        (loop :for item = (next-item) :while item :collect item)))))

(defun awl::split-string (targets string &key escape-char max-chunks from-end (start 0) end)
  (funcall (awl::make-string-splitter targets escape-char max-chunks from-end) string start end))



(defun awl::make-string-replacer (replacement targets &optional escape-char max-operations from-end)
  (awl::fbind ((make-string-splitter (awl::make-string-splitter-factory targets
                                                                       :escape-char escape-char
                                                                       :max-chunks max-operations
                                                                       :from-end from-end :eof-signal-p nil)))
    (lambda (string &optional (start 0) end)
      (awl::fbind ((next-item (make-string-splitter string start end)))
        (with-output-to-string (stream)
          (let ((first-item (next-item)))
            (when first-item
              (princ first-item stream)))
          (loop :for item := (next-item) :while item
             :do (princ replacement stream)
             :do (princ item stream)))))))

(defun awl::string-replace (replacement targets string &key escape-char max-operations from-end (start 0) end)
  (funcall (awl::make-string-replacer replacement targets escape-char max-operations from-end) string start end))

(defun awl::join (separator sequence)
  (awl::fbind ((next (awl::make-generator sequence)))
    (with-output-to-string (stream)
      (handler-case
          (do ((action (princ (next) stream) (princ (prog1 (next)
                                                      (princ separator stream))
                                                    stream)))
              (nil))
        (awl::no-next-item-error ())))))

;;;;* Reading Files

(defun awl::slurp-file (pathname)
  "Read the file at PATHNAME into a single string and return this string."
  (with-open-file (stream pathname)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))




;;;;* Functions

(defmacro awl::dlambda (destr-lambda-list &body body)
  "Return a function using a destructuring lambda-list."
  `(lambda (&rest args)
     (destructuring-bind ,destr-lambda-list args ,@body)))

(defun awl::f-or* (fns)
  "Return a function F s.t. F(args)= (or F1(args) .. Fn(args))."
  (lambda (&rest args)
    (some (lambda (fn)
            (apply fn args))
          fns)))

(defun awl::f-or (&rest fns)
  (awl::f-or* fns))
(define-compiler-macro awl::f-or (&rest fns)
  `(lambda (&rest args)
     (or ,@(mapcar (lambda (fn)
                     `(apply ,fn args))
                   fns))))
(setf (documentation 'awl::f-or 'function) (documentation 'awl::f-or* 'function))


(defun awl::f-and* (fns)
  "Return a function F s.t. F(args)= (and F1(args) .. Fn(args))."
  (lambda (&rest args)
    (every (lambda (fn)
             (apply fn args))
           fns)))
(defun awl::f-and (&rest fns)
  (awl::f-and* fns))
(define-compiler-macro awl::f-and (&rest fns)
  `(lambda (&rest args)
     (and ,@(mapcar (lambda (fn)
                     `(apply ,fn args))
                   fns))))
(setf (documentation 'awl::f-and 'function) (documentation 'awl::f-and* 'function))

(defun awl::f-comp* (fns &aux (fns (nreverse fns)))
  "Return a function which is obtained by funcalling each function among FNS successively from the right to the left to the result of the previous function resp. the input args for the first function. When FNS is NIL, return a function which returns always nil."
  (compile nil `(lambda (&rest args)
                  ,(reduce (lambda (r fn)
                             `(multiple-value-call ,(etypecase fn
                                                       (function fn)
                                                       (symbol (list 'quote fn))) ,r))
                           (rest fns)
                           :initial-value `(apply ,(let ((fn (first fns)))
                                                     (etypecase fn
                                                       (function fn)
                                                       (symbol (list 'quote fn)))) args)))))

(defun awl::f-comp (&rest fns)
  (awl::f-comp* fns))
(setf (documentation 'awl::f-comp 'function) (documentation 'awl::f-comp* 'function))

;;;;** Partially Evaluated Functions
;;;;
;;;; 
(defconstant awl::$ 'awl::$
  "Undefined value. Should exclusively be used in AWL:PARTIAL as indicator for an unbound positional function argument.")
(defun awl::partial* (fn bound-args)
  "Return a function whose arglist is the arglist of the function FN from which the args bound in BOUND-ARGS are removed. Unbound positional arguments can be indicated by the constant awl:$. Example: if the function FOO takes the argument (a b c &key d e) then the invocation of (PARTIAL 'FOO 0 awl:$ 2 :e 4) returns a function equivalent to (lambda (b &rest rest &key d) (apply 'foo 0 b 2 rest))."
  (let* ((arglist (awl::function-arglist fn))
         (positional-args (awl::positional-args arglist))
         (rest-arg (make-symbol "REST"))
         (bound-key-vals (last bound-args (max 0 (- (length bound-args) (length positional-args)))))
         (bound-keys (awl::map-plist (lambda (k v) (declare (ignore v)) k) bound-key-vals))
         (unbound-keys (delete-if (lambda (k) (member k bound-keys)) (awl::key-args arglist)))
         (unbound-key-vars (mapcar (awl::f-comp #'make-symbol #'string) unbound-keys))
         (lambda-list (append (loop :with bound-args := bound-args
                                 :for arg :in positional-args
                                 :for value := (pop bound-args)
                                 :for count :downfrom (length bound-args)
                                 :when (and (< 0 count) (eq awl::$ value))
                                 :collect arg)
                              (list '&rest rest-arg)
                              (when unbound-keys
                                (append '(&key) (mapcar (awl::f-comp #'list #'list)
                                                        unbound-keys unbound-key-vars)
                                        (member '&allow-other-keys arglist)))))
         (call-list (append (loop :with bound-args := bound-args
                               :for arg :in positional-args
                               :for value := (pop bound-args)
                               :for count :downfrom (length bound-args)
                               :collect (if (and (< 0 count) (eq awl::$ value))
                                            arg value))
                            bound-key-vals
                            (list rest-arg))))
    (compile nil `(lambda ,lambda-list
                    (declare (ignore ,@unbound-key-vars)
                             (optimize speed))
                    (apply ,fn ,@call-list)))))

(declaim (inline awl::partial))
(defun awl::partial (fn &rest bound-args)
  (awl::partial* fn bound-args))
(setf (documentation 'awl::partial 'function ) (documentation 'awl::partial* 'function))

(defun awl::make-lambda-list-from-arglist (arglist)
  (multiple-value-bind (reqvars optvars restp restvar keyp keys allowp auxvars)
      (awl::parse-lambda-list arglist)
    (append reqvars
            (when optvars (list* '&optional optvars))
            (when restp (list '&rest restvar))
            (when keyp (list* '&key (mapcar (lambda (key)
                                              (typecase key
                                                (keyword (awl::symb key))
                                                (t key)))
                                            keys)))
            (when allowp (list '&allow-other-keys))
            (when auxvars (list* '&aux auxvars)))))

;;;;** Lifting Functions
(defun awl::lift (fn lift arg-spec)
  "Return a function which corresponds to the function FN whose arguments are lifted by the key functions indicated in ARG-SPEC and whose result is applied to the function indicated by the function or symbol LIFT. ARG-SPEC is either a function or a symbol indicating a function, in which case all required arguments are lifted by this function or a list which specifies those args of FN which are lifted by giving a function or the name of a function. This specification has a for remembering an ordinary lambda list; however the keyword &optional is ignored and &aux is not applicable. Example: if the function FOO takes the argument (a b c &optional d e &key f g) then the invocation of (LIFT 'FOO nil `(1+ nil ,#'- / &rest 1- &key (g *))) returns a function which applies #'1+ to the first, nothing to the second, #'- to the third, #'/ to the fourth (if given) and #'1- to all remaining arguments except the key argument :g which is applied to #'*."
  (let ((arglist (awl::function-arglist fn)))
    (multiple-value-bind (reqvars optvars restp restvar keyp)
        (awl::parse-lambda-list arglist)
      (declare (ignore restvar))
      (let ((arglift-spec (typecase arg-spec
                            ((or symbol function)(list '&rest arg-spec))
                            (t arg-spec))))
        (multiple-value-bind (reqlifts optlifts restp2 restlift keyp2 keylifts)
            (awl::parse-lambda-list arglift-spec)
          (declare (ignore restp2))
          (let* ((reqlifts (append reqlifts optlifts))
                 (lifted-reqvars (mapcar (lambda (var &aux (lift (if reqlifts (pop reqlifts) restlift)))
                                           (if lift
                                               `(funcall ',lift ,var)
                                               var))
                                         reqvars))
                 (optlifts (let (optlifts)
                             (dolist (lift (let (optlifts)
                                             (do ((list optvars (rest (the list list))))
                                                 ((null list) optlifts)
                                               (push (if reqlifts (pop reqlifts) restlift) optlifts)))
                                      optlifts)
                               (unless (and (null lift)
                                            (null optlifts))
                                 (push lift optlifts)))))
                 (has-rest-p (or optvars restp keyp)))
            (let ((lift-rest (compile nil
                                      `(lambda (rest)
                                         ,(if keyp
                                              (awl::fbind ((keylift (if keyp2
                                                                       (let ((hash (make-hash-table)))
                                                                         (loop :for (key lift) :in keylifts
                                                                            :do (setf (gethash (typecase key
                                                                                                 (list (first key))
                                                                                                 (t (intern (string key) :keyword)))
                                                                                               hash)
                                                                                      lift))
                                                                         (lambda (key) (gethash key hash restlift)))
                                                                       restlift)))
                                                `(awl::mapcan-plist ,#'(lambda (key val)
                                                                         (list key
                                                                               (let ((lift (keylift key)))
                                                                                 (if lift
                                                                                     (funcall lift val)
                                                                                     val))))
                                                                    rest))
                                              (if restlift
                                                  `(mapcar ,#'(lambda (val)
                                                                (funcall restlift val))
                                                           rest)
                                                  'rest))))))
              (compile nil
                       `(lambda (,@reqvars ,@(when has-rest-p `(&rest rest)))
                          (let ((result (apply ',fn ,@lifted-reqvars
                                               , (when  has-rest-p
                                                   (if optlifts
                                                       `(append (mapcan (lambda (lift)
                                                                          (when rest (list (funcall lift (pop rest)))))
                                                                        ',optlifts)
                                                                (funcall ,lift-rest rest))
                                                       `(funcall ,lift-rest rest))))))
                            ,(if lift
                                 `(funcall ',lift result)
                                 'result)))))))))))


(defun awl::n-tuple-fn (fct &rest fcts)
  "Take functions which are expected to have the same arglist and return a function which takes this arglist of arguments and returns as values lists of the values of the individual input functions as follows: If the functions f_0 .. f_n return the values v_i_0 .. v_i_mi for each i from 0 to n, the form \(apply \(n-tuple-fn f_0 .. f_n) args) returns the values \(v_0_0 .. v_n_0) .. \(v_0_(min m0 .. mn) .. v_n_(min m0 .. mn))."
  (if (null fcts)
      fct
      (lambda (&rest args)
        (apply #'values
               (apply #'mapcar
                      #'list
                      (mapcar (lambda (fn)
                                (multiple-value-list (apply fn args)))
                              (cons fct
                                    fcts)))))))
;;;;* MOP-specific Stuff
;;;; Uses closer-mop
(defmacro awl::with-slot-definition-locations ((&rest slots) class-name &body body)
  ""
  (awl::with-gensyms (class slot-defs)
    `(let ((,class (find-class ',class-name)))
       (unless (c2mop:class-finalized-p ,class)
         (c2mop:finalize-inheritance ,class))
       (let* ((,slot-defs (c2mop:class-slots ,class))
              ,@(mapcar (lambda (name)
                          (destructuring-bind (var-name slot-name)
                              (etypecase name
                                (symbol (list name name))
                                ((cons symbol (cons symbol null)) name))
                            `(,var-name (c2mop:slot-definition-location
                                         (find ',slot-name ,slot-defs
                                               :key #'c2mop:slot-definition-name)))))
                        slots))
         ,@body))))

(defun awl::get-slot-values (instance &optional (unbound-slot :unbound-slot))
  "Return a list containing the values of all slots of INSTANCE sorted by slot-definition-location of the instance's class. If a slot is unbound UNBOUND-SLOT, which defaults to the keyword :unbound-slot, is returned instead."
  (mapcar (lambda (slot)
            (handler-case
                (slot-value instance (c2mop:slot-definition-name slot))
              (unbound-slot ()
                unbound-slot)))
          (sort (c2mop:class-slots (class-of instance)) #'<
                :key #'c2mop:slot-definition-location)))
(defun awl::identical (o1 o2 &key (test #'eql))
  "Return T if O1 and O2 are of the same type and have same slots according to TEST; NIL else."
  (when (eq (class-of o1)
            (class-of o2))
    (loop
       :with equalp := t
       :for slot :in (c2mop:class-slots (class-of o1))
       :while equalp
       :do (setq equalp
                 (handler-case
                     (funcall test
                              (slot-value o1 (c2mop:slot-definition-name slot))
                              (slot-value o2 (c2mop:slot-definition-name slot)))
                   (unbound-slot (e)
                     (let ((slot-name (cell-error-name e)))
                       (not (or (slot-boundp o1 slot-name)
                                (slot-boundp o2 slot-name)))))))
       :finally (return equalp))))




;;;;* Hash
(defparameter awl::*to-keyword-hook* nil
  "Hook of functions used by TO-KEYWORD. Each function will be given the out put of the previous function, whereas the first receives a string. The last function must return a string-designator.")
(defun awl::to-keyword (string-designator)
  "Intern STRING-DESIGNATOR into the keyword package after having applied the functions in *to-keyword-hook* to (string STRING-DESIGNATOR). If STRING-DESIGNATOR is already in the keyword package STRING-DESIGNATOR is returned unchanged."
  (etypecase string-designator
    (keyword string-designator)
    ((or symbol string)
     (intern (reduce #'(lambda (res fn)
                         (funcall fn res))
                     awl::*to-keyword-hook*
                     :initial-value (string string-designator))
             'keyword))))

;;;;** Multi Hash-Table

(defun awl::get-hash* (hash keys &optional default)
  "Return the object in HASH whose keys is the same as keys under the hash's equivalence test and T as secondary value if such object exists. Return nil, nil else."
  (if default (multiple-value-bind (value not-empty-p followed-path remaining-path)
                  (funcall hash :get keys nil)
                (values (if not-empty-p value default) not-empty-p followed-path remaining-path))
      (funcall hash :get keys nil)))
(define-compiler-macro awl::get-hash* (hash keys &optional (default nil defaultp))
  (if defaultp
      `(multiple-value-bind (value not-empty-p followed-path remaining-path)
           (funcall ,hash :get ,keys nil)
        (values (if not-empty-p value ,default) not-empty-p followed-path remaining-path))
      `(funcall ,hash :get ,keys nil)))
(defun awl::get-hash (hash &rest keys)
  "Return the object in HASH whose keys is the same as keys under the hash's equivalence test and T as secondary value if such object exists. Return nil, nil else."
  (awl::get-hash* hash keys))
(defsetf awl::get-hash* (hash keys &optional default) (value)
  "Set the value of the hash to the given value."
  (declare (ignorable default))
  `(funcall ,hash :set ,keys ,value))
(defsetf awl::get-hash (hash &rest keys) (value)
  "Set the value of the hash to the given value."
  `(funcall ,hash :set ,keys ,value))
(defun awl::rem-hash* (hash keys)
  "Remove the entry for KEYS in HASH, if any. Returns true if there was such an entry, or false otherwise."
  (funcall hash :rem keys nil))
(defun awl::rem-hash (hash &rest keys)
  "Remove the entry for KEYS in HASH, if any. Returns true if there was such an entry, or false otherwise."
  (awl::rem-hash* hash keys))
(defun awl::clr-hash (hash)
  "Remove all entries in HASH, if any, and returns the empty HASH."
  (funcall hash :clr nil nil)
  hash)
(defun awl::hash-count (hash)
  "Return the number of entries in HASH. If HASH has just been created or newly cleared (see clr-hash) the entry count is 0."
  (funcall hash :cnt nil nil))
(defun awl::map-hash (fct hash)
  "Map HASH to the function FCT in depth first manner. FCT takes two arguments: the first is the key the second must be optional and takes the value. blabla."
  (funcall hash :map fct nil))



(let ((empty '#:nil))
  (labels
      ((follow-path (nodes path followed-path)
         (let ((node (first nodes)))
           (if node
               (handler-case
                   (destructuring-bind (step &rest remaining-path) path
                     (follow-path (cons (gethash step (cdr node)) nodes) remaining-path (cons step followed-path)))
                 (error ()
                   (values nodes followed-path path)))
               (error "No node."))))
       (map-children (fct node)
         (let ((hash (cdr node)))
           (when hash
             (maphash fct (cdr node)))))
       (map-tree (fct node)
         (unless (eq empty (car node))
           (funcall fct nil (car node)))
         (map-children (lambda (s n)
                         (funcall fct s)
                         (map-tree fct n))
                       node)))
    (defun make-hash-tree (options)
      (let ((root (cons empty nil)))
        (lambda (action path value)
          (ecase action
            (:get (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (let* ((value (car (first nodes)))
                           (emptyp (or (eq empty value) remaining-path)))
                      (values (unless emptyp value) (not emptyp) followed-path remaining-path))))
            (:set (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (declare (ignore followed-path))
                    (let ((node (first nodes)))
                      (dolist (step remaining-path)
                        (unless (cdr node)
                          (setf (cdr node) (apply #'make-hash-table options)))
                        (let ((hash (cdr node)))
                          (setf (gethash step hash) (cons empty nil))
                          (setq node (gethash step hash))))
                      (setf (car node) value))))
            (:rem (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (unless remaining-path
                      (setf (car (first nodes)) empty)
                      (loop :for (node . prev-nodes) :on nodes
                         :for step :in followed-path
                         :do (if (and (eq empty (car node))
                                      (null (cdr node)))
                                 (remhash step (cdr (first prev-nodes)))
                                 (return)))
                      t)))
            (:clr (setq root (cons empty nil)))
            (:cnt (let ((counter 0))
                    (labels ((count-non-empty (node)
                               (unless (eq empty (car node))
                                 (incf counter))
                               (map-children (lambda (s n)
                                               (declare (ignore s))
                                               (count-non-empty n))
                                             node)))
                      (count-non-empty root))
                    counter))
            (:map (progn
                    (map-tree path root)
                    nil))))))))



(defclass awl::hash-tree (c2mop:funcallable-standard-object) ()
  (:metaclass c2mop:funcallable-standard-class))
(let ((hash-tree-class (find-class 'awl::hash-tree)))
  (unless (c2mop:class-finalized-p hash-tree-class)
    (c2mop:finalize-inheritance hash-tree-class)))
(defmethod initialize-instance :after ((self awl::hash-tree) &key options)
  (c2mop:set-funcallable-instance-function self (make-hash-tree options)))
(defun awl::make-hash-tree (&rest options &key test size rehash-size
                            rehash-threshold #+(or ccl clisp sbcl) weak &allow-other-keys)
  (declare (ignore test size rehash-size rehash-threshold #+(or ccl clisp sbcl) weak))
  #+sbcl (progn
           (setf (getf options :weakness) (getf options :weak))
           (remf options :weak))
  (make-instance 'awl::hash-tree :options options ))



;;;; Depreciated stuff -- should be eliminated.
(defgeneric make-multi-hash-getter-setter (class options))

(let ((empty '#:nil))
  (defun get-multi-hash (hash-table key other-keys)
    (handler-case
	(if other-keys
	    (get-multi-hash (cdr (gethash key hash-table))
                            (first other-keys) (rest other-keys))
            (let ((hash (gethash key hash-table)))
              (if hash
                  (let ((value (car hash)))
                    (if (eq empty value)
                        (values nil nil :empty-inner-node)
                        (values value t)))
                  (values nil nil :empty-leaf))))
      (type-error ()
	(values nil nil :error))))
  (labels ((new-hash (key hash-table hash)
             (etypecase hash
               ((cons t null) hash)
               (null (let ((hash (cons empty nil)))
                       (setf (gethash key hash-table) hash)
                       hash)))))
    (defun set-multi-hash (hash-table options value key other-keys)
      (let ((hash (gethash key hash-table)))
        (if other-keys
            (set-multi-hash (cdr (typecase hash
                                   ((cons t hash-table) hash)
                                   (t (rplacd (new-hash key hash-table hash)
                                              (apply #'make-hash-table options)))))
                            options value (first other-keys) (rest other-keys))
            (rplaca (typecase hash
                      ((cons t hash-table) hash)
                      (t (new-hash key hash-table hash)))
                    value))))))


(defclass awl::multi-hash-table ()
  (get-fn set-fn rem-fn count-fn))
;;;; SBCL (and CMUCL?) finalize their classes late in order to allow forward referenced superclasses
(let ((multi-hash-table-class (find-class 'awl::multi-hash-table)))
  (unless (c2mop:class-finalized-p multi-hash-table-class)
    (c2mop:finalize-inheritance multi-hash-table-class)))
(defmethod make-multi-hash-getter-setter ((type awl::multi-hash-table) options)
  (let ((hash-table (apply #'make-hash-table options)))
    (values (lambda (key other-keys)
              (get-multi-hash hash-table key other-keys))
            (lambda (value key other-keys)
              (set-multi-hash hash-table options value key other-keys)))))
(defmethod initialize-instance :after ((self awl::multi-hash-table) &rest initargs
                                       &key options &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (get-fn set-fn rem-fn count-fn)
      (make-multi-hash-getter-setter self options)
   (setf (slot-value self 'get-fn) get-fn
         (slot-value self 'set-fn) set-fn
         (slot-value self 'rem-fn) rem-fn
         (slot-value self 'count-fn) count-fn)))

(defun awl::make-multi-hash-table (&rest options &key &allow-other-keys)
  (make-instance 'awl::multi-hash-table :options options))

(awl::with-slot-definition-locations (get-fn set-fn rem-fn count-fn)
    awl::multi-hash-table
  (defun awl::get-multi-hash (hash-table key &rest other-keys)
    "Return the value of the multi hash corresponding to KEY and OTHER-KEYS."
    (funcall (c2mop:standard-instance-access hash-table get-fn) key other-keys))
  (defun awl::get-multi-hash* (hash-table keys)
    "Return the value of the multi hash corresponding to KEY and OTHER-KEYS."
    (funcall (c2mop:standard-instance-access hash-table get-fn) (first keys) (rest keys)))
  (defsetf awl::get-multi-hash (hash-table key &rest other-keys) (value)
    "Set the value of the multi hash to the given value."
    `(progn
       (funcall (c2mop:standard-instance-access ,hash-table ,set-fn)
                ,value ,key ,other-keys)
       ,value))
  (defsetf awl::get-multi-hash* (hash-table keys) (value)
    "Set the value of the multi hash to the given value."
    `(destructuring-bind (key &rest other-keys) ,keys
       (funcall (c2mop:standard-instance-access ,hash-table ,set-fn)
                ,value key other-keys)
       ,value))
  (defun awl::rem-multi-hash (hash-table key &rest other-keys)
    "Remove KEYS from HASH-TABLE."
    (funcall (c2mop:standard-instance-access hash-table rem-fn) key other-keys))
  (defun awl::rem-multi-hash* (hash-table keys)
    "Remove KEYS from HASH-TABLE."
    (funcall (c2mop:standard-instance-access hash-table rem-fn) (first keys) (rest keys)))
  (defun awl::multi-hash-count (hash-table)
    "Return the number of entries in HASH-TABLE."
    (funcall (c2mop:standard-instance-access hash-table count-fn))))

;;;;** Fixed Multi Hash Tables
  
(defun get-fixed-multi-hash (hash-table keys)
  (handler-case
      (loop with foundp
         for key in keys
         do (multiple-value-setq (hash-table foundp)
              (gethash key hash-table))
         finally (return (values hash-table foundp)))
    (type-error ()
      (values nil nil))))
(defun set-fixed-multi-hash (hash-table options value key other-keys)
  (if other-keys
      (set-fixed-multi-hash (or (gethash key hash-table)
                                (setf (gethash key hash-table)
                                      (apply #'make-hash-table (first options))))
                            (rest options) value (first other-keys) (rest other-keys))
      (setf (gethash key hash-table) value)))
(defun rem-fixed-multi-hash (hash-table keys)
  (handler-case
      (loop :with continue-p := t :and removed-p := nil
         :for (key hash) :in (nreverse
                              (loop :with hash := hash-table
                                 :for key :in keys
                                 :collect (list key
                                                (prog1 hash
                                                  (setq hash (gethash key hash))))))
         :while continue-p
         :do (progn
               (setq removed-p (remhash key hash))
               (setq continue-p (zerop (hash-table-count hash))))
         :finally (return removed-p))
    (type-error ())))
(defun clr-fixed-multi-hash (hash-table)
  (clrhash hash-table))

(defclass awl::fixed-range-multi-hash-table (awl::multi-hash-table) ())
(labels ((count-values (node sum)
           (maphash (lambda (k v)
                      (declare (ignore k))
                      (if (hash-table-p v)
                          (setq sum (count-values v sum))
                          (incf sum)))
                    node)
           sum))
  (defmethod make-multi-hash-getter-setter ((type awl::fixed-range-multi-hash-table)
                                            options)
    (let ((hash-table (apply #'make-hash-table (first options)))
          (nbr-keys (length options)))
      (values
       (lambda (key other-keys)
         (if (= nbr-keys (1+ (length other-keys)))
             (get-fixed-multi-hash hash-table (cons key other-keys))
             (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                    (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
       (lambda (value key other-keys)
         (if (= nbr-keys (1+ (length other-keys)))
             (set-fixed-multi-hash hash-table (rest options) value
                                   key other-keys)
             (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                    (< (1+ (length other-keys)) nbr-keys) (cons key other-keys)
                    nbr-keys)))
       (lambda (key other-keys)
         (if (= nbr-keys (1+ (length other-keys)))
             (rem-fixed-multi-hash hash-table (cons key other-keys))
             (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                    (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
       (lambda ()
         (count-values hash-table 0))))))

(defun awl::make-fixed-multi-hash-table (&rest options)
  (funcall #'make-instance 'awl::fixed-range-multi-hash-table :options options))


;;;;* Queues
(defun awl::make-queue (&key (synchronized t) &aux (synchronized (when synchronized t)))
  "Return a queue which is synchronized by default unless the key argument :SYNCHRONIZED is set to nil. It provides a function in one variable which is one of the symbols :enqueue :dequeue :length :peeker and :dump and which returns the corresponding method."
  (let (queue
        (lock (bt:make-lock "QUEUE"))
        (length 0))
    (declare (integer length))
    (compile nil `(lambda (method)
                    (ecase method
                      (:enqueue ,(if synchronized
                                     (lambda (value)
                                       "Enqueue value to the queue."
                                       (bt:with-lock-held (lock)
                                         (let ((elt (cons value (cdr queue))))
                                           (if queue
                                               (setf (cdr queue) elt)
                                               (setf (cdr elt) elt))
                                           (setf queue elt)
                                           (the integer (incf length)))))
                                     (lambda (value)
                                       "Enqueue value to the queue."
                                       (let ((elt (cons value (cdr queue))))
                                         (if queue
                                             (setf (cdr queue) elt)
                                             (setf (cdr elt) elt))
                                         (setf queue elt)
                                         (the integer (incf length))))))
                      (:dequeue ,(if synchronized
                                     (lambda ()
                                       "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
                                       (bt:with-lock-held (lock)
                                         (when queue
                                           (decf length)
                                           (values (if (eq queue (cdr queue))
                                                       (prog1 (car queue)
                                                         (setf queue nil))
                                                       (pop (cdr queue)))
                                                   t))))
                                     (lambda ()
                                       "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
                                       (when queue
                                           (decf length)
                                           (values (if (eq queue (cdr queue))
                                                       (prog1 (car queue)
                                                         (setf queue nil))
                                                       (pop (cdr queue)))
                                                   t)))))
                      (:length ,#'(lambda ()
                                    "Return the length of the queue."
                                    length))
                      (:peeker (let* ((eof "eof")
                                      (generator (awl::make-generator-from-circle (funcall ,(lambda () (cdr queue))) :eof eof)))
                                 (lambda (&aux (item (funcall generator)))
                                   "Iterate through the queue without changing its state. This function is not synchronized."
                                   (unless (eq eof item)
                                     (values item t)))))
                      (:dump ,(if synchronized
                                  (lambda ()
                                    "Empty the queue."
                                    (bt:with-lock-held (lock)
                                      (setq queue nil)))
                                  (lambda ()
                                    "Empty the queue."
                                    (setq queue nil))))
                      (:synchronizedp ,(lambda () synchronized)))))))
(declaim (inline awl::get-queue-method awl::enqueue awl::dequeue awl::queue-length awl::queue-dump awl::queue-synchronizedp awl::get-iterator))
(defun awl::get-queue-method (queue method)
  "Return the method METHOD of queue QUEUE."
  (declare ((function (symbol) (function)) queue)
           (optimize speed))
  (funcall queue method))
(defun awl::enqueue (queue value)
  "Enqueue value to the queue."
  (declare ((function (symbol) (function (*) integer)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :enqueue) value))
(defun awl::dequeue (queue)
  "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
  (declare ((function (symbol) (function () *)) queue)
           (optimize speed))
  (funcall (the (function ()) (awl::get-queue-method queue :dequeue))))
(defun awl::queue-length (queue)
  "Return the length of the queue."
  (declare ((function (symbol) (function () integer)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :length)))
(defun awl::queue-dump (queue)
  "Empty the queue."
  (declare ((function (symbol) (function () null)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :dump)))
(defun awl::queue-synchronizedp (queue)
  "Return T if the queue is synchronized and NIL else."
  (declare ((function (symbol) (function () null)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :synchronizedp)))
(defun awl::queue-iterator (queue)
  "Iterate through the queue without changing its state. This function is not synchronized."
  (declare ((function (symbol) (function () integer)) queue)
           (optimize speed))
  (awl::get-queue-method queue :peeker))


;;;;* Cached Structures
(defmacro awl::define-cached-struct (name &rest slots)
  "Like CL:DEFSTRUCT but defines the constructors in such a way that when called twice with same args it returns the same instance. The args are hereby tested by the equality test given with the option :test (possible values EQ EQL EQUAL EQUALP). Copier function definitions are not allowed, since it would be contrary to the principle of caching instances."
  (assert (and (not (null name))
               (or (symbolp name)
                   (listp name))))
  (assert (or (symbolp name)
              (notany (lambda (x) (or (eq :copier x)
                                      (and (consp x) (eq :copier (car x)))))
                      (cdr name))))
  (let* ((slots (mapcar (lambda (slot)
                          (setq slot (etypecase slot
                                       (symbol (list slot nil))
                                       (cons slot)))
                          (setf (getf (cddr slot) :read-only) t)
                          slot)
                        slots))
         (constructors (mapcar (lambda (constructor)
                                 (when (symbolp constructor)
                                   (setq constructor (list constructor)))
                                 (unless (second constructor)
                                   (setf (cdr constructor) (list (intern (format nil "MAKE-~A" name)))))
                                 (unless (third constructor)
                                   (setf (cddr constructor) (list (list* '&key (mapcar #'car slots)))))
                                 constructor)
                               (if (listp name)
                                   (remove-if (lambda (option)
                                                (etypecase option
                                                  (symbol (not (eq :constructor option)))
                                                  (cons (destructuring-bind (option-type &rest option-spec)
                                                            option
                                                          (or (not (eq :constructor option-type))
                                                              (and option-spec
                                                                   (null (car option-spec))))))))
                                              (cdr name))
                                   (list (list :constructor)))))
         (structure-name (typecase name
                           (symbol name)
                           (list (car name)))))
    (multiple-value-bind (name test)
        (typecase name
          (symbol (list structure-name constructors))
          (t (cons (car name)
                   (awl::remove-if (lambda (x) (eq (car x) :test))
                                   (cdr name)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,name
           ,@(mapcar (lambda (slot)
                       (when (symbolp slot)
                         (setq slot (list slot nil)))
                       (setf (getf (cddr slot) :read-only) t)
                       slot)
                     slots))
         (let ((cache (awl::make-hash-tree ,@(car test))))
           (let (,@(mapcar (lambda (constructor)
                             (let ((constructor-name (second constructor)))
                               `(,constructor-name (symbol-function ',constructor-name))))
                           constructors))
             ,@(mapcar (lambda (constructor)
                         `(symbol-macrolet (,@(mapcar (lambda (slot)
                                                        `(,(first slot) ',(second slot)))
                                                      slots))
                            (defun ,@(cdr constructor)
                                (or (awl::get-multi-hash cache ,@(mapcar #'car slots))
                                 (setf (awl::get-multi-hash cache ,@(mapcar #'car slots)) (apply ,@(cdr constructor)))))))
                       constructors)))
         ',name))))

;;;;* I/O

(defun awl::readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))
(defun awl::prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
(defun awl::break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
     (let ((in (apply #'awl::prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~A~%" (funcall fn in))))))


;;;;** Controlling declarations
(defvar awl::*typespec-check-hook* nil
  "List of functions taking as single argument an expression and returning either NIL or a valid typespec.")

(defvar awl::*default-typespec* 'number)

(defun awl::get-effective-typespec (typespec)
  "Take an \"extended\" typespec, i.e. a valid typespec or an expression which can be transformed to a valid typespec by one of the functions in awl::*typespec-check-hook*, and return a valid typespec. If the expression resulting by applying awl::*typespec-check-hook* is not a valid typespec, an error is signalled."
  (if (awl::type-spec-p typespec)
      typespec
      (let ((typespec (when (boundp 'awl::*typespec-check-hook*)
                        (dolist (fn awl::*typespec-check-hook*)
                          (let ((typespec (funcall fn typespec)))
                            (when typespec (return typespec)))))))
        (if (awl::type-spec-p typespec)
            typespec
            (let ((typespec (or (when (boundp 'awl::*default-typespec*) awl::*default-typespec*)
                                t)))
              (check-type typespec awl::type-spec "a valid typespec")
              typespec)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awl::get-var-expr-typespec (binding&typespec)
    "Parse BINDING&TYPESPEC, which must be of the form var | (var &optional expr typespec), and return the three values var, expr and effective-typespec, where effective-typespec has been determined by the function get-effective-typespec."
    (multiple-value-bind (var expr typespec)
        (etypecase binding&typespec
          (awl::variable (values binding&typespec binding&typespec))
          ((cons awl::variable list) (values (first binding&typespec)
                                            (or (second binding&typespec)
                                                (first binding&typespec))
                                            (third binding&typespec))))
      (let ((typespec (awl::get-effective-typespec typespec)))
        (values var expr typespec)))))


(defmacro awl::with-vars (bindings&typespecs &body body)
  "Locally bind variables with a declared type. BINDINGS&TYPESPECS is a list of bindings. Each binding and type declaration is of the form var | (var &optional expr typespec) where var is a variable name, expr is an expression and typespec a \"generalized\" typespec."
  (let ((bindings (mapcar (lambda (b&t)
                            (multiple-value-bind (var val typespec)
                                (etypecase b&t
                                  (awl::variable (values b&t))
                                  ((cons awl::variable list) (values-list b&t)))
                              (let ((g!var (gensym (string var)))
                                    (typespec (awl::get-effective-typespec (or typespec t))))
                                (list `(,g!var ,(if (eq typespec t) val `(coerce ,val ',typespec)))
                                      `(type ,typespec ,g!var)
                                      `(,g!var () ,g!var)
                                      `((setf ,g!var) (v) (setq ,g!var (coerce v ',typespec)))
                                      g!var
                                      `(ftype (function () ,typespec) ,g!var)
                                      `(,var (,g!var))))))
                          bindings&typespecs)))
    `(let ,(mapcar #'first bindings)
       (declare ,@(mapcar #'second bindings))
       (flet (,@(mapcar #'third bindings)
              ,@(mapcar #'fourth bindings))
         (declare (inline ,@(mapcar #'fifth bindings))
                  ,@(mapcar #'sixth bindings))
         (symbol-macrolet ,(mapcar #'seventh bindings)
           ,@body)))))


(defun awl::clean-declarations (code-expr)
  (awl::map-tree (lambda (expr)
                   (when (consp expr)
                     (case (car expr)
                       (declare (clean-declare expr))
                       (the (clean-the expr)))))
                 code-expr))

(defun clean-declare (declarations)
  (labels ((recursive-fun (decls)
             (let ((rest-decl (member 'type decls :key #'car)))
               (when rest-decl
                 (let ((type-decl (first rest-decl)))
                   (setf (second type-decl) (awl::get-effective-typespec (second type-decl))))
                 (recursive-fun rest-decl)))))
    (recursive-fun (rest declarations)))
  declarations)

(defun clean-the (the-invocation)
  (setf (second the-invocation) (awl::get-effective-typespec (second the-invocation)))
  the-invocation)



;;;;** Number Types


(defvar *default-float-format* *read-default-float-format*
  "Default float format used for computations.")

(defvar *addition-groups* '((complex long-float) (complex double-float) (complex single-float) (complex short-float)
                             (complex integer) (complex rational) complex
                             long-float double-float single-float short-float float
                             integer rational real number t)
  "List of types which represent addition groups of numbers.")


(defun dispatch-type (type &optional (domain :addition-group))
  (ecase domain
    (:addition-group (addition-group type))
    (:quotient-field (quotient-field type))
    (:cauchy-complete (cauchy-complete type))))

(defvar awl::*enumeration-format-string*
  "~{~#[ none~; ~S~; ~S and ~S ~:;~@{~#[~; and~] ~S~^,~}~]~}")

(defun resulting-type* (domain types)
  (dispatch-type (dolist (elt-type *addition-groups*
                          (error (concatenate 'string "The type~P" awl::*enumeration-format-string* "~2:* ~[are~;is~:;are~] incompatible.")
                                 (length types) types))
                   (when (some (lambda (type) (subtypep type elt-type)) types)
                     (return elt-type)))
                 domain))
(defun resulting-type (domain &rest types)
  (resulting-type* domain types))


(defun addition-group (type)
  (dolist (suptype *addition-groups*)
    (when (subtypep type suptype)
      (return suptype))))
(defun quotient-field (type)
  (let ((type (addition-group type)))
    (cond ((subtypep type 'rational) 'rational)
          ((subtypep type '(complex rational)) '(complex rational))
          (t type))))
(defun cauchy-complete (type)
  (let ((type (quotient-field type)))
    (cond ((subtypep type 'rational) 'long-float)
          ((subtypep type '(complex rational)) '(complex long-float))
          (t type))))




#+ (or)
(defun %make-typed-function (result-names result-types input-names input-types body context captured-symbols)
  (let* ((result-types (mapcar (lambda (rt) (awl::map-tree (lambda (expr)
                                                             (when (and (consp expr)
                                                                        (eq 'type-of (first expr)))
                                                               (let ((pos (position (second expr) input-names)))
                                                                 (when pos (setf (first expr) 'quote
                                                                                 (second expr) (nth pos input-types))))))
                                                           rt)) result-types))
         (names (append result-names input-names))) 
    (funcall (compile nil
                      `(lambda ()
                         (declare ,*speed-optimizations*)
                         (con::with-context (,@captured-symbols) ,context
                                            (list (lambda ,names
                                                    (declare ,*speed-optimizations*) 
                                                    ,@(awl::clean-declarations body)
                                                    (values ,@result-names))
                                                  (list ,@result-types ))))))))

#+ (or)
(defmacro define-typed-function (name (&rest input) (&rest result) &body options&body)
  "(define-typed-function foo (a b) ((c (make-foo (bar a) (foo-bar b)) (resulting-type :quotient-field (type-of a) (type-of b))))
     (let ((d (coerce 0 (type-of c))))
       (declare (type (type-of c) d))
       (setf d (the (type-of c) (/ a b))) etc..)) 
Be careful: the symbol cl:declare and cl:the should not be used else than in the standard context of declarations, since the code in the body is parsed in a primitive way."
  (let* ((input-spec (mapcar (lambda (in)
                               (etypecase in
                                 (awl::variable (list in #'type-of))
                                 ((awl::pair awl::variable function)
                                  (let* ((arglist (awl::function-arg-list (second in)))
                                         (mandatory-args (awl::get-mandatory-args arglist))
                                         (optional-args (awl::get-optional-args arglist)))
                                    (assert (or (= 1 (length mandatory-args))
                                                (and (zerop (length mandatory-args))
                                                     (< 0 (length optional-args)))) ()
                                            "The type-defining function must have exactly one mandatory or at least one optional argument ~
                                             but the actual arglist is ~S" arglist)
                                    in))))
                             input))
         (result-specs (mapcar (awl::dlambda ((name constructor &optional (type t)))
                                            (list name type
                                                  `(lambda ,input (declare (ignorable ,@input)) ,constructor)))
                               result))
         (function-name (awl::mkgnsym name)))
    (multiple-value-bind (options body)
        (loop :while (member (first (first options&body)) '(:check-input :documentation :capture-environment))
           :do (push (pop options&body) options)
           :finally (return options options&body))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let* ((captured-symbols (rest (assoc :capture-environment options)))
                (context (con:capture-environment captured-symbols))
                (dispatch-table (make-hash-table :test #'equal)))
           (defun ,function-name (input)
             (let* ((input-types (mapcar #'funcall ',(mapcar #'second input-spec)))
                    (dispatched-types (mapcar #'dispatch-type input-types))) 
               (values-list (or (gethash dispatched-types dispatch-table)
                                (setf (gethash dispatched-types dispatch-table) (%make-operation ',(mapcar #'first result-specs)
                                                                                                 ',(mapcar #'second result-specs)
                                                                                                 ',(mapcar #'first input-spec)
                                                                                                 dispatched-types
                                                                                                 ',body
                                                                                                 ',context ',captured-symbols)))))))
         (defun ,name ()
           ,(second (assoc :documentation options))
           (values ',function-name
                   ',input
                   (list ,@(mapcar #'third result-specs))
                   (let* ((check (rest (assoc :check-input options)))
                          (expr (first check))
                          (datum (rest check)))
                     (lambda ,input
                       (assert ,@(or expr '(t)) () ,@datum)))))))))

;;;; !!!! result specs should be lambda expressions !!!!
#+ (or)
(defmacro with-typed-function (name (&rest result-input) &body body)
  (multiple-value-bind (operator input-old-names result-constructors check-input)
      (funcall (symbol-function name))
    (let* ((length-input-old-names (length input-old-names)))
      (let ((difference (- (length result-input) length-input-old-names (length result-constructors))))
        (assert (zerop difference) ()
                "In ~S: ~D Argument~:P ~:[not enough~:;too much~]. ~D result and ~D input variables needed."
                result-input (abs difference) (plusp difference) (length result-constructors) length-input-old-names))
      (let* ((result-names (butlast result-input length-input-old-names))
             (input-bindings (mapcar #'awl::var-binding-pair (last result-input length-input-old-names)))
             (input-names (mapcar #'first input-bindings))
             (input-g!names (mapcar #'awl::mkgnsym input-names))
             (input-bindings (mapcar (lambda (g!name binding)
                                       (list g!name (second binding)))
                                     input-g!names input-bindings))
             (input-names-g!names (mapcar #'list input-names input-g!names))
             (input-oldnames-g!names (mapcar #'list input-old-names input-g!names))
             (results-name.spec (mapcar #'cons result-names result-constructors)))
        (awl::with-gensyms (g!input g!input-types g!op g!result-types
                                   g!nbr-rows g!nbr-cols g!elt-type)
          `(let ,input-bindings
             (funcall ,check-input ,@input-g!names)
             (multiple-value-bind (,g!op ,g!result-types)
                 (,operator ,input-g!names)
               (flet ((,name (,@result-names ,@input-names)
                        (funcall ,g!op ,@result-names ,@input-names)))
                 (let ,input-names-g!names
                   (declare (ignorable ,@input-names))
                   ,@body)))))))))



(do-symbols (sym '#:awl)
  (export sym '#:awl))
