;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;============================================================================
;;;; Basic Utilities
;;;;
;;;; This file contains the basic utilities which might be used in all other code
;;;; of the awl family.
;;;;
;;;;* Basic List Processing
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awl::ensure-list (o)
    "Return either the object O itself if O is a list or a list containing O otherwise."
    (typecase o
      (list o)
      (t (list o)))))

(defun awl::flatten (x)
  "Return a list of all atoms contained in the tree-structures cons X in the depth-first search order."
  (labels ((rec (x acc)
             (typecase x
               (null acc)
               (atom (cons x acc))
               (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;;;
;;;;* Some Types
;;;;
;;;;** Number Types
;;;;
;;;; The idea here is to provide a way to determine the type a certain
;;;; numeric variable or result might have.  E.g. if x is an integer variable
;;;; the result of (sin x) will be (or rational single-float) (in SBCL it will
;;;; be a single-float).  The result of a reational function however will be
;;;; of type rational.  Hence we shall define the generic types
;;;; (field numeric-type) and (transcendent numeric-type).
;;;; The type (complex numeric-type) does already exist.
;;;;
;;;; Moreover we define a function which returns, given a numeric-type, the
;;;; smallest group containing the type: (group numeric-type).


(defun awl::transcendent-field-type (numeric-type)
  "Return the type specification which corresponds to the smallest \"transcendent\" field containing NUMERIC-TYPE."
  (cond ((subtypep numeric-type 'real) (find-if (lambda (type) (subtypep numeric-type type))
                                                '(long-float double-float single-float short-float real)))
        ((subtypep numeric-type 'complex) (or (find-if (lambda (flt) (subtypep numeric-type `(complex ,flt)))
                                                       '(long-float double-float single-float short-float))
                                              'complex))
        (t (error "The argument NUMERIC-TYPE ~A is not a subtype of (or real complex)." numeric-type))))
(deftype awl::transcendent-field (numeric-type)
  "Type which corresponds to the smallest \"transcendent\" field containing NUMERIC-TYPE."
  (awl::transcendent-field-type numeric-type))

(defun awl::field-type (numeric-type)
  (cond ((subtypep numeric-type 'rational) 'rational)
        ((subtypep numeric-type '(complex rational)) '(or rational (complex rational)))
        (t `(awl::transcendent-field-type ,numeric-type))))
(deftype awl::field (numeric-type)
  (awl::field-type numeric-type))

(defun awl::group-type (numeric-type)
  (cond ((subtypep numeric-type 'integer) 'integer)
        ((subtypep numeric-type '(complex integer)) '(or integer (complex integer)))
        (t `(awl::field-type ,numeric-type))))
(deftype awl::group (numeric-type)
  (awl::group-type numeric-type))

;;;;
;;;;** Type-Spec
(defun awl::type-spec-p (expr &optional env)
  "Return true if expr is a valid type specification in the environment ENV, NIL else."
  (ignore-errors (typep nil expr env) t))
(deftype awl::type-spec ()
  "Type of an expression which is a valid type specification."
  '(satisfies awl::type-spec-p))

;;;;** Ntuple
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype awl::ntuple (&rest elt-types)
    "An ntuple is a list with n elements. For example, (ntuple * * *) is a triple of unspecified elements."
    (if elt-types
        `(cons ,(car elt-types) (awl::ntuple ,@(cdr elt-types)))
        'null))

;;;;** Pair
  (deftype awl::pair (&optional (first '*) (second first))
    "A pair is a list with two elements. The arguments specify the type of the elements. If only the first argument is given it means both elements are of that same type; for leaving unspecified the second element when specifying the first one, a * must be used, e.g. (awl:pair symbol *)."
    `(awl::ntuple ,first ,second)))

(defun awl::pairp (x &rest types)
  (typep x `(awl::pair ,@types)))

;;;;** List of
(defun awl::elt-type-p (sequence type &optional environment)
  "Return T if all elements in SEQUENCE are of type TYPE and NIL else."
  (declare (type sequence sequence))
  (every (lambda (elt) (typep elt type environment)) sequence))

(deftype awl::list-of (elt-type &environment environment)
  "A list with elements of type TYPE."
  (let ((g!elt-type (gensym "ELT-TYPE")))
    (setf (symbol-function g!elt-type)
          (lambda (list)
            (declare (type list list))
            (awl::elt-type-p list elt-type environment)))
    `(and list (satisfies ,g!elt-type))))

;;;;** String Designator
(deftype awl::string-designator ()
  "A string designator."
  '(or character symbol string))
;;;;** Function Designator
(deftype awl::function-designator ()
  '(or null symbol function))

;;;;* Functions
;;;;** Introspect Arglists of Functions
(defun awl::function-lambda-list (fn)
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
(defun awl::function-lambda-list (x &optional original-x)
  (typecase x
    (symbol (awl::function-lambda-list (symbol-function x) x))
    (compiled-function (read-from-string
                        (lisp::%primitive header-ref x
                                          lisp::%function-arg-names-slot)))
    (list (case (first x)
            (lambda (second x))
            (lisp::%lexical-closure% (awl::function-lambda-list (second x)))
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


(defclass lambda-argument ()
  ((name :initarg :name :reader lambda-argument-name)))
(defclass positional-lambda-argument (lambda-argument)
  ((position :initarg :position :reader lambda-argument-position)))
(defclass mandatory-lambda-argument (positional-lambda-argument) ())
(defclass assigned-argument-mixin ()
  ((value-expr :initarg :value :reader lambda-argument-value-expr)))
(defclass optional-argument-mixin (assigned-argument-mixin)
  ((supplied-p-parameter :initarg :supplied-p-parameter
                         :reader lambda-argument-supplied-p-parameter)))
(defclass optional-lambda-argument (positional-lambda-argument optional-argument-mixin) ())
(defclass rest-lambda-argument (lambda-argument) ())
(defclass key-lambda-argument (lambda-argument optional-argument-mixin)
  ((key :initarg :key :reader lambda-argument-key)))
(defclass aux-lambda-argument (lambda-argument assigned-argument-mixin) ())

(defun parse-lambda-list (lambda-list)
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
      (awl::parse-lambda-list lambda-list)
    (let ((pos -1) result)
      (dolist (arg required)
        (push (make-instance 'mandatory-lambda-argument
                             :name arg :position (incf pos))
              result))
      (dolist (arg optional)
        (let ((arg (awl::ensure-list arg)))
         (push (make-instance 'optional-lambda-argument
                              :name (car arg) :value (cadr arg)
                              :supplied-p-parameter (caddr arg)
                              :position (incf pos))
               result)))
      (when restp
        (push (make-instance 'rest-lambda-argument
                             :name rest)
              result))
      (when keyp
        (dolist (arg keys)
          (let ((arg (let ((arg (awl::ensure-list arg)))
                       (cons (awl::ensure-list (car arg)) (cdr arg)))))
            (push (make-instance 'key-lambda-argument
                                 :name (caar arg)
                                 :key (or (cadar arg) (intern (string (caar arg)) :keyword ))
                                 :value (cadr arg)
                                 :supplied-p-parameter (caddr arg))
                  result))))
      (when allowp
        (push :allow-other-keys result))
      (dolist (arg aux)
        (let ((arg (awl::ensure-list arg)))
          (push (make-instance 'aux-lambda-argument
                               :name (car arg) :value (cadr arg))
                result)))
      (nreverse result))))

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
  (cadr (member '&rest arglist)))
(defun awl::mandatory-args (arglist)
  "Return the mandatory arguments of the function arglist ARGLIST."
  (loop :for arg :in arglist
     :until (member arg '(&optional &rest &key &aux)) :collect arg))
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
              (awl::variable (intern (string key) :keyword))
              ((awl::pair awl::variable *) (intern (string (first key)) :keyword))
              ((awl::pair (awl::pair symbol awl::variable) *) (first (first key)))))
          (awl::key-args arglist)))

;;;;TODO: This is just a scetch and certainly not correct.
(defun awl::sub-lambda-list-p (lambda-list-1 lambda-list-2)
  "Return true if any list of arguments compatible with lambda-list-1 is also compatible with lambda-list-2."
  (multiple-value-bind (required-1 optional-1 restp-1 rest-1 keyp-1 keys-1 allowp-1)
      (awl::parse-lambda-list lambda-list-1)
    (declare (ignore rest-1 keys-1))
    (multiple-value-bind (required-2 optional-2 restp-2 rest-2 keyp-2 keys-2 allowp-2)
        (awl::parse-lambda-list lambda-list-2)
      (declare (ignore rest-2 keys-2))
      (and (>= (length required-1) (length required-2))
           (or (>= (+ (length required-1) (length optional-1))
                   (+ (length required-2) (length optional-2)))
               (and restp-2 (not keyp-2)))
           (not (and restp-1 (not keyp-1) keyp-2))
           (or (and (subsetp (awl::arglist-keys lambda-list-1) (awl::arglist-keys lambda-list-2))
                    (not allowp-1))
               allowp-2)))))
(defun awl::satisfies-lambda-list-p (arglist lambda-list)
  "Return true if the list ARGLIST can be applied to the lambda list LAMBDA-LIST."
  (eval `(ignore-errors (destructuring-bind ,lambda-list
                            ',arglist
                          (declare (ignore . ,(awl::get-lambda-variables lambda-list)))
                          t))))
;;;;
;;;;** Some Types
;;;;*** Lambda List
(defun awl::lambda-list-p (list)
  (ignore-errors (awl::parse-lambda-list list) t))
(deftype awl::lambda-list (&optional (lambda-list *))
  "Type of the ordinary lambda lists. If the optional argument LAMBDA-LIST is given, it designates the set of ordinary lambda lists which are compatible with LAMBDA-LIST in the sense that any list of arguments compatible with LAMBDA-LIST is also compatible with the lambda lists of the type."
  (if (eq '* lambda-list)
      (progn (assert (awl::lambda-list-p lambda-list))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (fdefinition '#1=#:sub-lambda-list-p)
                     (lambda (ll) (awl::sub-lambda-list-p lambda-list ll))))
             '(satisfies #1#))
      '(satisfies awl::lambda-list-p)))
;;;;*** Lambda Expressions
(defun awl::lambda-expr-p (expr)
  "Return true if expr is a lambda expression, NIL else."
  (typep expr 'awl::lambda-expr))
(deftype awl::lambda-expr (&optional (lambda-list *))
  `(awl::ntuple (eql lambda) (awl::lambda-list ,lambda-list) *))

(defun awl::lambda-expr-1-p (expr)
  "Return true if expr is a lambda expression with exactly one argument and this argument is mandatory, NIL else."
  (ignore-errors (destructuring-bind (lambda (arg) &rest body)
                     expr
                   (declare (ignore arg body))
                   (eq 'lambda lambda))))
(deftype awl::lambda-expr-1 ()
  '(satisfies awl::lambda-expr-1-p))

(defun awl::thunk-lambda-list-p (list)
  (ignore-errors (null (awl::parse-lambda-list list))))
(deftype awl::thunk-lambda-expr ()
  '(awl::ntuple (eql lambda) (satisfies awl::thunk-lambda-list-p) *))

;;;;** Variables
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awl::variablep (x &optional environment)
    (and (symbolp x) (not (constantp x environment)))))
(deftype awl::variable () '(satisfies awl::variablep))

(defun awl::specialp (symbol &optional environment)
  "Return true if symbol has been declared special in the environment ENVIRONMENT."
  (check-type symbol symbol)
  (eq :special (awl::variable-information symbol environment)))

(defun awl::declaimed-specialp (symbol)
  "Return true if symbol has been declaimed special. Useful for checking if a symbol can be used for a symbol-macro(let)."
  (awl::specialp symbol))
(defun awl::local-variable-p (x &optional environment)
  (and (awl::variablep x environment) (not (awl::declaimed-specialp x))))

(deftype awl::local-variable () '(satisfies awl::local-variable-p))

(defun awl::declared-type (symbol &optional environment)
  (check-type symbol symbol)
  (or (cdr (assoc 'type (nth-value 2 (awl::variable-information symbol environment)))) t))

;;;; Other (buggy) tentatives to define SPECIALP:
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

;;;;* Basic Macros
;;;;** Code Generating Utilities
(eval-when (:compile-toplevel :load-toplevel :execute)

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

  (defun awl::parse-body (body)
    "Parse the expression BODY as the body of a DEFUN-like macro and return as multiple values (1) the effective body, (2) the declaration part, (3) the documentation string."
    (let* ((first-body (first body))
           (rest-body (rest body))
           (docstring (when (and rest-body (stringp first-body))
                        first-body))
           (body (if docstring rest-body body))
           (declarations (loop :while (and (consp (first body))
                                           (eq 'declare (caar body)))
                               :for form := (pop body)                          
                               :collect form)))
      (values body declarations docstring)))
  
  (defun awl::var-binding-pair (pair)
    "Return (list PAIR PAIR) if PAIR is of type awl::variable, return PAIR if PAIR is a pair with first element of type awl::variable or signals an error of type type-error otherwise."
    (etypecase pair
      (awl::variable (list pair pair))
      ((awl::pair awl::variable *) pair)))

  (defun awl::expand-funcall (function &rest args &aux (fun-const-p (constantp function)))
    "Return an expression which, when evaled, calls FUNCTION to its arguments ARGS. The expansion depends on the type of FUNCTION; if FUNCTION is NIL expand to a progn on ARGS."
    (let ((function (if fun-const-p
                        (eval function)
                        function)))
      (or (when fun-const-p
            (typecase function
              (null (case (length args)
                      ((0 1) (first args))
                      (t `(progn ,@args))))
              (symbol `(,function ,@args))))
          `(funcall ,function ,@args)))))


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

;;;;** Compile Closures
#+ (or);;;; TODO: Not good...
(defmacro awl::compile-closure ((&rest lexical-vars) arglist &body body)
  "Compile a closure over LEXICAL-VARS with arg list ARGLIST and body BODY."
  (let* ((lexical-vars (mapcar 'awl::ensure-list lexical-vars)))
    `(funcall (compile nil
                       '(lambda ,(mapcar 'first lexical-vars)
                         (lambda ,arglist
                           ,@body)))
              ,@(mapcar 'second lexical-vars))))

;;;;** Binding Forms
;;;;
;;;; TODO: The name is not satisfying.
;;;; TODO: The interface is not satisfying: For the moment BODY, FORM and ENV are
;;;; implicitly bound to the &body, &whole resp. &environment variables of the
;;;; macro NAME being defined. Maybe one wants other args to be defined
;;;; in the arglist of the macro... the interface should not become too
;;;; complicated however. +++ This might be addressed now by the keywords +++
(defmacro awl::define-recursive-macro (name (arg-destructuring
                                             &key key
                                             (args-name (intern "ARGS"))
                                             (body-name (intern "BODY"))
                                             (env-name (intern "ENV")))
                                       &body body)
  "Define a macro with name NAME and lambda list of the form ((&rest args) &body body) where args is a list of forms which, after being applied to KEY, are of the form which can be destructured by ARG-DESTRUCTURING."
  (multiple-value-bind (definition declarations docstring)
      (awl::parse-body body)
    (let* ((first-arg (awl::expand-funcall key `(first ,args-name)))
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
      `(defmacro ,name (&whole form (&rest ,args-name)
                        &body ,body-name &environment ,env-name)
         ,@(awl::ensure-list docstring)
         (declare (ignorable ,env-name))
         ,@declarations
         (let ((,body-name (let ((rest-args (rest ,args-name)))
                             (if rest-args
                                 `((,(first form) ,rest-args ,@,body-name))
                                 ,body-name))))
           (if ,args-name
               ,definition
               `(progn ,@,body-name)))))))

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
;;;; 
;;;; #:macrolet-wrapper is a dummy macro wrapping a macrolet
;;;; and for which we define a setf-expander. A call to a
;;;; macrolet macro is expanded before setf dispaches its
;;;; own expanding mechanisms. Macrolets from awl::macrolet*
;;;; are all expanded into (macrolet (...) ...). However, for
;;;; macrolet -- at least in scbl -- no setf-expander is defined
;;;; and no such can be portably defined. Hence, we wrap the
;;;; above macrolet in a dummy macro for which we define a
;;;; setf-expander.
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
  (awl::define-recursive-macro awl::macrolet* ((name arglist &body macro-body)
                                               :args-name 'definitions)
    "Like CL:MACROLET, however invocations in the body of the local macro being defined of the macro of the same name as the local macro being defined are not recursive, but call the macro of function defined in the next outer lexical environment (if that macro or function exists)."
    (multiple-value-bind (macro-body declarations docstring)
        (awl::parse-body macro-body)
      (let ((macro-function (macro-function name env)))
        (if macro-function
            `(macrolet
                 ((,name ,arglist ,docstring ,@declarations
                    `(#1# (macrolet ((,',name (&whole form &rest args)
                                       (declare (ignore args))
                                       (funcall ,',macro-function form ,',env)))
                            ,,@macro-body))))
               ,@body)
            (let ((g!name (gensym (string name))))
              `(flet ((,g!name (&rest args)
                        (apply (function ,name) args)))
                 (declare (inline ,g!name))
                 (macrolet ((,name ,arglist ,docstring ,@declarations
                              `(#1# (macrolet ((,',name (&rest args)
                                                 `(,',',g!name ,@args)))
                                      ,,@macro-body))))
                   ,@body))))))))

(progn
  (defmacro #1=#:macrolet-wrapper (expr)
    expr)
  (define-setf-expander #1# (expr)
    (awl::with-gensyms (g!value)
      (values nil
              nil
              `(,g!value)
              (append (butlast expr) (list `(setf ,@(last expr) ,g!value)))
              expr)))
  (defmacro awl::macrolet* ((&rest bindings) &body body 
                            &environment env)
    "Like CL:MACROLET, however invocations in the body of the local macro being defined of the macro of the same name as the local macro being defined are not recursive, but call the macro or the function defined in the next outer lexical environment (if that macro or function exists)."
    (let ((body (let ((rest (rest bindings)))
                  (if rest
                      `((awl::macrolet* ,rest
                          ,@body))
                      body))))
      (if bindings
          (destructuring-bind (name arglist &rest macro-body)
              (first bindings)
            (multiple-value-bind (macro-body declarations docstring)
                (awl::parse-body macro-body)
              (let ((macro-function (macro-function name env)))
                (if macro-function
                    `(macrolet
                         ((,name ,arglist
                            ,@(awl::ensure-list docstring)
                            ,@declarations
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
                                ,@(awl::ensure-list docstring)
                                ,@declarations
                                `(#1# (macrolet ((,',name (&rest args)
                                                   `(,',',g!name ,@args)))
                                        ,,@macro-body))))
                           ,@body)))))))
          `(progn ,@body)))))


;;;;*** Locally Bind Functions
;;;; Locally bind functions to symbols via `flet'. The rationale for this
;;;; macro is to locally rename functions or to easily give names to functions
;;;; stored in symbol-value. For this `flet' is enough.
#- (or)
(awl::define-recursive-macro awl::fbind ((var fct-expr)
                                         :key #'awl::var-binding-pair
                                         :args-name bindings)
  "Locally bind functions to symbols via `flet'. The rationale for this macro is to locally rename functions or to easily give names to functions stored in symbol-value thus avoiding the use of funcall. The syntax is as in let except if a binding consists of only one symbol s the function stored  in s in the lexical environment is flet to the symbol s."
  (awl::with-gensyms (g!fct)
    `(let ((,g!fct (compile nil ,fct-expr)))
       (declare (type function ,g!fct))
       (flet ((,var (&rest args) (apply (the function ,g!fct) args)))
         (declare (inline ,var))
         ,@body))))
#+ (or)
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
#- (or)
(awl::define-recursive-macro awl::with-protected-macros
    ((var macroname) :key (lambda (pair)
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
#+ (or)
(defmacro awl::with-protected-macros (names &body body &environment env)
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


;;;;** From Let over Lambda 

(defun g!-symbol-p (s)
  (and (symbolp s)
       (< 2 (length (symbol-name s)))
       (string= (symbol-name s) "G!" :start1 0 :end1 2)))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (< 2 (length (symbol-name s)))
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
          ,@(awl::ensure-list docstring)
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
  (declaim (inline awl::force-delay)
           (ftype (function (cons)) awl::force-delay))
  (defun awl::force-delay (delay)
    "Return the result of the expressions delayed with the macro DELAY. The result is unspecified and most probably an error if the input is not a delay produced with the macro DELAY."
    (declare (optimize speed (safety 1)))
    (if (eq (car delay) '#2#) (cdr delay) (funcall (the function (cdr delay)))))
  (deftype awl::delay ()
    `(cons (or (eql #1#) (eql #2#)) *)))

;;;;** Pipes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro awl::make-pipe (head &body tail)
    "Return a pipe (a lazy list) containing as first element HEAD and in which the elements in the tale are generated by successively evaluating (progn . tail) in the current lexical environment."
    `(cons ,head (cons (lambda () ,@tail) '#1=#:tail)))
  (defun awl::tail (pipe)
    "Return a pipe representing the tail of PIPE."
    (let ((tail (rest pipe)))
      (when (and (consp tail) (eq '#1# (cdr tail)))
        (let ((fn (car tail)))
          (setf (car tail) (funcall fn)
                (cdr tail) (cons fn '#1#))))
      tail)))
(setf (symbol-function 'awl::head) (symbol-function 'first)
      (documentation 'awl::head 'function) (documentation 'first 'function))
(defun awl::pipe-empty-p (pipe)
  (null pipe))





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
                   (compile nil `(lambda (string &optional (start 0) end)
                                   ,(format nil "Return the position of the first match from the ~:[beginning~;end~] of ~v[~;~:;one of ~]~{~S~#[~; or ~:;, ~]~} in STRING between START and END, unless escaped with the character ~S."
                                            from-end (length targets) targets escape-char)
                                   (values-list (reduce (lambda (a b)
                                                          (awl::fbind ((following-p ,(if from-end #'> #'<)))
                                                                      (if a
                                                                          (if (and b (following-p (car b) (car a)))
                                                                              b
                                                                              a)
                                                                          b)))
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

(declaim (inline awl::f-or*))
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
  "Return a function whose arglist is the arglist of the function FN from which the args bound in BOUND-ARGS are removed. Unbound positional arguments can be indicated by uninterned symbols. Example: if the function FOO takes the argument (a b c &key d e) then the invocation of (PARTIAL 'FOO 0 #:b 2 :e 4) returns a function equivalent to (lambda (b &rest rest &key d) (apply 'foo 0 b 2 rest))."
  (let* ((arglist (awl::function-lambda-list fn))
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
                                 :when (and (< 0 count) (null (symbol-package value)))
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
                               :collect (if (and (< 0 count) (null (symbol-package value)))
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
  (let ((arglist (awl::function-lambda-list fn)))
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



;;;;* I/O
;;;; Some simple utilities for interactive I/O
(defun awl::readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))
(defun awl::prompt (format-string &rest args)
  (apply #'format *query-io* format-string args)
  (read *query-io*))
(defun awl::break-loop (fn quit &rest args)
  (format *query-io* "~&Entering break-loop.")
  (loop
     (let ((in (apply #'awl::prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~&~A" (funcall fn in))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))
