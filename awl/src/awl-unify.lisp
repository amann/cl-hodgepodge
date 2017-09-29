;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;* Utilities
(defmacro with-dictionary ((&rest vars) dictionary &body body)
  "Establish within BODY a lexical binding of variables to value-entries in the dictionary DICTIONARY. The argument VARS is a list of either pairs of symbols-expressions of the form (var expr) or symbols var. The expressions expr or the symbols var, in the latter case, are looked up in DICTIONARY by the mean of the function GET-BINDING, which is accessible in the lexical environment surrounding the macro call. A local macro (BOUND-P var) is defined in the scope of BODY returning true if the variable var is bound within DICTIONARY, i.e. if a value associated to the corresponding expr (or var in the latter case) has been found in the dictionary. The macro relies on the existence in the surrounding environment of the following functions:
- (get-binding expr) returning a binding cell object of DICTIONARY associated to expr.
- (binding-val binding) taking a (valid) binding cell object of DICTIONARY and returning the value part of the binding cell."
  (let ((vars (mapcar (lambda (var)
                        (typecase var
                          (cons (first var))
                          (t var)))
                      vars))
        (exprs (mapcar (lambda (var)
                         (typecase var
                           (cons (second var))
                           (t var)))
                       vars))
        (g!exprs (map-into (make-list (length vars)) 'gensym))
        (g!binding-vars (map-into (make-list (length vars)) 'gensym)))
    (awl:with-gensyms (g!bindings)
      `(let ((,g!bindings ,dictionary))
         (let ,(mapcar 'list g!exprs exprs)
           (let ,g!binding-vars
             (macrolet ((bound-p (var)
                          (case var
                            ,@(mapcar (lambda (var g!binding-var g!expr)
                                        `(,var `(or ,',g!binding-var
                                                    (setq ,',g!binding-var (get-binding ,',g!expr ,',g!bindings)))))
                               vars g!binding-vars g!exprs))))
               (symbol-macrolet (,@(mapcar (lambda (var g!binding-var g!expr)
                                             `(,var (binding-val (or ,g!binding-var
                                                                     (setq ,g!binding-var (get-binding ,g!expr ,g!bindings))))))
                                           vars g!binding-vars g!exprs))
                 ,@body))))))))
;;;;* Unification of Objects in S-Expressions

(define-condition awl::unification-failure (error)
  ((expression1 :initarg :expr1 :reader awl::unification-failure-expr1)
   (expression2 :initarg :expr2 :reader awl::unification-failure-expr2))
  (:report (lambda (condition stream)
             (format stream "Could not unify ~A and ~A."
                     (awl::unification-failure-expr1 condition)
                     (awl::unification-failure-expr2 condition)))))

;;;;** Unify

(defun unify (expr1 expr2 variablep equal-p &optional bindings)
  "See if the s-expressions EXPR1 and EXPR2 match, where as variables are used objects which satisfy VARIABLEP and which are compared for equality using EQUAL-P. If the s-expressions match, return an association list of bindings of the variables to their constant values, else signal an error of type AWL::UNIFICATION-FAILURE."
  (flet ((variablep (x) (funcall variablep x))
         (equal-p (x y) (funcall equal-p x y))
         (binding-val (binding) (cdr binding))
         (get-binding (var bindings) (assoc var bindings :test equal-p))
         (add-binding (var val bindings) (cons (cons var val) bindings))
         (fail (expr1 expr2 bindings) (declare (ignore bindings))
           (error 'awl::unification-failure :expr1 expr1 :expr2 expr2)))
    (declare (inline variablep equal-p get-binding add-binding))
    (labels ((unify (expr1 expr2 bindings)
               (cond ((equal-p expr1 expr2) bindings)
                     ((variablep expr1) (unify-var expr1 expr2 bindings))
                     ((variablep expr2) (unify-var expr2 expr1 bindings))
                     ((and (consp expr1) (consp expr2))
                      (unify (rest expr1) (rest expr2) (unify (first expr1) (first expr2) bindings)))
                     (t (fail expr1 expr2 bindings))))
             (unify-var (var expr bindings)
               "Unify VAR and EXPR in the case where VAR is a variable according to VARIABLEP and return the extended binding."
               (with-dictionary ((val var)
                                 (expr-val expr))
                 bindings
                 (cond ((bound-p val) (unify val expr bindings))
                       ((bound-p expr-val) (unify var expr-val bindings))
                       ((occurs-p var expr bindings) (fail var expr bindings))
                       (t (add-binding var expr bindings)))))
             (occurs-p (var expr bindings)
               "Return true if VAR occurs in the value associated in BINDINGS to the expression EXPR with respect to EQUAL-P."
               (with-dictionary ((val expr)) bindings
                 (cond ((equal-p var expr) t)
                       ((bound-p val) (occurs-p var val bindings))
                       ((consp expr) (or (occurs-p var (car expr) bindings)
                                         (occurs-p var (cdr expr) bindings))))))
             (substitute-variables (expr bindings)
               "Return an expression based on EXPR but in which all (sub-)expressions of EXPR bound to a value in BINDINGS are recursively substituted by their value, until only unbound (terminal) expressions are left. This function may destructively modify EXPR."
               (with-dictionary ((expr-val expr)) bindings
                 (if (bound-p expr-val)
                     (substitute-variables expr-val bindings)
                     (progn (when (and bindings (consp expr))
                              (setf (car expr) (substitute-variables (car expr) bindings)
                                    (cdr expr) (substitute-variables (cdr expr) bindings)))
                            expr)))))
      (values (let ((bindings (unify expr1 expr2 bindings)))
                (dolist (binding bindings bindings)
                  (setf (cdr binding) (substitute-variables (cdr binding) bindings))))
              equal-p))))

;;;;* Dictionaries

;;;;** Binding Cells
(defstruct (binding
            (:type list)
            (:constructor make-binding (var val)))
  var val)
(declaim (inline to-cons))
(defun to-cons (binding)
  (cons (binding-var binding) (binding-val binding)))

;;;;** Dictionary

(defgeneric get-binding (dictionary var &key &allow-other-keys)
  (declare (optimize speed))
  (:argument-precedence-order dictionary var)
  (:documentation "Return an object being the binding-cell of VAR in the dictionary DICTIONARY and whose value can be extracted with binding-val. If no binding exists, return nil."))

(defgeneric (setf get-binding) (val dictionary var &key &allow-other-keys)
  (declare (optimize speed))
  (:argument-precedence-order dictionary var val)
  (:documentation "Set the binding of VAR to VAL to the dictionary DICTIONARY and return the corresponding binding cell. Primary methods which specialize this generic function must take as argument VAL the binding object.")
  (:method :around (var dictionary val &rest keys &key &allow-other-keys)
    (let ((binding (apply 'get-binding var dictionary keys)))
      (if binding
          (setf (binding-val binding) val)
          (call-next-method (make-binding var val) dictionary var))
      binding)))

(defgeneric rem-binding (dictionary key &key &allow-other-keys)
  (declare (optimize speed))
  (:argument-precedence-order dictionary key)
  (:documentation "Remove the binding for variable VAR in dictionary DICTIONARY and return the modified dictionary (which does not need to be EQL to the original dictionary) and as secondary value true if there was such binding, false otherwise."))

(defgeneric awl::lookup (dictionary key &key &allow-other-keys)
  (declare (optimize speed))
  (:argument-precedence-order dictionary key)
  (:documentation "Lookup for KEY in DICTIONARY. If found, return the corresponding value and the binding-cell as secondary value, If not found, return NIL, NIL.")
  (:method (dictionary var &rest keys &key &allow-other-keys)
    (let ((binding (apply 'get-binding var dictionary keys)))
      (when binding
        (values (binding-val binding)
                binding)))))

(defgeneric awl::to-association-list (dictionary)
  (declare (optimize speed))
  (:method ((dictionary null)))
  (:documentation "Return an association list containing the bindings as conses of key val."))



;;;;** Implementations of Dictionaries

;;;;*** Hash-Table

(defmethod get-binding ((dictionary hash-table) var &key &allow-other-keys)
  (gethash var dictionary))
(defmethod (setf get-binding) (binding (dictionary hash-table) var &key &allow-other-keys)
  (setf (gethash var dictionary) binding))
(defmethod rem-binding ((dictionary hash-table) var &key &allow-other-keys)
  (values dictionary (remhash var dictionary)))
(defmethod awl::to-association-list ((dictionary hash-table))
  (loop :for binding :being :each :hash-value :in dictionary
        :collect (to-cons binding)))

;;;;*** Association List

(defmethod get-binding ((dictionary list) var &key (test 'eql) &allow-other-keys)
  (find var dictionary :key 'binding-var :test test))
(defmethod (setf get-binding) (val (dictionary list) var &key &allow-other-keys)
  (push val dictionary))
(defmethod rem-binding ((dictionary list) var &key (test 'eql) &allow-other-keys)
  (let (removedp)
    (values (delete var dictionary
                    :key 'binding-var
                    :test (lambda (a b)
                            (when (funcall test a b)
                              (setq removedp t))))
            removedp)))
(defmethod awl::to-association-list ((dictionary list))
  (mapcar 'to-cons dictionary))

;;;;* Environment
;;;; An object containing bindings
(defclass awl::environment ()
  ((bindings :initform nil :initarg :bindings)
   (test :initform 'eql :initarg :test :reader environment-test)))

(defmethod get-binding ((environment awl::environment) var &key &allow-other-keys)
  (with-slots (bindings test)
      (get-binding bindings var :test test)))
(defmethod (setf get-binding) (val (environment awl::environment) var &key &allow-other-keys)
  (with-slots (bindings test)
      (setf (get-binding bindings var :test test) val)))
(defmethod rem-binding ((environment awl::environment) var &key &allow-other-keys)
  (with-slots (bindings test)
      (values environment
              (nth-value 1 (rem-binding bindings var :test test)))))
(defmethod awl::to-association-list ((environment awl::environment))
  (awl::to-association-list (slot-value environment 'bindings)))

;;;;* Unify
(declare (inline awl::unify))
(defun awl::unify (expr1 expr2 variablep equal-p &optional bindings)
  "See if the s-expressions EXPR1 and EXPR2 match, where as variables are used objects which satisfy VARIABLEP and which are compared for equality using EQUAL-P. If the s-expressions match, return an association list of bindings of the variables to their constant values, else signal an error of type AWL::UNIFICATION-FAILURE."
  (unify expr1 expr2 variablep equal-p (awl::to-association-list bindings)))


(do-symbols (symbol "AWL")
  (export symbol "AWL"))

