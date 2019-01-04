;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Dictionary

(defgeneric awl::lookup (dictionary key)
  "Lookup for KEY in DICTIONARY. If found, return the corresponding value and a cons (KEY . VALUE) as secondary value, If not found, return NIL, NIL.")

(defgeneric (setf awl::lookup) (value dictionary key)
  "Set VALUE for KEY in DICTIONARY.")

(defgeneric awl::remove-key (dictionary key)
  "Remove KEY in DICTIONARY. Return true if DICTIONARY contained KEY.")

(defgeneric awl::to-association-list (dictionary)
  (declare (optimize speed))
  (:method ((dictionary null)))
  (:documentation "Return an association list containing the bindings as conses of key value."))

;;;;** Implementations of Dictionaries
;;;;*** Hash-Table
(let ((not-found (gensym)))
  (defmethod awl::lookup ((dictionary hash-table) key)
    (let ((value (gethash key dictionary not-found)))
      (if (eql value not-found)
          (values nil nil)
          (values value (cons key value))))))
(defmethod (setf awl::lookup) (value (dictionary hash-table) key)
  (setf (gethash key dictionary) value))
(defmethod awl::remove-key ((dictionary hash-table) key)
  (remhash key dictionary))

(defmethod awl::to-association-list ((dictionary hash-table))
  (let (alist)
    (maphash (lambda (key value) (push (cons key value) alist)) dictionary)
    alist))

;;;;*** Association List
(defclass awl::alist ()
  ((alist :initform nil :initarg :alist)
   (test :initform 'eql :initarg :test :reader awl::alist-test)))

(defmethod awl::lookup ((dictionary awl::alist) key)
  (with-slots (alist test)
      dictionary
    (let ((binding (assoc key alist :test test)))
      (if binding
          (values (car binding) (cons (car binding) (cdr binding)))
          (values nil nil)))))
(defmethod (setf awl::lookup) (value (dictionary awl::alist) key)
  (with-slots (alist test)
      dictionary
    (let ((binding (assoc key dictionary :test test)))
      (if binding
          (rplacd binding value)
          (push (cons key value) alist)))))
(defmethod awl::remove-key ((dictionary awl::alist) key)
  (with-slots (alist test)
      dictionary
    (let (removedp)
      (progn
        (delete var alist
                :key 'car
                :test (lambda (a b)
                        (when (funcall test a b)
                          (setq removedp t))))
        removedp))))
(defmethod awl::to-association-list ((dictionary awl::alist))
  (slot-value dictionary 'alist))


;;;;* Unification of Objects in S-Expressions

(define-condition awl::unification-failure (error)
  ((expression1 :initarg :expr1 :reader awl::unification-failure-expr1)
   (expression2 :initarg :expr2 :reader awl::unification-failure-expr2))
  (:report (lambda (condition stream)
             (format stream "Could not unify ~A and ~A."
                     (awl::unification-failure-expr1 condition)
                     (awl::unification-failure-expr2 condition)))))

;;;;** Unify

(defun awl::substitute-variables (bindings expression)
  "Return an expression based on EXPR but in which all (sub-)expressions of EXPR bound to a value in BINDINGS are recursively substituted by their value, until only unbound (terminal) expressions are left. This function may destructively modify EXPR."
  (multiple-value-bind (binding-value boundp)
      (awl::lookup bindings expression)
    (cond (boundp (awl::substitute-variables bindings binding-value))
          ((and bindings (consp expression))
           (cons (awl::substitute-variables bindings (car expr))
                 (awl::substitute-variables bindings (cdr expr))))
          (t expr))))


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
               (let ((var-binding (get-binding var bindings))
                     (expr-binding (get-binding expr bindings)))
                 (cond (var-binding (unify (binding-val var-binding) expr bindings))
                       (expr-binding (unify var (binding-val expr-binding) bindings))
                       ((occurs-p var expr bindings) (fail var expr bindings))
                       (t (add-binding var expr bindings)))))
             (occurs-p (var expr bindings)
               "Return true if VAR occurs in the value associated in BINDINGS to the expression EXPR with respect to EQUAL-P."
               (let ((var-binding (get-binding var bindings)))
                 (cond ((equal-p var expr) t)
                       (var-binding (occurs-p var val bindings))
                       ((consp expr) (or (occurs-p var (car expr) bindings)
                                         (occurs-p var (cdr expr) bindings))))))
             (substitute-variables (expr bindings)
               "Return an expression based on EXPR but in which all (sub-)expressions of EXPR bound to a value in BINDINGS are recursively substituted by their value, until only unbound (terminal) expressions are left. This function may destructively modify EXPR."
               (let ((expr-binding (get-binding expr bindings)))
                 (cond (expr-binding (substitute-variables (binding-val expr-binding) bindings))
                       ((and bindings (consp expr))
                        (cons (substitute-variables (car expr) bindings)
                              (substitute-variables (cdr expr) bindings)))
                       (t expr)))))
      (values (let ((bindings (unify expr1 expr2 bindings)))
                (dolist (binding bindings bindings)
                  (setf (cdr binding) (substitute-variables (cdr binding) bindings))))
              equal-p))))

;;;;* Unify
(declaim (inline awl::unify))
(defun awl::unify (expr1 expr2 variablep equal-p &optional bindings)
  "See if the s-expressions EXPR1 and EXPR2 match, where as variables are used objects which satisfy VARIABLEP and which are compared for equality using EQUAL-P. If the s-expressions match, return an association list of bindings of the variables to their constant values, else signal an error of type AWL::UNIFICATION-FAILURE."
  (unify expr1 expr2 variablep equal-p (awl::to-association-list bindings)))


(do-symbols (symbol "AWL")
  (export symbol "AWL"))

