;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Unification of Objects in S-Expressions

(define-condition awl::unification-failure (error)
  ((expression1 :initarg :expr1 :reader awl::unification-failure-expr1)
   (expression2 :initarg :expr2 :reader awl::unification-failure-expr2))
  (:report (lambda (condition stream)
             (format stream "Could not unify ~A and ~A."
                     (awl::unification-failure-expr1 condition)
                     (awl::unification-failure-expr2 condition)))))

(defvar awl::*variable-p* (lambda (x)
			    (and (symbolp x)
				 (eql #\? (let ((name (symbol-name x)))
					    (elt name (1- (length name))))))))
(defvar awl::*equal-p* #'eql)

(defvar awl::*acceptable-p* (lambda (var expr)
                              (declare (ignore var expr))
                              t))

(defvar awl::*do-occurs-check* t)

(defmacro when-bound ((var &optional (key var)) alist &body body)
  "When KEY appears in ALIST and the associated value is not awl:*equal-p* to KEY, lexically bind VAR to the associated value."
  `(let ((association (assoc ,key ,alist :test awl::*equal-p*)))
     (when association
       (let ((,var (cdr association)))
         (unless (funcall awl::*equal-p* (car association) ,var)
           ,@body)))))

(defun substitute-variables (bindings expression &key (variable-p awl::*variable-p*)
						   (equal-p awl::*equal-p*))
  "Return an expression based on EXPRESSION but in which all (sub-)expressions of EXPRESSIONS bound to a value in BINDINGS are recursively substituted by their value, until only unbound (terminal) expressions are left. "
  (flet ((variable-p (x) (funcall variable-p x)))
    (let ((awl::*variable-p* variable-p)
	  (awl::*equal-p* equal-p))
      (labels ((substitute-variables (expression)
                 (first (or (when-bound (expression)
                                        bindings
                              (list (substitute-variables expression)))
                            (when (and (consp expression)
                                       (not (variable-p expression)))
                              (list (cons (substitute-variables (car expression))
                                          (substitute-variables (cdr expression)))))
                            (list (copy-tree expression))))))
        (substitute-variables expression)))))

(defun unify (expr1 expr2 &key
			    (variable-p awl::*variable-p*)
			    (equal-p awl::*equal-p*)
                            (acceptable-p awl::*acceptable-p*))
  "See if the s-expressions EXPR1 and EXPR2 match, where as variables are used objects which satisfy VARIABLE-P and which are compared for equality using EQUAL-P. If the s-expressions match, return an association list of bindings of the variables to their constant values, else signal an error of type AWL::UNIFICATION-FAILURE."
  (flet ((variable-p (x) (funcall variable-p x))
	 (equal-p (x y) (funcall equal-p x y))
         (acceptable-p (var expr) (funcall acceptable-p var expr))
	 (fail (expr1 expr2)
	   (error 'awl::unification-failure :expr1 expr1 :expr2 expr2)))
    (declare (inline variable-p equal-p))
    (labels ((unify (expr1 expr2 bindings)
	       (cond ((equal-p expr1 expr2) bindings)
		     ((variable-p expr1) (unify-var expr1 expr2 bindings))
		     ((variable-p expr2) (unify-var expr2 expr1 bindings))
		     ((and (consp expr1)
			   (consp expr2))
		      (unify (rest expr1) (rest expr2)
			     (unify (first expr1) (first expr2) bindings)))
		     (t (fail expr1 expr2))))
	     (unify-var (var expr bindings)
	       "Unify VAR and EXPR in the case where VAR is a variable according to VARIABLE-P and return the extended binding."
	       (or (when-bound (var)
                       bindings
                     (unify var expr bindings))
		   (when-bound (expr)
                       bindings
                     (unify var expr bindings))
		   (when (and awl::*do-occurs-check*
                              (occurs-p var expr bindings))
		     (fail var expr))
                   (if (acceptable-p var expr)
                       (cons (cons var expr) bindings)
                       (fail var expr))))
	     (occurs-p (var expr bindings)
	       "Return true if VAR occurs in the value associated in BINDINGS to the expression EXPR with respect to EQUAL-P."
	       (or (equal-p var expr)
		   (when-bound (expr)
		       bindings
		     (occurs-p var expr bindings))
		   (when (and (consp expr)
			      (not (variable-p expr)))
		     (or (occurs-p var (car expr) bindings)
			 (occurs-p var (cdr expr) bindings))))))
      (values (let ((bindings (unify expr1 expr2 nil)))
		(dolist (binding bindings bindings)
		  (rplacd binding (substitute-variables bindings (cdr binding)))))
	      variable-p
	      equal-p))))


(defun apply-rule (rule expr &aux (expr (list expr)))
  (let ((mask (car rule))
        (template (cdr rule)))
    (labels ((substitute-subexpression (pointer)
               (let ((subexpr (car pointer)))
                 (concatenate 'list
                              (let ((bindings (ignore-errors (unify mask subexpr))))
                                (when bindings
                                  (rplaca pointer (substitute-variables bindings template))
                                  (prog1
                                      (copy-tree expr)
                                    (rplaca pointer subexpr))))
                              (when (consp subexpr)
                                (mapcon #'substitute-subexpression subexpr))))))
      (substitute-subexpression expr))))


(defun format-bindings (destination bindings)
  (format destination "~:{ ~S <- ~S~}" (mapcar (lambda (x) (list (car x) (cdr x))) bindings)))



(defun depth (expression)
  "Return the nesting depth of EXPRESSION."
  (labels ((depth (expr n)
             (if (consp expr)
                 (max (depth (car expr) (1+ n))
                      (depth (cdr expr) n))
                 n)))
    (depth expression 0)))


(defclass ruleset ()
  ((parameters :initform nil :initarg :parameters)
   (rules :initform nil)))
(defvar *rulesets* (make-hash-table))

(defmethod initialize-instance :after ((ruleset ruleset) &key rules)
  (setf (slot-value ruleset 'rules)
        (mapcan (lambda (rule)
                  (let ((type (first rule))
                        (rule (cons (first (rest rule))
                                    (second (rest rule)))))
                    (ecase type
                      (--> (list rule))
                      (<-> (list rule (cons (cdr rule) (car rule)))))))
                rules)))

(defun make-ruleset (parameters rules)
  (make-instance 'ruleset :parameters parameters :rules rules))
(defun register-ruleset (name ruleset)
  (declare (type (or null ruleset) ruleset))
  (if ruleset
      (setf (gethash name *rulesets*) ruleset)
      (remhash name *rulesets*)))


(defmacro define-ruleset ((name &rest parameters) &body rules)
  `(register-ruleset ',name (make-ruleset ',parameters ',rules)))




(defun find-ruleset (ruleset)
  (etypecase ruleset
    (ruleset ruleset)
    ((and symbol (not null)) (gethash ruleset *rulesets*))))

(defun extract-rules (ruleset &rest parameters
                      &aux (ruleset (find-ruleset ruleset)))
  (etypecase ruleset
    (ruleset (mapcar (let ((bindings (delete-if (lambda (x)
                                                  (funcall awl::*equal-p* (car x) (cdr x)))
                                                (loop :for param :in (slot-value ruleset 'parameters)
                                                      :collect (cons param (or (pop parameters)
                                                                               (error "Missing parameter ~S." param)))))))
                       (lambda (rule)
                         (cons (substitute-variables bindings (car rule))
                               (substitute-variables bindings (cdr rule)))))
                     (slot-value ruleset 'rules)))))

(defun find-rules (ruleset &rest parameters)
  (let ((ruleset (etypecase ruleset
                   (ruleset ruleset)
                   (symbol (gethash ruleset *rulesets* ruleset)))))
    (etypecase ruleset
      (ruleset (mapcar (let ((bindings (delete-if (lambda (x)
                                                    (funcall awl::*equal-p* (car x) (cdr x)))
                                                  (loop :for param :in (slot-value ruleset 'parameters)
                                                        :collect (cons param (or (pop parameters)
                                                                                 (error "Missing parameter ~S." param)))))))
                         (lambda (rule)
                           (cons (substitute-variables bindings (car rule))
                                 (substitute-variables bindings (cdr rule)))))
                       (slot-value ruleset 'rules))))))

(defvar *max-search-steps* 1000)
(defvar *max-search-depth* 4)
(defun reduce-expression (expr target &rest rulesets-parameters
                          &aux (rules (mapcan (lambda (ruleset-parameters)
                                                (apply #'extract-rules ruleset-parameters))
                                              rulesets-parameters))
                            (parameters (remove-duplicates (mapcan #'rest rulesets-parameters))))
  (let ((awl::*acceptable-p* (lambda (var expr)
                               (declare (ignore var))
                               (not (member expr parameters)))))
    (let ((search-fn (awl::a*-search-factory (lambda (node neighbor)
                                               (1+ (- (depth neighbor)
                                                      (depth node))))
                                             (lambda (expr)
                                               (depth expr))
                                             (lambda (expr)
                                                (remove-duplicates (remove-if (lambda (x)
                                                                                (< *max-search-depth* (depth x)))
                                                                              (mapcan (lambda (rule)
                                                                                        (apply-rule rule expr))
                                                                                      rules))
                                                                   :test #'equalp))
                                             #'equalp)))
      (let ((step-count 0))
        (values-list (concatenate 'list
                                  (multiple-value-list (funcall search-fn expr (lambda (node)
                                                                                 (incf step-count)
                                                                                 (or (< *max-search-steps* step-count)
                                                                                     (ignore-errors (unify node target))))))
                                  (list step-count)))))))


(defun apply-rulesets (expr &rest rulesets-parameters
                       &aux (rules (mapcan (lambda (ruleset-parameters)
                                             (apply #'extract-rules ruleset-parameters))
                                           rulesets-parameters))
                         (parameters (remove-duplicates (mapcan #'rest rulesets-parameters))))
  (let ((awl::*acceptable-p* (lambda (var expr)
                               (declare (ignore var))
                               (not (member expr parameters)))))
    (remove-duplicates (mapcan (lambda (rule)
                                 (apply-rule rule expr))
                               rules)
                       :test #'equalp)))




(define-ruleset (associativity *)
  (<-> (* a?) a?)
  (<-> (* a? (* b? . c?))
       (* a? b? . c?))
  (<-> (* a? b? . c?)
       (* (* a? b?) . c?)))
(define-ruleset (neutral-element * 1)
  (<-> (* 1 a?) a?)
  (<-> (* a? 1) a?))
(define-ruleset (inverse-element * / 1)
  (<-> (* (/ a?) a?) 1)
  (<-> (* a? (/ a?)) 1))
(define-ruleset (commutativity +)
  (<-> (+ a? b?) (+ b? a?)))

(define-ruleset (absorbing-element * 0)
  (<-> (* 0 a?) 0)
  (<-> (* a? 0) 0))
(define-ruleset (distributivity * +)
  (<-> (* a? (+ b? c?))
       (+ (* a? b?) (* a? c?)))
  (<-> (* (+ b? c?) . a?)
       (+ (* b? . a?) (* c? . a?))))








(do-symbols (symbol "AWL")
  (export symbol "AWL"))


