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


(defstruct (ruleset (:constructor %make-ruleset))
  parameters
  rules)
(defvar *rulesets* (make-hash-table))

(defun parse-rules (rules)
  (let (rule-classes)
    (dolist (rule rules rule-classes)
      (let ((type (pop rule))
            (rule (cons (pop rule)
                        (pop rule)))
            (tags (or rule (list nil))))
        (let ((rules (ecase type
                       (--> (list rule))
                       (<-> (list rule (cons (cdr rule) (car rule)))))))
          (dolist (tag tags)
            (let ((class (assoc tag rule-classes)))
              (if class
                  (dolist (rule rules)
                    (push rule (cdr class)))
                  (push (cons tag rules) rule-classes)))))))))

(defun make-ruleset (parameters rules)
  (%make-ruleset :parameters parameters :rules (parse-rules rules)))
(defun register-ruleset (name ruleset)
  (declare (type (or null ruleset) ruleset))
  (if ruleset
      (setf (gethash name *rulesets*) ruleset)
      (remhash name *rulesets*)))


(defmacro define-ruleset ((name &rest parameters) &body rules-with-tags)
  `(register-ruleset ',name (make-ruleset ',parameters ',rules-with-tags)))




(defun find-ruleset (ruleset)
  (etypecase ruleset
    (ruleset ruleset)
    ((and symbol (not null)) (gethash ruleset *rulesets*))))



(defun extract-rules (ruleset tag &rest parameters
                      &aux (ruleset (find-ruleset ruleset)))
  (etypecase ruleset
    (ruleset (mapcar (let ((bindings (delete-if (lambda (x)
                                                  (funcall awl::*equal-p* (car x) (cdr x)))
                                                (loop :for param :in (ruleset-parameters ruleset)
                                                      :collect (cons param (or (pop parameters)
                                                                               (error "Missing parameter ~S." param)))))))
                       (lambda (rule)
                         (cons (substitute-variables bindings (car rule))
                               (substitute-variables bindings (cdr rule)))))
                     (cdr (assoc tag (ruleset-rules ruleset)))))))

(defun find-rules (ruleset tag &rest parameters)
  (declare (dynamic-extent parameters))
  (let ((ruleset (find-ruleset ruleset)))
    (apply #'extract-rules ruleset tag parameters)))


(defun emapcar (fn list &rest more-lists)
  (if more-lists
      (do* (result
            (lists (cons list more-lists))
            (args (maplist (lambda (cons)
                             (when (consp (car cons))
                               (pop (car cons))))
                           lists)
                  (maplist (lambda (cons)
                             (when (consp (car cons))
                               (pop (car cons))))
                           lists)))
           ((every #'null args) (nreverse result))
        (push (apply fn args) result))
      (mapcar* fn list)))
(defun mapcar* (fn list &rest more-lists)
  (do (result
       (lists (cons list more-lists)))
      ((not (every #'consp lists)) (nreverse result))
    (push (apply fn (maplist (lambda (cons)
                                 (pop (car cons)))
                               lists))
          result)))

(defun elength (expression &optional (n 0))
  (if (consp expression)
      (elength (cdr expression) (1+ n))
      n))

(defun expression-structure (expression)
  (when (consp expression)
    (cons (1- (elength expression))
          (apply #'emapcar (lambda (&rest args)
                             (apply '+ (delete nil args)))
                 (mapcar* #'expression-structure expression)))))

(defun expression-structure-distance (expr-struct1 expr-struct2)
  (reduce '+ (mapcar (lambda (x)
                       (* x x))
                     (emapcar (lambda (x y)
                                (- (or x 0)
                                   (or y 0)))
                              expr-struct1
                              expr-struct2))))
(defun expression-distance (expr1 expr2)
  (expression-structure-distance (expression-structure expr1)
                                 (expression-structure expr2)))

(defvar *max-search-steps* 1000)
(defvar *max-search-depth* 4)
(defvar *target*)
(defun reduce-expression (expr strategy &rest rulesets-parameters
                          &aux
                            (targets (mapcar 'second strategy))
                            (strategy (mapcar 'first strategy))
                            (rules (mapcar (lambda (tag)
                                             (mapcan (lambda (ruleset-parameters)
                                                       (apply #'extract-rules
                                                              (first ruleset-parameters)
                                                              tag
                                                              (rest ruleset-parameters)))
                                                     rulesets-parameters))
                                           strategy))
                            (parameters (delete-if 'integerp (remove-duplicates (mapcan #'rest rulesets-parameters)))))
  (let ((awl::*acceptable-p* (lambda (var expr)
                               (declare (ignore var))
                               (not (member expr parameters)))))
    (let ((search-fn (awl::a*-search-factory (lambda (node neighbor)
                                               (+ 1
                                                  (- (depth neighbor)
                                                     (depth node))))
                                             (lambda (expr)
                                               (* 5 (depth expr)))
                                             (lambda (expr)
                                               (declare (special rules))
                                               (remove-duplicates (remove-if (lambda (x)
                                                                               (< *max-search-depth* (depth x)))
                                                                             (mapcan (lambda (rule)
                                                                                       (apply-rule rule expr))
                                                                                     rules))
                                                                  :test #'equalp))
                                             #'equalp)))
      (let ((step-count 0) costs paths foundps)
        (dolist (rules rules (values (nreverse paths)
                                     (nreverse costs)
                                     (nreverse foundps)
                                     step-count))
          (declare (special rules))
          (let ((*target* (pop targets)))
           (multiple-value-bind (path cost foundp)
               (funcall search-fn expr (lambda (node)
                                         (declare (ignorable node))
                                         (incf step-count)
                                         (or (< *max-search-steps* step-count)
                                             (when (and (boundp '*target*) *target*)
                                               (or (ignore-errors (unify node *target*))
                                                   (equalp node *target*))))))
             (push path paths)
             (push cost costs)
             (push foundp foundps)
             (setf expr (first (last path))))))))))


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

(defvar *rules*)
#+(or)
(defun do-actions (actions expresion
                   &aux (actions (typecase actions
                                   (cons actions)
                                   (t (list actions)))))
  )
#+(or)
(defmacro with-rulesets ((&rest rulesets-parameters) &body body)
  )



(define-ruleset (associativity *)
  (--> (* a?) a? simplify)
  (--> a? (* a?))
  (--> (* a? (* b? . c?))
       (* a? b? . c?) simplify)
  (--> (* a? b? (* c? . d?))
       (* a? b? c? . d?) simplify)
  (--> (* a? b? c? . d?)
       (* a? b? (* c? . d?)) simplify)
  (--> (* a? b? . c?)
       (* a? (* b? . c?)) simplify)
  (--> (* (* a? b?) . c?)
       (* a? b? . c?) simplify)
  (--> (* a? b? . c?)
       (* (* a? b?) . c?) simplify))
(define-ruleset (neutral-element * 1)
  (--> (* 1 a?) a? simplify)
  (--> a? (* 1 a?))
  (--> (* a? 1) a? simplify)
  (--> a? (* a? 1))
  (--> (*) 1 simplify))
(define-ruleset (inverse-element * / 1)
  (--> (* (/ a?) a?) 1 simplify)
  (--> (* a? (/ a?)) 1 simplify))

(define-ruleset (opposite-operation * /)
  (--> (* a? (/ (* b? . c?))) (/ a? b? . c?) simplify)
  (--> (/ a? b? . c?) (* a? (/ (* b? . c?))) simplify expand))

(define-ruleset (commutativity +)
  (<-> (+ a? b?) (+ b? a?)))

(define-ruleset (absorbing-element * 0)
  (--> (* 0 a?) 0 simplify)
  (--> (* a? 0) 0 simplify))
(define-ruleset (distributivity * +)
  (--> (* a? (+ b? c?))
       (+ (* a? b?) (* a? c?)) expand)
  (--> (+ (* a? b?) (* a? c?))
       (* a? (+ b? c?)) factorize)
  (--> (* (+ b? c?) a?)
       (+ (* b? a?) (* c? a?)) expand)
  (--> (+ (* b? a?) (* c? a?))
       (* (+ b? c?) a?) factorize))


(define-ruleset (opposite-action * - 1)
  (--> (* (- 1) a?) (- a?) simplify)
  (--> (- a?) (* (- 1) a?))
  (--> (* a? (- 1)) (- a?) simplify)
  (--> (- a?) (* a? (- 1))))





(do-symbols (symbol "AWL")
  (export symbol "AWL"))


