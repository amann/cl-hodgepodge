;;;; $Source: /home/remote/oam/CVSrepository/lisp/fsl/functions.lisp,v $
;;;; $Revision: 1.21 $
;;;;****************************************************************************
(load #p"clos-ext")
(in-package :cl)
(defpackage :fsl.functions
  (:use :cl
	:ch.amann-wolowyk.clos-ext)
  (:export #:element-p))

(in-package :fsl.functions)



;;;abstract class
(defclass subset ()
  ()
  (:metaclass abstract-class))

(defclass real-set (subset)
  ()
  (:metaclass abstract-class))

(defclass singleton (real-set)
  (($point :initarg :point)))
(defclass left-bounded (real-set)
  (($left-bound :initarg :left-bound)))
(defclass left-bounded* (left-bounded)
  ())
(defclass right-bounded (real-set)
  (($right-bound :initarg :right-bound)))
(defclass right-bounded* (right-bounded)
  ())
(defclass interval   (left-bounded right-bounded)
  ())
(defclass interval*- (interval left-bounded* right-bounded)
  ())
(defclass interval-* (interval left-bounded right-bounded*)
  ())
(defclass interval** (interval left-bounded* right-bounded*)
  ())
(defmethod initialize-instance :after ((instance interval) &key)
  (with-slots (left-bound right-bound)
      instance
    (cond
      ((= left-bound right-bound)
       (change-class instance 'singleton :point left-bound))
      ((< right-bound left-bound)
       (error "Trying to define an empty set as an interval: left-bound = ~A > ~A = right-bound"
	      left-bound right-bound)))))

(define-method-combination :and :identity-with-one-argument t :operator and) 
(defgeneric element-p (set object)
  (:method-combination :and))
(defmethod element-p :and ((set (eql nil)) (o t))
  nil)
(defmethod element-p :around ((set subset) (o t))
  (let ((result (call-next-method)))
    result))
(defmethod element-p :and ((set subset) (o t))
  t)
(defmethod element-p :and ((set real-set) (o real))
  t)
(defmethod element-p :and ((set left-bounded) (o real))
  (<= (slot-value set '$left-bound) o))
(defmethod element-p :and ((set left-bounded*) (o real))
  (/= (slot-value set '$left-bound) o))
(defmethod element-p :and ((set right-bounded) (o real))
  (<= o (slot-value set '$right-bound)))
(defmethod element-p :and ((set right-bounded*) (o real))
  (/= o (slot-value set '$right-bound)))
(defmethod element-p :and ((set singleton) (o real))
  (= (slot-value set '$point) o))


(defgeneric intersect (set-a set-b))
(defmethod intersect ((set-a singleton) (set-b real-set))
  (when (element-p set-b (slot-value set-a '$point))
    set-a))
(defmethod intersect ((set-a real-set) (set-b singleton))
  (intersect set-b set-a))
(defmethod intersect ((set-a right-bounded) (set-b right-bounded))
  (if (element-p set-b (slot-value set-a '$right-bound))
      set-a
      set-b))
(defmethod intersect ((set-a left-bounded) (set-b left-bounded))
  (if (element-p set-b (slot-value set-a '$left-bound))
      set-a
      set-b))
(defmethod intersect ((set-a right-bounded) (set-b left-bounded))
  (let ((right-bound (slot-value set-a '$right-bound))
	(left-bound (slot-value set-b '$left-bound)))
    (when (and (element-p set-b right-bound)
	       (element-p set-a left-bound))
      (make-instance (car (intersection
			   (clos-ext::class-direct-subclasses (class-of set-a))
			   (clos-ext::class-direct-subclasses (class-of set-b))))
		     :left-bound left-bound
		     :right-bound right-bound))))
(defmethod intersect ((set-a left-bounded) (set-b right-bounded))
  (intersect set-b set-a))
(defmethod intersect ((set-a interval) (set-b right-bounded))
  (let* ((class-a (class-of set-a))
	 (l-b (make-instance (first (clos-ext::class-direct-superclasses class-a))
			     :left-bound (slot-value set-a '$left-bound)))
	 (r-b (make-instance (second (clos-ext::class-direct-superclasses class-a))
			     :right-bound (slot-value set-a '$right-bound))))
    (intersect l-b (intersect r-b set-b))))
(defmethod intersect ((set-a interval) (set-b left-bounded))
  (let* ((class-a (class-of set-a))
	 (l-b (make-instance (first (clos-ext::class-direct-superclasses class-a))
			     :left-bound (slot-value set-a '$left-bound)))
	 (r-b (make-instance (second (clos-ext::class-direct-superclasses class-a))
			     :right-bound (slot-value set-a '$right-bound))))
    (intersect r-b (intersect l-b set-b))))
(defmethod intersect ((set-a right-bounded) (set-b interval))
  (intersect set-b set-a))
(defmethod intersect ((set-a left-bounded) (set-b interval))
  (intersect set-b set-a))


(defgeneric unify (set-a set-b))
(defgeneric sub (set-a set-b))



(defgeneric compare (pre-order o1 o2)
  (:documentation "Return T if `o1' is strictly anterior to `o2' according the operator `pre-order' and . `pre-order' defines pre-order relation on the types of `o1' and `o2', i.e. a relation which is anti symetric and transitive, but not reflexive."))


(defgeneric make-iterator (iterable-object))
(defmethod make-iterator ((object simple-vector))
  (let ((idx 0)
	(max (length object)))
    (list #'(lambda ()
	      (< idx max))
	  #'(lambda ()
	      (prog1 (svref object idx)
		(incf idx)))
	  #'(lambda ()
	      (- max idx 1)))))
(defmethod make-iterator ((object list))
  (let ((pos object)
	(rest-length (length object)))
    (list #'(lambda ()
	      (cdr pos))
	  #'(lambda ()
	      (decf rest-length)
	      (car (setq pos (cdr pos))))
	  #'(lambda ()
	      rest-length))))

(defmacro defiterator ((object type) (&rest aux)
		       &key has-next get-next get-length remove)
  `(defmethod make-iterator ((,object ,type))
    (let ,aux
      (values (lambda ()
                ,(or has-next (error "HAS-NEXT method needs to be implemented.")))
              (lambda ()
                ,(or get-next (error "GET-NEXT method needs to be implemented.")))
              (lambda ()
                ,(or get-length `(error "GET-LENGTH method is not implemented for type ~A." ,type)))
              (lambda ()
                ,(or remove `(error "REMOVE method is not implemented for type ~A." ,type)))))))

(defmacro with-iterators ((&rest variables-iterable-objects) &body body)
  (let* ((variables-values (mapcar (lambda (x)
			      (list (car x) (cadr x)))
			    variables-iterable-objects))
	 (variables (mapcar #'car variables-iterable-objects))
	 (iterators (gensym)))
    `(let* (,@variables-values
	    (,iterators (mapcar (lambda (x)
				 (cons x (make-iterator x)))
			       (list ,@variables))))
      (labels
	   ((has-next (o)
	      (funcall (second (assoc o ,iterators))))
	    (get-next (o)
	      (funcall (third (assoc o ,iterators))))
	    (get-length (o)
	      (funcall (fourth (assoc o ,iterators)))))
	,@body))))



(defclass partition-table ()
  (($table :type table-entry :initarg :table)
   ($comparator :allocation :class)
   ($partition-class-combinator :allocation :class))
  (:documentation "This class represents an association table where the association \"key\" is not chosen by an equality relation but by an \"is element\" or \"satisfies condition\" relation. For the concept to work, the keys should form a partition of the whole set of possibilities."))

(defclass)

(defun make-partition-table (list-of-intervals-values)
  ())

(defmethod make-iterator ((object partition-table))
  (make-iterator (slot-value object '$table)))

(defmethod compare ((operator (eql '<)) (a right-bounded) (b right-bounded))
  (< (slot-value a '$right-bound)
     (slot-value b '$right-bound)))
(defmethod compare ((operator (eql '<)) (a right-bounded*) (b right-bounded))
  (<= (slot-value a '$right-bound)
      (slot-value b '$right-bound)))

(defclass table-entry ()
  (($partition-class :initarg :partition-class)
   ($value :initarg :value)
   ))

(defmethod combine ((operator t) (entry-1 table-entry) (entry-2 table-entry))
  (make-instance 'table-entry
		 :interval (intersect (slot-value entry-1 '$interval)
				      (slot-value entry-2 '$interval))
		 :value (combine operator
				 (slot-value entry-1 '$value)
				 (slot-value entry-2 '$value))))

(defmethod combine ((operator t) (part-1 partition-table) (part-2 partition-table))
  (with-iterators ((iter-1 part-1)
		   (iter-2 part-2))
    (do* ((new-partition (make-array '(1) :adjustable t :fill-pointer 0))
	  (p-1 (get-next iter-1) (if p-2<p-1
				     p-1
				     (get-next iter-1)))
	  (p-2 (get-next iter-2) (if p-1<p-2
				     p-2
				     (get-next iter-2)))
	  (p-2<p-1 nil (and (compare '< p-2 p-1)
			    (not (has-next iter-1))))
	  (p-1<p-2 nil (and (compare '< p-1 p-2)
			    (not (has-next iter-2)))))
	 ((not (or (has-next iter-1)
		   (has-next iter-2)))
	  (make-instance 'partition-table :table new-partition))
      (vector-push-extend (combine operator p-1 p-2)))))









;;;;==================================================================================


(oam:define-project-package "CH.AMANN-WOLOWYK.FUNCTIONS" "FN")
(defpackage "CH.AMANN-WOLOWYK.FUNCTIONS.$"
  (:nicknames "$")
  (:use))
(defconstant $::not-a-number 'fn::not-a-number
  "Symbol signifying a result of a \"generalized function\" which is not a number.")

(defclass fn::distribution-metaclass (c2mop:funcallable-standard-class oam-clos:standard-cached-class) ())

(defmethod c2mop:validate-superclass ((class fn::distribution-metaclass) (superclass fn::function)) t)

(defclass fn::distribution (c2mop:funcallable-standard-object) ())
(defclass fn::dirac (fn::distribution)
  (($impulse :type real :initarg :delay :reader fn::delay))
  (:metaclass c2mop:funcallable-standard-object))
(defmethod initialize-instance ((self fn::dirac) &key delay &allow-other-keys)
  (c2mop:set-funcallable-instance-function self (lambda (x)
                                                  (if (= x delay)
                                                      fn::not-a-number
                                                      0))))
(setf (symbol-function '$::dirac) (make-instance 'fn::dirac :delay 0)
      (documentation '$::dirac 'function) "Dirac distribution.")
(defclass fn::function (fn::distribution)
  ())
(defclass fn::real-function (fn::function)
  ())
(defclass fn::measurable-function (fn::real-function fn::distribution) ())
(defclass fn::jump-function (fn::measurable-function)
  ((default-value :initarg :default-value :reader default-value)
   (partition :initarg :partition :reader partition)))



;;;;------------------------------------------------------
(defclass fn::elementary-function (fn::real-function) ()
  (($name :type symbol :reader fn::name))
  (:metaclass fn::function-metaclass))
(defmethod initialize-instance :before ((self fn::elementary-function) &key function)
  (assert (= 1 (length (oam:function-arg-list function))) ()
          "The function must be a function in one argument."))
(defmethod initialize-instance :after ((self fn::elementary-function) &key name function documentation &allow-other-keys
                                       &aux (name (intern (string name) "$")))
  (setf (slot-value self '$name) name)
  (c2mop:set-funcallable-instance-function self function)
  (setf (symbol-function name) self
        (documentation name 'function) documentation))

(map nil (lambda (fct)
           (apply #'make-instance 'fn::elementary-function
                  (mapcan (lambda (key value)
                            '(:name :documentation :function)
                            fct))))
     `((exp "Exponential function." ,#'exp)
       (sin "Sinus function." ,#'sin)
       (cos "Cosinus function." ,#'cos)
       (sinh "Sinus hyperbolicus function." ,#'sinh)
       (cosh "Cosinus hyperbolicus function." ,#'cosh)
       (log "Logarithmus function." ,(lambda (x) (log x)))
       (identity "Identity function x |-> x." ,#'identity)))
;;;;------------------------------------------------------

(defclass fn::characteristic-function (fn::real-function) ()
  (:metaclass fn::function-metaclass))
(defmethod initialize-instance :after ((self fn::characteristic-function) &key predicate &allow-other-keys)
  (c2mop:set-funcallable-instance-function self (compile nil `(lambda (x)
                                                                ,@(etypecase predicate
                                                                    (function `((if (funcall ,predicate x)
                                                                                    1
                                                                                    0)))
                                                                    (null) '((declare (ignore x))
                                                                             0)
                                                                    ((t) '((declare (ignore x))
                                                                           1)))))))

(setf (symbol-function '$::zero-fn) (make-instance 'fn::characteristic-function :predicate nil)
      (documentation '$::zero-fn 'function) "Function constantly zero (0)."
      (symbol-function '$::one-fn) (make-instance 'fn::characteristic-function :predicate t)
      (documentation '$::one-fn 'function) "Function constantly one (1).")







(defclass fn::polynomial-function (fn::real-function)
  (($coefficients :type list :initarg :coefficients :reader coefficients))
  (:documentation "coefficients: '(a0 a1 a2 ... an) --> a0 + a1 * x + ... + an * x^n")
  (:metaclass fn::function-metaclass))
(defmethod initialize-instance :after ((self fn::polynomial-function) &key coefficients &allow-other-keys)
  (setf (slot-value self '$coefficients) (coerce coefficients 'simple-vector))
  (c2mop:set-funcallable-instance-function self
                                           (lambda (x)
                                             (reduce (lambda (r c) (+ c (* r x)))
                                                     coefficients :from-end t))))

(defclass fn::power-function (fn::real-function)
  (($power :type real :initarg :power :reader fn::power))
  (:documentation "x^r")
  (:metaclass fn::function-metaclass))
(defmethod initialize-instance :after ((self fn::power-function) &key power &allow-other-keys)
  (c2mop:set-funcallable-instance-function self
                                           (lambda (x) (expt x power))))


(defclass fn::expression (fn::function)
  (($expression :type cons :initarg :expression :reader fn::expression))
  (:metaclass c2mop:funcallable-standard-object))
(defmethod initialize-instance :after ((self fn::expression) &key expression &allow-other-keys)
  (c2mop:set-funcallable-instance-function self
                                           (lambda (x)
                                             (funcall-expression expression x))))


;;;;------------------------------------------------------

(defgeneric unary-operation (unary-operator operand)
  (:documentation "Return the result by applying the unary-operator to operand."))
(defgeneric binary-operation (binary-operator operand1 operand2)
  (:documentation "Return the result by applying the binary-operator to operand1 and operand2."))




(defgeneric fn::neutral-element (operator)
  (:documentation "Return the neutral element of the operator."))
(defmethod fn::neutral-element ((operator (eql 'fn::addition)))
  #'$::zero-fn)
(defmethod fn::neutral-element ((operator (eql 'fn::multiplication)))
  #'$::one-fn)
(defmethod fn::neutral-element ((operator (eql 'fn::composition)))
  #'$::identity)
(defmethod fn::neutral-element ((operator (eql 'fn::convolution)))
  #'$::dirac)



(defun fn::compose* (operator operands &key from-end)
  (reduce (oam::partev #'binary-operator operator) operands
          :from-end from-end :initial-value (neutral-element operator)))
(defun fn::compose (operator &rest operands)
  (operate* operator operands))




(flet ((make-sequence-iterator (sequence)
         (let ((size (length sequence))
               (i -1))
           (lambda ()
             (if (< (incf i) size)
                 (elt sequence i)
                 (error 'oam::no-next-element-condition))))))
  (defun merge-partitions (preorder-pred operator def-val1 partition1 def-val2 partition2)
    (oam:fbind ((next1 (make-sequence-iterator partition1))
                (next2 (make-sequence-iterator partition2))
                ($< preorder-pred)
                ($* (oam::partev #'binary-operation operator)))
      (values ($* def-val1 def-val2)
              (flet ((next-merged ()
                       (declare (special prev-fn1 prev-fn2 point1 point2))
                       (destructuring-bind ((p1 v1 . fn1) (p2 v2 . fn2))
                           (list point1 point2)
                         (cond
                           (($< p1 p2) (setq prev-fn1 fn1 point1 (next1))
                            (p1 ($* v1 (funcall prev-fn2 p1)) . ($* fn1 prev-fn2)))
                           (($< p2 p1) (setq prev-fn2 fn2 point2 (next2))
                            (p2 ($* (funcall prev-fn1 p2) v2) . ($* prev-fn1 fn2)))
                           (t (setq prev-fn1 fn1 prev-fn2 fn2
                                    point1 (next1) point2 (next2))
                              (p1 ($* v1 v2) . ($* fn1 fn2)))))))
                (let ((prev-fn1 def-val1)
                      (prev-fn2 def-val2)
                      (point1 (next1))
                      (point2 (next2))
                      result)
                  (declare (special prev-fn1 prev-fn2 point1 point2))
                  (handler-case
                      (loop (push (next-merged) result))
                    (oam::no-next-element-condition ()
                      (vector (nreverse result))))))))))



(defmethod binary-operation (operator (fn1 fn::jump-function) (fn2 fn::jump-function))
  (multiple-value-bind (default-value partition)
      (merge-partitions #'< operator (default-value fn1) (partition fn1) (default-value fn2) (partition fn2))
    (make-instance 'fn::jump-function :default-value default-value :partition partition)))

(defmethod binary-operation (operator (fn1 fn::jump-function) (fn2 fn::)))

(defmethod unary-operation ((operator (eql 'fn::opposition)) (function polynomial))
  (make-instance 'fn::polynomial-function :coefficients (mapcar #'- (slot-value function '$coefficients))))

(defmethod binary-operation ((operator (eql 'fn::subtraction)) (function-a fn::function) (function-b fn::function))
  (combine '+ function-a (apply-operator '- function-b)))





(defmethod binary-operation ((operator (eql 'fn::addition)) (function-a polynomial) (function-b polynomial))
  (make-instance 'polynomial
		 :coefficients (let* ((coeff-a (coefficients function-a))
				      (coeff-b (coefficients function-b))
				      (length-a (length coeff-a))
				      (length-b (length coeff-b))
				      (rest (cond
					      ((< length-a length-b)
					       (last coeff-b (- length-b length-a)))
					      ((< length-b length-a)
					       (last coeff-a (- length-a length-b))))))
				 (append (mapcar #'+ coeff-a coeff-b)
					 rest))))

(defmethod binary-operation ((operator (eql 'fn::multiplication) (fun-a polynomial) (fun-b polynomial)))
  (let* ((coeff-a (coefficients fun-a))
	 (coeff-b (coefficients fun-b))
	 (degree-a (1- (length coeff-a)))
         (degree-b (1- (length coeff-b)))
         (degree (+ degree-a degree-b)))
    (make-instance 'fn::polynomial-function
                   :coefficients
                   (loop :for i :upto degree
                      :collect (loop
                                  :for k :form (max 0 (- i degree-b)) :upto (min i degree-a)
                                  :sum (* (svref coeff-a k) (svref coeff-b (- i k))))))))








(defgeneric evaluate (function argument))
;;;                   function        argument
(defmethod evaluate ((function funct) (x t))
  (funcall (slot-value function '$value-function) x))
(defmethod evaluate ((value number) (x t))
  value)
(defmethod evaluate ((function function) (x t))
  (funcall function x))



;;;
;;;
;;;(let* ((f-a (make-instance 'characteristic-function
;;;			   :set (make-instance 'real-set :element-p (lambda (x)
;;;								      (<= 2 x 7)))
;;;			   :value #'cos))
;;;       (f-b (make-instance 'characteristic-function
;;;			   :set (make-instance 'real-set :element-p (lambda (x)
;;;								      (<= 4 x 9)))))
;;;       (f-c (combine #'+ f-a f-b)))
;;;  (evaluate f-c 5))
;;;


;;;;======================================================================================

(defclass <elementary-function> ()
  (($evaluation :reader :eval-fun :initarg :eval-fun)))

(defgeneric .+ (expr1 expr2)
  (:method ((expr1 list) (expr2 list))
    ())
  (:method (())))


(defgeneric evaluate (expr args))


(defun evalu)
(defmethod evaluate ((expr cons) &rest garbage)
  (apply #'evaluate expr))
(defmethod evaluate ((funct (eql '+)) &rest args)
  ())


;;;;======================================================================================
;;;;======================================================================================
(defmethod D ((expr cons) &rest garbage)
  (apply #'D expr))
(defmethod D ((funct (eql '+)) &rest args)
  `(+ ,@(mapcar #'D args)))
(defmethod D ((funct (eql 'expt)) &rest args)
  (let ((base (car args))
	(expt (cadr args)))
    `(* ,expt (expt ,base ,(1- expt)) ,(D base))))
(defmethod D ((expr symbol) &rest garbage)
  (if (eql expr *variable*)
      1
      0))
(defvar *variable* nil)

(defun derive (expr vars)
  (mapcar (lambda (x)
	    (let ((*variable* x))
	      (declare (special *variable*))
	      (D expr)))
	  vars))


(derive '(+ (expt x 2) (expt x 1) (expt x 0) (expt y 3)) '(x y))




(defun find-last-not-after (point partition pred &key key)
  (oam:fbind ((point.> (compile nil `(lambda (elt)
                                       (funcall ,pred
                                                ,(if key `(funcall ,key elt) 'elt)
                                                ,point)))))
    (let* ((max (- (length partition) 1))
           (elt-max (elt partition max)))
      (if (point.> elt-max)
          (values elt-max nil max)
          (loop
             :with min := 0 :and max := (1- max)
             :until (< max min)
             :do (let* ((i (truncate (+ min max) 2))
                        (i+1 (1+ i))
                        (i-1 (1- i))
                        (elt-i (elt partition i))
                        (elt-i+1 (elt partition i+1)))
                   (cond
                     ((point.> elt-i+1)
                      (setq min i+1))
                     ((not (point.> elt-i))
                      (setq max i-1))
                     (t
                      (return (values elt-i elt-i+1 i i+1)))))
             :finally (return (values nil (elt partition 0) -1 0)))))))


(defun find-last-not-after (point partition pred)
  (oam:fbind ((.< pred))
    (let ((min 0)
          (max (- (length partition) 2))
          i)
      (if (.< (elt partition (1+ max)) point)
          (values (elt partition (1+ max)) nil (1+ max))
          (loop
             :while (not (< max min))
             :do (progn
                   (setq i (truncate (+ min max) 2))
                   (cond
                     ((.< (elt partition (1+ i)) point)
                      (setq min (1+ i)))
                     ((not (.< (elt partition i) point))
                      (setq max (1- i)))
                     (t
                      (return (values (elt partition i) i)))))
             :finally (return (values :too-low -1)))))))

(defun find-in (point pred partition &key key)
  "Return the elements elt and next-elt of PARTITION as first and secondary result which satisfie (and (PRED (funcall KEY elt) POINT) (not (PRED (funcall KEY next-elt) POINT))) where next-elt is the element adjacent to the right of elt and hence satisfies (PRED (funcall KEY elt) (funcall KEY next-elt)). The secondary and third result are respectively the position of elt and the position of next-elt. The vector PARTITION is suposed to be ordered by a total preorder which is equivalent to PRED."
  (flet ((.< (elt point) pred))
    (let ((length (length partition))
          (inverse-p (.< (elt partition (1- length))
                         (elt partition 0))))
     (funcall (compile nil `(lambda ()
                              (multiple-value-bind (min max)
                                  (values ,@(funcall (if inverse-p
                                                         #'nreverse
                                                         #'identity)
                                                     (list 0 (- length 2))))
                                (if (.< (elt ,partition (1+ max)) point)
                                    (values (elt ,partition (1+ max)) (1+ max))
                                    (loop :while (not (< max min))
                                       :do (progn
                                             (setq i (truncate (+ min max) 2))
                                             (cond
                                               ((.< (elt ,partition (1+ i)) ,point)
                                                (setq min (1+ i)))
                                               ((not (.< (elt ,partition i) ,point))
                                                (setq max (1- i)))
                                               (t
                                                (return (values (elt ,partition i) i)))))
                                       :finally (return (values :too-low -1)))))))))))