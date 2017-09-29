

#+ (or)
(let* ((underlying-asset :sx5e)
       (strike-date #d "2012-5-24")
       (initial-valuation-date #D"2012-12-21")
       (valuation-dates (mapcar (lambda (n) (add-to initial-valuation-date n :y)) '(0 1 2 3 4 5 6 7 8)))
       (initial-level (value underlying-asset strike-date))
       (coupon-barrier (* 1/2 initial-level))
       (early-redemption-barrier initial-level)
       (protection-level (* 1/2 initial-level)))
  (list
   (bermudan-option (if (< early-redemption-barrier (value underlying-asset *time*))
                        (one currency)
                        (maplist (lambda (valuation-dates)
                                   (bermudan-option (if (< coupon-barrier (value underlying-asset *time*))
                                                        (scale (fixed-rate *time*) (one currency))
                                                        (zero))
                                                    valuation-dates))
                                 valuation-dates))
                    (mapcar (lambda (i) (elt i valuation-dates)) '(4 5 6 7)))
   (european-option (if (<= protection-level (value underlying-asset *time*))
                        (one currency)
                        (* (1- (* 2 (- 1/2 (/ (value underlying-asset *time*)
                                              initial-level))))
                           (one currency)))
                    (last valuation-dates))))

;;;;* Expressions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass expression ()
    ((slot-names :initform nil :allocation :class)))
  (defclass cons-expression (expression)
    ((simplified :initform nil)))

  (defgeneric expression-type (return-type)
    (:method (return-type)
      (error "Not an expression type: ~S." return-type))
    (:method :around ((return-type symbol))
      (if (subtypep return-type 'expression)
          return-type
          (call-next-method)))
    (:method ((return-type expression))
      (class-name return-type))))

(defun expressionp (o)
  (typep o 'expression))

(defgeneric simplify (expr)
  (:method ((expr cons))
    (mapcar 'simplify expr))
  (:method ((expr expression))
    expr)
  (:method :around ((expr cons-expression))
    (or (slot-value expr 'simplified)
        (setf (slot-value expr 'simplified) (call-next-method)))))
(defgeneric expr-equalp (e1 e2)
  (:method-combination or)
  (:method (e1 e2) nil)
  (:method ((e1 null) (e2 null)) t)
  (:method ((e1 cons) (e2 cons))
    (every 'expr-equalp e1 e2))
  (:method ((e1 expression) (e2 expression))
    (every (lambda (slot-name)
             (expr-equalp (slot-value e1 slot-name)
                          (slot-value e2 slot-name)))
           (slot-value e1 'slot-names))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-expression-class-definition (expression-classes name slots simplifier class-options)
    "EXPRESSION-CLASSES: a list of super classes to derive from
NAME: the name of the new class to define
SLOTS: the slots of the class in the form ((slot-name slot-type) ..). Only the lambda-list keyword &rest is allowed; however in the case of &rest the variable is then a list whose elements are of type slot-type.
SIMPLIFIER: The body of the SIMPLIFY method specialized to NAME
CLASS-OPTIONS: Class options"
    (multiple-value-bind (mandatory optional restp rest keyp)
        (awl:parse-lambda-list slots)
      (assert (not keyp) () "No &key keyword allowed.")
      (assert (null optional) () "No &optional keyword allowed.")
      (let* ((types (append (mapcar (lambda (s-t) (expression-type (second s-t))) mandatory)
                            (when restp (list `(awl:list-of ,(expression-type (second rest)))))))
             (mandatory (mapcar 'first mandatory))
             (rest (first rest))
             (slots (append mandatory (when restp (list rest))))
             (readers (mapcar (lambda (slot)
                                (intern (format nil "~S-~S" name slot)))
                              slots))
             (lambda-list (append mandatory (when restp (list '&rest rest)))))
        (flet ((reader-calls (expr) (mapcar (lambda (fn) `(,fn ,expr)) readers)))
          (let ((g!contract (gensym "G!CONTRACT")))
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (defclass ,name ,expression-classes
                 (,@(mapcar (lambda (slot type reader) `(,slot :initform (error "No value given for slot ~S." ',slot)
                                                               :type ,type
                                                               :initarg ,(intern (string slot) :keyword)
                                                               :reader ,reader))
                            slots types readers)
                  (slot-names :initform ',slots :allocation :class))
                 ,@class-options)
               (defun ,name ,lambda-list
                 (let ,(when restp `((,rest (mapcan 'awl:ensure-list ,rest))))
                   (make-instance ',name ,@(mapcan (lambda (slot)
                                                     `(,(intern (string slot) :keyword) ,slot))
                                                   slots))))
               (defmethod make-load-form ((object ,name) &optional environment)
                 (declare (ignorable environment))
                 `(,',name ,,@(mapcar (lambda (c) `(make-load-form ,c)) (mapcar (lambda (fn) `(,fn 'object)) (if restp (butlast readers)
                                                                                                                 readers)))
                           ,',@(when restp `(mapcar 'make-load-form (,@(last readers) object)))))
               (defmethod simplify ((expr ,name))
                 (let ,(mapcar (lambda (var val)
                                 `(,var (simplify ,val)))
                        slots (reader-calls 'expr))
                   (let (,g!contract)
                     (flet ((,g!contract ()
                              (or ,g!contract
                                  (setf (slot-value ,g!contract 'simplified)
                                        (setq ,g!contract (,name ,@slots))))))
                       (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                       (symbol-macrolet ((,name (,g!contract)))
                         ,@simplifier))))))))))))

;;;;* Items
;;;;
;;;; Items represent entities used as unit for valuation of other entities; example of
;;;; entities: currencies, stocks, commodities, etc.
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defclass item () ()))
(defvar *valuation-method*)
(defgeneric value (entity item)
  (:method (entity (item item))
    (error "No valuation method defined for entity ~S" entity)))
(defmethod expression-type ((expr (eql 'item)))
  'item)
;;;;* Observables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass observable (expression) ())
  (defclass constant-observable (observable)
    ((value :initform (error "No value given.")
            :initarg :value
            :reader constant-value)
     (slot-names :allocation :class :initform '(value))))
  (defclass combined-observable (observable cons-expression) ())
  (defclass @boolean (observable) ())
  (defclass @false (@boolean constant-observable) ())
  (defclass @true (@boolean constant-observable) ()))

(defconstant @false (or (and (boundp '@false) @false)
                        (make-instance '@false :value nil)))
(defconstant @true (or (and (boundp '@true) @true)
                       (make-instance '@true :value t)))
(defmethod make-load-form ((object @false) &optional environment)
  (declare (ignore environment))
  '@false)
(defmethod make-load-form ((object @true) &optional environment)
  (declare (ignore environment))
  '@true)
(defun @falsep (o) (eq o @false))
(defun @truep (o) (eq o @true))
(defun @boolean (boolean)
  (if boolean
      @true
      @false))

(defmacro define-combinator (name (&rest slots) type &body options)
  (let ((simplifier (rest (assoc :simplifier options)))
        (options (remove :simplifier options :key 'car)))
    (expand-expression-class-definition `(,(expression-type type) combined-observable)
                                        name slots simplifier options)))

(define-combinator @not ((observable @boolean)) @boolean
  (:simplifier (typecase observable
                 (@not (@not-observable observable))
                 (@and (apply '@or (mapcar '@not (@and-observables observable))))
                 #+ (or)
                 (@or (apply '@and (mapcar '@not (@or-observables observable))))
                 (t @not))))
(define-combinator @or (&rest (observables @boolean)) @boolean
  (:simplifier (let ((observables (mapcan (lambda (obs)
                                            (typecase obs
                                              (@or (@or-observables obs))
                                              (t (list obs))))
                                          observables)))
                 (cond ((some '@truep observables) @true)
                       ((every '@falsep observables) @false)
                       (t (@or observables))))))

(defun first-level-expand (list expansion-fn)
  (labels ((doit (done todo)
             (if todo
                 (doit (loop :for d :in done
                             :nconc (multiple-value-bind (expansion expandedp)
                                         (funcall expansion-fn (first todo))
                                       (if expandedp
                                           (loop :for e :in expansion
                                                 :collect (cons e d))
                                           (list (cons expansion d)))))
                       (rest todo))
                 done)))
    (mapcar 'reverse (doit '(nil) list))))

(define-combinator @and (&rest (observables @boolean)) @boolean
  (:simplifier (let ((observables (mapcar (lambda (obs)
                                            (cond ((some '@falsep obs) @false)
                                                  ((every '@truep obs) @true)
                                                  (t (@and obs))))
                                          (first-level-expand (mapcan (lambda (obs)
                                                                        (typecase obs
                                                                          (@and (@and-observables obs))
                                                                          (t (list obs))))
                                                                      observables)
                                                              (lambda (i)
                                                                (typecase i
                                                                  (@or (values (@or-observables i) t))
                                                                  (t i)))))))
                 (cond ((some '@truep observables) @true)
                       ((every '@falsep observables) @false)
                       (t (@or observables))))))

(defgeneric observable=> (cond1 cond2)
  (:documentation "Return true if it can be shown that COND2 is true each time when COND1 is true.")
  (:method-combination or)
  (:method ((cond1 @boolean) (cond2 @boolean))
    (expr-equalp cond1 cond2))
  (:method ((cond1 @false) cond2) t)
  (:method ((cond1 @true) (cond2 @true)) t))

(define-combinator @=> ((cond1 @boolean) (cond2 @boolean)) @boolean
  (:simplifier (@boolean (observable=> cond1 cond2))))

(define-combinator @equalp ((obs1 observable) (obs2 observable)) @boolean
  (:simplifier (if (expr-equalp obs1 obs2)
                   @true
                   @equalp)))

;;;;** Real Observables
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defclass @real (observable) ())
  (defclass @constant (@real constant-observable) ())
  (defclass @zero (@constant) () (:default-initargs :value 0))
  (defclass @one (@constant) () (:default-initargs :value 1)))

(defconstant @zero (or (and (boundp '@zero) @zero)
                       (make-instance '@zero)))
(defconstant @one (or (and (boundp '@one) @one)
                      (make-instance '@one)))
(defmethod make-load-form ((object @zero) &optional environment)
  (declare (ignore environment))
  '@zero)
(defmethod make-load-form ((object @one) &optional environment)
  (declare (ignore environment))
  '@one)
(defmethod make-load-form ((object @constant) &optional environment)
  (declare (ignore environment))
  `(@constant ,(constant-value object)))
(defun constant-observable-p (obs)
  (typep obs 'constant-observable))
(defun @constant (value)
  (check-type value real)
  (cond
    ((zerop value) @zero)
    ((= 1 value) @one)
    (t (make-instance '@constant :value value))))

(define-combinator @= ((observable @real) &rest (more-observables @real)) @boolean
  (:simplifier (if (every (lambda (obs)
                            (expr-equalp observable obs))
                          more-observables)
                   @true
                   @=)))
(defun constantly-p (pred observable more-observables)
  (when (and (constant-observable-p observable)
             (every 'constant-observable-p more-observables))
    (@boolean (apply pred (constant-value observable)
                     (mapcar 'constant-value more-observables)))))
(define-combinator @< ((observable @real) &rest (more-observables @real)) @boolean
  (:simplifier (or (constantly-p '< observable more-observables)
                   @<)))
(define-combinator @<= ((observable @real) &rest (more-observables @real)) @boolean
  (:simplifier (or (constantly-p '<= observable more-observables)
                   @<=)))
(define-combinator @> ((observable @real) &rest (more-observables @real)) @boolean
  (:simplifier (or (constantly-p '> observable more-observables)
                   @>)))
(define-combinator @>= ((observable @real) &rest (more-observables @real)) @boolean
  (:simplifier (or (constantly-p '>= observable more-observables)
                   @>=)))

(define-combinator @zerop ((observable @real)) @boolean
  (:simplifier (typecase observable
                 (@constant (@boolean (zerop (constant-value observable))))
                 (t @zerop))))
(define-combinator @plusp ((observable @real)) @boolean
  (:simplifier (typecase observable
                 (@constant (@boolean (plusp (constant-value observable))))
                 (t @plusp))))
(define-combinator @minusp ((observable @real)) @boolean
  (:simplifier (typecase observable
                 (@constant (@boolean (minusp (constant-value observable))))
                 (t @minusp))))

(define-combinator @- ((observable @real) &rest (more-observables @real)) @real
  (:simplifier (if more-observables
                   (simplify (@+ observable (@- (@+ more-observables))))
                   (typecase observable
                     (@- observable)
                     (@zero @zero)
                     (@constant (@constant (- (constant-value observable))))
                     (t @-)))))
(define-combinator @+ (&rest (observables @real)) @real
  (:simplifier (let* ((constant (let ((constant (@constant (reduce '+ (mapcar 'constant-value (remove-if-not 'constant-observable-p observables))))))
                                  (unless (eq @zero constant)
                                    (list constant))))
                      (varying (remove-if 'constant-observable-p observables))
                      (negated (let ((negated (remove-if-not (lambda (x) (typep x '@-)) varying)))
                                 (when negated
                                   (@- (simplify (@+ (mapcar '@-observable negated)))))))
                      (others (mapcan (lambda (s)
                                        (typecase s
                                          (@+ (@+-observables s))
                                          (t (list s))))
                                      (remove-if (lambda (x) (typep x '@-)) varying))))
                 (let ((observables (append constant others negated)))
                   (case (length observables)
                     (0 @zero)
                     (1 (first observables))
                     (t (@+ constant others negated)))))))

(define-combinator @/ ((observable @real) &rest (more-observables @real)) @real
  (:simplifier (if more-observables
                   (simplify (@* observable (@/ (@* more-observables))))
                   (typecase observable
                     (@/ observable)
                     (@zero @zero)
                     (@constant (@constant (/ (constant-value observable))))
                     (t @/)))))
(define-combinator @* (&rest (observables @real)) @real
  (:simplifier (let* ((constant (let ((constant (@constant (reduce '* (mapcar 'constant-value (remove-if-not 'constant-observable-p observables))))))
                                  (unless (eq @zero constant)
                                    (list constant))))
                      (varying (remove-if 'constant-observable-p observables))
                      (negated (let ((negated (remove-if-not (lambda (x) (typep x '@/)) varying)))
                                 (when negated
                                   (@/ (simplify (@* (mapcar '@/observable negated)))))))
                      (others (mapcan (lambda (s)
                                        (typecase s
                                          (@* (@*-observables s))
                                          (t (list s))))
                                      (remove-if (lambda (x) (typep x '@/)) varying))))
                 (let ((observables (append constant others negated)))
                   (case (length observables)
                     (0 @one)
                     (1 (first observables))
                     (t (@+ constant others negated)))))))

(define-combinator @exp ((observable @real)) @real
  (:simplifier (typecase observable
                 (@constant (@constant (exp (constant-value observable))))
                 (@log observable)
                 (t @exp))))
(define-combinator @log ((observable @real)) @real
  (:simplifier (typecase observable
                 (@constant (@constant (log (constant-value observable))))
                 (@exp observable)
                 (t @log))))


(defmethod expr-equalp ((expr1 @constant) (expr2 @constant))
  (= (constant-value expr1) (constant-value expr2)))
(defmethod expr-equalp ((expr1 @+) (expr2 @+))
  (let ((sum1 (@+-observables expr1))
        (sum2 (@+-observables expr2)))
    (and (= (length sum1) (length sum2))
         (null (set-difference sum1 sum2 :test 'expr-equalp)))))

;;;;** Dates
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass date (observable) ())
  (defclass @date (date constant-observable) ()))

(defconstant @date (or (and (boundp '@date) @date)
                       (make-instance 'date)))
(defun @date (date-time)
  (check-type date-time awl:date-time)
  (make-instance '@date :value date-time))
(defmethod make-load-form ((object @date) &optional environment)
  (declare (ignore environment))
  `(@date ,(make-load-form (constant-value object))))

;;;;* Contracts
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass $contract (expression) ())
  (defclass combined-contract ($contract cons-expression) ()))

(defmacro define-contract (name (&rest slots) &body options)
  "Define a contract of name NAME and with slots SLOTS. OPTIONS is a  Furthermore, in SIMPLIFIER comes the body of the SIMPLIFY method in which the slot names are bound to the already simplified slots of this contract and the variable NAME is bound to an instance of the contract NAME whose slots are simplified."
  (let ((simplifier (rest (assoc :simplifier options)))
        (documentation (rest (assoc :documentation options))))
    (expand-expression-class-definition '(combined-contract) name slots simplifier documentation)))


;;;;** Zero
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass $zero ($contract) ()))
(defconstant $zero (or (and (boundp '$zero) $zero)
                       (make-instance '$zero)))
(defmethod make-load-form ((object $zero) &optional environment)
  (declare (ignore environment))
  '$zero)
;;;;** One
(define-contract $one ((item item))
  (:simplifier $one))

;;;;** Give
(define-contract $give ((contract $contract))
  (:simplifier (typecase contract
                 ($zero $zero)
                 ($give ($give-contract contract))
                 (t $give))))

;;;;** Scale
(define-contract $scale ((multiplier @real) (contract $contract))
  (:simplifier (typecase contract
                 ($zero $zero)
                 ($scale ($scale (simplify (@* multiplier ($scale-multiplier contract)))
                                 ($scale-contract contract)))
                 (t $scale))))

;;;;** All
(define-contract $all (&rest (basket $contract))
  (:simplifier (let ((basket (remove $zero
                                     (mapcan (lambda (contract)
                                               (typecase contract
                                                   ($all ($all-basket contract))
                                                   (t (list contract))))
                                             basket))))
                 (if basket
                     (if (rest basket)
                         (apply '$all basket)
                         (first basket))
                     $zero))))

;;;;** At
(define-contract $at ((date @date) (contract $contract))
  (:simplifier (typecase contract
                 ($zero $zero)
                 (t $at))))

;;;;** If
(define-contract $if ((condition @boolean) (then $contract) (else $contract))
  (:simplifier (cond
                 ((and (typep then '$zero)
                       (typep else '$zero))
                  $zero)
                 ((and (typep then '$if)
                       (@=> condition ($if-condition then)))
                  ($if condition ($if-then then) else))
                 ((and (typep then '$if)
                       (@=> condition (@not ($if-condition then))))
                  ($if condition ($if-else then) else))
                 ((typep else '$if)
                  (simplify ($if (@not condition) else then)))
                 (t ($if condition then else)))))



(defun autocall ()
  (let ((coupon ($if (@< 'coupon-barrier 'underlying)
                     ($scale 'rate ($one :usd))
                     $zero)))
    (labels ((coupon (pay-dates)
               (if pay-dates
                   ($at (first pay-dates)
                        ($all ($if (@< $zero coupon)
                                   coupon
                                   $zero)
                              ($if (@not (@< $zero coupon))
                                   (coupon (rest pay-dates))
                                   $zero)))
                   $zero))
             (coupon2 (pay-dates)
               (if pay-dates
                   ($all ($at (first pay-dates)
                              ($if (@< $zero coupon)
                                   coupon
                                   $zero))
                         ($at (first pay-dates)
                              ($if (@not (@< $zero coupon))
                                   (coupon (rest pay-dates))
                                   $zero))))))
      (coupon2 '(t1 t2)))))

(equiv (fn a) (funcall (fn) a))

(equiv ($if (@xor c d) a $zero) ($all ($if (@and c (@not d)) a $zero) ($if (@and (@not c) d) c $zero)))
(equiv ($if c a b) ($all ($if c a $zero) ($if c $zero b)))
(equiv ($all ($if c a b) ($if c d e)) ($if c ($all a d) ($all b e)))
(equiv ($at d ($all a)) ($all (mapcar ($at d) a)))
(equiv ($all ($if c a b) ($if c d e)) ($if c ($all a d) ($all b e)))
(equiv ($all a b c d) ($all a c b d))
(equiv ($all a $zero) a)
(equiv ($all a) a)
(equiv ($all ($at d a) ($at d b)) ($at d ($all a b)))
(equiv ($if c ($scale k a) ($scale k b)) ($scale k ($if c a b)))
(equiv ($scale k ($scale l a)) ($scale (@* k l) a))
(equiv ($scale k $zero) $zero)
(equiv ($all (mapcar ($scale k) a)) ($scale k ($all a)))
(equiv ($give ($one i)) ($scale -1 ($one i)))
(equiv ($give ($give a)) a)


(equiv (@+ a b c d) (@+ a c b d))
(equiv (@+ (+ a b)) (@+ a b))

(let (($ '$)
      (underlying 'underlying))
 (flet ((barrier (i) `(barrier ,i))
        (rate (i) `(rate ,i))
        (pay (i) ($scale (rate i) ($one $)))
        (condition (i) (@< (barrier i) underlying))
        (binary-barrier-opt ())
        (coupon (i)
          ))))



