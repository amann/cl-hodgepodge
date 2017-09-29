

(defun walk-tree (fn tree)
  (typecase tree
    (null nil)
    (cons (let ((first (car tree)))
                  (if (consp first)
                      (walk-tree fn first)
                      (funcall fn first)))
     (walk-tree fn (cdr tree)))
    (t (funcall fn tree))))

(defun map-tree (fn tree)
  "Return a fresh tree having the same structure as TREE and in which all atoms are applied to the function FN."
  (typecase tree
    (null nil)
    (cons (cons (let ((first (car tree)))
                  (if (consp first)
                      (map-tree fn first)
                      (funcall fn first)))
                (map-tree fn (cdr tree))))
    (t (funcall fn tree))))
(defun search-tree (start neighbors-fn goalp)
  (flet ((neighbors (state) (funcall neighbors-fn state))
         (goalp (state) (funcall goalp state)))
    (declare (inline neighbors goalp))
    (labels ((searchit (states)
               (when states
                 (dolist (state states (searchit (mapcan #'neighbors states)))
                   (when (goalp state)
                     (return state))))))
      (searchit (list start)))))

;;;;* Axioms

(defclass category-parameter () ())
(defclass cp-element (category-parameter) ())
(defclass cp-operation (category-parameter) ())
(defclass cp-domain (category-parameter) ())

(defmacro define-category (name (&rest parameters) (&rest super-categories)
                           &body axioms)
  `(let (())
     (macrolet ((define-operation (name signature type &body axioms)
                  `(macrolet ((--> (expr1 expr2)
                                `(add-implication ',expr1 ',expr2))
                              (<-> (expr1 expr2)
                                `(add-equivalence ',expr1 ',expr2)))))))))


()


(define-category eql (?a) ()
  (with-types ((?s (eql ?a)) (?t (eql ?b)))
    (of-type ?a ?s)
    (<-> (of-type ?b ?s) (of-type ?a ?t))
    (<-> (eql ?c ?a) (of-type ?c ?s))
    (<-> (eql ?a ?b) (eql ?b ?a))))

(define-category nil () ()
  (--> (of-type ?a nil) (of-type ?a ?b))
  (--> (of-type ?a ?b) (not-eql ?b nil)))

(define-category list-of (?T) ()
  (for-all (?t1 ?t2)
    (<-> (superof-type ?t1 ?t2) (superof-type (list-of ?t1) (list-of ?t2))))
  (with-types (?T (?L (list-of ?T)))
    (of-type nil ?L)
    (for-all (((?a . ?b) ?L))
      (of-type ?a ?T))
    (for-all ((?a ?T) (?b ?L))
      (of-type (?a . ?b) ?L))))


()

(--> (* a (* b c)) (* (* a b) c)) ---> (lambda (op signature type)
                                         (multiple-value-bind (required optional restp rest keyp)
                                             (awl:parse-lambda-list signature) 
                                           (assert (not keyp) () "Keys not allowed.") 
                                          `(progn
                                             (defgeneric)
                                             (lambda (expr)
                                               (declare (type ,g!* expr))
                                               (with-slots (?a ?b) expr
                                                 (declare (type ,name ?a) (type ,g!* ?b))
                                                 (,* (,* ?a (,*.1 ?b)) (,*.2 ?b)))))))
(--> (* a . b) (* a (* . b))) ---> `(lambda (?a &rest ?b) (declare (type ,g!* ?a) (type (awl:list-of ,g!*) ?b)) (,* ?a (apply #',* ?b)))

;;;; Expressions are either atoms or conses of the form (op . args), where op,
;;;; in general, is a symbol which designates an operator and args are expressions
;;;; which are the arguments of the operator. The arguments themselves are expressions.
;;;; Atoms in an expression which are not symbols are literal objects, like numbers.
;;;; Symbols are variables standing for a certain expression. Which expression they
;;;; stand for depends on the context and on their name. Symbols whose name starts
;;;; with a #\? are open variables waiting to be matched by unification. A special
;;;; kind of atom are the indefinite constants which may appear for example in anti-
;;;; derivatives, e.g. the K in (int x x) = (+ (* 1/2 (expt x 2)) K). Those are
;;;; implemented as a struct of type INDEFINITE-CONSTANT.
;;;; If the CDR of the last cons of an expression is a symbol, it stands for a list
;;;; taking any number of arguments. 

(defun expr-op (expr)
  "Return the operator of the expression EXPR if EXPR is a cons or NIL else."
  (when (consp expr) (car expr)))
(defun expr-args (expr)
  "Return a list of the arguments of the expression EXPR; if EXPR is an atom, return NIL."
  (when (consp expr) (cdr expr)))
(defstruct indefinite-constant name ruleset type)

(defstruct (expression-template (:constructor make-expression-template
                                    (pattern
                                     &aux
                                       (fn.symbs (make-template-function pattern))
                                       (var-symbols (cdr fn.symbs))
                                       (function (car fn.symbs)))))
  pattern var-symbols var-types function)

(defun make-template-function (symbols expr)
  "Return as primary value a function having as arguments the symbols in the list of symbols SYMBOLS and returning an expression resembling EXPR but in which those have been replaced by the value of the corresponding arguments; the secondary value is the arglist of the function."
  (cons (compile nil `(lambda ,symbols
                        (declare (ignorable ,@symbols))
                        ,(destructure expr symbols)))
        symbols))


;;;;* Rules and Rulesets
;;;;
;;;; A ruleset is a set of rules :-) A rule consists in an expression
;;;; which forms a pattern in which symbols with names prefixed with #\?
;;;; are variables which will be mached by unification, and a template
;;;; function which takes as input the values of the matched variables
;;;; and returns an expression containing these values.
;;;;
;;;; In a ruleset, the rules are classified by 1) their toplevel type
;;;; (return type) and 2) by their operator symbol. For example, a rule
;;;; (foo ?a ?b) -> (bar ?a ?a ?b) will be stored in foo-type --> foo.
;;;; Each operator has a signature, e.g. (foo type1 type2) foo-type.
;;;; When trying to match an expression (foo bla (bar baz asd qwe)) to
;;;; a rule, we first look for the type of the expression -- foo-type
;;;; look in the hash-table under foo-type for a hash-table in which
;;;; we look for the operator foo and sequencially try all rules there.
;;;; For finding a transformation of some argument of the expression,
;;;; e.g. bla, we look in the signature of foo for the type of the first
;;;; argument of foo -- type1, look in the table for the that type and
;;;; search then for the operator nil, since bla is an atom.
;;;; It must be said that an operator cannot (or should not) be named NIL.

;;;;** Categories

(defstruct category 
  direct-parents parameters axioms)

()

;;;;** Types

(defvar *ruleset*)

(defstruct ruleset type-categorizations)



(defstruct type category parameters type-spec)
(defun categoryp (type category)
  (search-tree (type-category type) (function category-direct-parents)
               (lambda (c) (eql category c))))

;;;;** Rules
(define-condition rule-not-matching (error) ((rule :initarg :rule :reader not-matching-rule)))

(defstruct (axiom (:constructor make-rule (parameters pattern type arg-types end-expression
                                           &aux
                                             )))
  parameters pattern variables toplevel-type types template)



(defstruct (rule (:constructor make-rule (axiom parameters type arg-types
                                          &aux )))
  pattern variables toplevel-type types template)


#S(rule :pattern '(?* ?a . ?b) :variables '((?a ?M) (?b (list-of ?M))) :toplevel-type '(?* ?M)
        :types '((?M (magma ?*))) :template (lambda (?* ?a ?b) `(,?* ,?a (,?* . ,?b))))

(apply-rule rule '(+ q e r) '+ '((q integer) (e real) (r integer)))

1. (unify:unify rule '(+ q e r)) --> ((?* +) (?a q) (?b (e r)))
1.a ((integer ?M) (real ?M) (integer ?M))
2. --> ((?a integer) (?b (list-of real))) Notice: One needs to know that (is-subtype integer real)
3. --> (common-supertype integer real) -> real --> (?M real)
4. (typep real '(magma +)) --> true
5. (funcall (rule-template rule) '+ 'q '(e r))





(defun try-apply-rule (rule expression op arg-types)
  (handler-case
      (let ((env (unify:unify expression (rule-pattern rule))))
        (let ((types (mapcar (lambda (m)
                               (list (car m) (common-supertype (remove-duplicates (cdr m)))))
                             (reduce (lambda (r m)
                                       (let ((type-class (assoc (car m) r)))
                                         (if type-class
                                             (setf (cdr (last type-class)) (cdr m))
                                             (push m r)))
                                       r)
                                     (loop :for (var type) :in (rule-variables rule)
                                           :collect (let ((var-value (unify:find-variable-value var env nil)))
                                                      (typecase type
                                                        ((cons (eql list-of) *)
                                                         (cons (second type)
                                                               (mapcan (lambda (val)
                                                                         (cdr (assoc val arg-types)))
                                                                       var-value)))
                                                        (t (cons type (cdr (assoc var-value arg-types)))))))
                                     :initial-value nil))))
          types))
    (unify:unification-failure ())))

(defun common-supertype (types)
  (reduce (lambda (&optional r type)
            (if r
                (cond
                  ((subtypep type r) r)
                  ((subtypep r type) type)
                  (t t))
                t))
          types))




(defstruct (ruleset (:constructor %make-ruleset (&optional name))
                    (:conc-name %ruleset-))
  (types (make-hash-table :test 'equalp) :read-only t)
  (rules (make-hash-table :test 'equalp) :read-only t)
  (name nil :type symbol))

(defvar *rulesets* (make-hash-table))
(declaim (ftype (function ((or symbol ruleset)) ruleset)))
(defun find-ruleset (ruleset &optional (errorp t))
  (etypecase ruleset
    (ruleset ruleset)
    (symbol (or (gethash ruleset *rulesets*)
                (when errorp
                  (error "No such ruleset: ~A." ruleset))))))
(declaim (inline find-ruleset))
(defun (setf find-ruleset) (value ruleset &optional errorp)
  (declare (ignore errorp))
  (check-type value ruleset)
  (let ((ruleset-name (etypecase ruleset
                        (ruleset (%ruleset-name ruleset))
                        (symbol ruleset))))
    (check-type ruleset-name (and symbol (not null)))
    (let ((old-ruleset (find-ruleset ruleset-name nil))) 
      (when old-ruleset
        (warn "Replacing old ruleset ~S by new ruleset ~S at name ~S."
              old-ruleset value ruleset-name)
        (setf (%ruleset-name old-ruleset) nil)))
    (setf (%ruleset-name value) ruleset-name
          (gethash ruleset-name *rulesets*) value)))

(defun make-ruleset (name &optional signatures initial-rules)
  (let ((ruleset (%make-ruleset name)))
    (when name (setf (find-ruleset name) ruleset))
    (dolist (signature signatures)
      (destructuring-bind (expr type)
          signature
        (typecase expr
          (cons))))
    (dolist (rule initial-rules)
      (destructuring-bind (expr1 . expr2)
          rule
        (add-equivalence-rule expr1 expr2 ruleset)))
    ruleset))

(defun ruleset-supertypes (ruleset type)
  (labels ((direct-supertypes (type)
             (gethash type (%ruleset-types ruleset)))
           (search (type result &aux (result (cons type result)))
             (case type
               ((t) (nreverse result))
               (t (search (mapcan #'direct-supertypes types) result)))))
    (search (list type) nil)))

(defun ruleset-rules (ruleset type &optional (operator nil operatorp))
  "Return a list of the rules in RULESET which are associated to OPERATOR of toplevel type TYPE."
  (let ((place (gethash type (%ruleset-rules (find-ruleset ruleset)))))
    (when place
      (if operatorp
          (gethash operator (car place))
          (cdr place)))))
(defun (setf ruleset-rules) (value ruleset type &optional (operator nil operatorp))
  (declare (type (cons rule list) value) (type symbol operator))
  (let ((rules (%ruleset-rules (find-ruleset ruleset))))
    (let ((place (or (gethash type rules)
                     (setf (gethash type rules) (cons (make-hash-table :test 'eq) nil)))))
      (if operatorp
          (setf (gethash operator (car place)) value)
          (setf (cdr place) value)))))


(defun add-transformation-rule (from-expr to-expr type var-types ruleset)
  (let ((operator (expr-op from)))
    (multiple-value-bind (template symbols)
        (make-template to) 
      (pushnew (make-rule :pattern from :type type :symbols var-types :template template)
               (if operator
                   (ruleset-rules ruleset type operator)
                   (ruleset-rules ruleset type))
               :test 'equalp :key 'rule-pattern))))

(defun add-equivalence-rule (expr1 expr2 ruleset)
  (add-transformation-rule expr1 expr2 ruleset)
  (add-transformation-rule expr2 expr1 ruleset))
(defun get-equivalence-rules (expr ruleset)
  (ruleset-rules ruleset (expr-op expr)))

(defun merge-rulesets (ruleset1 ruleset2)
  (let ((ruleset (make-ruleset nil)))
    (maphash (lambda (op rules)
               (setf (gethash op (%ruleset-hash-table ruleset))
                     rules))
             (%ruleset-hash-table (find-ruleset ruleset1)))
    (maphash (lambda (op rules)
               (setf (gethash op (%ruleset-hash-table ruleset))
                     (append (gethash op (%ruleset-hash-table ruleset)) rules)))
             (%ruleset-hash-table (find-ruleset ruleset2)))
    ruleset))

(defmacro define-ruleset (name (&rest include) &body rules)
  (if include
      `(do ((include ',include (rest include))
            (ruleset (make-ruleset nil ',rules) (merge-rulesets ruleset (first include))))
           ((null include) (setf (find-ruleset ',name) ruleset)))
      `(make-ruleset ',name ',(cdr (assoc :signatures rules)) ',(cdr (assoc :rules rules)))))

(defun apply-rule (expr rule)
  "Return the transformation of the expression EXPR by the rule RULE, if possible, NIL else."
  (let ((env (ignore-errors (unify:unify expr (rule-pattern rule)))))
    (when env
      (apply (rule-template rule) (mapcar (lambda (s)
                                            (unify:find-variable-value s env))
                                          (rule-symbols rule))))))
(defun get-toplevel-transforms (expr type ruleset)
  "Return a list of all direct transformations of the expression EXPR using the ruleset RULESET on top level, i.e. without applying transformations on the arguments of EXPR."
  (mapcan (lambda (rule)
            (let ((transform (apply-rule expr rule)))
              (when transform (list transform))))
          (get-equivalence-rules expr type ruleset)))


(defun get-direct-transforms (expr ruleset &aux (op (list (expr-op expr))) (args (expr-args expr)))
  "Return a list of all direct transformations of the expression EXPR using the ruleset RULESET."
  (append (get-toplevel-transforms expr ruleset)
          (mapcan (let ((arg-pos 0))
                    (lambda (transforms)
                      (let ((first-args (subseq args 0 arg-pos))
                            (last-args (subseq args (incf arg-pos))))
                        (mapcar (lambda (trans) (append op first-args (list trans) last-args)) transforms))))
                  (mapcar (lambda (arg) (get-direct-transforms arg ruleset)) args))))




#+ (or)
(defgeneric write-aa (expr &optional stream))
#+ (or)
(defmethod write-aa ((expr equity-binary-option)&optional stream)
  (destructuring-bind (type ccy buy-sell expiry payoff strike underlying)
      expr
    (apply 'format stream "Object=EquityBinaryOption,Reference=,MtM=<undefined>,Currency=~A,Discount_Rate=,Buy_Sell=Buy,Expiry_Date=~A,Payoff_Currency=USD,Payoff_Type=Standard,Cash_Payoff=~A,Option_Type=Call,Strike_Price=~A,Equity=~A"
           (mapcar 'write-aa (list ccy buy_sell expiry payoff strike underlying)))))






;;;;* Find Transformation
;;;;
;;;;
(defvar *ruleset*)
(defun neighbours (expr &optional (ruleset *ruleset*))
  (get-direct-transforms expr ruleset))


(defmacro in-ruleset (ruleset)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (ruleset-user-package ,ruleset))))

;;;;* Ruleset Definitions



(defmacro with-types ((&rest type-categories) &body body)
  (declare (ignore type-categories body))
  (error "Illegal context."))
(defmacro --> (hypothesis thesis)
  (declare (ignore hypothesis thesis))
  (error "Illegal context."))
(defmacro for-all ((&rest elt-spec) &body body)
  (declare (ignore elt-spec body))
  (error "Illegal context."))
(defmacro define-category name ((&rest parameters) (&rest base-categories) &body axiomes)
  (multiple-value-bind (body declarations documentation)
      (awl:parse-body axiomes)
    (awl:with-gensyms (g!category)
      `(let ((,g!category (make-category ',name ',parameters ',base-categories ',documentation)))
         (macrolet ((for-all ((&rest elt-spec) &body body)
                      `(let ((*elt-types* ',elt-spec))
                         ,@body))
                    (with-types ((&rest types) &body body)
                      `(let ((*types* ',types))
                         ,@body)))
           (flet ((--> (hypothesis thesis)
                    (add-axiom ,g!category hypothesis thesis)))
             (flet ((<-> (expr1 expr2)
                      (progn (--> expr1 expr2)
                             (--> expr2 expr1))))
               ,@body)))))))


;;;;* Axioms

(define-category)

(define-category eql (?a) ()
  (with-types ((?s (eql ?a)) (?t (eql ?b)))
    (of-type ?a ?s)
    (<-> (of-type ?b ?s) (of-type ?a ?t))
    (<-> (eql ?c ?a) (of-type ?c ?s))
    (<-> (eql ?a ?b) (eql ?b ?a))))

(define-category nil () ()
  (--> (of-type ?a nil) (of-type ?a ?b))
  (--> (of-type ?a ?b) (not-eql ?b nil)))

(define-category list-of (?T) ()
  (for-all (?t1 ?t2)
    (<-> (superof-type ?t1 ?t2) (superof-type (list-of ?t1) (list-of ?t2))))
  (with-types (?T (?L (list-of ?T)))
    (of-type nil ?L)
    (for-all (((?a . ?b) ?L))
      (of-type ?a ?T))
    (for-all ((?a ?T) (?b ?L))
      (of-type (?a . ?b) ?L))))

(define-category boolean (?0 ?1 ?not ?and ?or) ()
  (with-types ((?bool (boolean ?0 ?1 ?not ?and ?or)))
    (of-type ?0 ?bool)
    (of-type ?1 ?bool)
    
    (for-all ((?a ?b ?bool) (?c (list-of ?bool)))
      (of-type (?not ?a) ?bool)
      (of-type (?and ?a ?b) ?bool)
      (of-type (?or ?a ?b) ?bool)
      
      (<-> (?not ?1) ?0)
      (<-> (?not (?not ?a)) ?a)
      
      (<-> (?and) ?1)
      (<-> (?and ?a ?1) ?a)
      (<-> (?and ?a . ?c) (?and ?a (?and . ?c)))
      (<-> (?and ?a ?b .?c) (?and ?b ?a . ?c))

      (<-> (?or) ?0)
      (<-> (?or ?a . ?c) (?not (?and (?not ?a) (?or . ?c)))))))

(define-category set (?=) ()
  (with-types ((set (set ?=)))
    (for-all ((?a ?b set))
      (of-type (?= ?a ?b) boolean))))

(define-category magma (?*) ()
  (with-types ((ma (magma ?*)))
    (for-all ((?a ?b ma))
      (of-type (?* ?a ?b) ma))))

(define-category groupoid (?*) ((magma ?*))
  (with-types ((gd (groupoid ?*)))
    (for-all ((?a ?b ?c gd)
              (?d (list-of gd)))
      (<-> (?* ?a (?* ?b ?c)) (?* (?* ?a ?b) ?c))
      
      (of-type (?* ?a) ma)
      (<-> (?* ?a) ?a)
      (<-> (?* ?a ?b . ?d) (?* ?a (?* ?b . ?d))))))

(define-category monoid (?* ?1) ((groupoid ?*))
  (with-types ((md (monoid ?* ?1)))
    (of-type ?1 md)
    (for-all ((?a md))
      (<-> (?* ?1 ?a) ?a)
      (<-> (?* ?a ?1) ?a))
    (for-all ((?a md) (?b (list-of md))) 
      (<-> (?*) ?1)
      (<-> (?* ?a . ?b) (?* ?a (?* . ?b))))))

(define-category group (?* ?1 ?/) ((monoid ?* ?1))
  (with-types ((gr (group ?* ?1 ?/)))
    (for-all ((?a gr))
      (of-type (?/ ?a) gr)
      (<-> (?* (?/ ?a) ?a) ?1)
      (<-> (?* ?a (?/ a)) ?1))
    (for-all ((?a gr)
              (?b (list-of gr)))
      (<-> (?/ ?a . ?b) (?* ?a (?/ (?* . ?b)))))))

(define-category abelian (?+ ?0 ?-) ((group ?+ ?0 ?-))
  (with-types ((ab (abelian ?+ ?0 ?-)))
    (for-all ((?a ?b ?ab) (?c (list-of ?ab)))
      (<-> (?+ ?a ?b . ?c) (?+ ?b ?a . ?c)))))

(define-category ring (?+ ?0 ?- ?* ?1) ((abelian ?+ ?0 ?-) (monoid ?* ?1))
  (with-types ((rg (ring ?+ ?0 ?- ?* ?1)))
    (for-all ((?a ?b ?c rg))
      ((?* ?a (?+ ?b ?c)) (?+ (?* ?a ?b) (?* ?a ?c))))))

(define-category integrity-domain (?+ ?0 ?- ?* ?1) ((ring ?+ ?0 ?- ?* ?1))
  (with-types ((id (integrity-domain ?+ ?0 ?- ?* ?1)))
    (for-all ((?a id) (?b (and id (not (eql ?0)))))
      (<-> (eql ?0 (?* ?a ?b)) (eql ?0 ?a)))))

(define-category field (?+ ?0 ?- ?* ?1 ?/) ((integrity-domain ?+ ?0 ?- ?* ?1))
  (with-types ((fd (field ?+ ?0 ?- ?* ?1 ?/)))
    (for-all ((?a (and fd (not (eql ?0)))))
      (<-> (?* (?/ ?a) ?a) ?1)
      (<-> (?* ?a (?/ ?a)) ?1))
    (for-all ((?a ?b fd))
      (<-> (?* ?a ?b) (?* ?b ?a)))))

(define-category quotient-field (?g ?+ ?0 ?- ?* ?1 ?/) ((field ?+ ?0 ?- ?* ?1 ?/))
  (assert (type-p ?g (group ?+ ?0 ?-)))
  (with-types ((Qf (quotient-field ?g ?+ ?0 ?- ?* ?1 ?/)))
    (subtype-p ?g Qf)))



;;;;* TODO
(define-ruleset order (logic)
  (:types (?set (set ?=))
          (?os (ordered-set ?< ?=) (set ?=)))
  (:rules ((boolean (?a ?b ?set)
                    (?c (list-of ?set))) ((= ?a) t)
                                         ((= ?a ?b . ?c) (and (= ?a ?b) (= ?b . ?c)))
                                         ((= ?a ?b) (= ?b ?a)))

          ((boolean (?a ?b ?set)
                    (?c (list-of ?set))) ((< ?a) t)
                                         ((< ?a ?b . ?c) (and (< ?a ?b) (< ?b . ?c)))
                                         ((< ?a ?b) (not (< ?b ?a)))

                                         ((<= ?a) t)
                                         ((<= ?a ?b . ?c) (and (or (< ?a ?b) (= ?a ?b)) (<= ?b . ?c)))

                                         ((> ?a) t)
                                         ((> ?a ?b . ?c) (and (not (<= ?b ?a)) (> ?b . ?c)))

                                         ((>= ?a) t)
                                         ((>= ?a ?b . ?c) (and (not (< ?b ?a)) (>= ?b . ?c))))))

;;;;** Arithmetics Rules
(define-ruleset arithmetics ()
  (:signatures (0 number)
               (1 number)
               ((+ . number) number)
               ((- number . number) number)
               ((* . number) number)
               ((/ number . number) number))
  (:rules ((+) . 0)
          ((+ ?a) . ?a)
          ((+ ?a . ?b) . (+ ?a (+ . ?b)))
          ((+ ?a ?b . ?c) . (+ ?b ?a . ?c))
          ((+ 0 . ?a) . (+ . ?a))
          
          ((+ ?a (- ?a) . ?b) . (+ . ?b))
          ((- (- ?a)) . ?a)

          ((- ?a . ?b) . (+ ?a (- (+ . ?b))))
          
          ((- (+ ?a . ?b)) . (+ (- ?a) (- (+ . ?b))))
          
          ((*) . 1)
          ((* ?a) . ?a)
          ((* ?a . ?b) . (* ?a (* . ?b)))
          ((* ?a ?b . ?c) . (* ?b ?a . ?c))
          ((* 1 . ?a) . (* . ?a))

          ((* ?a (/ ?a) . ?b) . (* . ?b))
          ((/ (/ ?a)) . ?a)
          
          ((/ ?a . ?b) . (* ?a (/ (* . ?b))))
          
          ((/ (* ?a . ?b)) . (* (/ ?a) (/ (* . ?b))))
          ;; Distributivity between * and +
          ((* ?a (+ ?b . ?c)) . (+ (* ?a ?b) (* ?a (+ . ?c))))))




;;;;** Logic Rules
(define-ruleset logic ()
  (:atom t boolean)
  (:atom nil boolean)
  (:operator (and . boolean) boolean)
  (:operator (or . boolean) boolean)
  (:operator (not boolean) boolean)
  (:operator (xor . boolean) boolean)
  (:rules ((and) . t)
          ((and ?a) . ?a)
          ((and ?a . ?b) . (and ?a (and . ?b)))
          ((and ?a ?b . ?c) . (and ?b ?a . ?c))
          ((and t . ?a) . (and . ?a))
          ((and nil . ?a) . nil)
   
          ((or) . nil)
          ((or ?a) . ?a)
          ((or ?a . ?b) . (or ?a (or . ?b)))
          ((or ?a ?b . ?c) . (or ?b ?a . ?c))
          ((or nil . ?a) . (or . ?a))
          ((or t . ?a) . t)
   
          ((and (or ?a . ?b) . ?c) . (or (and ?a . ?c) (and ?b . ?c)))
   
          ((not t) . nil)
          ((not (not ?a)) . ?a)
   
          ((not (or ?a . ?b)) . (and (not ?a) (not (or . ?b))))

          ((xor) . nil)
          ((xor ?a . ?b) . (and (or ?a (xor . ?b)) (not (and ?a (xor .?b)))))))

;;;;** Comparison Rules
(define-ruleset comparison (logic)
  (:operator (= ?comparable . ?comparable) boolean)
  (:operator (< ?comparable . ?comparable) boolean)
  (:operator (<= ?comparable . ?comparable) boolean)
  (:operator (> ?comparable . ?comparable) boolean)
  (:operator (>= ?comparable . ?comparable) boolean)
  (:rules ((= ?a) . t)
          ((= ?a ?b . ?c) . (and (= ?a ?b) (= ?b . ?c)))
          ((= ?a ?b) . (= ?b ?a))

          ((< ?a) . t)
          ((< ?a ?b . ?c) . (and (< ?a ?b) (< ?b . ?c)))
          ((< ?a ?b) . (not (< ?b ?a)))

          ((<= ?a) . t)
          ((<= ?a ?b . ?c) . (and (or (< ?a ?b) (= ?a ?b)) (<= ?b . ?c)))

          ((> ?a) . t)
          ((> ?a ?b . ?c) . (and (not (<= ?b ?a)) (> ?b . ?c)))

          ((>= ?a) . t)
          ((>= ?a ?b . ?c) . (and (not (< ?b ?a)) (>= ?b . ?c)))))

;;;;** Elementary Contracts
(define-ruleset contracts (arithmetics comparison)
  (:atom $zero contract)
  (:operator ($one item) contract)
  (:operator ($scale number contract) contract)
  (:operator ($give contract) contract)
  (:operator ($all . contract) contract)
  (:operator ($if boolean contract contract) contract)
  (:operator ($at date-time contract) contract)
  (:rules (($scale ?a $zero) . $zero)
          (($scale 1 ?a) . ?a)
          (($scale 0 ($one ?a)) . $zero)
          ;;(($scale 0 ?a) . $zero) ; Not sure that this is really true
          (($scale ?a ($scale ?b ?c)) . ($scale (* ?a ?b) ?c))

          (($give $zero) . $zero)
          (($give ($one ?a) . ($scale -1 ($one ?a))))
          (($give ($give ?a)) . ?a)
          
          (($all) . $zero)
          (($all ?a) . ?a)
          (($all ?a . ?b) . ($all ?a ($all . ?b)))
          (($all ?a ?b . ?c) . ($all ?b ?a . ?c))
          (($all $zero . ?a) . ($all . ?a))
          
          (($if t ?a ?b) . ?a)
          (($if nil ?a ?b) . ?b)
          (($if (xor ?a . ?b) ?c ?d) . ($all ($if (and ?a (not ?b)) ?c ?d) ($if (and (not ?a) ?b) ?c ?d)))

          (($at ?a $zero) . $zero)
          (($at ?a ($at ?b ?c)) . ($if (< ?a ?b) ($at ?b ?c) $zero))
          
          (($scale ?a ($give ?b)) . ($give ($scale ?a ?b)))
          (($scale ?a ($all ?b . ?c)) . ($all ($scale ?a ?b) ($scale ?a ($all . ?c))))
          (($scale (+ ?a . ?b) ?c) . ($all ($scale ?a ?c) ($scale (+ . ?b) ?c)))
          (($scale ?a ($if ?b ?c ?d)) . ($if ?b ($scale ?a ?c) ($scale ?a ?d)))
          (($scale ?a ($at ?b ?c)) . ($at ?b ($scale ?a ?c)))
          
          (($give ($all ?a . ?b)) . ($all ($give ?a) ($give ($all . ?b))))
          (($give ($if ?a ?b ?c)) . ($if ?a ($give ?b) ($give ?c)))
          (($give ($at ?a ?b)) . ($at ?a ($give ?b)))

          (($if ?a ($all ?b ?d) ($all ?c ?e)) . ($all ($if ?a ?b ?c) ($if ?a ?d ?e)))
          
          (($at ?a ($all ?b . ?c)) . ($all ($at ?a ?b) (at ?a ($all . ?c))))))

(define-ruleset meta ()
  ((list) . nil)
  ((list ?a) . (cons ?a nil))
  ((list ?a . ?b) . (cons ?a (list . ?b)))
  
  ((last nil) . nil)
  ((last (list ?a) . (list ?a)))
  ((last (list ?a ?b . ?c)) . (last (list ?b . ?c))))

(define-ruleset autocall (contracts)
  (:operator (auto-call-call-feature contract (list date-time) (list boolean) (list contract)) . contract)
  (:operator (auto-call-coupon (list date-time) (list boolean) (list contract)) . contract)
  (:operator (auto-call-final-redemption date-time boolean contract) . contract)
  (:rules ((auto-call-call-feature ?contract nil ?conditions ?redemptions) . ?contract)
          ((auto-call-call-feature ?contract ?calldates nil ?redemptions) . ?contract)
          ((auto-call-call-feature ?contract ?calldates ?conditions nil) . ?contract)
          ((auto-call-call-feature ?contract (?calldate . ?calldates) (?condition . ?conditions) (?redemption . ?redemptions))
           . ($all ?contract ($at ?calldate ($if ?condition
                                                 ($all ?redemption ($give ?contract))
                                                 (auto-call-call-feature ?contract ?calldates ?conditions ?redemptions)))))

          ((auto-call-coupon ?paydates ?conditions nil) . $zero)
          ((auto-call-coupon ?paydates nil ?payoffs) . $zero)
          ((auto-call-coupon nil ?conditions ?payoffs) . $zero)
          ((auto-call-coupon (?paydate . ?paydates) (?condition . ?conditions) (?payoff . ?payoffs))
           . ($at ?paydate ($if ?condition ?payoff (auto-call-coupon ?paydates ?conditions ?payoffs))))

          ((auto-call-final-redemption ?paydate ?condition ?redemption) . ($at ?paydate ($if ?condition ?redemption $zero)))

          ((auto-call ))))

(define-ruleset aa-deals (contracts)
  (:operator (structured-deal item symbol . contract) contract)
  (:operator (binary-option item (member :buy-sell=buy :buy-sell=sell) date-time item (member :payoff-type=standard)
                            number (member :option-type=call :option-type=put) number item))
  ((structured-deal ?currency :buy-sell=buy . ?contracts) . ($all . ?contracts))
  
  ((binary-option ?currency :buy-sell=buy ?expiry-date ?payoff-currency :payoff-type=standard ?cash-payoff :option-type=call ?strike-price ?equity)
   . ($at ?expiry-date ($if (< ?strike (@value ?equity ?currency)) ($scale ?cash-payoff ($one ?payoff-currency)) $zero)))
  ((binary-option ?currency :buy-sell=sell ?expiry-date ?payoff-currency :payoff-type=standard ?cash-payoff :option-type=call ?strike-price ?equity)
   . ($give (binary-option ?currency :buy-sell=buy ?expiry-date ?payoff-currency :payoff-type=standard ?cash-payoff :option-type=call ?strike-price ?equity)))
  ((binary-option ?currency :buy-sell=buy ?expiry-date ?payoff-currency :payoff-type=standard ?cash-payoff :option-type=put ?strike-price ?equity)
   . ($at ?expiry-date ($if (< (@value ?equity ?currency) ?strike) ($scale ?cash-payoff ($one ?payoff-currency)) $zero)))
  ((binary-option ?currency :buy-sell=sell ?expiry-date ?payoff-currency :payoff-type=standard ?cash-payoff :option-type=put ?strike-price ?equity)
   . ($give (binary-option ?currency :buy-sell=buy ?expiry-date ?payoff-currency :payoff-type=standard ?cash-payoff :option-type=put ?strike-price ?equity)))
  ((barrier-binary-option ? ?)))
