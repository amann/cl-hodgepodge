;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Order Relations

(defun exploit-transitivity (binary-operator)
  (lambda (a &rest rest)
    (every binary-operator (cons a rest) rest)))



;;;;* Lattice
;;;;
;;;; A lattice is a partially ordered set (poset) in which for every
;;;; two elements exist a supremum and an infimum -- the join and
;;;; the meet.
;;;;
;;;; Consider a poset P with order relation <=.  We extend it to the poset Q
;;;; associated to the free distributive lattice generated by the elements of
;;;; the poset P.  We do this by defining the binary operations JOIN and MEET
;;;; satisfying:
;;;; - Commutativity: (join a b) == (join b a); (meet a b) == (meet b a)
;;;; - Associativity: (join a (join b c)) == (join (join a b) c)
;;;;                  (meet a (meet b c)) == (meet (meet a b) c)
;;;; - Absorption:    (join a (meet a b)) == a; (meet a (join a b)) == a
;;;; - Distributivity: (meet a (join b c)) == (join (meet a b) (meet a c))
;;;; which implies:    (join a (meet b c)) == (meet (join a b) (join a c))
;;;;
;;;; We have (meet a b) == a whenever (<= a b), similarly (join a b) == b
;;;; whenever (<= a b). In general we have (<= a (join a b)) and
;;;; (<= (meet a b) a).
;;;;
;;;; By the associativity law it makes sense to define
;;;; - (meet a) == a
;;;; - (meet a b . c) == (meet a (meet b . c))
;;;; - (join a) == a
;;;; - (join a b . c) == (join a (join b . c))
;;;; where c is any finite subset of the poset.
;;;; If the poset contains a top element 1, we can also define
;;;; - (meet) == 1
;;;; and for the bottom element 0,
;;;; - (join) == 0
;;;; The top element and the bottom element can also be defined as such.
;;;;
;;;; We have (meet . a) == (meet . (bottom-elts . a))
;;;; where (bottom-elts . a) = {x in a | for all y in a, (<= x y) => x = y}
;;;; and (join . a) == (join . (top-elts . a))
;;;; where (top-elts . a) = {x in a | for all y in a, (<= y x) => x = y}
;;;; 
;;;; Proof:
;;;; For (meet), there is nothing to do.
;;;; Consider (meet a . b) == (meet a (meet . b)) == (meet a (meet . (bottom-elts . b)))
;;;; == (meet a . (bottom-elts . b)).
;;;; Let c be in (bottom-elts . b) with (<= c a), then (bottom-elts a . b) == (bottom-elts . b)
;;;; By associativity and commutativity
;;;; (meet a . (bottom-elts . b)) == (meet a c . (set-minus (bottom-elts . b) c))
;;;; == (meet (meet a c) . (set-minus (bottom-elts . b) c))
;;;; == (meet c . (set-minus (bottom-elts . b) c))
;;;; == (meet . (bottom-elts . b)) == (meet . (bottom-elts a . b))
;;;; In the other case (a . (bottom-elts . b)) == (bottom-elts a . b)
;;;; and hence (meet a . (bottom-elts b)) == (meet . (bottom-elts a . b)) #
;;;; 
;;;; The bottom-elts and the top-elts are by construction antichains,
;;;; i.e. their elements are not mutually comparable.
;;;;
;;;; We have seen that for a subset (a . b),
;;;; - if there exists c in b with (<= c a) then
;;;; (bottom-elts a . (bottom-elts . b)) == (bottom-elts . b)
;;;; - else
;;;; (bottom-elts a . (bottom-elts b))
;;;; == (set a . (remove-if (lambda (x) (< a x)) (bottom-elts . b)))
;;;;
;;;; In a free lattice, let a be an antichain, let b be a subset of the
;;;; power set of a whose elements are mutually not comparable by set-inclusion.
;;;; Then (mapcar 'meet* b) is an antichain.
;;;;
;;;; Proof:
;;;; We have to show that for two elements c and d of b, their meets are
;;;; not comparable.  If (<= (meet . c) (meet . d)) then
;;;; (every (lambda (x) (<= (meet . c) x)) d) hence
;;;; (every (lambda (x) (== (meet . c) (meet x . c)) d) and
;;;; (every (lambda (x) (== (bottom-elts . c) (bottom-elts x . c))) d)
;;;; Since d and c are subsets of an antichain, we have (subsetp d c). #
;;;;
;;;; ...
;;;;
;;;; As a corollary, the elements of a free distributive lattice generated
;;;; by a set a of elements can be written as (join . (mapcar 'meet b))
;;;; where a is a subset of the power set of b whose elements are mutually not
;;;; comparable by set-inclusion -- i.e. an antichain in the power-set of a.
;;;;
;;;; Given a poset p, we want to build a free distributive lattice in which
;;;; the poset p would be embedded.  For an element x of the poset p, let
;;;; (down-set x) == (remove-if-not (lambda (y) (<= y x)) p).
;;;; The generators g are
;;;; (union (bottom-elts . p) (mapcar 'make-generator (set-minus p (bottom-elts . p))))
;;;; and we add the relations
;;;; (map (lambda (e) (assert (== e (join (gen e) . (top-elts . (set-minus (down-set e) e)))))) p)
;;;; where (gen e) == e if e in (bottom-elts p) and (gen e) is an abstract object else.
;;;; We have g == (mapset 'gen p) and #'gen is a bijection between p and g.
;;;;
;;;; Let (antichains g) be the set of antichains of (power-set g).  The elements
;;;; of the free distributive lattice over g are in a 1-1 relation with
;;;; (antichains g), by the fact that each element can be written as
;;;; (join . (mapcar 'meet a)) for each element a in (antichains g).
;;;;
;;;; For two elements a and b of (power-set g), we have (subsetp a b)
;;;; ==> (<= (meet . b) (meet . a)) and thus
;;;; (<= (join (meet . b) . c) (join (meet . a) . c)).
;;;; In fact these elements are direct neighbours.
;;;;
;;;; If we write for two elements a b the relation (<! a b) iff
;;;; (<= a c b) ==> (or (eql a c) (eql c b))  which means that b is a cover of a, we
;;;; have
;;;; (<! (join (meet a . b) . c) (join (meet . b) . c))
;;;; (<! (join . (mapcar 'meet e)) (join . (mapcar 'meet (list d . e))))
;;;; These are the only covers.
;;;;
;;;;
;;;; A recursive way is:
;;;; if (<= (meet . b) a) then (meet a . b) == (meet . b)
;;;; if (<= a (meet . b)) then (meet a . b) == a
;;;; where (<= a (meet . b)) iff (every (lambda (x) (<= a x)) b)
;;;; By distributivity, we have
;;;; (meet (join a . b) . c) == (meet (join a . b) (meet . c))
;;;; == (meet (meet . c) (join a . b))
;;;; == (meet (meet . c) (join a (join . b)))
;;;; == (join (meet (meet . c) a) (meet (meet . c) (join . b)))
;;;; == (join (meet a . c) (meet (join . b) . c))
;;;; As special case
;;;; (meet (join) . c) == (join)
;;;; And finally
;;;; (meet (meet . b) . c) == (meet . (bottom-elts (union b c)))
;;;;
;;;; This new lattice is then by construction distributive.  (To be proved)
;;;; We add to the lattice also an upper and a lower bound 1 and 0.
;;;; We define that two elements are disjoint if they have no common precedents.
;;;; In this case their meet is 0.  If the dag has a finite number of final
;;;; elements, i.e. without successors, their join is the upper bound of the
;;;; lattice.  In particular, if the dag has one single top element, it is also
;;;; the upper bound of the lattice.



;;;; (meet (join a . b) . c) == (join (meet a . c) (meet (join . b) . c))
;;;; As special case
;;;; (meet (join) . c) == (join)
;;;; And finally
;;;; (meet (meet . b) . c) == (meet . (lower-boundary (union b c)))
;;;;
;;;;
;;;;

;;;;* Free Distributive Lattice (FDL)


(defclass free-distributive-lattice ()
  ((join :initarg :join)
   (meet :initarg :meet)
   (bottom :initarg :bottom)
   (top :initarg :top)))
(defclass fdl-element ()
  ((lattice :initarg :lattice :initform (gensym "FDL") :reader lattice)
   (canonic-representation :initform nil :type list)))

(defgeneric canonic-representation (object lattice)
  (:method (object lattice)
    (list (list object)))
  (:method :around ((object fdl-element) lattice)
    (mapcar 'copy-list (call-next-method)))
  (:method ((object fdl-element) lattice)
    (if (eql lattice (lattice object))
        (slot-value object 'canonic-representation)
        (call-next-method))))

(defun awl::equal-as-set-p (a b &key (test #'eql))
  (and (subsetp a b :test test)
       (subsetp b a :test test)))

(defun simplify-representation (args test)
  ""
  (let ((subset-p (lambda (a b)
                    (subsetp a b :test test)))
        (set-equal-p (lambda (a b)
                       (equal-as-set-p a b :test test))))
    (awl:bottom-elts subset-p ;; subset-p on the args of MEET is dual
                     ;; to the order on the MEET of the args.
                     (remove-duplicates (mapcar (lambda (group)
                                                  (remove-duplicates group
                                                                     :test test
                                                                     :from-end t))
                                                args)
                                        :test set-equal-p
                                        :from-end t))))

(defmethod shared-initialize :after ((instance fdl-element) slot-names
                                     &key (test 'eql) args &allow-other-keys)
  (setf (slot-value instance 'canonic-representation) (simplify-representation args test)))

(declaim (ftype (function (fdl-element stream) (values)) print-fdl-element))
(defun print-fdl-element (object stream)
  (let ((slots '((join bottom) (meet top) nil))
        (lattice (lattice object)))
    (declare (type free-distributive-lattice lattice))
    (labels ((print-args (args slots)
               (destructuring-bind (operator . slots) slots
                 (if operator
                     (destructuring-bind (operator extremum) operator
                       (let ((args-length (length args)))
                         (case args-length
                           (0 (print-unreadable-object (object stream)
                                (princ (slot-value lattice extremum) stream)))
                           (1 (print-args (car args) slots))
                           (t (print-unreadable-object (object stream)
                                (princ (slot-value lattice operator) stream)
                                (dolist (sub-args args)
                                  (princ #\Space stream)
                                  (print-args sub-args slots)))))))
                     (princ args stream)))))
      (print-args (canonic-representation object (lattice object)) slots))))
(defmethod print-object ((object fdl-element) stream)
  (print-fdl-element object stream))

(defun awl::make-free-distributive-lattice (&key lattice (test 'eql))
  (let ((canonic-representation (lambda (arg)
                                  (canonic-representation arg lattice))))
    (flet ((join (&rest args)
             (make-instance 'fdl-element
                            :args (mapcan canonic-representation args)
                            :test test
                            :lattice lattice))
           (meet (&rest args)
             (make-instance 'fdl-element
                            :args (if args
                                      (mapcar (lambda (tuple) (apply 'append tuple))
                                              (awl:cartesian-product* (mapcar canonic-representation args)))
                                      (list nil))
                            :test test
                            :lattice lattice))
           (equal-p (a b)
             (awl::equal-as-set-p (canonic-representation a lattice)
                                  (canonic-representation b lattice)
                                  :test (lambda (a b) (awl::equal-as-set-p a b :test test)))))
      (values #'join #'meet #'equal-p (lambda (a b) (equal-p (join a b) b))))))

(defmacro awl::with-free-distributive-lattice ((join meet bottom top equalp orderp &key (test #'eql)) &body body)
  (let ((lattice (make-instance 'free-distributive-lattice :join join :meet meet :bottom bottom :top top)))
    (multiple-value-bind (join-fn meet-fn equalp-fn orderp-fn)
        (awl::make-free-distributive-lattice :lattice lattice :test test)
      `(awl:fbind ((,join ,join-fn) (,meet ,meet-fn) (,equalp ,equalp-fn) (,orderp ,orderp-fn))
         ,@body))))



#+ (or)

(flet ((join (&rest integers)
         (reduce '*
                 (awl:top-elts 'dividep integers)))
       (meet (&rest integers)
         (let ((bottom-elts (awl:bottom-elts 'dividep integers)))
           (case (length bottom-elts)
             (1 (first bottom-elts))
             (0 (error "Infinity"))
             (t  1)))))
  (join (join 5 3 6 7 8 9 3 2 1) (join) (meet 3)
        (meet 4 5 8 6 9 2 7 8 5 6 8 2)))









(defclass node-operation () ((operand-fn-slot :reader operands)
                             (precede-fn-slot :reader precede-fn)
                             (succede-fn-slot :reader succede-fn)))
(defmethod operands :around ((operation node-operation))
  (funcall (call-next-method)))
(defclass meet (node-operation) ())
(defmethod shared-initialize :after ((instance meet) slot-names &key precedep operands &allow-other-keys)
  (with-slots (operands-fn-slot precede-fn-slot succede-fn-slot)
      (setf operand-fn-slot (compile nil `(lambda () (copy-list ',operands)))
            precede-fn-slot (compile nil `(lambda (other)
                                            (or ,@(mapcar (lambda (op)
                                                            `(funcall ,precedep ,op other))
                                                          operands))))
            succede-fn-slot (compile nil `(lambda (other)
                                            (and ,@(mapcar (lambda (op)
                                                             `(funcall ,precedep other ,op))
                                                           operands)))))))
(defclass join (node-operation) ())
(defmethod shared-initialize :after ((instance meet) slot-names &key precedep operands &allow-other-keys)
  (with-slots (operand-fn-slot precede-fn-slot succede-fn-slot)
      (setf operand-fn-slot (compile nil `(lambda () (copy-list ',operands)))
            precede-fn-slot (compile nil `(lambda (other)
                                            (and ,@(mapcar (lambda (op)
                                                             `(funcall ,precedep ,op other))
                                                           operands))))
            succede-fn-slot (compile nil `(lambda (other)
                                            (or ,@(mapcar (lambda (op)
                                                            `(funcall ,precedep other ,op))
                                                          operands)))))))

;;;; x | y ←→ (x <= y) v (y <= x) : x and y are comparable.
;;;; 
;;;; x <= A ^ B ←→ (x <= A) ^ (x <= B)
;;;; 
;;;; A ^ B < x ←→ ¬(x <= A ^ B) ^ ((x | A) v (x | B))
;;;; 
;;;; A ^ B = x ←→ (x <= A ^ B) ^ ((x < y) → ¬(y <= A ^ B))


(defun extend-order-fn (order-fn)
  (flet ((precedep (a b)
           (typecase a
             (node-operation
              (funcall (precede-fn a) b))
             (t
              (typecase b
                (node-operation
                 (funcall (succede-fn b) a))
                (t
                 (funcall order-fn a b)))))))
    (declare (inline precedep))
    (function precedep)))


(defmacro with-extended-order ((ext-orderp order-fn &key (meet 'meet) (join 'join)) &body body)
  "Establish a lexical environment around BODY in which following function names are bound to the symbols hold in
- EXT-ORDERP (a b): the extended order predicate derived from ORDER-FN;
- MEET (&rest operands): the constructor to build the meet of operands;
- JOIN (&rest operands): the constructor to build the join of operands."
  (awl:with-gensyms (g!ext-orderp)
    `(let* ((,g!ext-orderp ,(extend-order-fn order-fn)))
       (flet ((,ext-orderp (a b)
                (funcall ,g!ext-orderp a b))
              (,meet (&rest operands)
                (make-instance 'meet :precedep ,g!ext-orderp :operands operands))
              (,join (&rest operands)
                (make-instance 'join :precedep ,g!ext-orderp :operands operands)))
         (declare (inline ,ext-orderp))
         ,@body))))





