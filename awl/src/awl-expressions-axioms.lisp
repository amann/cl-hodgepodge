


(define-category boolean (0 1 not and or) ()
  (of-type 0 $)
  (of-type 1 $)
  (define-operation not ($) $
    (<-> (not 1) 0)
    (<-> (not (not a)) a))
  (define-operation and (&rest $) $
    (<-> (and) 1)
    (<-> (and a 1) a)
    (<-> (and a . c) (and a (and . c)))
    (<-> (and a b . c) (and b a . c)))
  (define-operation or (&rest $) $
    (<-> (or) 0)
    (<-> (or a . c) (not (and (not a) (or . c))))))

(define-domain boolean '(nil t) (boolean nil t not and or))

(define-category set (=) ()
  (define-relation = ($ &rest $)
    (<-> (= a) t)
    (<-> (= a b . c) (= b a . c))
    (<-> (= a b . c) (and (= a b) (= b . c)))))

(define-category magma (*) ()
  (define-operation * ($ $) $))

(define-category groupoid (*) ((magma *))
  (define-operation * ($ &rest $) $
    (<-> (* a (* b c)) (* (* a b) c))
    (<-> (* a) a)
    (<-> (* a b . c) (* a (* b . c)))))
(define-category monoid (* 1) ((groupoid *))
  (of-type 1 $)
  (define-operation * (&rest $) $
    (<-> (* a 1) a)
    (<-> (* 1 a) a)
    (<-> (*) 1)
    (<-> (* a) a)))
(define-category group (* 1 /) ((monoid * 1))
  (define-operation / ($ &rest $) $
    (define-operation * (&rest $) $
      (<-> (* (/ a) a) 1)
      (<-> (* a (/ a)) 1)
      (<-> (/ a . b) (* a (/ (* . b)))))))

(define-category abelian (+ 0 -) ((group + 0 -))
  (define-operation + (&rest $) $
    (<-> (+ a b . c) (+ b a . c))))

(define-category ring (+ 0 - * 1) ((abelian + 0 -) (monoid * 1))
  (define-operation + (&rest $) $
    (define-operation * (&rest $) $
      (<-> (* a (+ b c)) (+ (* a b) (* a c)))
      (<-> (* (+ a b) c) (+ (* a c) (* b c))))))

(define-category integrity-domain (+ 0 - * 1) ((ring + 0 - * 1))
  (with-types (($* (and $ (not (eql 0)))))
    (declare (of-category $* (monoid * 1)))
    (define-operation * (&rest $*) $*)))

(define-category field (+ 0 - * 1 /) ((integrity-domain + 0 - * 1))
  (with-types (($* (and $ (not (eql 0)))))
    (declare (of-category $* (abelian * 1 /)))))

(define-category quotient-field (g + 0 - * 1 /) ((field + 0 - * 1 /))
  (assert (of-category-p g (group + 0 -)))
  (declare (subtype g $)))


