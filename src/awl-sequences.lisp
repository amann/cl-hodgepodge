;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Sequences, Vectors and Lists
;;;;** Coertion between Sequences
;;;;
;;;; The function CL:COERCE uses the size information of the given type spec.
;;;; This can be quite painful if one wants to do something like
;;;; (coerce sequence (type-of other-sequence)) and sequence and other-sequence
;;;; have different sizes.  For this, we define the function awl::coerce-sequence
;;;; which does not take account of the size information.  We can even write
;;;; (awl::coerce-sequence sequence other-sequence) and the result will be the
;;;; coertion of sequence to the type of other-sequence.
;;;; TODO: It is not enough to strip the size, one should also upgrade the element
;;;; types.
(defgeneric sequence-type-strip-size-constraint (type)
  (declare (optimize speed))
  (:method (type)
    (cond ((subtypep type 'list) type)
          ((subtypep type 'vector) (typecase type
                                    (cons (case (car type)
                                            (simple-vector 'simple-vector)
                                            (vector `(vector ,(or (cadr type) '*) *))
                                            (t (car type))))
                                    (t type))))))
(defun awl::coerce-sequence (sequence type)
  "Coerce the sequence SEQUENCE to an object of type TYPE. TYPE must be a type spec of a subtype of sequence or an instance of a subtype of sequence."
  (let ((type (if (awl::type-spec-p type)
                  type
                  (type-of type))))
    (assert (subtypep type 'sequence) ()
            "The value TYPE is ~A and does not represent a subtype of sequence." type)
    (coerce sequence (sequence-type-strip-size-constraint type))))

;;;;** Sequences
;;;; TODO review the algorithm: it is inefficient but should work as expected.
(defun awl::remove-if* (test sequence &rest options
                        &key (key #'identity)
                          from-end start end count)
  "Same as cl:remove-if except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (let ((items-to-be-removed (apply #'cl:remove-if-not test sequence options))
        (resulting-sequence sequence))
    (map nil (lambda (item)
               (setq resulting-sequence
                     (apply #'cl:remove item resulting-sequence
                            :count 1 :test #'eq :key #'identity options)))
         items-to-be-removed)
    (values resulting-sequence items-to-be-removed)))

(defun awl::remove* (item sequence &rest options &key (test #'eql) key from-end start end count)
  "Same as cl:remove except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (apply #'awl::remove-if* (lambda (elt) (funcall test item elt))
         sequence options))

(declaim (ftype (function (* sequence &rest * &key (:key awl:function-designator) (:from-end boolean)
                             (:start (or null fixnum)) (:end (or null fixnum)) (:count (or null fixnum)))
                          sequence)
                awl::keep-if)
         (inline awl::keep-if))
(defun awl::keep-if (test sequence &rest options &key (key 'identity) from-end start end count)
  "Alias for cl:remove-if-not."
  (declare (ignore key from-end start end count))
  (apply 'remove-if-not test sequence options))

(defgeneric filter (fn sequence &key)
  (:documentation "Generic function for implementing awl:filter for sequences."))
(defun awl::filter (fn sequence &key key &aux acc)
  "You give filter a function and a sequence, and get back a list of whatever non-nil values are returned by the function as it is applied to the elements of the list."
  (flet ((test (x) (funcall fn (funcall key x))))
    (declare (inline test))
    (etypecase sequence
      (list (dolist (x sequence (nreverse acc))
              (let ((val (test x)))
                (when val (push val acc)))))
      (vector (loop :for x :across sequence
                    :for val := (test x)
                    :when val :collect val))
      (sequence (filter fn sequence :key key)))))

(define-compiler-macro awl::filter (fn list &key key)
  (awl::with-gensyms (g!acc g!x)
    `(let (,g!acc)
       (dolist (,g!x ',list (nreverse ,g!acc))
         (let ((val ,(awl::expand-funcall fn (awl::expand-funcall key g!x))))
           (if val (push val ,g!acc)))))))


(defun awl::classify (sequence &key (test 'eql) (key 'identity))
  "Return a list of sublists of elements of SEQUENCE; the elements in each sublist are, when applied to KEY, equivalent under TEST."
  (let* ((classes (make-hash-table :test test))
         keys result)
    (map nil (lambda (elt)
               (let ((key (funcall key elt))) 
                 (unless (nth-value 1 (gethash key classes))
                   (push key keys))
                 (push elt (gethash key classes))))
         sequence)
    (dolist (key keys)
      (push (nreverse (gethash key classes)) result))
    result))
(define-compiler-macro awl::classify (sequence &key (test 'eql) key)
  `(let* ((classes (make-hash-table :test ,test))
          keys result)
     (map nil (lambda (elt)
                (let ((key ,(awl::expand-funcall key 'elt))) 
                  (unless (nth-value 1 (gethash key classes))
                    (push key keys))
                  (push elt (gethash key classes))))
          ',sequence)
     (dolist (key keys)
       (push (nreverse (gethash key classes)) result))
     result))

;;;;** Split Sequence
(declaim (ftype (function (vector (or function symbol) &optional boolean) (values list list)) nsplit-vector))
(defun nsplit-vector (vector separator-p &optional include-separator)
  (let ((vector-length (length vector)))
    (awl:fbind ((shift-position (compile nil
                                          `(lambda (position)
                                             (declare (type fixnum position))
                                             ,(if include-separator
                                                  `(max ,vector-length (1+ position))
                                                  'position)))))
       (labels ((split (start result separators)
                  (let* ((position (or (and (< start vector-length)
                                            (position-if separator-p vector :start start))
                                       vector-length))
                         (result (cons (make-array (- (shift-position position) start)
                                                   :element-type (array-element-type vector)
                                                   :displaced-to vector
                                                   :displaced-index-offset start)
                                       result)))
                    (if (< position vector-length)
                        (split (1+ position) result (cons (aref vector position) separators))
                        (values (nreverse result)
                                (nreverse separators))))))
         (split 0 nil nil)))))

(declaim (ftype (function (list (or function symbol) &optional boolean) (values list list)) nsplit-list))
(defun nsplit-list (list separator-p &optional include-separator)
  "Destructively modify LIST by splitting it along SEPARATOR. Return as secondary value a list of the separators."
  (do ((prec (cons nil list) list)
       (list list (cdr list))
       ptr result separators)
      ((null list) (values (nreverse (cons ptr result))
                           (nreverse separators)))
    (when (funcall separator-p (car list))
      (push ptr result)
      (push (pop list) separators)
      (setf ptr list)
      (if include-separator
          (setf (cddr prec) nil)
          (setf (cdr prec) nil)))))

(defgeneric nsplit-sequence (sequence separator-p &optional include-separator)
  (:method ((sequence sequence) separator-p &optional include-separator)
    (let ((seq-length (length sequence)))
      (awl::fbind ((shift-position (compile nil
                                          `(lambda (position)
                                             ,(if include-separator
                                                  `(max ,seq-length (1+ position))
                                                  'position)))))
       (labels ((split (start result separators)
                  (let* ((position (or (and (< start seq-length)
                                            (position-if separator-p sequence :start start))
                                       seq-length))
                         (result (cons (subseq sequence start (shift-position position)) result)))
                    (if (< position seq-length)
                        (split (1+ position) result (cons (elt sequence position) separators))
                        (values (nreverse result)
                                (nreverse separators))))))
         (split 0 nil nil))))))

(declaim (ftype (function (sequence (or function symbol) &optional boolean) (values list list)) awl::nsplit-sequence)
         (inline awl::nsplit-sequence))
(defun awl::nsplit-sequence (sequence separator-p &optional include-separator)
  "Return a list of sub sequences of SEQUENCE obtained by splitting SEQUENCE along SEPARATOR.  If INCLUDE-SEPARATOR is true, the subsequences contain the separator at their end (except maybe for the last subsequence).  If SEQUENCE is a vector, the sub sequences are arrays displaced on SEQUENCE. If SEQUENCE is a list, it is destructively modified by the splitting."
  (etypecase sequence
    (list (nsplit-list sequence separator-p include-separator))
    (vector (nsplit-vector sequence separator-p include-separator))
    (sequence (nsplit-sequence sequence separator-p include-separator))))


(declaim (ftype (function (string (or character string list (function (character) *)) &optional boolean) list) awl::nsplit-string))
(defun awl::nsplit-string (string separator &optional include-separator)
  "Return a list of displaced substrings of STRING obtained by splitting STRING along SEPARATOR.  If INCLUDE-SEPARATOR is true, the subsequences contain the separator at their end (except maybe for the last subsequence).  SEPARATOR may be a character, a list of characters or a string -- in which case it holds as a bag of separators -- or a function taking a character and returning true where the string has to be split."
  (nsplit-vector string (etypecase separator
                          (character (lambda (x) (declare (character x)) (eql x separator)))
                          ((or string list) (lambda (x) (declare (character x)) (find x separator))) 
                          (function separator))
                 include-separator))

(defun awl::seq-contains (test-seq sequence &key (test 'eql) key1 key2)
  "Return the position of the first match from left if SEQUENCE contains a subsequence whose elements are successively equal to those of TEST-SEQ under the equality-predicate TEST, NIL else. If TEST-SEQ is empty, return 0, since an empty set is always contained in any set."
  (let* ((sequence (map 'simple-vector (or key2 'identity) sequence))
         (test-seq (if key1 (map 'simple-vector key1 test-seq) test-seq))
         (length-test-seq (length test-seq))
         (first-item (elt test-seq 0))
         (max-pos (- (length sequence) length-test-seq)))
    (cond
      ((zerop (length test-seq)) 0)
      ((minusp max-pos) nil)
      (t (let ((displaced-sequence (make-array length-test-seq :displaced-to sequence
                                                               :displaced-index-offset 0)))
           (loop :for start := 0 :then (1+ pos)
                 :for pos := (position first-item sequence :test test :start start)
                 :until (or (null pos)
                            (when (< max-pos pos) (setq pos nil) t)
                            (every test test-seq (adjust-array displaced-sequence length-test-seq
                                                               :displaced-to sequence
                                                               :displaced-index-offset pos)))
                 :finally (return pos)))))))


(declaim (ftype (function (awl:function-designator sequence &key (:key awl:function-designator)) list)
                awl::bottom-elts
                awl::top-elts)
         (inline awl::bottom-elts awl::top-elts))
(defun %expand-bottom-filter (order-pred key)
  (flet ((_<_ (a b)
           `(and ,(awl:expand-funcall `',order-pred a b)
                 (not ,(awl:expand-funcall `',order-pred b a))))
         (cell-value (a) (when key `(&aux (,a (cdr ,a))))))
    (let ((<s `(lambda (a ,@(cell-value 'a)) ,(_<_ 'a 's)))
          (s< `(lambda (a ,@(cell-value 'a)) ,(_<_ 's 'a)))
          (s (if key
                 `(if cells
                      (let ((cell (pop cells)))
                        (setf (car cell) o
                              (cdr cell) s))
                      (cons o s))
                 's)))
      `(lambda (,@(when key `(&aux cells)))
         (declare (special antichain))
         (lambda (s ,@(when key `(&aux (o s) (s ,(awl:expand-funcall `',key 's)))))
           (if (some ,<s antichain)
               antichain
               (setf antichain (cons ,s (remove-if ,s< antichain)))))))))
(defun awl::bottom-elts (order-pred sequence &key key)
  "Return a list of the bottom elements of SEQUENCE with respect to the order predicate ORDER-PRED lifted by KEY if KEY is not null.  If we denote by <= the order, the set of the bottom elements is defined as {x in SEQUENCE | for all y in SEQUENCE, (y <= x) ==> (x <= y)}.  By construction this is an antichain."
  (let (antichain)
    (declare (special antichain))
    (map nil
         (funcall (compile nil (%expand-bottom-filter order-pred key)))
         sequence)
    (when key
      (setf antichain (mapcar 'car antichain)))
    (nreverse antichain)))
(declaim (notinline awl::bottom-elts))
(defun awl::top-elts (order-pred sequence &key key)
  "Return a list of the top elements of SEQUENCE with respect to the order predicate ORDER-PRED lifted by KEY if KEY is not null."
  (declare (inline awl::bottom-elts))
  (awl::bottom-elts (awl::dual-order order-pred) sequence :key key))
(declaim (notinline awl::top-elts))



#+(or) (declaim (ftype (function ((awl:list-of sequence)) list) awl::cartesian-product*)
         (inline awl::cartesian-product*))
(declaim (inline awl::cartesian-product*))
(defun awl::cartesian-product* (sequences)
  "Return the cartesian product, in form of a list of lists, of sets given as a list of sequences in SEQUENCES."
  (labels ((product (sequences &aux (rest (rest sequences)))
             (if rest
                 (apply 'append (map 'list (lambda (elt)
                                             (loop :for other-elt :in (product rest)
                                                   :collect (cons elt other-elt)))
                                     (first sequences)))
                 (map 'list 'list (first sequences)))))
    (product sequences)))
(declaim (notinline awl::cartesian-product*))

(declaim (ftype (function (&rest sequence) list) awl::cartesian-product))
(defun awl::cartesian-product (&rest sequences)
  "Return the cartesian product, in form of a list of lists, of sets given in SEQUENCES."
  (declare (inline awl::cartesian-product*))
  (awl::cartesian-product* sequences))

(defun awl::power-set (set)
  "Return the power set in form of a list of lists of the set SET given as a list."
  (labels ((build-power-set (prev set &aux (power-set prev))
             (if set
                 (dolist (subset prev (build-power-set power-set (rest set)))
                   (push (cons (first set) subset) power-set))
                 power-set)))
    (build-power-set (list nil) (reverse set))))






(defun awl::insert (pos sequence &rest values)
  "Return a fresh sequence with VALUES inserted at POS. If POS is negative the position is relative to the end of SEQUENCE, -1 being after the last element."
  (let* ((type (let ((type (type-of sequence)))
                 (etypecase type
                   (cons (car type))
                   (null 'list)
                   (symbol (case type
                             (null 'list)
                             (t type))))))
         (sl (length sequence))
         (output-type (case type
                        (simple-vector (list type (+ sl (length values))))
                        (t type)))
         (sl+1 (1+ sl))
         (pos (mod (max (min pos sl) (- sl+1)) sl+1)))
    (concatenate output-type (subseq sequence 0 pos) values (subseq sequence pos))))


(defun awl::ninsert (pos sequence &rest values)
  "Return a sequence of same type as SEQUENCE with VALUES inserted at POS. NINSERT may destructively modify SEQUENCE by inserting values at position POS. If POS is negative the position is relative to the end of SEQUENCE, -1 being after the last element. It is unspecified and probably an error (or worse: e.g. infinite loop) if SEQUENCE is a unproper list."
  (etypecase sequence
    (null values)
    (cons (let* ((list sequence)
                 (ll (length list))
                 (ll+1 (1+ ll))
                 (pos (mod (max (min pos ll) (- ll+1)) ll+1))) 
            (if (zerop pos)
                (progn (awl::ninsert 1 list (car list))
                       (apply #'awl::ninsert 1 list (cdr values))
                       (rplaca list (car values)))
                (setf (cdr (nthcdr (1- pos) list))
                      (append values (nthcdr pos list))))
            list))
    (sequence (apply #'awl::insert pos sequence values))))


;;;;** Sorting
;;;;*** Order Relations
;;;;

(defun awl::dual-order (order-predicate)
  "Return the dual order predicate induced from ORDER-PREDICATE."
  (lambda (a b) (funcall order-predicate b a)))


(defun awl::lexicographic-order (order-predicate &rest keys &key test key start1 start2 end1 end2)
  "Return a 2-parameter predicate function representing the lexicographic (pre)order on the sequences induced by ORDER-PREDICATE on the elements of the sequences.  The return value of the returned predicate nil if the arguments are not comparable; else it is the position of the first mismatch of the sequences.  See also CL:STRING<= etc. "
  (declare (ignore test start1 start2 end1 end2))
  (compile nil (awl:with-gensyms (g!a g!b g!pos g!lengtha)
                 `(lambda (,g!a ,g!b)
                    (let ((,g!pos (mismatch ,g!a ,g!b ,@keys))
                          (,g!lengtha (length ,g!a)))
                      (if ,g!pos
                          (when (or (<= g!lengtha ,g!pos)
                                    (and (< ,g!pos (length ,g!b))
                                         ,(awl:expand-funcall `',order-predicate
                                                              (awl:expand-funcall `',key `(elt ,g!a ,g!pos))
                                                              (awl:expand-funcall `',key `(elt ,g!b ,g!pos)))))
                            ,g!pos)
                          ,g!lengtha))))))

(defun awl::projective-order (order-predicate bottom)
  "Return an order predicate having BOTTOM as a smallest element and projecting the downset of BOTTOM to the top locally keeping the order relation."
  (flet ((<=* (a b) (funcall order-predicate a b)))
    (declare (inline <=*))
    (flet ((<* (a b) (and (<=* a b) (not (<=* b a)))))
      (declare (inline <*))
      (lambda (a b)
        (cond ((<=* bottom a)
               (or (<=* a b) (<* b bottom)))
              ((<* a bottom)
               (and (<=* a b) (<* b bottom)))
              (t (<=* a b)))))))

;;;;*** Order Vector of a Sequence
;;;;
;;;; An order vector of a sequence is a simple vector containing the original position indices
;;;; of the elements of the sequence sorted after some order-predicate.  Such order vector
;;;; represents the sorting operation on the sequence and can be applied to any other sequence
;;;; of same length by (map type (lambda (i) (elt other-sequence i)) order-vector).  The inverse
;;;; sorting operation is represented by (order order-vector '<).
(defmacro %order-expr (order-fn-name sequence predicate key)
  `(let ((sequence ,sequence)
         (key (or ,key #'identity)))
     (multiple-value-bind (sequence key)
         (typecase sequence
           (list (values (map 'simple-vector ,key sequence) nil))
           (t (values sequence ,key)))
       (let ((indices (map-into (make-sequence 'simple-vector (length sequence))
                                (let ((i -1)) (lambda () (incf i))))))
         (let ((deref (compile nil
                               `(lambda (i)
                                  ,(awl:expand-funcall `',key `(,(typecase sequence
                                                                   (simple-vector 'svref)
                                                                   (vector 'aref)
                                                                   (t 'elt)) ',sequence i))))))
           (declare (dynamic-extent deref))
           (,order-fn-name indices ,predicate :key deref))))))
(defun awl::order (sequence predicate &key key)
  "Return the order vector (the vector of the original position indices of the ordered elements of SEQUENCE) of SEQUENCE sorted with the order predicate PREDICATE lifted by the function KEY.  The original sequence SEQUENCE is not modified.  The return value is of type SIMPLE-VECTOR."
  (%order-expr sort sequence predicate key))
(defun awl::stable-order (sequence predicate &key key)
  "Return the order vector (the vector of the original position indices of the ordered elements of SEQUENCE) of SEQUENCE stable-sorted with the order predicate PREDICATE lifted by the function KEY.  The original sequence SEQUENCE is not modified.  The return value is of type SIMPLE-VECTOR."
  (%order-expr stable-sort sequence predicate key))

;;;;*** Binary Search on a Sequence
(defclass awl::binary-search (c2mop:funcallable-standard-object)
  (index-fn)
  (:metaclass c2mop:funcallable-standard-class))

(declaim (ftype (function (function simple-vector &optional awl::type-spec) (values (or null fixnum) (or null fixnum) fixnum))))
(defun compile-binary-search-function (order-pred grid &optional elt-type)
  "Take an order predicate ORDER-PRED on the elements of the accordingly ordered simple-vector GRID. The optional argument ELT-TYPE indicates the type of the elements in GRID. Return a function taking a unique argument X and which returns the entities xl as primary, xu as secondary and i as third value satisfying either
-- xl = nil, x < xu, i = -1 and xu is the leftmost element of GRID,
-- not(x < xl) and x < xu, 0 <= i < length(GRID), xl is the ith element and xu the i+1th element of GRID,
-- not(x < xl), xu = nil, i = length(GRID) - 1 and xl is the rightmost element of GRID,
with respect to the preorder relation < given by PREORDER-FN and defined on the elements of GRID.  Notice: if GRID contains elements which are indistinguible for < then the leftmost of those elements can only be an xu and the rightmost element can only be a xl; in particular, if there are more than 2 elements which are indistinguible, those inbetween will never be an output of the binary search."
  (let ((m (1- (length grid))))
    (let* ((i* '(setq i (the unsigned-byte (+ lo (floor (the unsigned-byte (- up lo)) 2)))))
           (x<xl (awl::expand-funcall `',order-pred 'x `(setq xl (svref ,grid ,i*))))
           (x<xu (awl::expand-funcall `',order-pred 'x `(setq xu (svref ,grid (1+ i))))))
      (compile nil `(lambda (x)
                      (declare ,@(when elt-type `((type ,elt-type x))) (optimize speed))
                      (let ((i 0) (lo 0) (up ,m) xl xu)
                        (declare (unsigned-byte i lo up))
                        (loop (cond (,x<xl (when (zerop i) (return (values nil xl -1)))
                                           (setq up i))
                                    ((= ,m i) (return (values xl nil ,m)))
                                    (,x<xu (return (values xl xu i)))
                                    (t (setq lo (the unsigned-byte (1+ i))))))))))))

(defmethod shared-initialize :after ((instance awl::binary-search) slot-names
                                     &key order-pred sequence &allow-other-keys)
  (let* ((sequence (coerce sequence 'vector))
         (order (awl::stable-order sequence order-pred))
         (simple-vector (map 'simple-vector (lambda (i) (svref sequence i)) order))
         (m (1- (length sequence))))
    (awl::fbind ((binary-search (compile-binary-search-function order-pred simple-vector)))
     (c2mop:set-funcallable-instance-function instance (lambda (x) (binary-search x))))
    (setf (slot-value instance 'index-fn) (lambda (i)
                                            (values (if (eql i -1) i (svref order i))
                                                    (if (eql i m) (1+ i) (svref order (1+ i))))))))

(declaim (ftype (function (awl::binary-search *) (values * * fixnum fixnum * *)) awl::binary-search))
(defun awl::binary-search (binary-search x)
  "Return the values xl, xu and i satisfying either
-- xl = nil, x < xu, i = -1 and xu is the leftmost element of GRID,
-- not(x < xl) and x < xu, 0 <= i < length(GRID), xl is the ith element and xu the i+1th element of GRID,
-- not(x < xl), xu = nil, i = length(GRID) - 1 and xl is the rightmost element of GRID,
with respect to the preorder relation < given by PREORDER-FN and defined on the elements of GRID."
  (funcall binary-search x))
(declaim (ftype (function (awl::binary-search fixnum) (values * * fixnum)) awl::binary-search-original-indices))
(defun awl::binary-search-original-indices (binary-search i)
  (funcall (slot-value binary-search 'index-fn) i))

;;;;** Lists
;;;; Several utilities which might be more or less useful. I think
;;;; most of them are not useful... functions whose documentation
;;;; is more complicated than reprogram the equivalent function are
;;;; at least questionable.

(declaim (ftype (function (list (integer 1 #.(- most-positive-fixnum 2)))) awl::group))
(defun awl::group (list n)
  "Return a partition of the proper list LIST in form of a list of sublists of lenght N from left to right; the last sublist is of length (mod (length LIST) N)."
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                   (rec rest (cons (subseq list 0 n) acc))
                   (nreverse (cons list acc))))))
    (when list
      (rec list nil))))

(defun awl::prune (test tree)
  "Every leaf for which the function returns true is removed."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))


(defun awl::find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (awl::find2 fn (cdr lst))))))

(defun awl::before (x y list &key (test #'eql) key)
  "Return the cons in LIST beginning with the object X given as the first argument if we encounter the first argument before encountering the second object Y.  It will also return true if the second argument doesn't occur in the list at all.  TEST is the equality test and KEY, if given, is applied to each element of LIST."
  (labels ((before (list)
             (and list
                  (let ((first (if key
                                   (funcall key (car list))
                                   (car list))))
                    (cond ((funcall test y first) nil)
                          ((funcall test x first) list)
                          (t (awl::before x y (cdr list) :test test :key key)))))))
    (before list)))

(defun awl::after (x y list &key (test #'eql) key)
  "Return the cons in LIST beginning with the object X given as the first argument if we encounter the first argument X after encountering the second object Y.  TEST is the equality test and KEY, if given, is applied to each element of LIST."
  (let ((rest (awl::before y x list :test test :key key)))
    (and rest (member x rest :test test :key key))))

(defun awl::duplicate (obj list &key (test #'eql) key)
  "Return the cons in LIST beginning with the object OBJ given as the first argument if we encounter the first argument a second time in LIST."
  (member obj (cdr (member obj list :test test :key key))
          :test test :key key))

(defun awl::split-if (fn list)
  "Return as primary value a new list with the leftmost elements of the proper list LIST for which the function FN, when applied to those elements, returns NIL. The secondary value is the rest of LIST from the first element of LIST on for which FN returns non NIL."
  (let ((acc nil))
    (do ((src list (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun awl::most (order-pred fn list)
  "Return as primary value the first element of the proper list LIST for whom the value returned by the function FN when applied to those elements is maximal with respect to ORDER-PRED. The secondary value is the value returned by FN."
  (if (null list)
      (values nil nil)
      (let* ((wins (car list))
             (max (funcall fn wins)))
        (dolist (obj (cdr list))
          (let ((score (funcall fn obj)))
            (when (funcall order-pred score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun awl::best (fn list)
  (if (null list)
      nil
      (let ((wins (car list)))
        (dolist (obj (cdr list))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun awl::mostn (fn list)
  "Return as primary value a list of those elements in LIST for which the value obtained by applying them to the function FN is maximal. The secondary value is this maximal value."
  (declare ((function (*) real) fn))
  (if (null list)
      (values nil nil)
      (let ((result (list (car list)))
            (max (funcall fn (car list))))
        (declare (real max))
        (dolist (obj (cdr list))
          (let ((score (funcall fn obj)))
            (declare (real score))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defun awl::ncond-insert (item list test &optional termination)
  "Insert ITEM into LIST in front of the first element e (from the left) of LIST for which TEST(e) is true and return the resulting list; If TEST results to NIL fo all elements of LIST up to the tail TERMINATION, ITEM is appended to the front of TERMINATION. This operation destructively modifies LIST (except if ITEM is placed in front of LIST)."
  (declare ((function (*)) test)
           (optimize speed))
  (cond
    ((null list) (list item))
    ((funcall test (car list))
     (cons item list))
    (t (do ((prev list curr)
            (curr (cdr list) (cdr curr)))
           ((or (eq curr termination) (funcall test (car curr)))
            (setf (cdr prev) (cons item curr))
            list)))))

(defun awl::nordered-insert (item list order-pred &key key)
  "Insert ITEM into LIST in front of the first element e (from the left) of LIST for which ORDER-PRED(ITEM e) is true and return the resulting list. If KEY is given, ITEM and e are first applied to KEY before comparing with ORDER-PRED. This operation destructively modifies LIST (except if ITEM is placed in front of LIST)."
  (let ((test (compile nil
                       `(lambda (elt)
                          ,(awl:expand-funcall `',order-pred
                                               (awl:expand-funcall `',key `',item)
                                               (awl:expand-funcall `',key 'elt))))))
    (awl::ncond-insert item list test)))

;;;;**** Mapping

(defun awl::map0-n (fn n)
  "Return a list resulting of mapping the function FN to all integers not below 0 and below N."
  (awl::mapa-b fn 0 n))
(defun awl::map1-n (fn n)
  (awl::mapa-b fn 1 n))
(defun awl::mapa-b (fn a b &optional (step 1))
  "Return a list resulting of mapping the function FN to the numbers from A inclusive to B exclusive by steps STEP."
  (declare (type (function (real) *) fn)
           (type real a b step)
           (optimize speed))
  (let ((test (cond ((< 0 step) (lambda (i) (not (< i b))))
                    ((< step 0) (lambda (i) (not (< b i))))
                    (t (constantly t)))))
    (declare (type (function (real) *) test))
    (do ((i a (+ i step))
         (result nil (cons (funcall fn i) result)))
        ((funcall test i) (nreverse result)))))
(defun awl::map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
(defun awl::mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
(defun awl::mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
(defun awl::rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
               (lambda (&rest args)
                 (apply #'awl::rmapcar fn args))
               args)))

(defun awl::map-tree (fn tree)
  "Map the function FN to each node of the cons structure TREE and return NIL. As node are considered the CARs of the CONSes."
  (declare (type (or symbol (function (*))) fn))
  (when tree
    (let ((node (car tree)))
      (funcall fn node)
      (when (consp node)
        (awl::map-tree fn node))
      (awl::map-tree fn (cdr tree)))))


;;;;*** Circles

(defun awl::cyclic-p (cons)
  "Return T if the CONS contains cyclic references in the structure."
  (let ((hash (make-hash-table :test #'eq)))
    (labels ((seen-p (c)
               (and (consp c)
                    (or (gethash c hash)
                        (and (setf (gethash c hash) t)
                             (seen-p (car c))
                             (seen-p (cdr c)))))))
      (seen-p cons))))
(defun awl::circle-p (list)
  "Return T if the list LIST contains a circle and nil else. As second value return the length of the list resp. the circle. N.B. in opposition to CYCLIC-P, CIRCLE-P looks only in the CDR for cyclic references; we are interested in LIST as as list and not as a tree."
  (loop :with hash := (make-hash-table :test #'eq)
     :for l :from 0
     :for i :on list
     :for test := (gethash i hash)
     :do (if test
             (return (values t l test))
             (setf (gethash i hash) i))))
(defun awl::length* (seq)
  "Like cl:length but accepting circles."
  (etypecase seq
    (vector (length seq))
    (list (nth-value 1 (awl::circle-p seq)))))
(defun awl::last* (list &optional (n 1))
  "As cl:last but applicable on circles, in which case it returns (last (nth-value 2 (awl::circle-p LIST)) n)."
  (last (nth-value 2 (awl::circle-p LIST)) n))

(defun awl::proper-list-p (o)
  "Return T if o is a propoer list and nil else."
  (and (listp o)
       (not (awl::circle-p o))
       (null (cdr (last o)))))
(defun awl::make-circular (list)
  "Return LIST after having destructively modified it to form a circular list. LIST must be a proper list."
  (rplacd (last list) list)
  list)
(declaim (inline awl::make-circle*))
(defun awl::make-circle* (list)
  "Return a fresh circular list containing the elements of the proper list LIST in the same order with the head pointer pointing to the first element of LIST."
  (awl::make-circular (copy-list list)))
(declaim (inline awl::make-circle))
(defun awl::make-circle (&rest elts)
  "Return a fresh circular list containing the elements ELTS in the same order with the head pointer pointing to the first element of ELTS."
  (awl::make-circle* elts))

;;;;*** Alists and plists
(defun awl::alistp (o &key (key-type #'symbolp))
  "Return T if o is an association list and nil else. An association list is a propoer list of CONSes whose CAR satisfy KEY-TYPE."
  (and (listp o)
       (every (lambda (item)
                (and (consp item)
                     (funcall key-type (car item))))
              o)))
(defun awl::plistp (o &key (key-type #'symbolp))
  "Return T if o is a property list and nil else. A property list is a proper list with an even number of elements and whose elements on even position satisfy KEY-TYPE."
  (and (listp o)
       (evenp (length o))
       (every (let ((role :key))
                (awl::fbind
                    ((valid-key-p (cond
                                    ((functionp key-type)
                                     key-type)
                                    ((and (consp key-type)
                                          (eq 'lambda (first key-type)))
                                     (compile nil key-type))
                                    (t (lambda (x) (typep x key-type))))))
                  (lambda (item)
                    (case role
                      (:value (setq role :key)
                              t)
                      (:key (setq role :value)
                            (valid-key-p item))))))
              o)))

(defun awl::map-plist (fct plist)
  "Like cl:mapcar but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :collect (funcall fct k v)
       :until (null plist))))
(defun awl::map*-plist (fct plist)
  "Like MAP-PLIST but returning only NIL. This is used only for side effects."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :do (funcall fct k v)
       :until (null plist))))
(defun awl::mapcan-plist (fct plist)
  "Like cl:mapcan but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :append (funcall fct k v)
       :until (null plist))))
(defun awl::filter-plist (fct plist)
  "Return a list of non-nil results of FCT when applied on key-values of PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :for r := (funcall fct k v)
       :when r :collect r
       :until (null plist))))
(defun awl::keys+values<-plist (plist)
  "Return as primary value a list of all keys of the property list PLIST and as secondary value a list of all their corresponding values in the same order."
  (let (keys vals)
    (awl::map*-plist (lambda (k v)
                       (push k keys)
                       (push v vals))
                     plist)
    (values (nreverse keys) (nreverse vals))))

(defun awl::alist<-plist (plist)
  "Return an association list from a property list. Note: An association list is a proper list of CONSes and not of pairs."
  (awl::map-plist #'cons plist))
(defun awl::alist2<-plist (plist)
  "Return an association list from a property list. Note: Here an association list is a proper list of pairs and not of CONSes."
  (awl::map-plist #'list plist))

(defun awl::plist<-alist (alist)
  "Return a property list corresponding to the association list ALIST."
  (mapcan (lambda (x)
            (list (car x) (cdr x)))
          alist))
(defun awl::plist<-alist2 (alist2)
  "Return a property list made of the keys and the first element of each association in the association list ALIST2. This is especially used to map malformed association lists where the values of the associations are singletons."
  (mapcan (lambda (x)
            (list (car x) (cadr x)))
          alist2))

(defmacro awl::do-plist ((key value plist &optional result) &body body)
  "Like CL:DOLIST except itering KEY and VALUE over a plist. Within BODY the variable awl:first-property-p is bound to true when treating the first property and nil else."
  (awl::with-gensyms (g!plist g!next-p)
    `(let ((,g!plist ,plist))
       (when ,g!plist
         (let (,g!next-p)
           (declare (boolean ,g!next-p))
           (loop
             :for ,key := (pop ,g!plist)
             :for ,value := (pop ,g!plist)
             :do (symbol-macrolet ((awl::first-property-p (or ,g!next-p (setq ,g!next-p t))))
                   . ,body)
             :finally (return ,result)))))))










(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))
