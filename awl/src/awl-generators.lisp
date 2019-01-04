;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Generators, Iterators and Cursors
;;;;
;;;; Generators, Iterators and Cursors are entities including an inner state
;;;; and which, when invoked (by means of the function `NEXT' for example)
;;;; change the inner state and return a value (called henceforth "item")
;;;; depending on this new state.
;;;; 

;;;;** Generators: Implementation and Interface
;;;;
;;;; In this library we take as convention that a generator is a (anonymous)
;;;; function without arguments which, when successively called, changes its
;;;; internal state and returns an item according to this new state. When no
;;;; further items can be returned (eof), an error of type `NO-NEXT-ITEM-ERROR'
;;;; must be signalled or a customary eof object returned.
;;;;
;;;; As data abstraction, the function `NEXT' can be used instead of funcalling
;;;; the generator.
;;;;
;;;;** Iterators
;;;;
;;;; An Iterator is like a generator but has a supplementary feature to peek
;;;; ahead as far as possible for fetching elements but without changing the
;;;; inner state of the iterator. The Iterator must be implemented as a
;;;; function taking one optional integer argument (which may be restricted
;;;; to be an UNSIGNED-BYTE).
;;;; If called without argument, it behaves exactly like a Generator. If
;;;; called with 0, it returns the item which corresponds to the current
;;;; state of the Iterator; It is allowed for the Iterator to signal an error
;;;; of type `ITERATOR-NOT-INITIALIZED-ERROR' before the first item has been
;;;; generated. It is however also allowed for an Iterator to return a default
;;;; initial value instead or to implicitly generate the first item and to
;;;; keep it ready to be fetched with the 0 argument.
;;;;
;;;; When called with a positive integer N, the Iterator returns the item which
;;;; correspond to the state which would be obtained when generating N times
;;;; further items; the inner state however is left unchanged. If there are
;;;; less than N further states left, an error of type `NO-NEXT-ITEM-ERROR'
;;;; may be signalled.
;;;;
;;;; The interfaces for Iterators are given by the function `NEXT' for changing
;;;; the inner state to the next one and returning the corresponding item, the
;;;; function `CURRENT' for returning the item corresponding to the current
;;;; state, the function `PEEK' for peeking N items ahead without changing the
;;;; state, and the two error conditions mentioned above.
;;;;
;;;;** Cursors
;;;;
;;;; A Cursor is like an Iterator but has the additional feature to allow for
;;;; changing the internal state back to a previous one. The Cursor is
;;;; implemented as a function taking three optional arguments:
;;;; position absolute state.
;;;;
;;;; The argument position is an integer (possibly restricted to a FIXNUM)
;;;; which indicates the position of the state in the chain of possible
;;;; internal states of the Cursor; this position is either absolute beginning
;;;; with zero as the state prior first call of NEXT, if the argument absolute
;;;; is non nil, or relative to the position of the actual state, if the
;;;; argument absolute is nil (the default).
;;;; When called without argument, the Cursor changes the internal state to
;;;; the next one (c.f. Iterators) and returns the corresponding item. When
;;;; called with one or two arguments, the Cursor returns the item
;;;; corresponding to the state as indicated by the positional arguments
;;;; (see above) without changing the internal state except when called with
;;;; the third argument non nil, in which case the internal state is changed
;;;; as well.

;;;;** Implementation
;;;;*** Interface
;;;;**** Error Conditions
(define-condition awl::no-next-item-error (error)
  ((generator :initarg :generator :reader generator))
  (:report (lambda (condition stream)
             (format stream "There is no next element for Generator ~S."
                     (generator condition))))
  (:documentation "Condition of supertype `error' which is signaled by the Generator when no next element can be generated."))
(define-condition awl::iterator-not-initialized-error (error)
  ((iterator :initarg :iterator))
  (:report (lambda (condition stream)
             (format stream "The Iterator ~S has not been initialized."
                     (slot-value condition 'iterator))))
  (:documentation "Condition of supertype `error' which is signaled by the Iterator when no current element is defined, i.e. the Iterator is not initialized."))
;;;;**** Functions
(declaim (inline awl::next awl::back awl::current awl::peek awl::goto-item))

(declaim (ftype (function ((function) &optional fixnum)) awl::next))
(defun awl::next (generator &optional (n 1 np))
  "Change the internal state of the generator to the next following state or the state n positions ahead and return the corresponding item. If none such exists an error of type NO-NEXT-ITEM-ERROR may be signalled or a eof value returned. The optional value n cannot be used with simple generators."
  (declare (optimize speed)
           (fixnum n)
           ((function) generator))
  (if np
      (funcall generator n nil t)
      (funcall generator)))
(define-compiler-macro awl::next (generator &optional (n 1 np))
  "Change the internal state of the generator to the next following state or the state n positions ahead and return the corresponding item. If none such exists an error of type NO-NEXT-ITEM-ERROR may be signalled or a eof value returned. The optional value n cannot be used with simple generators."
  (declare (optimize speed))
  (if np
      `(funcall ,generator ,n nil t)
      `(funcall ,generator)))
(declaim (ftype (function ((function) &optional fixnum)) awl::back))
(defun awl::back (cursor &optional (n 1))
  "Change the internal state of the cursor to the next previous state or the state n positions back and return the corresponding item. If none such exists an error of type ITERATOR-NOT-INITIALIZED-ERROR may be signalled or a default initial value returned."
  (declare (optimize speed)
           (fixnum n)
           ((function) cursor))
  (funcall cursor n nil t))
(defun awl::current (iterator)
  "Return the item corresponding to the current internal state of the iterator. If iterator has never been used before, an error of type ITERATOR-NOT-INITIALIZED-ERROR may be signalled or a default initial value returned."
  (declare (optimize speed)
           ((function) iterator))
  (funcall iterator 0))
(defun awl::peek (iterator &optional (n 1) (absolute nil absp))
  "Return the item which is N positions ahead if N is positive or back if N is negative in ITERATOR but without changing the state of ITERATOR. If none such item exists, an error of type NO-NEXT-ITEM-ERROR or of type ITERATOR-NOT-INITIALIZED-ERROR may be is signalled or a default initial value or a eof value returned."
  (declare (optimize speed)
           (fixnum n)
           ((function) iterator))
  (if absp
      (funcall iterator n nil absolute)
      (funcall iterator n)))
(define-compiler-macro awl::peek (iterator &optional (n 1) (absolute nil absp))
  (declare (optimize speed)
           (fixnum n))
  (if absp
      `(funcall ,iterator ,n nil ,absolute)
      `(funcall ,iterator ,n)))
(defun awl::goto-item (cursor n)
  "Change the internal state of the cursor to the state which is N positions ahead if N is positive or back if N is negative and return the corresponding item. If none such item exists, an error of type NO-NEXT-ITEM-ERROR or of type ITERATOR-NOT-INITIALIZED-ERROR may be is signalled or a default initial value or a eof value returned."
  (declare (optimize speed)
           ((function) cursor))
  (funcall cursor n t t))
;;;;*** Constructors
;;;;
;;;;**** Iterators
(defclass awl::iterator (c2mop:funcallable-standard-object) () (:metaclass c2mop:funcallable-standard-class))
(defmethod initialize-instance :after ((self awl::iterator) &key function)
  (when function
    (c2mop:set-funcallable-instance-function self function)))
(declaim (ftype (function (function &optional *)) make-iterator-from-generator))
(defun make-iterator-from-generator (generator &optional (initial-value nil initial-value-p))
  "Wrap the GENERATOR into an iterator. A generator is a thunk which returns a value at each call. An iterator can be applied to the functions NEXT, CURRENT and PEEK."
  (declare (optimize speed))
  (let ((cache (when initial-value-p (list initial-value))))
    (labels ((peek (n c)
               (declare (fixnum n))
               (if (< 0 n)
                   (peek (1- n)
                         (or (rest c)
                             (let ((current (list (awl::next generator))))
                               (setq cache (append cache current))
                               current)))
                   (if c
                       (car c)
                       (error 'awl::iterator-not-initialized-error :iterator n))))
             (next ()
               (prog1 (peek 1 cache) 
                 (pop cache))))
      (declare (inline next))
      (lambda (&optional (n 0 peekp))
        (declare (optimize speed) (fixnum n))
        (if peekp (peek (the fixnum n) cache) (next))))))

(defmacro awl::build-iterator (initial-value &body body)
  "Return an iterator defined by BODY."
  (declare (optimize speed))
  `(awl::make-iterator-from-generator (lambda () ,@body) ,initial-value))

(defgeneric compute-iterator-factory (object))

(defmethod compute-iterator-factory (object)
  (compute-iterator-factory (awl::make-generator object)))

(defmethod compute-iterator-factory ((object function))
  "Wrap the GENERATOR into an iterator. A generator is a thunk which returns a value at each call. An iterator can be applied to the functions NEXT, CURRENT and PEEK."
  #'make-iterator-from-generator)

(defmethod compute-iterator-factory ((object awl::iterator))
  #'make-iterator-from-generator)

(defun awl::make-iterator (object &optional (initial-value nil initial-value-p))
  (let ((iterator-factory (unless (and (typep object 'awl::iterator)
                                       (not initial-value-p))
                            (compute-iterator-factory object))))
    (if iterator-factory
        (make-instance 'awl::iterator
                       :function (apply iterator-factory
                                        object
                                        (when initial-value-p initial-value)))
        object)))



;;;;**** Cursors
(defclass awl::cursor (awl::iterator) () (:metaclass c2mop:funcallable-standard-class))

(declaim (ftype (function ((function) &optional *)) make-cursor-from-generator))
(defun make-cursor-from-generator (generator &optional (initial-value nil initial-value-p))
  (let ((vector (make-array 0 :adjustable t :fill-pointer t))
        (state -1))
    (declare (type fixnum state))
    (symbol-macrolet ((m (the fixnum (1- (length vector)))))
      (lambda (&optional (n 1 np) absolutep (change-state (not np)))
        (declare (optimize speed)
                 (type fixnum n)
                 (type boolean absolutep change-state))
        (let ((i (if absolutep n (+ state n))))
          (declare (type fixnum i))
          (cond
            ((< i 0) (if initial-value-p
                         (progn (when change-state (setq state -1))
                                initial-value)
                         (error 'awl::iterator-not-initialized-error :iterator n)))
            ((< m i) (loop :while (< m i)
                        :do (vector-push-extend (awl::next generator) vector)))
            (t (when change-state (setq state i)) (aref vector i))))))))

(defun make-cursor-from-vector (vector &optional (initial-value nil initial-value-p))
  (declare (type vector vector))
  (symbol-macrolet ((m (the fixnum (1- (length vector)))))
    (let ((state -1))
      (declare (type fixnum state))
      (lambda (&optional (n 1 np) absolutep (change-state (not np)))
        (declare (optimize speed)
                 (type fixnum n))
        (let ((i (if absolutep n (+ state n))))
          (declare (type fixnum i))
          (cond
            ((< i 0) (if initial-value-p
                         (progn (when change-state (setq state -1))
                                initial-value)
                         (error 'awl::iterator-not-initialized-error :iterator n)))
            ((< m i) (error 'awl::no-next-item-error :generator n))
            (t (when change-state (setq state i)) (aref vector i))))))))


(defgeneric compute-cursor-factory (object))

(defmethod compute-cursor-factory (object)
  (compute-cursor-factory (awl::make-generator object)))

(defmethod compute-cursor-factory ((object function))
  #'make-cursor-from-generator)

(defmethod compute-cursor-factory ((object awl::iterator))
  #'make-cursor-from-generator)

(defmethod compute-cursor-factory ((object vector))
  #'make-cursor-from-vector)

(defun awl::make-cursor (object &optional (initial-value nil initial-value-p))
  (let ((cursor-factory (unless (and (typep object 'awl::cursor)
                                       (not initial-value-p))
                            (compute-cursor-factory object))))
    (if cursor-factory
        (make-instance 'awl::cursor
                       :function (apply cursor-factory
                                        object
                                        (when initial-value-p initial-value)))
        object)))

;;;; TODO

;;;;*** Some Utilities
(declaim (ftype (function (awl:function-designator awl:function-designator list) (values * * * fixnum)) awl::mingle*)
         (inline awl::mingle*))
(defun awl::mingle* (preorder-p key generators)
  "Return a generator producing at each invocation of NEXT an item of GENERATORS which is minimal among the next items of GENERATORS with respect to the preorder relation PREORDER-P lifted by KEY."
  (funcall (compile nil `(lambda (queue &aux no-next-item-error)
                           (let ((this (lambda ()
                                         (if queue
                                             (let ((cell (pop queue)))
                                               (unwind-protect
                                                    (values (cadr cell) (car cell) (caddr cell) (cdddr cell))
                                                 (handler-case
                                                     (setq queue (awl::nordered-insert (let ((item (awl::next (caddr cell))))
                                                                                         (setf (car cell) ,(awl:expand-funcall `',key 'item)
                                                                                               (cadr cell) item)
                                                                                         cell)
                                                                                       queue ',preorder-p
                                                                                       :key 'car))
                                                   (awl::no-next-item-error ()))))
                                             (error no-next-item-error)))))
                             (setq no-next-item-error (make-condition 'awl::no-next-item-error :generator this))
                             this)))
           (let (queue (i -1))
             (dolist (gen generators queue)
               (handler-case
                   (setq queue (awl::nordered-insert (let ((item (awl::next gen)))
                                                       (list* (funcall key item)
                                                              item gen (incf i)))
                                                     queue preorder-p :key 'car))
                 (awl::no-next-item-error ()))))))

(defun awl::multi-mingle* (preorder-p key generators)
  "Return a generator producing lists of items generated by each iterator in GENERATORS in following way: when (c1 ... cn) is the current item and n1, n2, ... nn are the next items respectively of the iterators 1 to n, the next item is (m1 .. mn) with mi = ni if ni is a minimal element of n1 ... nn with respect to the preorder relation defined by PREORDER-P and mi = ci else."
  (let* ((iterators (mapcar #'make-iterator-from-generator generators))
         (cells (mapcar (lambda (iterator) (cons nil iterator)) iterators))
         (peek (compile nil
                        `(lambda (cell)
                           (handler-case
                               (progn
                                 (setf (car cell) ,(awl:expand-funcall `',key '(awl::peek (cdr cell))))
                                 (list cell))
                             (awl::no-next-item-error ())))))
         no-next-item-error)
    (let ((this (lambda ()
                  (dolist (iterator (awl:bottom-elts preorder-p (or (mapcan peek cells)
                                                                    (error no-next-item-error))
                                                     :key 'car))
                    (awl::next (cdr iterator)))
                  (mapcar 'awl::current iterators))))
      (setq no-next-item-error (make-condition 'awl::no-next-item-error :generator this))
      this)))




(defun awl::map-generators (fn generator &rest generators)
  "Return a generator consisting of the output of the function FN applied to the consecutive elements of GENERATOR and GENERATORS."
  (lambda ()
    (apply fn (awl::next generator)
           (mapcar 'awl::next generators))))

;;;;TODO This function is not good. Can be replaced by map-into*
(defun awl::generator-to-sequence (sequence generator &key (start 0) end)
  "Destructively modifies sequence by replacing the elements of SEQUENCE bounded by START and END with elements provided from GENERATOR."
  (declare (type (or null (and fixnum unsigned-byte)) end)
           (type (and fixnum unsigned-byte) start)) 
  (let* ((i (1- start))
         (max (if end
                  (min (length sequence) end)
                  (length sequence)))
         (setter (typecase sequence
                   (list (let ((sublist (nthcdr start sequence)))
                           (lambda (v) 
                             (prog1
                                 (setf (car sublist) v)
                               (pop sublist)))))
                   (simple-vector
                    (lambda (v)
                      (setf (svref sequence i) v)))
                   (vector
                    (lambda (v)
                      (setf (aref sequence i) v))))))
    (flet ((next ()
             (funcall generator))
           ((setf next) (v)
             (unless (< (incf i) max)
               (error 'awl::no-next-item-error
                      :generator sequence))
            (funcall setter v)))
      (handler-case
          (loop (setf (next) (next)))
        (awl::no-next-item-error ()
          #+ (or)
          (unless (member (generator e) (list sequence generator)) 
            (error e))
          (values sequence i))))))

;;;;** Make Generator
(defgeneric awl::make-generator (object &key eof &allow-other-keys)
  (:documentation "Return a generator taking OBJECT as base and returning EOF if given or throwing a `no-next-item-error' if EOF is not given. A generator is a (anonymous) function which when successively called returns a new object. The specializer OUTPUT-TYPE may be used to specify the output type by implementing an adequate conversion."))

(defun awl::make-generator-from-list (list &key (step 1) key (eof nil eofp) &allow-other-keys)
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(null list))
	 (eof-signal (if eofp eof '(error 'awl::no-next-item-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next (if (< 1 step)
                   `(dotimes (i ,step) (pop list))
                   `(pop list))))
    (funcall (compile nil `(lambda ()
                             (declare (optimize speed))
                             (let ((list ',list))
                               (lambda ()
                                 ,(if test
                                      `(if ,test
                                           ,eof-signal
                                           (prog1 ,get
                                             ,next))
                                      `(prog1 ,get
                                         ,next)))))))))

(defun awl::make-generator-from-circle (list &key (step 1) key (eof nil eofp) &allow-other-keys)
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(or (gethash aux seen-conses) (null list)))
	 (eof-signal (if eofp eof '(error 'awl::no-next-item-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next `(progn
                  ,(if (< 1 step)
                      `(dotimes (i ,step) (pop list))
                      `(pop list))
                  (setf (gethash aux seen-conses) t))))
    (funcall (compile nil
                      `(lambda (list)
                         (declare (optimize speed))
                         (let ((seen-conses (make-hash-table :test #'eq
                                                             #+sbcl :weakness
                                                             #+(or ccl clisp) :weak
                                                             #+(or sbcl ccl clisp) :key)))
                           (lambda ()
                             (let ((aux list))
                               ,(if test
                                    `(if ,test
                                         ,eof-signal
                                         (prog1 ,get
                                           ,next))
                                    `(prog1 ,get
                                       ,next)))))))
             list)))

(declaim (ftype (function (unsigned-byte &key (:from (or null number)) (:step (or null number)) (:eof *)) (function nil number))
                awl::make-number-generator))
(defun awl::make-number-generator (count &key from (step 1) (eof nil eofp))
  "Return a generator producing COUNT numbers from FROM by steps STEP.  If STEP is not a number it is set to 1. A STEP of 0 produces an infinite sequence of the constant FROM (Not really useful). If FROM is not a number it is set to 0. If the key EOF is used it is returned when reaching the terminating condition instead of throwing a `no-next-item-error'."
  (declare (type unsigned-byte count))
  (check-type count unsigned-byte)
  (let* ((n (if (numberp from) from 0))
         (step (if (numberp step) step 1))
         (this (make-instance 'awl::iterator))
	 (eof-signal (if eofp eof `(error ,(make-condition 'awl::no-next-item-error :generator this))))
         (end (+ n (* count step))))
    (c2mop:set-funcallable-instance-function
     this
     (funcall (compile nil `(lambda (n i)
                              (declare (optimize speed)
                                       (type (integer -1 ,count) i)
                                       (type ,(find-if (lambda (type)
                                                         (and (typep n type) (typep step type) (typep end type)))
                                                       '(fixnum (complex fixnum) integer (complex integer)
                                                         long-float (complex long-float) double-float (complex double-float)
                                                         single-float (complex single-float) short-float (complex short-float)
                                                         real complex number))))
                              (lambda ()
                                (if (< (incf i) ,count)
                                    (prog1 n (incf n ,step))
                                    ,eof-signal))))
              n -1))))

#+(or)
(awl:defmacro! awl::make-generator-factory ((&rest lambdalist) &body body &key o!generator-form
                                            o!end-test o!eof-signal)
  (declare (ignore body))
  `(lambda (,@lambdalist)
     (funcall (compile nil `(lambda (,g!generator-form ,g!end-test)
                              (lambda ()
                                ,(if o!end-test
                                     `(if ,g!end-test
                                          ,g!eof-signal
                                          ,g!generator-form)
                                     g!generator-form)))))))



(defmethod awl::make-generator ((n number) &rest keys &key eof (step 1) from)
  "Return a generator producing numbers from FROM below TO by steps STEP. If TO <= FROM and 0 < STEP or FROM <= TO and STEP < 0 or if TO is not a number the generator never terminates. If STEP is not a number it is set to 1. A STEP of 0 produces an infinite sequence of the constant FROM (Not really useful). If FROM is not a number it is set to 0. If the key EOF is used it is returned when reaching the terminating condition instead of throwing a `no-next-item-error'."
  (declare (ignore eof step from))
  (apply #'awl::make-number-generator n keys))
(defmethod awl::make-generator ((list list) &rest keys &key eof (step 1) key once-only)
  "Return a generator from the list LIST applying KEY toe each element, if KEY is not null, advancing as if with (nthcdr STEP LIST).  If ONCE-ONLY is true stop when, in a circle, the already seen cons reapear."
  (declare (ignore eof step key))
  (if once-only
      (apply #'awl::make-generator-from-circle list keys)
      (apply #'awl::make-generator-from-list list keys)))
(defmethod awl::make-generator ((vector vector) &key &allow-other-keys)
  (make-cursor-from-vector vector))
(defmethod awl::make-generator ((iterator awl::iterator) &key &allow-other-keys)
  iterator)
(defmethod awl::make-generator ((iterator function) &key &allow-other-keys)
  iterator)

;;;;** 
(defun awl::map-into* (result-sequence function &rest iterables)
  "Destructively modifies RESULT-SEQUENCE to contain the results of applying function to each element in the argument ITERABLES in turn.  ITERABLES are objects for which there exists a method of the the generic function AWL::MAKE-GENERATOR."
  (let ((generators (mapcar 'awl::make-generator iterables)))
    (handler-case
        (map-into result-sequence
                  (compile nil
                           `(lambda ()
                              ,(apply 'awl:expand-funcall
                                      `',function
                                      (mapcar (lambda (gen)
                                                `(awl::next ,gen))
                                              generators)))))
      (awl::no-next-item-error ()
        result-sequence))))




;;;;* String Manipulation using Iterators

(defun awl::make-string-splitter-factory (targets &key escape-char max-chunks from-end eof-signal-p)
  (let ((no-next-element (when eof-signal-p (make-condition 'awl::no-next-item-error
                                                            :generator (format nil "#<string-splitter ~S, ~S, ~S, ~S>"
                                                                               targets escape-char max-chunks from-end))))
        (matcher (when (or (null max-chunks)
                           (< 1 max-chunks))
                   (awl::make-string-matcher targets escape-char from-end))))
    (compile nil
             `(lambda (string &optional (start 0) end)
                ,@(cond ((or (null max-chunks)
                             (< 1 max-chunks))
                         `((let ((status :continue)
                                 (s start)
                                 (e end)
                                 ,@(when max-chunks `((remaining-chunks ,max-chunks))))
                             (declare (type (member :continue :empty ,@(when max-chunks '(:last-element))) status))
                             (setq start 0 end nil)
                             (lambda ()
                               (case status
                                 ((:continue)
                                  ,@(when max-chunks `((when (= 1 (decf remaining-chunks))
                                                         (setq status :last-element))))
                                  (multiple-value-bind ,(if from-end
                                                            '(next-end start)
                                                            '(end next-start))
                                      (funcall ,matcher string s e)
                                    (prog1
                                        (subseq string (or start 0) end)
                                      ,(if from-end
                                           `(if next-end
                                                (setq end next-end
                                                      e next-end)
                                                (setq status :empty))
                                           `(if next-start
                                                (setq start next-start
                                                      s next-start)
                                                (setq status :empty))))))
                                 ,@(when max-chunks
                                     `((:last-element
                                        (setq status :empty)
                                        (subseq string ,(if from-end 0 'start) ,(when from-end 'end)))))
                                 (:empty ,(when eof-signal-p `(error ,no-next-element))))))))
                        ((= 1 max-chunks)
                         `((declare (ignore start end))
                           (lambda ()
                             (subseq string 0))))
                        (t
                         `((declare (ignore string start end))
                           (lambda ()
                             ,(when eof-signal-p `(error ,no-next-element))))))))))

(defun awl::make-string-splitter (targets &optional escape-char max-chunks from-end)
  (awl::fbind ((make-string-splitter (awl::make-string-splitter-factory targets
                                                                       :escape-char escape-char
                                                                       :max-chunks max-chunks
                                                                       :from-end from-end :eof-signal-p nil)))
    (lambda (string &optional (start 0) end)
      (awl::fbind ((next-item (make-string-splitter string start end)))
        (loop :for item = (next-item) :while item :collect item)))))

(defun awl::split-string (targets string &key escape-char max-chunks from-end (start 0) end)
  (funcall (awl::make-string-splitter targets escape-char max-chunks from-end) string start end))



(defun awl::make-string-replacer (replacement targets &optional escape-char max-operations from-end)
  (awl::fbind ((make-string-splitter (awl::make-string-splitter-factory targets
                                                                       :escape-char escape-char
                                                                       :max-chunks max-operations
                                                                       :from-end from-end :eof-signal-p nil)))
    (lambda (string &optional (start 0) end)
      (awl::fbind ((next-item (make-string-splitter string start end)))
        (with-output-to-string (stream)
          (let ((first-item (next-item)))
            (when first-item
              (princ first-item stream)))
          (loop :for item := (next-item) :while item
             :do (princ replacement stream)
             :do (princ item stream)))))))

(defun awl::string-replace (replacement targets string &key escape-char max-operations from-end (start 0) end)
  (funcall (awl::make-string-replacer replacement targets escape-char max-operations from-end) string start end))

(defun awl::join (separator sequence)
  (awl::fbind ((next (awl::make-generator sequence)))
    (with-output-to-string (stream)
      (handler-case
          (do ((action (princ (next) stream) (princ (prog1 (next)
                                                      (princ separator stream))
                                                    stream)))
              (nil))
        (awl::no-next-item-error ())))))



;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))
