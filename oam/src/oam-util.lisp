;;;; -*- outline-regexp:";;;;[*]+ +" -*-

;;;; $Id: oam-util.lisp,v 1.2 2010/08/25 13:36:23 amao Exp $
(in-package #:ch.amann-wolowyk.oam-system)

;;;; Delay/Force

(defclass delay () () (:metaclass c2mop:funcallable-standard-class))
(defmacro oam::delay (&body exprs)
  "Return a delay for the expressions EXPRS. The expressions as a whole are guaranteed to be evaled once at most when forced. The result when forcing the delay is the result of the last exression of EXPRS."
  `(make-instance 'delay :thunk #'(lambda () ,@exprs)))

(defmethod initialize-instance :after ((self delay) &key thunk &allow-other-keys)
  (assert (null (oam:function-arg-list thunk)) () "The closure ~S is not a thunk (a function with no arguments)." thunk)
  (c2mop:set-funcallable-instance-function self (lambda (&aux (result (funcall thunk)))
                                                  (c2mop:set-funcallable-instance-function self (lambda () result))
                                                  result)))
(defun oam::force (o)
  "Return the result of the expressions delayed with the macro DELAY."
  (typecase o
    (delay (funcall o))
    (t o)))


;;;;* Generators, Iterators and Cursors
;;;;
;;;; Generators, Iterators and Cursors are entities including an inner state and which, when invoked
;;;; (by means of the function `NEXT' for example) change the inner state return a value (called
;;;; henceforth "item") depending on this new state.
;;;; 

;;;;** Generators: Implementation and Interface
;;;;
;;;; In this library we take as convention that a generator is a (anonymous) function without arguments
;;;; which, when successively called, changes its internal state and returns an item according to this
;;;; new state. When no further items can be returned (eof), an error of type `NO-NEXT-ITEM-ERROR'
;;;; must be signalled or a customary eof object returned.
;;;;
;;;; As data abstraction, the function `NEXT' can be used instead of funcalling the generator.
;;;;
;;;;** Iterators
;;;;
;;;; An Iterator is like a generator but has a supplementary feature to peek ahead as far as possible for
;;;; fetching elements but without changing the inner state of the iterator. The Iterator must be implemented
;;;; as a function taking one optional integer argument (which may be restricted to be an UNSIGNED-BYTE).
;;;; If called without argument, it behaves exactly like a Generator. If called with 0, it returns the item
;;;; which corresponds to the current state of the Iterator; It is allowed for the Iterator to signal an error
;;;; of type `ITERATOR-NOT-INITIALIZED-ERROR' before the first item has been generated. It is however
;;;; also allowed for an Iterator to return a default initial value instead or to implicitly generate the
;;;; first item and to keep it ready to be fetched with the 0 argument.
;;;;
;;;; When called with a positive integer N, the Iterator returns the item which correspond to the state which
;;;; would be obtained when generating N times further items; the inner state however is left unchanged. If
;;;; there are less than N further states left, an error of type `NO-NEXT-ITEM-ERROR' may be signalled.
;;;;
;;;; The interfaces for Iterators are given by the function `NEXT' for changing the inner state to the next
;;;; one an returning the corresponding item, the function `CURRENT' for returning the item corresponding
;;;; to the current state, the function `PEEK' for peeking N items ahead without changing the state, and the
;;;; two error conditions mentioned above.
;;;;
;;;;** Cursors
;;;;
;;;; A Cursor is like an Iterator but has the additional feature to allow for changing the internal state
;;;; back to a previous one. The Cursor is implemented as a function taking three optional arguments:
;;;; position absolute state.
;;;;
;;;; The argument position is an integer (possibly restricted to a FIXNUM) which indicates the position
;;;; of the state in the chain of possible internal states of the Cursor; this position is either
;;;; absolute beginning with zero as the state prior first call of NEXT, if the argument absolute is non nil,
;;;; or relative to the position of the actual state, if the argument absolute is nil (the default).
;;;; When called without argument, the Cursor changes the internal state to the next one (c.f. Iterators)
;;;; and return the corresponding item. When called with one or two arguments, the Cursor returns the
;;;; item corresponding to the state as indicated by the positional arguments (see above) without changing
;;;; the internal state except when called with the third argument non nil, in which case the internal
;;;; state is changed as well.

;;;;** Implementation
;;;;*** Interface
;;;;**** Error Conditions
(define-condition oam::no-next-item-error (error)
  ((generator :initarg :generator))
  (:report (lambda (condition stream)
             (format stream "There is no next element for Generator ~S."
                     (slot-value condition 'generator))))
  (:documentation "Condition of supertype `error' which is signaled by the Generator when no next element can be generated."))
(define-condition oam::iterator-not-initialized-error (error)
  ((iterator :initarg :iterator))
  (:report (lambda (condition stream)
             (format stream "The Iterator ~S has not been initialized."
                     (slot-value condition 'iterator))))
  (:documentation "Condition of supertype `error' which is signaled by the Iterator when no current element is defined, i.e. the Iterator is not initialized."))
;;;;**** Functions
(declaim (inline oam::next oam::back oam::current oam::peek oam::goto-item))
(defun oam::next (generator &optional (n 1 np))
    "Change the internal state of the generator to the next following state or the state n positions ahead and return the corresponding item. If none such exists an error of type NO-NEXT-ITEM-ERROR may be signalled or a eof value returned. The optional value n cannot be used with simple generators."
    (declare (optimize speed)
             (unsigned-byte n)
             ((function) generator))
    (if np
        (funcall generator n nil t)
        (funcall generator)))
(define-compiler-macro oam::next (generator &optional (n 1 np))
    "Change the internal state of the generator to the next following state or the state n positions ahead and return the corresponding item. If none such exists an error of type NO-NEXT-ITEM-ERROR may be signalled or a eof value returned. The optional value n cannot be used with simple generators."
    (declare (optimize speed)
             (unsigned-byte n)
             ((function) generator))
    (if np
        `(funcall ,generator ,n nil t)
        `(funcall ,generator)))
  (defun oam::back (cursor &optional (n 1))
    "Change the internal state of the cursor to the next previous state or the state n positions back and return the corresponding item. If none such exists an error of type ITERATOR-NOT-INITIALIZED-ERROR may be signalled or a default initial value returned."
    (declare (optimize speed)
             (unsigned-byte n)
             ((function) cursor))
    (funcall cursor n nil t))
(defun oam::current (iterator)
  "Return the item corresponding to the current internal state of the iterator. If iterator has never been used before, an error of type ITERATOR-NOT-INITIALIZED-ERROR may be signalled or a default initial value returned."
  (declare (optimize speed)
           ((function) iterator))
  (funcall iterator 0))
(defun oam::peek (iterator &optional (n 1) (absolute nil absp))
  "Return the item which is N positions ahead if N is positive or back if N is negative in ITERATOR but without changing the state of ITERATOR. If none such item exists, an error of type NO-NEXT-ITEM-ERROR or of type ITERATOR-NOT-INITIALIZED-ERROR may be is signalled or a default initial value or a eof value returned."
  (declare (optimize speed)
           (fixnum n)
           ((function) iterator))
  (if absp
      (funcall iterator n nil absolute)
      (funcall iterator n)))
(define-compiler-macro oam::peek (iterator &optional (n 1) (absolute nil absp))
  (declare (optimize speed)
           (fixnum n)
           ((or symbol function) iterator))
  (if absp
      `(funcall ,iterator ,n nil ,absolute)
      `(funcall ,iterator ,n)))
(defun oam::goto-item (cursor n)
  "Change the internal state of the cursor to the state which is N positions ahead if N is positive or back if N is negative and return the corresponding item. If none such item exists, an error of type NO-NEXT-ITEM-ERROR or of type ITERATOR-NOT-INITIALIZED-ERROR may be is signalled or a default initial value or a eof value returned."
  (declare (optimize speed)
           ((function) cursor))
  (funcall cursor n t t))
;;;;*** Constructors
;;;;
;;;;**** Iterators
(defun oam::make-iterator-from-generator (generator &optional (initial-value nil initial-value-p))
  "Wrap the GENERATOR into an iterator. A generator is a thunk which returns a value at each call. An iterator is can be applied to the functions NEXT, CURRENT and PEEK."
  (declare ((function nil *) generator)
           (optimize speed))
  (let ((cache (list (if initial-value-p initial-value (funcall generator)))))
    (labels ((peek (n)
               (declare (unsigned-byte n))
               (do ((c cache (or (cdr c)
                                 (setf (cdr c) (list (funcall generator))))))
                   ((< n 1) (car c))
                 (setq n (the unsigned-byte (- n 1)))))
             (next ()
               (prog1 (peek 1) 
                 (pop cache))))
      (declare (inline next peek))
      (lambda (&optional (n 0 peekp))
        (declare (optimize speed) (fixnum n))
        (if peekp (peek (the fixnum n)) (next))))))
(defmacro oam::make-iterator (initial-value &body body)
  "Return an iterator defined by BODY."
  (declare (optimize speed))
  `(oam::make-iterator* (lambda () ,@body) ,initial-value))

;;;;**** Cursors

(defun oam::make-cursor-from-vector (vector)
  (symbol-macrolet ((m (1- (length vector))))
    (macrolet ((idx (n)
                 `(max 0 (min m ,n))))
      (let ((state -1) curr)
        (lambda (&optional (n 1 np) absolutep (change-state (not np)))
          (declare (optimize speed))
          (if (zerop n)
              curr
              (let ((i (if absolutep n (+ state n))))
                (cond
                  ((< i 0) (error 'oam::iterator-not-initialized-error :iterator n))
                  ((< m i) (error 'oam::no-next-item-error :generator n))
                  (t (when change-state (setq state i)) (setq curr (aref vector i)))))))))))

;;;; TODO

;;;;*** Some Utilities
(defun oam::make-iterator-cartesian-product (preorder-p &rest iterators)
  "Return an iterator producing lists of items generated by each iterator in ITERATORS in following way: when (c1 ... cn) is the current item and n1, n2, ... nn are the next items respectively of the iterators 1 to n the next item is (m1 .. mn) with mi = ni if ni is a minimal element of n1 ... nn with respect to the preorder relation defined by PREORDER-P and ci else."
  (oam::make-iterator-from-generator
   (lambda ()
     (mapc (oam::f-comp #'oam::next #'cdr)
           (oam::find-extrema (mapcar (lambda (g) (cons (oam::peek g) g)) iterators)
                              preorder-p :key #'car))
     (mapcar #'oam::current iterators))
   (mapcar #'oam::current iterators)))

(defun oam::map-generators (type fn generator &rest generators)
  "Map the output of GENERATORS to the function FN and return a sequence of the type TYPE containing the results of the mapping. If TYPE is nil, nil is returned. The loop terminates as soon as one of the generators throws a `no-next-item-error'. It is therefore expected that (at least one of) the generators throws a `no-next-item-error' to terminate the loop. TYPE is one of nil, list, vector or string."
  (let ((generators (cons generator generators)))
    (ecase type
      ((nil) (handler-case
                 (loop (apply fn (mapcar #'funcall generators)))
               (oam::no-next-element-error ())))
      (list (let (collection)
              (handler-case
                  (loop (push (apply fn (mapcar #'funcall generators))
                              collection))
                (oam::no-next-element-error ()
                  (nreverse collection)))))
      ((vector string)
       (let ((collection (case type
                           (vector
                            (make-array '(1)
                                        :adjustable t :fill-pointer 0))
                           (string
                            (make-array '(1)
                                        :adjustable t :fill-pointer 0
                                        :element-type 'character)))))
         (handler-case
               (loop (vector-push-extend
                      (apply fn (mapcar #'funcall generators))
                      collection))
           (oam::no-next-element-error ()
             collection)))))))


(defgeneric oam::make-generator (object &key eof &allow-other-keys)
  (:documentation "Return a generator taking OBJECT as base and returning EOF if given or throwing a `no-next-item-error' if EOF is not given. A generator is a (anonymous) function which when successively called returns a new object. The specializer OUTPUT-TYPE may be used to specify the output type by implementing an adequate conversion."))

(defun oam::make-generator-from-list (list &key (step 1) key (eof nil eofp))
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(null list))
	 (eof-signal (if eofp eof '(error 'oam::no-next-item-error)))
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

(defun oam::make-generator-from-circle (list &key (step 1) key (eof nil eofp))
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(or (gethash aux seen-conses) (null list)))
	 (eof-signal (if eofp eof '(error 'oam::no-next-item-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next `(progn
                  ,(if (< 1 step)
                      `(dotimes (i ,step) (pop list))
                      `(pop list))
                  (setf (gethash aux seen-conses) t))))
    (funcall (compile nil `(lambda (list)
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

(defun oam::make-number-generator (n &key to (step 1) (eof nil eofp))
  "Return a generator producing numbers from N below TO by steps STEP. If TO <= N and 0 < STEP or N <= TO and STEP < 0 or if TO is not a number the generator never terminates. If STEP is not a number it is set to 1. A STEP of 0 produces an infinite sequence of the constant N (Not really useful). If N is no a number it is set to 0. If EOF is not nil it is returned when reaching the terminating condition is reached instead of throwing a `no-next-item-error'."
  (let* ((n (if (numberp n)
		n
		0))
         (step (if (numberp step)
		   step
		   1))
	 (to (when (and (numberp to)
			(or (< (- n to) 0 step)
			    (< step 0 (- n to))))
	       to))
	 (test (when to `(<= ,to n)))
	 (eof-signal (if eofp eof '(error 'oam::no-next-item-error)))
	 (get 'n)
	 (next `(incf n ,step)))
    (funcall (compile nil `(lambda ()
                             (declare (optimize speed))
                             (let ((n ,n))
                               (lambda ()
                                 ,(if test
                                      `(if ,test
                                           ,eof-signal
                                           (prog1
                                               ,get
                                             ,next))
                                      `(prog1 ,get
                                         ,next)))))))))

#+(or)
(oam:defmacro! oam::make-generator-factory ((&rest lambdalist) &body body &key o!generator-form
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



(defmethod oam::make-generator ((n number) &key eof (step 1) to
                                &allow-other-keys)
  (oam::make-number-generator n :to to :step step :eof eof))
(defmethod oam::make-generator ((list list) &key eof (step 1) key
                                &allow-other-keys)
  (oam::make-generator-from-circle list :step step :key key :eof eof))

;;;;* String Utilities

;;;;** Chunk Matcher
(defun oam::make-chunk-mapper (delimiters &optional escape-char)
  "Return a function taking a function FCT in one character and optionally an input character stream defaulting to *standard-input*. The returned function maps at each call the function FCT to each character of the next chunk of the stream. A chunk is the sequence of character between two occurences of DELIMITERS which are not escaped by ESCAPE-CHAR. DELIMITERS and ESCAPE-CHAR can be either a predicate function, a single character or a list of characters. In the latter case a delimiter or an escape-character is any of that list. The behaviour is unspecified if a character may be at the same time a delimiter and an escape-character."
  (declare (optimize speed))
  (flet ((select-check-form (item)
           (etypecase item
             (function `(funcall ,item (the character char)))
             (character `(char= (the character char) ,item))
             (cons `(find (the character char) ',item :test #'char=)))))
    (let ((delimiter-check-form (list (list (select-check-form delimiters)
                                            :delimiter)))
          (escape-check-form (when escape-char
                               (list (list (select-check-form escape-char)
                                           '(setq escapedp t)
                                           :escape)))))
      (eval `(macrolet
                 ((check (char)
                    (declare (ignorable char))
                    `(if escapedp
                         (progn (setq escapedp nil)
                                :take-it)
                         ,',(append '(cond)
                                    delimiter-check-form
                                    escape-check-form
                                    '((t :take-it))))))
               (lambda (fct &optional (stream *standard-input*))
                 (declare (optimize speed))
                 (handler-case
                     (let (escapedp)
                       (loop :for char character := (read-char stream)
                          :do (case (check char)
                                (:take-it (funcall fct (the character char)))
                                (:delimiter (return char))
                                (:escape))))
                   (end-of-file ()))))))))


(oam:fbind ((map-chunk (oam::make-chunk-mapper #'upper-case-p)))
  (defun oam::camel-style-to-lisp (string)
    "Return a string obtained from input STRING by inserting in front of each uppercase character of STRING, except the first one, the character #\- and then changeing to upper case all letters of STRING."
    (string-upcase (let ((string (with-output-to-string (*standard-output*)
                                   (with-input-from-string (*standard-input* string)
                                     (loop :for upcase-char := (map-chunk #'write-char)
                                        :while upcase-char
                                        :do (format t "~A~A" #\- upcase-char))))))
                     (subseq string (min 1 (length string)))))))

;;;;** String Matcher
(defun oam::make-string-matcher (targets &optional escape-char from-end)
         "Return a function taking a string and returning as primary value the position in STRING of the first character of any of targets from the beginning (from the end if FROM-END is t) in the substring of STRING delimited by START and END and which is not preceeded by a single ESCAPE-CHAR. The secondary value is the position + target-length and the third value is the target which is at that position."
         (etypecase targets
           (atom (make-string-matcher targets escape-char from-end))
           (cons (let ((matchers (mapcar (lambda (target)
                                           (make-string-matcher target escape-char from-end))
                                         targets)))
                   (eval `(lambda (string &optional (start 0) end)
                            ,(format nil "Return the position of the first match from the ~:[beginning~;end~] of ~v[~;~:;one of ~]~{~S~#[~; or ~:;, ~]~} in STRING between START and END, unless escaped with the character ~S."
                                     from-end (length targets) targets escape-char)
                            (values-list (reduce  ,(eval `(lambda (a b)
                                                            (oam:fbind ((following-p ,(if from-end #'> #'<)))
                                                              (if a
                                                                  (if (and b (following-p (car b) (car a)))
                                                                      b
                                                                      a)
                                                                  b))))
                                                  ',matchers
                                                  :key (lambda (m)
                                                         (multiple-value-list (funcall m string start end)))))))))))



(defun make-string-matcher (target &optional escape-char from-end)
  (let* ((target-length (etypecase target
                          (character 1)
                          (string (length target))))
         (no-escape-char-? (if escape-char
                               `(or (= 0 pos)
                                    (char/= ,escape-char (aref string (1- pos))))
                               t))
         (full-match-? (if (< 1 target-length)
                           `(when (<= pos max-pos)
                              (string= ,target (subseq string pos pos+target-length)))
                           t)))
    (eval `(lambda (string &optional (start 0) end)
             ,(format nil "Return the position of the first match from the ~:[beginning~;end~] of ~S in STRING between START and END, unless escaped with the character ~S."
                      from-end target escape-char)
             (let* ,(list*
                     `(pos ,(if from-end 'end '(1- start)))
                     (when (< 1 target-length)
                       `((string-length (length string))
                         (max-pos (- string-length ,target-length)))))
               (loop (setq pos (position ,(etypecase target
                                                     (character target)
                                                     (string (aref target 0)))
                                         string
                                         :from-end ,from-end
                                         :start ,(if from-end
                                                     'start
                                                     '(1+ pos))
                                         :end ,(if from-end
                                                   'pos
                                                   'end)))
                  (if pos
                      (let ((pos+target-length (+ pos ,target-length)))
                        (when (and ,no-escape-char-? ,full-match-?)
                          (return (values pos pos+target-length ,target))))
                      (return (values)))))))))



(defun oam::make-string-splitter-factory (targets &key escape-char max-chunks from-end eof-signal-p)
  (let ((no-next-element (when eof-signal-p (make-condition 'oam::no-next-element-condition
                                                            :generator (format nil "#<string-splitter ~S, ~S, ~S, ~S>"
                                                                               targets escape-char max-chunks from-end))))
        (matcher (oam::make-string-matcher targets escape-char from-end)))
    (eval `(lambda ,(append `(string &optional (start 0) end
                                     &aux (status ,(if max-chunks
                                                       (cond
                                                         ((< 1 max-chunks)
                                                          :continue)
                                                         ((= 1 max-chunks)
                                                          :single-element)
                                                         ((= 0 max-chunks)
                                                          :empty))
                                                       :continue))
                                     (s start) (e end))
                            (when max-chunks `((remaining-chunks ,max-chunks))))
             (setq start 0 end nil)
             (lambda ()
               (case status
                 ((:continue)
                  ,(when max-chunks `(when (= 1 (decf remaining-chunks))
                                       (setq status :last-element)))
                  (multiple-value-bind ,(if from-end
                                            '(next-end start)
                                            '(end next-start))
                      (funcall ,matcher string s e)
                    (prog1
                        (subseq string start end)
                      ,(if from-end
                           `(if next-end
                                (setq end next-end
                                      e next-end)
                                (setq status :empty))
                           `(if next-start
                                (setq start next-start
                                      s next-start)
                                (setq status :empty))))))
                 (:last-element
                  (setq status :empty)
                  (subseq string ,(if from-end 0 'start) ,(when from-end 'end)))
                 (:single-element
                  (subseq string 0))
                 (:empty ,(when eof-signal-p `(signal ,no-next-element)))))))))

(defun oam::make-string-splitter (targets &optional escape-char max-chunks from-end)
  (oam:fbind ((make-string-splitter (oam::make-string-splitter-factory targets
                                                                       :escape-char escape-char
                                                                       :max-chunks max-chunks
                                                                       :from-end from-end :eof-signal-p nil)))
    (lambda (string &optional (start 0) end)
      (oam:fbind ((next-item (make-string-splitter string start end)))
        (loop :for item = (next-item) :while item :collect item)))))

(defun oam::split-string (targets string &key escape-char max-chunks from-end (start 0) end)
  (funcall (oam::make-string-splitter targets escape-char max-chunks from-end) string start end))


(defun oam::make-string-replacer (replacement targets &optional escape-char max-operations from-end)
  (oam:fbind ((make-string-splitter (oam::make-string-splitter-factory targets
                                                                       :escape-char escape-char
                                                                       :max-chunks max-operations
                                                                       :from-end from-end :eof-signal-p nil)))
    (lambda (string &optional (start 0) end)
      (oam:fbind ((next-item (make-string-splitter string start end)))
        (with-output-to-string (stream)
          (let ((first-item (next-item)))
            (when first-item
              (princ first-item stream)))
          (loop :for item := (next-item) :while item
             :do (princ replacement stream)
             :do (princ item stream)))))))

(defun oam::string-replace (replacement targets string &key escape-char max-operations from-end (start 0) end)
  (funcall (oam::make-string-replacer replacement targets escape-char max-operations from-end) string start end))


;;;;* Reading Files

(defun oam::slurp-file (pathname)
  "Read the file at PATHNAME into a single string and return this string."
  (with-open-file (stream pathname)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))




;;;;==============================================================================

(defun oam::type-spec-p (expr)
  "Return true if expr is a valid type specification in the NIL environment, NIL else."
  (ignore-errors (typep nil expr) t))

(defun oam::lambda-expr-p (expr)
  "Return true if expr is a lambda expression, NIL else."
  (ignore-errors (destructuring-bind (lambda (&rest arg) &rest body)
                     expr
                   (declare (ignore arg body))
                   (eq 'lambda lambda))))

(defun oam::lambda-expr-1-p (expr)
  "Return true if expr is a lambda expression with exactly one argument and this argument is mandatory, NIL else."
  (ignore-errors (destructuring-bind (lambda (arg) &rest body)
                     expr
                   (declare (ignore arg body))
                   (eq 'lambda lambda))))

(defmacro oam::dlambda (destr-lambda-list &body body)
  "Return a function using a destructuring lambda-list."
  `(lambda (&rest args)
     (destructuring-bind ,destr-lambda-list args ,@body)))

(defmacro oam::f-or (&rest fns)
  "Return a function F s.t. F(args)= (or F1(args) .. Fn(args))."
  `(lambda (&rest args)
     (or ,@(mapcar (lambda (fn)
                     `(apply ,fn args))
                   fns))))
(defmacro oam::f-and (&rest fns)
  "Return a function F s.t. F(args)= (and F1(args) .. Fn(args))."
  `(lambda (&rest args)
     (and ,@(mapcar (lambda (fn)
                     `(apply ,fn args))
                   fns))))

(defun oam::f-comp (&rest fns)
  "Return a function which is obtained by funcalling each function among FNS successively from the right to the left to the result of the previous function resp. the input args for the first function. When FNS is NIL, return a function which returns always nil."
  (oam::f-comp* fns))
(defun oam::f-comp* (fns &aux (fns (nreverse fns)))
  "Return a function which is obtained by funcalling each function among FNS successively from the right to the left to the result of the previous function resp. the input args for the first function. When FNS is NIL, return a function which returns always nil."
  (compile nil `(lambda (&rest args)
                  ,(reduce (lambda (r fn)
                             `(funcall ,fn ,r))
                           (rest fns)
                           :initial-value `(apply ,(first fns) args)))))


(defun oam::partev (fn &rest args)
  "Evaluate ARGS and return an anonymous function taking other-args and applying FN to the evaluated args and other-args."
  (oam::partev* fn args))
(defun oam::partev* (fn args)
  "Evaluate ARGS and return an anonymous function taking other-args and applying FN to the evaluated args and other-args."
  (compile nil `(lambda (&rest other-args)
                  (apply ,fn ,@(mapcar (lambda (arg) `',arg) args) other-args))))


(defun oam::n-tuple-fn (fct &rest fcts)
  "Take functions which are expected to have the same arglist and return a function which takes this arglist of arguments and returns as values lists of the values of the individual input functions as follows: If the functions f_0 .. f_n return the values v_i_0 .. v_i_mi for each i from 0 to n, the form \(apply \(n-tuple-fn f_0 .. f_n) args) returns the values \(v_0_0 .. v_n_0) .. \(v_0_(min m0 .. mn) .. v_n_(min m0 .. mn))."
  (if (null fcts)
      fct
      (lambda (&rest args)
        (apply #'values
               (apply #'mapcar
                      #'list
                      (mapcar (lambda (fn)
                                (multiple-value-list (apply fn args)))
                              (cons fct
                                    fcts)))))))
;;;;* MOP-specific Stuff
;;;; Uses closer-mop
(defmacro oam::with-slot-definition-locations ((&rest slots) class-name &body body)
  ""
  (oam:with-gensyms (class slot-defs)
    `(let ((,class (find-class ',class-name)))
       (unless (c2mop:class-finalized-p ,class)
         (c2mop:finalize-inheritance ,class))
       (let* ((,slot-defs (c2mop:class-slots ,class))
              ,@(mapcar (lambda (name)
                          (destructuring-bind (var-name slot-name)
                              (etypecase name
                                (symbol (list name name))
                                ((cons symbol (cons symbol null)) name))
                            `(,var-name (c2mop:slot-definition-location
                                         (find ',slot-name ,slot-defs
                                               :key #'c2mop:slot-definition-name)))))
                        slots))
         ,@body))))

(defun oam::get-slot-values (instance &optional (unbound-slot :unbound-slot))
  "Return a list containing the values of all slots of INSTANCE sorted by slot-definition-location of the instance's class. If a slot is unbound UNBOUND-SLOT, which defaults to the keyword :unbound-slot, is returned instead."
  (mapcar (lambda (slot)
            (handler-case
                (slot-value instance (c2mop:slot-definition-name slot))
              (unbound-slot ()
                unbound-slot)))
          (sort (c2mop:class-slots (class-of instance)) #'<
                :key #'c2mop:slot-definition-location)))
(defun oam::identical (o1 o2 &key (test #'eql))
  "Return T if O1 and O2 are of the same type and have same slots according to TEST; NIL else."
  (when (eq (class-of o1)
            (class-of o2))
    (loop
       :with equalp := t
       :for slot :in (c2mop:class-slots (class-of o1))
       :while equalp
       :do (setq equalp
                 (handler-case
                     (funcall test
                              (slot-value o1 (c2mop:slot-definition-name slot))
                              (slot-value o2 (c2mop:slot-definition-name slot)))
                   (unbound-slot (e)
                     (let ((slot-name (cell-error-name e)))
                       (not (or (slot-boundp o1 slot-name)
                                (slot-boundp o2 slot-name)))))))
       :finally (return equalp))))




;;;;* Hash
(defparameter oam::*to-keyword-hook* nil
  "Hook of functions used by TO-KEYWORD. Each function will be given the out put of the previous function, whereas the first receives a string. The last function must return a string-designator.")
(defun oam::to-keyword (string-designator)
  "Intern STRING-DESIGNATOR into the keyword package after having applied the functions in *to-keyword-hook* to (string STRING-DESIGNATOR). If STRING-DESIGNATOR is already in the keyword package STRING-DESIGNATOR is returned unchanged."
  (etypecase string-designator
    (keyword string-designator)
    ((or symbol string)
     (intern (reduce #'(lambda (res fn)
                         (funcall fn res))
                     oam::*to-keyword-hook*
                     :initial-value (string string-designator))
             'keyword))))

;;;;** Multi Hash-Table

(defun oam::get-hash* (hash keys &optional default)
  "Return the object in HASH whose keys is the same as keys under the hash's equivalence test and T as secondary value if such object exists. Return nil, nil else."
  (if default (multiple-value-bind (value not-empty-p followed-path remaining-path)
                  (funcall hash :get keys nil)
                (values (if not-empty-p value default) not-empty-p followed-path remaining-path))
      (funcall hash :get keys nil)))
(define-compiler-macro oam::get-hash* (hash keys &optional (default nil defaultp))
  (if defaultp
      `(multiple-value-bind (value not-empty-p followed-path remaining-path)
           (funcall ,hash :get ,keys nil)
        (values (if not-empty-p value ,default) not-empty-p followed-path remaining-path))
      `(funcall ,hash :get ,keys nil)))
(defun oam::get-hash (hash &rest keys)
  "Return the object in HASH whose keys is the same as keys under the hash's equivalence test and T as secondary value if such object exists. Return nil, nil else."
  (oam::get-hash* hash keys))
(defsetf oam::get-hash* (hash keys &optional default) (value)
  "Set the value of the hash to the given value."
  (declare (ignorable default))
  `(funcall ,hash :set ,keys ,value))
(defsetf oam::get-hash (hash &rest keys) (value)
  "Set the value of the hash to the given value."
  `(funcall ,hash :set ,keys ,value))
(defun oam::rem-hash* (hash keys)
  "Remove the entry for KEYS in HASH, if any. Returns true if there was such an entry, or false otherwise."
  (funcall hash :rem keys nil))
(defun oam::rem-hash (hash &rest keys)
  "Remove the entry for KEYS in HASH, if any. Returns true if there was such an entry, or false otherwise."
  (oam::rem-hash* hash keys))
(defun oam::clr-hash (hash)
  "Remove all entries in HASH, if any, and returns the empty HASH."
  (funcall hash :clr nil nil)
  hash)
(defun oam::hash-count (hash)
  "Return the number of entries in HASH. If HASH has just been created or newly cleared (see clr-hash) the entry count is 0."
  (funcall hash :cnt nil nil))
(defun oam::map-hash (fct hash)
  "Map HASH to the function FCT in depth first manner. FCT takes two arguments: the first is the key the second must be optional and takes the value. blabla."
  (funcall hash :map fct nil))



(let ((empty '#:nil))
  (labels
      ((follow-path (nodes path followed-path)
         (let ((node (first nodes)))
           (if node
               (handler-case
                   (destructuring-bind (step &rest remaining-path) path
                     (follow-path (cons (gethash step (cdr node)) nodes) remaining-path (cons step followed-path)))
                 (error ()
                   (values nodes followed-path path)))
               (error "No node."))))
       (map-children (fct node)
         (let ((hash (cdr node)))
           (when hash
             (maphash fct (cdr node)))))
       (map-tree (fct node)
         (unless (eq empty (car node))
           (funcall fct nil (car node)))
         (map-children (lambda (s n)
                         (funcall fct s)
                         (map-tree fct n))
                       node)))
    (defun make-hash-tree (options)
      (let ((root (cons empty nil)))
        (lambda (action path value)
          (ecase action
            (:get (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (let* ((value (car (first nodes)))
                           (emptyp (or (eq empty value) remaining-path)))
                      (values (unless emptyp value) (not emptyp) followed-path remaining-path))))
            (:set (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (declare (ignore followed-path))
                    (let ((node (first nodes)))
                      (dolist (step remaining-path)
                        (unless (cdr node)
                          (setf (cdr node) (apply #'make-hash-table options)))
                        (let ((hash (cdr node)))
                          (setf (gethash step hash) (cons empty nil))
                          (setq node (gethash step hash))))
                      (setf (car node) value))))
            (:rem (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (unless remaining-path
                      (setf (car (first nodes)) empty)
                      (loop :for (node . prev-nodes) :on nodes
                         :for step :in followed-path
                         :do (if (and (eq empty (car node))
                                      (null (cdr node)))
                                 (remhash step (cdr (first prev-nodes)))
                                 (return)))
                      t)))
            (:clr (setq root (cons empty nil)))
            (:cnt (let ((counter 0))
                    (labels ((count-non-empty (node)
                               (unless (eq empty (car node))
                                 (incf counter))
                               (map-children (lambda (s n)
                                               (declare (ignore s))
                                               (count-non-empty n))
                                             node)))
                      (count-non-empty root))
                    counter))
            (:map (progn
                    (map-tree path root)
                    nil))))))))



(defclass oam::hash-tree (c2mop:funcallable-standard-object) ()
  (:metaclass c2mop:funcallable-standard-class))
(let ((hash-tree-class (find-class 'oam::hash-tree)))
  (unless (c2mop:class-finalized-p hash-tree-class)
    (c2mop:finalize-inheritance hash-tree-class)))
(defmethod initialize-instance :after ((self oam::hash-tree) &key options)
  (c2mop:set-funcallable-instance-function self (make-hash-tree options)))
(defun oam::make-hash-tree (&rest options &key test size rehash-size
                            rehash-threshold #+(or ccl clisp sbcl) weak &allow-other-keys)
  (declare (ignore test size rehash-size rehash-threshold #+(or ccl clisp sbcl) weak))
  #+sbcl (progn
           (setf (getf options :weakness) (getf options :weak))
           (remf options :weak))
  (make-instance 'oam::hash-tree :options options ))



;;;; Depreciated stuff -- should be eliminated.
(defgeneric make-multi-hash-getter-setter (class options))

(let ((empty '#:nil))
  (defun get-multi-hash (hash-table key other-keys)
    (handler-case
	(if other-keys
	    (get-multi-hash (cdr (gethash key hash-table))
                            (first other-keys) (rest other-keys))
            (let ((hash (gethash key hash-table)))
              (if hash
                  (let ((value (car hash)))
                    (if (eq empty value)
                        (values nil nil :empty-inner-node)
                        (values value t)))
                  (values nil nil :empty-leaf))))
      (type-error ()
	(values nil nil :error))))
  (labels ((new-hash (key hash-table hash)
             (etypecase hash
               ((cons t null) hash)
               (null (let ((hash (cons empty nil)))
                       (setf (gethash key hash-table) hash)
                       hash)))))
    (defun set-multi-hash (hash-table options value key other-keys)
      (let ((hash (gethash key hash-table)))
        (if other-keys
            (set-multi-hash (cdr (typecase hash
                                   ((cons t hash-table) hash)
                                   (t (rplacd (new-hash key hash-table hash)
                                              (apply #'make-hash-table options)))))
                            options value (first other-keys) (rest other-keys))
            (rplaca (typecase hash
                      ((cons t hash-table) hash)
                      (t (new-hash key hash-table hash)))
                    value))))))


(defclass oam::multi-hash-table ()
  (get-fn set-fn rem-fn count-fn))
;;;; SBCL (and CMUCL?) finalize their classes late in order to allow forward referenced superclasses
(let ((multi-hash-table-class (find-class 'oam::multi-hash-table)))
  (unless (c2mop:class-finalized-p multi-hash-table-class)
    (c2mop:finalize-inheritance multi-hash-table-class)))
(defmethod make-multi-hash-getter-setter ((type oam::multi-hash-table) options)
  (let ((hash-table (apply #'make-hash-table options)))
    (values (lambda (key other-keys)
              (get-multi-hash hash-table key other-keys))
            (lambda (value key other-keys)
              (set-multi-hash hash-table options value key other-keys)))))
(defmethod initialize-instance :after ((self oam::multi-hash-table) &rest initargs
                                       &key options &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (get-fn set-fn rem-fn count-fn)
      (make-multi-hash-getter-setter self options)
   (setf (slot-value self 'get-fn) get-fn
         (slot-value self 'set-fn) set-fn
         (slot-value self 'rem-fn) rem-fn
         (slot-value self 'count-fn) count-fn)))

(defun oam::make-multi-hash-table (&rest options &key &allow-other-keys)
  (make-instance 'oam::multi-hash-table :options options))

(oam::with-slot-definition-locations (get-fn set-fn rem-fn count-fn)
    oam::multi-hash-table
  (defun oam::get-multi-hash (hash-table key &rest other-keys)
    "Return the value of the multi hash corresponding to KEY and OTHER-KEYS."
    (funcall (c2mop:standard-instance-access hash-table get-fn) key other-keys))
  (defun oam::get-multi-hash* (hash-table keys)
    "Return the value of the multi hash corresponding to KEY and OTHER-KEYS."
    (funcall (c2mop:standard-instance-access hash-table get-fn) (first keys) (rest keys)))
  (defsetf oam::get-multi-hash (hash-table key &rest other-keys) (value)
    "Set the value of the multi hash to the given value."
    `(progn
       (funcall (c2mop:standard-instance-access ,hash-table ,set-fn)
                ,value ,key ,other-keys)
       ,value))
  (defsetf oam::get-multi-hash* (hash-table keys) (value)
    "Set the value of the multi hash to the given value."
    `(destructuring-bind (key &rest other-keys) ,keys
       (funcall (c2mop:standard-instance-access ,hash-table ,set-fn)
                ,value key other-keys)
       ,value))
  (defun oam::rem-multi-hash (hash-table key &rest other-keys)
    "Remove KEYS from HASH-TABLE."
    (funcall (c2mop:standard-instance-access hash-table rem-fn) key other-keys))
  (defun oam::rem-multi-hash* (hash-table keys)
    "Remove KEYS from HASH-TABLE."
    (funcall (c2mop:standard-instance-access hash-table rem-fn) (first keys) (rest keys)))
  (defun oam::multi-hash-count (hash-table)
    "Return the number of entries in HASH-TABLE."
    (funcall (c2mop:standard-instance-access hash-table count-fn))))

;;;;** Fixed Multi Hash Tables
  
(defun get-fixed-multi-hash (hash-table keys)
  (handler-case
      (loop with foundp
         for key in keys
         do (multiple-value-setq (hash-table foundp)
              (gethash key hash-table))
         finally (return (values hash-table foundp)))
    (type-error ()
      (values nil nil))))
(defun set-fixed-multi-hash (hash-table options value key other-keys)
  (if other-keys
      (set-fixed-multi-hash (or (gethash key hash-table)
                                (setf (gethash key hash-table)
                                      (apply #'make-hash-table (first options))))
                            (rest options) value (first other-keys) (rest other-keys))
      (setf (gethash key hash-table) value)))
(defun rem-fixed-multi-hash (hash-table keys)
  (handler-case
      (loop :with continue-p := t :and removed-p := nil
         :for (key hash) :in (nreverse
                              (loop :with hash := hash-table
                                 :for key :in keys
                                 :collect (list key
                                                (prog1 hash
                                                  (setq hash (gethash key hash))))))
         :while continue-p
         :do (progn
               (setq removed-p (remhash key hash))
               (setq continue-p (zerop (hash-table-count hash))))
         :finally (return removed-p))
    (type-error ())))
(defun clr-fixed-multi-hash (hash-table)
  (clrhash hash-table))

(defclass oam::fixed-range-multi-hash-table (oam::multi-hash-table) ())
(labels ((count-values (node sum)
           (maphash (lambda (k v)
                      (declare (ignore k))
                      (if (hash-table-p v)
                          (setq sum (count-values v sum))
                          (incf sum)))
                    node)
           sum))
  (defmethod make-multi-hash-getter-setter ((type oam::fixed-range-multi-hash-table)
                                            options)
    (let ((hash-table (apply #'make-hash-table (first options)))
          (nbr-keys (length options)))
      (values
       (lambda (key other-keys)
         (if (= nbr-keys (1+ (length other-keys)))
             (get-fixed-multi-hash hash-table (cons key other-keys))
             (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                    (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
       (lambda (value key other-keys)
         (if (= nbr-keys (1+ (length other-keys)))
             (set-fixed-multi-hash hash-table (rest options) value
                                   key other-keys)
             (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                    (< (1+ (length other-keys)) nbr-keys) (cons key other-keys)
                    nbr-keys)))
       (lambda (key other-keys)
         (if (= nbr-keys (1+ (length other-keys)))
             (rem-fixed-multi-hash hash-table (cons key other-keys))
             (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                    (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
       (lambda ()
         (count-values hash-table 0))))))

(defun oam::make-fixed-multi-hash-table (&rest options)
  (funcall #'make-instance 'oam::fixed-range-multi-hash-table :options options))


;;;;* Queues
(defun oam::make-queue (&key (synchronized t) &aux (synchronized (when synchronized t)))
  "Return a queue which is synchronized by default unless the key argument :SYNCHRONIZED is set to nil. It provides a function in one variable which is one of the symbols :enqueue :dequeue :length :peeker and :dump and which returns the corresponding method."
  (let (queue
        (lock (bt:make-lock "QUEUE"))
        (length 0))
    (declare (integer length))
    (compile nil `(lambda (method)
                    (ecase method
                      (:enqueue ,(if synchronized
                                     (lambda (value)
                                       "Enqueue value to the queue."
                                       (bt:with-lock-held (lock)
                                         (let ((elt (cons value (cdr queue))))
                                           (if queue
                                               (setf (cdr queue) elt)
                                               (setf (cdr elt) elt))
                                           (setf queue elt)
                                           (the integer (incf length)))))
                                     (lambda (value)
                                       "Enqueue value to the queue."
                                       (let ((elt (cons value (cdr queue))))
                                         (if queue
                                             (setf (cdr queue) elt)
                                             (setf (cdr elt) elt))
                                         (setf queue elt)
                                         (the integer (incf length))))))
                      (:dequeue ,(if synchronized
                                     (lambda ()
                                       "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
                                       (bt:with-lock-held (lock)
                                         (when queue
                                           (decf length)
                                           (values (if (eq queue (cdr queue))
                                                       (prog1 (car queue)
                                                         (setf queue nil))
                                                       (pop (cdr queue)))
                                                   t))))
                                     (lambda ()
                                       "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
                                       (when queue
                                           (decf length)
                                           (values (if (eq queue (cdr queue))
                                                       (prog1 (car queue)
                                                         (setf queue nil))
                                                       (pop (cdr queue)))
                                                   t)))))
                      (:length ,#'(lambda ()
                                    "Return the length of the queue."
                                    length))
                      (:peeker (let* ((eof "eof")
                                      (generator (oam::make-generator-from-circle (funcall ,(lambda () (cdr queue))) :eof eof)))
                                 (lambda (&aux (item (funcall generator)))
                                   "Iterate through the queue without changing its state. This function is not synchronized."
                                   (unless (eq eof item)
                                     (values item t)))))
                      (:dump ,(if synchronized
                                  (lambda ()
                                    "Empty the queue."
                                    (bt:with-lock-held (lock)
                                      (setq queue nil)))
                                  (lambda ()
                                    "Empty the queue."
                                    (setq queue nil))))
                      (:synchronizedp ,(lambda () synchronized)))))))
(declaim (inline oam::get-queue-method oam::enqueue oam::dequeue oam::queue-length oam::queue-dump oam::queue-synchronizedp oam::get-iterator))
(defun oam::get-queue-method (queue method)
  "Return the method METHOD of queue QUEUE."
  (declare ((function (symbol) (function)) queue)
           (optimize speed))
  (funcall queue method))
(defun oam::enqueue (queue value)
  "Enqueue value to the queue."
  (declare ((function (symbol) (function (*) integer)) queue)
           (optimize speed))
  (funcall (oam::get-queue-method queue :enqueue) value))
(defun oam::dequeue (queue)
  "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
  (declare ((function (symbol) (function () *)) queue)
           (optimize speed))
  (funcall (the (function ()) (oam::get-queue-method queue :dequeue))))
(defun oam::queue-length (queue)
  "Return the length of the queue."
  (declare ((function (symbol) (function () integer)) queue)
           (optimize speed))
  (funcall (oam::get-queue-method queue :length)))
(defun oam::queue-dump (queue)
  "Empty the queue."
  (declare ((function (symbol) (function () null)) queue)
           (optimize speed))
  (funcall (oam::get-queue-method queue :dump)))
(defun oam::queue-synchronizedp (queue)
  "Return T if the queue is synchronized and NIL else."
  (declare ((function (symbol) (function () null)) queue)
           (optimize speed))
  (funcall (oam::get-queue-method queue :synchronizedp)))
(defun oam::queue-iterator (queue)
  "Iterate through the queue without changing its state. This function is not synchronized."
  (declare ((function (symbol) (function () integer)) queue)
           (optimize speed))
  (oam::get-queue-method queue :peeker))



;;;;* Sequences, Vectors and Lists


;;;;** Sequences
;;;; TODO review the algorithm: it is inefficient but should work as expected.
(defun oam::remove-if (test sequence &rest options &key (key #'identity) from-end start end count)
  "Same as cl:remove-if except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (let ((items-to-be-removed (apply #'cl:remove-if-not test sequence options))
        (resulting-sequence sequence))
    (map nil (lambda (item)
               (setq resulting-sequence (apply #'cl:remove item resulting-sequence
                                               :count 1 :test #'eq :key #'identity options)))
         items-to-be-removed)
    (values resulting-sequence items-to-be-removed)))

(defun oam::remove (item sequence &rest options &key (test #'eql) key from-end start end count)
  "Same as cl:remove except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (apply #'oam::remove-if (lambda (elt) (funcall test item elt))
         sequence options))

(defun oam::find-extrema (sequence predicate &key key (equality #'equal))
  "Return a list containing the extrema of SEQUENCE according to the preorder relation PREDICATE. An element e is extremal if for every element f in SEQUENCE which is not equal to e with respect to the equality predicate EQUALITY: PREDICATE(f e) = NIL. If KEY is given it is applied to the elements of sequence before being applied to PREDICATE. The keyword argument EQUALITY defaults to CL:EQUAL."
  (declare ((or symbol (function (* *) boolean)) predicate equality)
           ((or symbol (function (*) *)) key)
           (optimize speed))
  (flet ((key (a) (if key `(funcall ,key ,a) a))) 
    (symbol-macrolet ((y<x `(and (not (funcall ,equality y x))
                                 (funcall ,predicate ,(key 'y) ,(key 'x)))))
      (remove-if (compile nil `(lambda (x)
                                 (some (lambda (y) ,y<x)
                                       ',sequence)))
                 sequence))))
(defun oam::insert (pos sequence &rest values)
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


(defun oam::ninsert (pos sequence &rest values)
  "Return a sequence of same type as SEQUENCE with VALUES inserted at POS. NINSERT may destructively modify SEQUENCE by inserting values at position POS. If POS is negative the position is relative to the end of SEQUENCE, -1 being after the last element. It is unspecified and probably an error (or worse: e.g. infinite loop) if SEQUENCE is a unproper list."
  (etypecase sequence
    (cons (let* ((list sequence)
                 (ll (length list))
                 (ll+1 (1+ ll))
                 (pos (mod (max (min pos ll) (- ll+1)) ll+1))) 
            (if (zerop pos)
                (progn (oam::ninsert 1 list (car list))
                       (apply #'oam::ninsert 1 list (cdr values))
                       (rplaca list (car values)))
                (setf (cdr (nthcdr (1- pos) list)) (append values (nthcdr pos list))))
            list))
    (sequence (apply #'oam::insert pos sequence values))))

;;;;** Vectors

(defun oam::make-binary-search (preorder-fn sequence &key key)
  "Return a function taking a unique argument X and which returns the entities xl as primary, xu as secondary and i as third value satisfying either
-- xl = nil, x < KEY(xu), i = -1 and xu is the leftmost element of SEQUENCE,
-- not(x < KEY(xl)) and x < KEY(xu), 0 <= i < length(SEQUENCE), xl is the ith element and xu the i+1th element of SEQUENCE,
-- not(x < KEY(xl)), xu = nil, i = length(SEQUENCE) - 1 and xl is the rightmost element of SEQUENCE,
with respect to the preorder relation < given by PREORDER-FN and defined on the elements of SEQUENCE applied to KEY if KEY is given."
  (declare (optimize speed)
           ((function (* *) *) preorder-fn)
           ((or null (function (*) *)) key))
  (let* ((seq (sort (coerce sequence 'simple-vector) preorder-fn :key key))
         (< (compile nil `(lambda (a b)
                            (funcall ,preorder-fn a ,(if key `(funcall ,key b) 'b)))))
         (m (1- (length seq))))
    (declare (unsigned-byte m)
             (simple-vector seq)
             ((function (* *) *) <))
    (symbol-macrolet ((i* `(setq i (the unsigned-byte (+ lo (floor (the unsigned-byte (- up lo)) 2)))))
                      (x<xl `(funcall ,< x (setq xl (svref ,seq ,i*))))
                      (x<xu `(funcall ,< x (setq xu (svref ,seq (1+ i))))))
      (compile nil `(lambda (x)
                      (let ((i 0) (lo 0) (up ,m) xl xu)
                        (declare (unsigned-byte i lo up)
                                 (optimize speed))
                        (loop (cond (,x<xl (when (zerop i) (return (values nil xl -1)))
                                           (setq up i))
                                    ((= ,m i) (return (values xl nil ,m)))
                                    (,x<xu (return (values xl xu i)))
                                    (t (setq lo (the unsigned-byte (1+ i))))))))))))
;;;;** Lists

(defun oam::filter (fn lst)
  "You give filter a function and a list, and get back a list of whatever non-nil values are returned by the function as it is applied to the elements of the list."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun oam::prune (test tree)
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

(defun oam::classify (seq &key (test #'eql) (key #'identity))
  "Return a list of lists of elements of SEQ which are, when applied to KEY, equal under TEST."
  (let* ((classes (make-hash-table :test test))
         keys result)
    (map nil (lambda (elt)
               (let ((key (funcall key elt))) 
                 (unless (nth-value 1 (gethash key classes))
                   (push key keys))
                 (push elt (gethash key classes))))
         seq)
    (dolist (key keys)
      (push (nreverse (gethash key classes)) result))
    result))

(defun oam::find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (oam::find2 fn (cdr lst))))))
(defun oam::before (x y lst &key (test #'eql) key)
  "Return the cdr beginning with the object given as the first argument if we encounter the first argument before encountering
the second. It will also return true if the second argument doesn't occur in the list at all."
  (let ((key (when (functionp key) key)))
    (labels ((before (lst)
               (and lst
                    (let ((first (if key
                                     (funcall key (car lst))
                                     (car lst))))
                      (cond ((funcall test y first) nil)
                            ((funcall test x first) lst)
                            (t (oam::before x y (cdr lst) :test test :key key)))))))
      (before lst))))
(defun oam::after (x y lst &key (test #'eql) key)
  "Return the cdr beginning with the object given as the first argument if we encounter the first argument after encountering
the second."
  (let ((rest (oam::before y x lst :test test :key key)))
    (and rest (member x rest :test test :key key))))
(defun oam::duplicate (obj lst &key (test #'eql) key)
  "Return the cdr beginning with the object given as the first argument if we encounter the first argument a second time in LST."
  (member obj (cdr (member obj lst :test test :key key))
          :test test))
(defun oam::split-if (fn lst)
  "Return as primary value a new list with the leftmost elements of the proper list LST for which the function FN, when applied to those elements, returns NIL. The secondary value is the rest of LST from the first element of LST on for which FN returns non NIL."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun oam::most (fn lst)
  "Return as primary value the first element of the proper list LST for whom the value returned by the function FN when applied to those elements is maximal. The secondary value is the value returned by FN."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))
(defun oam::best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))
(defun oam::mostn (fn lst)
  "Return as primary value a list of those elements in LST for which the value obtained by applying them to the function FN is maximal. The secondary value is this maximal value."
  (declare ((function (*) real) fn))
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (declare (real max))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (declare (real score))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;;;**** Mapping

(defun oam::map0-n (fn n)
  "Return a list resulting of mapping the function FN to all integers not below 0 and below N."
  (oam::mapa-b fn 0 n))
(defun oam::map1-n (fn n)
  (oam::mapa-b fn 1 n))
(defun oam::mapa-b (fn a b &optional (step 1)
                    &aux (test (cond ((< 0 step) (lambda (i) (not (< i b))))
                                     ((< step 0) (lambda (i) (not (< b i))))
                                     (t (constantly t)))))
  "Return a list resulting of mapping the function FN to the numbers from A inclusive to B exclusive by steps STEP."
  (declare ((function (number) *) fn test)
           (number a b step)
           (optimize speed))
  (do ((i a (+ i step))
       (result nil (cons (funcall fn i) result)))
      ((funcall test i) (nreverse result))))
(defun oam::map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
(defun oam::mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
(defun oam::mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
(defun oam::rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
               (lambda (&rest args)
                 (apply #'oam::rmapcar fn args))
               args)))

;;;;***** Parallel Mapping Functions

(defun displace-split (vector count)
  "Return a list of COUNT displaced sub vectors of VECTOR."
  (let* ((length (length vector))
         (count-1 (1- count)))
    (multiple-value-bind (d r)
        (ceiling length count)
      (append (loop :for i :below count-1
                 :collect (make-array d :displaced-to vector :displaced-index-offset (* d i)))
              (list (make-array (+ d r) :displaced-to vector :displaced-index-offset (* d count-1)))))))

(defvar *cpu-count* (min 60 #+ccl (* 2 (ccl:cpu-count)) #-ccl 4))
(defvar *max-running-threads* 60)
(let ((thread-count 0)
      (lock (bt:make-lock)))
  (defun running-threads ()
    thread-count)
  (defun thread-count-inc-if (pred)
    (bt:with-lock-held (lock)
      (when (funcall pred thread-count) (incf thread-count)))))
(defun determine-split-factor (vects)
  "Return the number of sub vectors the vectors in VECTS should be split into for parallelization."
  (let ((min-size (reduce #'min (mapcar #'length vects))))
    (ceiling min-size *cpu-count*)))
(defun oam::pmap-into (vector fn &rest vects &aux threads (count (determine-split-factor vects)))
    "Like cl:map-into but parallelize the process over several threads. Unlike with cl:map-into, the order in which the elements of VECTS are applied to FN is not known. The number of threads which are spawned is determined using the special variable *parallel-thread-count*."
    (when vects 
      (apply #'map nil
             (lambda (sv &rest svs)
               (push (bt:make-thread (compile nil
                                              `(lambda ()
                                                 (apply ,#'map-into ,sv ,fn ',svs))))
                     threads))
             (displace-split vector count)
             (mapcar (lambda (vect)
                       (displace-split vect count))
                     vects)))
    (map nil #'bt:join-thread threads)
    vector)
(defun oam::pmap (fn &rest vects &aux threads (count (determine-split-factor vects)))
    "Like cl:map but parallelize the process over several threads and applies only to vectors. Unlike with cl:map, the order in which the elements of VECTS are applied to FN is not known. The number of threads which are spawned is hold in the special variable *parallel-thread-count*."
    (when vects 
      (apply #'map nil (lambda (&rest svs)
                         (push (bt:make-thread (compile nil
                                                        `(lambda ()
                                                           (apply ,#'map nil ,fn ',svs))))
                               threads))
             (mapcar (lambda (vect)
                       (displace-split vect count))
                     vects)))
    (map nil #'bt:join-thread threads))

;;;;*** Circles

(defun oam::cyclic-p (cons)
  "Return T if the CONS contains cyclic references in the structure."
  (let ((hash (make-hash-table :test #'eq)))
    (labels ((seen-p (c)
               (and (consp c)
                    (or (gethash c hash)
                        (and (setf (gethash c hash) t)
                             (seen-p (car c))
                             (seen-p (cdr c)))))))
      (seen-p cons))))
(defun oam::circle-p (list)
  "Return T if the list LIST contains a circle and nil else. As second value return the length of the list resp. the circle. N.B. in opposition to CYCLIC-P, CIRCLE-P looks only in the CDR for cyclic references; we are interested in LIST as as list and not as a tree."
  (let* ((length 0)
         (circle-p (loop :with hash := (make-hash-table :test #'eq)
                     :for i :on list
                     :do (if (gethash i hash)
                             (return t)
                             (progn (setf (gethash i hash) T)
                                    (incf length))))))
    (values circle-p length)))
(defun oam::length* (seq)
  "Like cl:length but accepting circles."
  (etypecase seq
    (vector (length seq))
    (list (nth-value 1 (oam::circle-p seq)))))
(defun oam::last* (list &optional (n 1))
  "As cl:last but applicable on circles, in which case it returns (last (nth-value 2 (oam::circle-p LIST)) n)."
  (last (nth-value 2 (oam::circle-p LIST)) n))

(defun oam::proper-list-p (o)
  "Return T if o is a propoer list and nil else."
  (and (listp o)
       (not (oam::circle-p o))
       (null (cdr (last o)))))

;;;;*** Alists and plists
(defun oam::alistp (o &key (key-type #'symbolp))
  "Return T if o is an association list and nil else. An association list is a propoer list of CONSes whose CAR satisfy KEY-TYPE."
  (and (listp o)
       (every (lambda (item)
                (and (consp item)
                     (funcall key-type (car item))))
              o)))
(defun oam::plistp (o &key (key-type #'symbolp))
  "Return T if o is a property list and nil else. A property list is a proper list with an even number of elements and whose elements on even position satisfy KEY-TYPE."
  (and (listp o)
       (evenp (length o))
       (every (let ((role :key))
                (oam::fbind
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

(defun oam::map-plist (fct plist)
  "Like cl:mapcar but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :collect (funcall fct k v)
       :until (null plist))))
(defun oam::map*-plist (fct plist)
  "Like MAP-PLIST but returning only NIL. This is used only for side effects."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :do (funcall fct k v)
       :until (null plist))))
(defun oam::mapcan-plist (fct plist)
  "Like cl:mapcan but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       :for k := (pop plist)
       :for v := (pop plist)
       :append (funcall fct k v)
       :until (null plist))))
(defun oam::keys+values<-plist (plist)
  "Return as primary value a list of all keys of the property list PLIST and as secondary value a list of all their corresponding values in the same order."
  (let (keys vals)
    (oam::map*-plist (lambda (k v)
                       (push k keys)
                       (push v vals))
                     plist)
    (values (nreverse keys) (nreverse vals))))

(defun oam::alist<-plist (plist)
  "Return an association list from a property list. Note: An association list is a proper list of CONSes and not of pairs."
  (oam::map-plist #'cons plist))
(defun oam::alist2<-plist (plist)
  "Return an association list from a property list. Note: Here an association list is a proper list of pairs and not of CONSes."
  (oam::map-plist #'list plist))

(defun oam::plist<-alist (alist)
  "Return a property list corresponding to the association list ALIST."
  (mapcan (lambda (x)
            (list (car x) (cdr x)))
          alist))
(defun oam::plist<-alist2 (alist2)
  "Return a property list made of the keys and the first element of each association in the association list ALIST2. This is especially used to map malformed association lists where the values of the associations are singletons."
  (mapcan (lambda (x)
            (list (car x) (cadr x)))
          alist2))


;;;;** Cached Structures
(defmacro oam::define-cached-struct (name &rest slots)
  "Like CL:DEFSTRUCT but defines the constructors in such a way that when called twice with same args it returns the same instance. The args are hereby tested by the equality test given with the option :test (possible values EQ EQL EQUAL EQUALP). Copier function definitions are not allowed, since it would be contrary to the principle of caching instances."
  (assert (and (not (null name))
               (or (symbolp name)
                   (listp name))))
  (assert (or (symbolp name)
              (notany (lambda (x) (or (eq :copier x)
                                      (and (consp x) (eq :copier (car x)))))
                      (cdr name))))
  (let* ((slots (mapcar (lambda (slot)
                          (setq slot (etypecase slot
                                       (symbol (list slot nil))
                                       (cons slot)))
                          (setf (getf (cddr slot) :read-only) t)
                          slot)
                        slots))
         (constructors (mapcar (lambda (constructor)
                                 (when (symbolp constructor)
                                   (setq constructor (list constructor)))
                                 (unless (second constructor)
                                   (setf (cdr constructor) (list (intern (format nil "MAKE-~A" name)))))
                                 (unless (third constructor)
                                   (setf (cddr constructor) (list (list* '&key (mapcar #'car slots)))))
                                 constructor)
                               (if (listp name)
                                   (remove-if (lambda (option)
                                                (etypecase option
                                                  (symbol (not (eq :constructor option)))
                                                  (cons (destructuring-bind (option-type &rest option-spec)
                                                            option
                                                          (or (not (eq :constructor option-type))
                                                              (and option-spec
                                                                   (null (car option-spec))))))))
                                              (cdr name))
                                   (list (list :constructor)))))
         (structure-name (typecase name
                           (symbol name)
                           (list (car name)))))
    (multiple-value-bind (name test)
        (typecase name
          (symbol (list structure-name constructors))
          (t (cons (car name)
                   (oam::remove-if (lambda (x) (eq (car x) :test))
                                   (cdr name)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,name
           ,@(mapcar (lambda (slot)
                       (when (symbolp slot)
                         (setq slot (list slot nil)))
                       (setf (getf (cddr slot) :read-only) t)
                       slot)
                     slots))
         (let ((cache (oam::make-hash-tree ,@(car test))))
           (let (,@(mapcar (lambda (constructor)
                             (let ((constructor-name (second constructor)))
                               `(,constructor-name (symbol-function ',constructor-name))))
                           constructors))
             ,@(mapcar (lambda (constructor)
                         `(symbol-macrolet (,@(mapcar (lambda (slot)
                                                        `(,(first slot) ',(second slot)))
                                                      slots))
                            (defun ,@(cdr constructor)
                                (or (oam::get-multi-hash cache ,@(mapcar #'car slots))
                                 (setf (oam::get-multi-hash cache ,@(mapcar #'car slots)) (apply ,@(cdr constructor)))))))
                       constructors)))
         ',name))))

;;;;**** I/O

(defun oam::readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))
(defun oam::prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
(defun oam::break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
     (let ((in (apply #'oam::prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~A~%" (funcall fn in))))))

;;;; Lambda Lists
(defun get-lmbd-keydefs (lambda-list)
  (let* ((aux (cdr (member '&key lambda-list)))
         (pos (position-if (lambda (item) (member item lambda-list-keywords)) aux)))
    (subseq aux 0 pos)))
(defun oam::get-lambda-keys (lambda-list)
  (mapcar (lambda (keydef)
            (etypecase keydef
              (symbol (intern (string keydef) :keyword))
              ((cons symbol t) (intern (string (car keydef)) :keyword))
              ((cons (cons symbol (cons symbol null)) t) (caar keydef))))
          (get-lmbd-keydefs lambda-list)))
(defun oam::get-lambda-key-variables (lambda-list)
  (mapcar (lambda (keydef)
            (etypecase keydef
              (symbol keydef)
              ((cons symbol t) (car keydef))
              ((cons (cons symbol (cons symbol null)) t) (cadar keydef))))
          (get-lmbd-keydefs lambda-list)))

;;;; Arglist
(defun oam::get-positional-args (arglist)
  (let (result)
    (nreverse (dolist (arg arglist result)
                (case arg
                  ((&rest &key) (return result))
                  (&optional)
                  (t (push arg result)))))))
(defun oam::get-rest-arg (arglist)
  (second (member '&rest arglist)))
(defun oam::get-mandatory-args (arglist)
  (loop :for arg :in arglist
     :until (member arg '(&optional &rest &key)) :collect arg))
(defun oam::get-optional-args (arglist)
  (loop :for arg :in (rest (member '&optional arglist))
     :until (member arg '(&rest &key)) :collect arg))

(defun oam::get-keys (arglist)
  (rest (member '&key arglist)))

(defun oam::make-lambda-list-from-arglist (arglist)
  (append (oam::get-mandatory-args  arglist)
          (let ((optional-args (oam::get-optional-args arglist)))
            (when optional-args (list* '&optional optional-args)))
          (let ((rest-arg (oam::get-rest-arg arglist)))
            (when rest-arg (list '&rest rest-arg)))
          (let* ((keys (oam::get-keys arglist))
                 (key-vars (loop :for key :in keys
                              :until (eq key '&allow-other-keys) :collect (make-symbol (string key)))))
            (when keys (append (list* '&key (mapcar (oam::f-comp #'list #'list) keys key-vars)) (member '&allow-other-keys keys))))))


;;;;** Partially Evaluated Functions
;;;;
;;;; 
(defconstant oam::$ 'oam::$
  "Undefined value.")
(defun oam::partial (fn &rest bound-args)
  "Return a function whose arglist is the arglist of the function FN from which the args bound in BOUND-ARGS are removed. The function applies then the "
  (let* ((arglist (oam::function-arg-list fn))
         (positional-args (oam::get-positional-args arglist))
         (rest-arg (or (oam::get-rest-arg arglist) (make-symbol "REST")))
         (bound-key-vals (last bound-args (max 0 (- (length bound-args) (length positional-args)))))
         (bound-keys (oam::map-plist (lambda (k v) (declare (ignore v)) k) bound-key-vals))
         (unbound-keys (delete-if (lambda (k) (member k bound-keys)) (oam::get-keys arglist)))
         (unbound-key-vars (mapcar (oam::f-comp #'make-symbol #'string) unbound-keys))
         (lambda-list (append (loop :with bound-args := bound-args
                                 :for arg :in positional-args
                                 :for value := (pop bound-args)
                                 :for count :downfrom (length bound-args)
                                 :when (and (< 0 count) (eq oam::$ value))
                                 :collect arg)
                              (list '&rest rest-arg)
                              (when unbound-keys
                                (append '(&key) (mapcar (oam::f-comp #'list #'list)
                                                        unbound-keys unbound-key-vars)
                                        (member '&allow-other-keys arglist)))))
         (call-list (append (loop :with bound-args := bound-args
                               :for arg :in positional-args
                               :for value := (pop bound-args)
                               :for count :downfrom (length bound-args)
                               :collect (if (and (< 0 count) (eq oam::$ value))
                                            arg value))
                            bound-key-vals
                            (list rest-arg))))
    (compile nil `(lambda ,lambda-list
                    (declare (ignore ,@unbound-key-vars)
                             (optimize speed))
                    (apply ,fn ,@call-list)))))

(oam:export-interface '#:oam)