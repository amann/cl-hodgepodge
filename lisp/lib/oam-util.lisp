;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: oam-util.lisp,v 1.2 2010/08/25 13:36:23 amao Exp $
(in-package #:cl-user)
(defpackage #:oam
  (:use #:cl)
  (:export #:match-string
           #:string-replace
           #:to-keyword))

(in-package #:oam)

;;;;* String Utilities
(defun match-string (target string)
  "Return the position in STRING (beginnig with 0) of the first (from the left) character of the first substring of STRING matching TARGET. If there is no match, nil is returned."
  (etypecase target
    (cons
     (let ((positions (mapcan #'(lambda (targ)
                                  (multiple-value-bind (pos targ) (match-string targ string)
                                    (when pos (list (list pos targ)))))
                              target)))
       (apply #'values (reduce #'(lambda (&optional prev curr)
                                   (when prev
                                    (let* ((prev-p (first prev))
                                           (curr-p (first curr)))
                                      (if (< curr-p prev-p)
                                          curr
                                          prev))))
                               positions))))
    (character
     (values (position target string) (make-string 1 :initial-element target)))
    (string
     (let* ((tar0 (elt target 0))
            (n (length target)))
       (do ((position (position tar0 string) (let ((pos (position tar0 (subseq string (1+ position)))))
                                               (when pos (+ position pos 1)))))
           ((or (null position)
                (string= target (subseq string position (+ position n))))
            (when position (values position target))))))))

(defun string-replace (prefix replace targets string)
  "Return the concatenation of PREFIX and the string obtained form STRING by replacing in STRING all occurencies of TARGET by REPLACE."
  (if (string= "" string)
      prefix
      (multiple-value-bind (position target)
          (match-string targets string)
        (let* ((prefix (concatenate 'string prefix (if position (concatenate 'string (subseq string 0 position) replace) string)))
               (string (if position (subseq string (+ position (length target))) "")))
          (string-replace prefix replace targets string)))))


;;;;* Macros
(defun function-arg-list (fn)
  #+clisp (ext:arglist fn)
  #+sbcl (sb-introspect:function-arglist fn)
  #+lucid (system::arglist fn)
  #+allegro (excl::arglist fn)
  #+openmcl (ccl:arglist fn)
  ;; GCLisp 1.1 version
  #+gclisp (if (macro-function fn)
               '(&rest "Form =")
               (lambda-list fn))
  #+kcl (let ((x (symbol-function fn)))
          (cond ((atom x) nil)
                ((eq (first x) 'macro) (list '&rest "Form ="))
                (t (third x)))))

;;; CMU Common Lisp version.  This version looks in a symbol's
;;; function cell and knows how to take apart lexical closures
;;; and compiled code objects found there.
#+cmu
(defun function-arg-list (x &optional original-x)
  (typecase x
    (symbol (function-arg-list (symbol-function x) x))
    (compiled-function (read-from-string
                        (lisp::%primitive header-ref x
                                          lisp::%function-arg-names-slot)))
    (list (case (first x)
            (lambda (second x))
            (lisp::%lexical-closure% (function-arg-list (second x)))
            (system:macro '(&rest "Form ="))
            (t '(&rest "Arglist:"))))
    (t (cerror (format nil
                       "Use a reasonable default argument list for ~S"
                       original-x)
               "Unkown object in function cell of ~S:  ~S" original-x x)
       '())))
 
(defun get-parameter-list (lambda-list)
  (let (rest)
    (append
     (mapcan (let ((state '&ordinary))
               (lambda (x)
                 (if (member x lambda-list-keywords)
                     (progn (setq state x) nil)
                     (case state
                       (&ordinary (etypecase x
                                    (list (list (get-parameter-list x)))
                                    (symbol (list x))))
                       (&optional (list (or (and (consp x) (car x)) x)))
                       ((&rest &body) (progn (setq rest x) nil))
                       ((&aux &whole &environment)
                        (progn (setq state '&ordinary) nil))
                       (&key (unless rest
                               (if (consp x)
                                   (let ((x (car x)))
                                     (if (consp x)
                                         x
                                         (list (intern (symbol-name x)
                                                       :keyword) x)))
                                   (list (intern (symbol-name x)
                                                 :keyword) x))))))))
             lambda-list)
     (list rest))))

(defun get-lambda-variables (lambda-list)
  (mapcan (let ((state '&ordinary))
            (lambda (x)
              (if (member x lambda-list-keywords)
                  (progn
                    (setq state x)
                    nil)
                  (case state
                    (&ordinary (etypecase x
                                 (list (get-lambda-variables x))
                                 (symbol (list x))))
                    ((&rest &body) (list x))
                    ((&whole &environment) (progn
                                             (setq state '&ordinary)
                                             (list x)))
                    ((&optional &aux &key) (if (consp x)
                                               (let ((x (car x))
                                                     (y (third x)))
                                                 (list* (if (consp x)
                                                            (cadr x)
                                                            x)
                                                        (when y (list y))))
                                               (list x)))))))
          lambda-list))

;;;;==============================================================================

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@(mapcar #'(lambda (name)
                      `(,name (gensym ,(string name))))
                  names))
     ,@body))

(defun expand-list (list)
  (list* 'list* (if (listp (car list))
                    (expand-list (car list))
                    (car list)) (when (cdr list)
                    (list (expand-list (cdr list))))))

#+nil
(defmacro with-actual-macros (names &body body &environment env)
  (let ((gensyms (mapcar #'(lambda (name) (gensym (string name))) names)))
    (labels
        ((make-macro-def-code (names)
           (mapcar #'(lambda (gensym name)
                       `(,gensym ,(function-arg-list name)
                                 (macroexpand
                                  (list* ',name
                                         ,(expand-list
                                           (get-parameter-list
                                            (function-arg-list name))))
                                              ,env)))
                   gensyms names)))
      `(let (,@(mapcar #'list names gensyms))
         `(macrolet (,,@(make-macro-def-code names))
            ,,@body)))))

;;;;* Functions
(defun compose-fn (fct &rest fcts)
  "Return a function which is a composition on the primary values of the functions given as input from the left to right. (compose-fn f0 .. fn) = fn o .. o f0."
  (if (null fcts)
      fct
      #'(lambda (&rest args)
          (reduce #'(lambda (r fn)
                      (funcall fn r))
                  fcts
                  :initial-value (apply fct args)))))
(defun n-tuple-fn (fct &rest fcts)
  "Take functions which are expected to have the same arglist and return a function which takes this arglist of arguments and returns as values lists of values of the individual input functions as follows: If the functions f_0 .. f_n return the values v_i_0 .. v_i_mi for each i from 0 to n, the form (apply (n-tuple-fn f_0 .. f_n) args) returns the values (v_0_0 .. v_n_0) .. (v_0_(min m0 .. mn) .. v_n_(min m0 .. mn))."
  (if (null fcts)
      fct
      #'(lambda (&rest args)
          (apply #'values
                 (apply #'mapcar
                        #'list
                        (mapcar #'(lambda (fn)
                                    (multiple-value-list (apply fn args)))
                                (cons fct
                                      fcts)))))))


;;;;* Hash
(defparameter *to-keyword-hook* `(,#'string-upcase)
  "Hook of functions used by TO-KEYWORD. Each function will be given the out put of the previous function, whereas the first receives a string. The last function must return a string-designator.")
(defun to-keyword (string-designator)
  "Intern STRING-DESIGNATOR into the keyword package after having applied the functions in *to-keyword-hook* to (string STRING-DESIGNATOR). If STRING-DESIGNATOR is already in the keyword package STRING-DESIGNATOR is returned unchanged."
  (etypecase string-designator
    (keyword string-designator)
    ((or symbol string)
     (intern (reduce #'(lambda (res fn)
                         (funcall fn res))
                     *to-keyword-hook*
                     :initial-value (string string-designator))
             'keyword))))

;;;;* Lists
(defun oam::insert (value list &optional (place 0))
  "Destructively modifies the non null proper LIST by inserting value at position PLACE. The default value of PLACE is 0 meaning the head of the list. If PLACE is negative the position is relative to the end of LIST, -1 being after the last element. It is unspecified and probably an error (or worse: e.g. infinite loop) if LIST is not a proper list."
  (unless list (error 'type-error :expected-type 'cons :datum list))
  (let* ((ll (length list))
         (ll+1 (1+ ll))
         (place (mod (max (min place ll) (- ll+1)) ll+1))) 
    (if (zerop place)
        (progn (oam::insert (car list) list 1)
               (rplaca list value))
        (push value (cdr (nthcdr (1- place) list)))))
  list)
(defun oam::insert* (values list &optional (place 0))
  (let* ((ll (length list))
         (ll+1 (1+ ll))
         (place (mod (max (min place ll) (- ll+1)) ll+1))) 
    (if (zerop place)
        (progn (oam::insert (car list) list 1)
               (oam::insert* (cdr values) list 1)
               (rplaca list (car values)))
        (setf (cdr (nthcdr (1- place) list)) (append values (nthcdr place list)))))
  list)

(defun oam::has-cycle-p (o)
  "Return T if o is a list containing a cycle and nil else."
  (loop with hash = (make-hash-table :test #'eq)
     for i on o
     do (if (gethash i hash)
            (return-from has-cycle-p t)
            (setf (gethash i hash) T))))
(defun oam::last2 (list &optional (n 1))
  (or (loop with hash = (make-hash-table :test #'eq)
         and dummy = "dummy"
         for cdr on list
         and res on (append (loop repeat n collect dummy) list)
         until (gethash cdr hash)
         do (setf (gethash cdr hash) T)
         count cdr into i
         finally (return (loop repeat (- n i) do (pop res)
                            finally (return res))))
      (last list)))

(defun oam::proper-list-p (o)
  "Return T if o is a propoer list and nil else."
  (and (listp o)
       (not (has-cycle-p o))
       (null (cdr (last o)))))
(defun oam::alistp (o &key (key-type #'symbolp))
  "Return T if o is an association list and nil else. An association list is a propoer list of CONSes whose CAR satisfy KEY-TYPE."
  (and (listp o)
       (every #'(lambda (item)
                  (and (consp item)
                       (funcall key-type (car item))))
              o)))
(defun oam::plistp (o &key (key-type #'symbolp))
  "Return T if o is a property list and nil else. A property list is a proper list with an even number of elements and whose elements on even position satisfy KEY-TYPE."
  (and (listp o)
       (evenp (length o))
       (every (let ((role :key))
                #'(lambda (item)
                    (case role
                      (:value (setq role :key)
                              t)
                      (:key (setq role :value)
                            (funcall key-type item)))))
              o)))

(defun oam::map-plist (fct plist)
  (when plist
    (loop
       for k = (pop plist)
       for v = (pop plist)
       collect (funcall fct k v)
       until (null plist))))
(defun oam::mapcan-plist (fct plist)
  (when plist
    (loop
       for k = (pop plist)
       for v = (pop plist)
       append (funcall fct k v)
       until (null plist))))

(defun oam::alist<-plist (plist)
  "Return an association list from a property list. Note: An association list is a proper list of CONSes and not of pairs."
  (oam::map-plist #'cons plist))
(defun oam::alist2<-plist (plist)
  "Return an association list from a property list. Note: An association list is a proper list of CONSes and not of pairs."
  (oam::map-plist #'list plist))

(defun oam::plist<-alist (alist)
  "Return a property list corresponding to the association list ALIST."
  (mapcan #'(lambda (x)
              (list (car x) (cdr x)))
          alist))
(defun oam::plist<-alist2 (alist2)
  "Return a property list made of the keys and the first element of each association in the association list ALIST2. This is especially used to map malformed association lists where the values of the associations are singletons."
  (mapcan #'(lambda (x)
              (list (car x) (cadr x)))
          alist2))


;;;;; Cursor interface
;;;;
;;;; Interface for cursors. A cursor is a (anonymous) function which when successively called
;;;; returns a new object. It may also be judicable to store the new value into a common (mutable)
;;;; object to avoid repeated consing. When no further object can be returned (eof), an error of type
;;;; `no-next-element-error' must be thrown or an eof object returned.
(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.cursor
  (:nicknames #:cursor)
  (:use #:common-lisp #:oam)
  (:export #:no-next-element-error
           #:map-cursors
           #:make-cursor
           #:make-list-cursor
           #:make-mumber-cursor))
(in-package #:cursor)

(define-condition no-next-element-error (error)
  ((cursor :initarg :cursor))
  (:report (lambda (condition stream)
             (format stream "There is no next element for cursor ~A."
                     (slot-value condition 'cursor))))
  (:documentation "Condition of supertype `error' which is signaled by the cursor when no next element can be generated."))

(defun map-cursors (type fn cursor &rest cursors)
  "Map the output of CURSORS to the function FN and return a sequence of the type TYPE containing the results of the mapping. If TYPE is nil, nil is returned. The loop terminates as soon as one of the cursors throws a `no-next-element-error'. It is therefore expected that (at least one of) the cursors throws a `no-next-element-error' to terminate the loop. TYPE is one of nil, list, vector or string."
  (let ((cursors (cons cursor cursors)))
    (ecase type
      ((nil) (handler-case
                 (loop (apply fn (mapcar #'funcall cursors)))
               (no-next-element-error ())))
      (list (let (collection)
              (handler-case
                  (loop (push (apply fn (mapcar #'funcall cursors))
                              collection))
                (no-next-element-error ()
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
                      (apply fn (mapcar #'funcall cursors))
                      collection))
           (no-next-element-error ()
             collection)))))))


(defgeneric make-cursor (object &key eof &allow-other-keys)
  (:documentation "Return a cursor taking OBJECT as base and returning EOF if non nil or throwing a `no-next-element-error' if EOF is nil. A cursor is a (anonymous) function which when successively called returns a new object."))

(defun make-list-cursor (list &key (step 1) key eof)
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(null list))
	 (eof-signal (or eof '(error 'no-next-element-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next `(dotimes (i ,step) (pop list))))
    (eval `(let ((list ',list))
             #'(lambda ()
                 ,(if test
                      `(if ,test
                           ,eof-signal
                           (prog1 ,get
                             ,next))
                      `(prog1 ,get
                         ,next)))))))

(defun make-number-cursor (n &key to (step 1) (eof nil))
  "Return a cursor producing numbers from N below TO by steps STEP. If TO <= N and 0 < STEP or N <= TO and STEP < 0 or if TO is not a number the cursor never terminates. If STEP is not a number it is set to 1. A STEP of 0 produces an infinite sequence of the constant N (Not really useful). If N is no a number it is set to 0. If EOF is not nil it is returned when reaching the terminating condition is reached instead of throwing a `no-next-element-error'."
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
	 (eof-signal (or eof '(error 'no-next-element-error)))
	 (get 'n)
	 (next `(incf n ,step)))
    (eval `(let ((n ,n))
             #'(lambda ()
                 ,(if test
                      `(if ,test
                           ,eof-signal
                           (prog1
                               ,get
                             ,next))
                      `(prog1 ,get
                         ,next)))))))

(defmethod make-cursor ((n number) &key eof (step 1) to &allow-other-keys)
  (make-number-cursor n :to to :step step :eof eof))
(defmethod make-cursor ((list list)&key eof (step 1) key &allow-other-keys)
  (make-list-cursor list :step step :key key :eof eof))



