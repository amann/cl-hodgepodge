;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: oam-util.lisp,v 1.2 2010/08/25 13:36:23 amao Exp $
(in-package #:ch.amann-wolowyk.oam-system)


;;;;* Generators and Iterators



;;;;* String Utilities

(defun oam::make-string-matcher (targets &optional escape-char from-end)
         "Return a function taking a string and returning as primary value the position in STRING of the first character of any of targets from the beginning (from the end if FROM-END is t) in the substring of STRING delimited by START and END and which is not preceeded by a single ESCAPE-CHAR. The secondary value is the position + target-length and the third value is the target which is at that position."
         (etypecase targets
           (atom (make-string-matcher targets escape-char from-end))
           (cons (let ((matchers (mapcar #'(lambda (target)
                                             (make-string-matcher target escape-char from-end))
                                         targets)))
                   (eval `#'(lambda (string start end)
                              ,(format nil "Return the position of the first match from the ~:[beginning~;end~] of ~v[~;~:;one of ~]~{~S~#[~; or ~:;, ~]~} in STRING between START and END, unless escaped with the character ~S."
                                       from-end (length targets) targets escape-char)
                              (values-list (reduce  ,(eval `#'(lambda (a b)
                                                                (if a
                                                                    (if (and b (funcall ,(if from-end #'> #'<) (car b) (car a))
                                                                             b
                                                                             a)
                                                                        b))))
                                                    ,matchers
                                                    :key #'(lambda (m)
                                                             (funcall m start end))))))))))



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
    (eval `#'(lambda (string &optional (start 0) end)
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
                        (return))))))))


(define-condition oam::no-next-element-condition (condition)
  ((generator :initarg :generator))
  (:report (lambda (condition stream)
             (format stream "There is no next element for cursor ~A."
                     (slot-value condition 'generator))))
  (:documentation "Condition of supertype `error' which is signaled by the cursor when no next element can be generated."))

(defun oam::make-string-splitter-factory (targets &key escape-char max-chunks from-end eof-signal-p)
  (let ((no-next-element (when eof-signal-p (make-condition 'oam::no-next-element-condition
                                                            :generator (format nil "#<string-splitter ~S, ~S, ~S, ~S>"
                                                                               targets escape-char max-chunks from-end))))
        (matcher (oam::make-string-matcher targets escape-char from-end)))
    (eval `#'(lambda ,(append `(string &optional (start 0) end
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
               #'(lambda ()
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
    #'(lambda (string &optional (start 0) end)
        (oam:fbind ((next-item (make-string-splitter string start end)))
          (loop :for item = (next-item) :while item :collect item)))))

(defun oam::split-string (targets string &key escape-char max-chunks from-end (start 0) end)
  (funcall (oam::make-string-splitter targets escape-char max-chunks from-end) string start end))


(defun oam::make-string-replacer (replacement targets &optional escape-char max-operations from-end)
  (oam:fbind ((split-string (oam::make-string-splitter targets escape-char max-operations from-end)))
    #'(lambda (string &optional (start 0) end)
        (reduce #'(lambda (r s) (concatenate 'string r replacement s))
                (split-string string start end)))))

(defun oam::string-replace (replacement targets string &key escape-char max-operations from-end (start 0) end)
  (funcall (oam::make-string-replacer replacement targets escape-char max-operations from-end) string start end))


;;;;* Functions
(defun oam::function-arg-list (fn)
  "return the arg list of `fn'."
  #+clisp (ext:arglist fn)
  #+sbcl (sb-introspect:function-lambda-list fn)
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
(defun oam::function-arg-list (x &optional original-x)
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

(defun oam::get-parameter-list (lambda-list)
  "Return the argument list used to call a function whose arg list is `lambda-list'."
  (let (rest)
    (append
     (mapcan (let ((state '&ordinary))
               (lambda (x)
                 (if (member x lambda-list-keywords)
                     (progn (setq state x) nil)
                     (case state
                       (&ordinary (etypecase x
                                    (list (list (oam::get-parameter-list x)))
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

(defun oam::get-lambda-variables (lambda-list)
  "Return an alist2 containing the assignments of the variable names and their default value (if provided) defined in an ordinary or macro lambda list `lambda-list'. Useful for assignments by LET in macros."
  (mapcan (let ((state '&ordinary))
            (lambda (x)
              (if (member x lambda-list-keywords)
                  (progn
                    (setq state x)
                    nil)
                  (case state
                    (&ordinary (etypecase x
                                 (list (oam::get-lambda-variables x))
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
(defun oam::compose-fn (fct &rest fcts)
  "Return a function which is a composition on the primary values of the functions given as input from the left to right. \(apply \(compose-fn f0 .. fn) args) == \(fn \(fn-1 .. \(apply #'f0 args) ..))."
  (if (null fcts)
      fct
      #'(lambda (&rest args)
          (reduce #'(lambda (r fn)
                      (funcall fn r))
                  fcts
                  :initial-value (apply fct args)))))

(defun oam::n-tuple-fn (fct &rest fcts)
  "Take functions which are expected to have the same arglist and return a function which takes this arglist of arguments and returns as values lists of the values of the individual input functions as follows: If the functions f_0 .. f_n return the values v_i_0 .. v_i_mi for each i from 0 to n, the form \(apply \(n-tuple-fn f_0 .. f_n) args) returns the values \(v_0_0 .. v_n_0) .. \(v_0_(min m0 .. mn) .. v_n_(min m0 .. mn))."
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
(defparameter oam::*to-keyword-hook* `(,#'string-upcase)
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
  (getter-fn setter-fn))
(defmethod initialize-instance :after ((self oam::multi-hash-table) &rest options
                                       &key &allow-other-keys)
  (let ((hash-table (apply #'make-hash-table options)))
    (setf (slot-value self 'getter-fn) #'(lambda (key other-keys)
                                           (get-multi-hash hash-table key other-keys))
          (slot-value self 'setter-fn) #'(lambda (value key other-keys)
                                           (set-multi-hash hash-table options value key other-keys)))))
(defun oam::make-multi-hash-table (&rest options &key &allow-other-keys)
  (apply #'make-instance 'oam::multi-hash-table options))

(let* ((slot-defs (c2mop:compute-slots (find-class 'oam::multi-hash-table)))
       (getter-fn-loc (c2mop:slot-definition-location (find 'getter-fn slot-defs :key #'c2mop:slot-definition-name)))
       (setter-fn-loc (c2mop:slot-definition-location (find 'setter-fn slot-defs :key #'c2mop:slot-definition-name))))
  (defun oam::get-multi-hash (hash-table key &rest other-keys)
    (funcall (c2mop:standard-instance-access hash-table getter-fn-loc) key other-keys))
  (defsetf oam::get-multi-hash (hash-table key &rest other-keys) (value)
    "Set the value of the multi hash to the given value."
    `(progn
       (funcall (c2mop:standard-instance-access ,hash-table ,setter-fn-loc) ,value ,key ,other-keys)
       ,value)))

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
(defclass oam::fixed-range-multi-hash-table (oam::multi-hash-table) ())
(defmethod initialize-instance :after ((self oam::fixed-range-multi-hash-table) &rest options
                                       &key &allow-other-keys)
  (let ((hash-table (apply #'make-hash-table (first options)))
        (nbr-keys (length options)))
    (setf (slot-value self 'getter-fn) #'(lambda (key other-keys)
                                           (if (= nbr-keys (1+ (length other-keys)))
                                               (get-fixed-multi-hash hash-table (cons key other-keys))
                                               (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                                                      (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
          (slot-value self 'setter-fn) #'(lambda (value key other-keys)
                                           (if (= nbr-keys (1+ (length other-keys)))
                                               (set-fixed-multi-hash hash-table (rest options) value key other-keys)
                                               (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                                                      (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys))))))
(defun oam::make-fixed-multi-hash-table (&rest options)
  (apply #'make-instance 'oam::fixed-range-multi-hash-table options))




;;;;* Sequences and Lists

;;;;** Sequences
;;;; TODO review the algorithm: it is inefficient but should work as expected.
(defun oam::remove-if (test sequence &rest options &key (key #'identity) from-end start end count)
  "Same as cl:remove-if except that this one returns as second value a sequence of same type as SEQUENCE containing all removed items."
  (declare (ignore key from-end start end count))
  (let ((items-to-be-removed (apply #'cl:remove-if-not test sequence options))
        (resulting-sequence sequence))
    (map nil #'(lambda (item)
                 (setq resulting-sequence (apply #'cl:remove item resulting-sequence
                                                 :count 1 :test #'eq :key #'identity options)))
         items-to-be-removed)
    (values resulting-sequence items-to-be-removed)))

(defun oam::find-extrema (sequence predicate &key key)
  "Return a list containing the extrema of SEQUENCE according to the preorder relation PREDICATE. An element e is extremal if for every element f in LIST: PREDICATE(f e) = NIL. If KEY is given it is applied to the elements of sequence before being applied to PREDICATE."
  (oam::fbind (predicate (key (or key #'identity)))
    (remove-if #'(lambda (x)
                   (some #'(lambda (y)
                             (predicate (key y) (key x)))
                         sequence))
               sequence)))
(defun oam::insert (pos sequence &rest values)
  "Return a fresh sequence with VALUES inserted at POS. If POS is negative the position is relative to the end of SEQUENCE, -1 being after the last element."
  (let* ((type (let ((type (type-of #(1 2 3 4))))
                        (typecase type
                          (cons (car type))
                          (symbol type))))
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

;;;;** Lists
;;;;*** Circles

(defun oam::circle-p (list)
  "Return T if the list LIST contains a circle and nil else. As second value return the length of the list resp. the circle. As third value return a fresh list containing all items of LIST before the circle."
  (let* ((length 0)
         proper-list
         (circle-p (loop with hash = (make-hash-table :test #'eq)
                     for i on list
                     do (if (gethash i hash)
                            (return t)
                            (progn (setf (gethash i hash) T)
                                   (incf length)
                                   (push (car i) proper-list))))))
    (values circle-p length (nreverse proper-list))))
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
  "Like cl:mapcar but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
  (when plist
    (loop
       for k = (pop plist)
       for v = (pop plist)
       collect (funcall fct k v)
       until (null plist))))
(defun oam::mapcan-plist (fct plist)
  "Like cl:mapcan but applying to FCT at each turn the car and the cadr of PLIST and advancing two steps in PLIST."
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
  "Return an association list from a property list. Note: Here an association list is a proper list of pairs and not of CONSes."
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

(oam:export-interface '#:oam)