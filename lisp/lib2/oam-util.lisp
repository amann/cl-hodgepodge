;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: bp2ntm.lisp,v 1.2 2010/05/17 16:01:44 amao Exp $
(in-package #:cl-user)
(defpackage #:oam
  (:use #:cl)
  (:export #:match-string
           #:string-replace
           #:if*
           #:when*
           #:to-keyword))

(in-package #:oam)

;;;;* IF* and WHEN* utilities
(defmacro if* (test then &optional else)
  "Like `if' but the return value of TEST is locally bound to the variable IT or <VAR> if the argument test is given in the form (<VAR> test)."
  (let* ((var (or (and (consp test) (= 2 (length test)) (car test))
                  'it))
         (test (if (eq var 'it)
                   test
                   (cadr test))))
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro when* (test &body then)
  "Like `when' but the return value of TEST is locally bound to the variable IT or <VAR> if the argument test is given in the form (<VAR> test)."
  (let* ((var (or (and (consp test) (= 2 (length test)) (car test))
                  'it))
         (test (if (eq var 'it)
                   test
                   (cadr test))))
    `(let ((,var ,test))
       (when ,var ,@then))))

;;;;* String Utilities
(defun match-string (target string)
  "Return the position in STRING (beginnig with 0) of the first (from the left) character of the first substring of STRING matching TARGET. If there is no match, nil is returned."
  (etypecase target
    (character
     (position target string))
    (string
     (let* ((tar0 (elt target 0))
            (n (length target)))
       (do ((position (position tar0 string) (let ((pos (position tar0 (subseq string (1+ position)))))
                                               (and pos (+ position pos 1)))))
           ((or (null position)
                (string= target (subseq string position (+ position n))))
            position))))))

(defun string-replace (prefix replace target string)
  "Return the concatenation of PREFIX and the string obtained form STRING by replacing in STRING all occurencies of TARGET by REPLACE."
  (if (string= "" string)
      prefix
      (let* ((position (match-string target string))
             (prefix (concatenate 'string prefix (if position (concatenate 'string (subseq string 0 position) replace) string)))
             (string (if position (subseq string (+ position (length target))) "")))
        (string-replace prefix replace target string))))



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
  "Intern STRING-DESIGNATOR into the keyword package after having applied the functions in *to-keyword-hook* to (string STRING-DESIGNATOR)."
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
(defpackage #:cursor
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



;;;;** The Date Package
;;;;
;;;; The class date::date-time is a simple wrapper for the common-lisp universal-time.
;;;; The date-times are cached in a weak hash-table, therefore one should not create
;;;; instances directly using make-instance but either by using date::get-date-time
;;;; or date::make-date-time. The variable plain-odbc::*universal-time-to-date-dataype*
;;;; is set to #'date::get-date-time and the variable plain-odbc::*date-datatype-to-universal-time*
;;;; is set to #'(lambda (x) (slot-value x 'date::universal-time)) in order to convert
;;;; automatically sql-dates to date::date-time.
;;;;
;;;; Furthermore are implemented some utilities like date::add-seconds, date::add-days
;;;; and date::add-months.
(in-package #:cl-user)
(defpackage #:date
  (:export #:date-time
           #:get-date-time
           #:make-date-time
           #:decode-date-time
           #:add-seconds
           #:add-days
           #:add-months))

(defclass date::date-time ()
  ((date::universal-time :initform (error "No universal-time given.") :initarg :universal-time)))

(let ((cache #+(or clisp openmcl) (make-hash-table :weak :value :test #'eql)
             #+sbcl (make-hash-table :test #'eql :weakness :value)))
  (defun date::get-date-time (universal-time)
    (assert (integerp universal-time) () "Invalid universal-time: ~S" universal-time)
    (or (gethash universal-time cache)
        (setf (gethash universal-time cache) (make-instance 'date::date-time
                                                            :universal-time universal-time)))))
(defun date::make-date-time (year &optional (month 1) (date 1) (hour 0) (minute 0) (second 0)
                             time-zone)
  (date::get-date-time (encode-universal-time second minute hour date month year time-zone)))



(defun date::decode-date-time (date-time &optional time-zone)
  (decode-universal-time (slot-value date-time 'date::universal-time) time-zone))
(defun date::get-second (date-time &optional time-zone)
  (first (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-minute (date-time &optional time-zone)
  (second (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-hour (date-time &optional time-zone)
  (third (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-date (date-time &optional time-zone)
  (fourth (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-month (date-time &optional time-zone)
  (fifth (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-year (date-time &optional time-zone)
  (sixth (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-day-of-week (date-time &optional time-zone)
  (seventh (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-daylight-p (date-time &optional time-zone)
  (eighth (multiple-value-list (date::decode-date-time date-time time-zone))))
(defun date::get-time-zone (date-time &optional time-zone)
  (ninth (multiple-value-list (date::decode-date-time date-time time-zone))))

(defun date::add-seconds (date-time nbr-sec)
  (make-instance 'date::date-time
                 :universal-time (+ (slot-value date-time 'date::universal-time)
                                    nbr-sec)))

(let ((+number-of-seconds-per-day+ #.(* 24 60 60)))
  (labels ((adjust-time (univ-time ref-univ-time time-zone)
             (+ univ-time
                (multiple-value-bind (ref-s ref-m ref-h)
                    (decode-universal-time ref-univ-time time-zone)
                  (multiple-value-bind (s m h)
                      (decode-universal-time univ-time time-zone)
                    (+ (- ref-s s)
                       (* 60 (- ref-m m))
                       (* #.(* 60 60) (- ref-h h))))))))
    (defun date::add-days (date-time nbr-days &optional time-zone)
      (let* ((univ-time (slot-value date-time 'date::universal-time))
             (univ-time+1day (+ univ-time (* nbr-days +number-of-seconds-per-day+))))
        (make-instance 'date::date-time
                       :universal-time (adjust-time univ-time+1day
                                                    univ-time time-zone))))))
(defvar date::*end-of-month-convention* 1)
(let ((+number-of-months-in-year+ 12))
  (labels ((leap-year-p (year)
             (and (zerop (mod year 4))
                  (or (not (zerop (mod year 100)))
                      (zerop (mod year 400)))))
           (days-in-current-month (year month)
             (aref (if (leap-year-p year)
                       #(31 31 29 31 30 31 30 31 31 30 31 30 31)
                       #(31 31 28 31 30 31 30 31 31 30 31 30 31))
                   month))
           (add-months (year month day nbr)
             (multiple-value-bind (dy dm) (floor (+ month nbr -1) +number-of-months-in-year+)
               (let* ((pos-from-the-end (- (days-in-current-month year month) day))
                      (year  (+ year dy))
                      (month (+ 1 dm))
                      (day (if (<= pos-from-the-end date::*end-of-month-convention*)
                               (- (days-in-current-month year month) pos-from-the-end)
                               day)))
                 (values day month year)))))
    (defun date::add-months (date-time nbr-months &optional time-zone)
      (multiple-value-bind (second minute hour day month year)
          (date::decode-date-time date-time time-zone)
        (multiple-value-call #'date::make-date-time (add-months year month day nbr-months)
                             hour minute second time-zone)))
    (defun date::leap-year-p (date-time &optional time-zone)
      (leap-year-p (date::get-year date-time time-zone)))
    (defun date::number-of-days-in-current-month (date-time &optional time-zone)
      (days-in-current-month (date::get-year date-time time-zone)
                             (date::get-month date-time time-zone)))))

(defgeneric date::format-date (date-time format &key time-zone stream &allow-other-keys)
  (:documentation "Send DATE-TIME in the format specified by FORMAT to the stream STREAM and return nil if STREAM is non nil. The default value of STREAM is nil, in which case FORMAT-DATE returns the formatted date as a string."))

(defmethod date::format-date ((date-time date::date-time) (format (eql :iso8601)) &key full time-zone stream &allow-other-keys)
  (declare (ignore format))
  (multiple-value-bind (second minute hour day month year)
      (date::decode-date-time date-time time-zone)
    (format stream "~4D-~2,'0D-~2,'0D~:[~;T~@{~2,'0D~^:~}~]"
            year month day full hour minute second)))
(defmethod date::format-date ((date-time date::date-time) (format (eql :MonYY)) &key time-zone stream &allow-other-keys)
  (declare (ignore format))
  (format stream "~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]~2,'0D"
          (date::get-month date-time time-zone) (mod (date::get-year date-time time-zone) 100)))
(defun date::now ()
  (date::get-date-time (get-universal-time)))

