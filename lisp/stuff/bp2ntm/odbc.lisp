;;;; -*- outline-regexp:";;;;;+ +" -*-
;;;; $Id: bp2ntm.lisp,v 1.1 2010/03/24 17:15:46 amao Exp $
;;;;; The BluePrint to NTM Mapping Program
;;;;
;;;;

;;;;; Some Utilities
(in-package #:cl-user)
(defpackage #:utils
  (:use #:common-lisp)
  (:export #:if*
           #:when*))
(in-package #:utils)

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


;;;;; Cursor interface
;;;;
;;;; Interface for cursors. A cursor is a (anonymous) function which when successively called
;;;; returns a new object. It may also be judicable to store the new value into a common (mutable)
;;;; object to avoid repeated consing. When no further object can be returned (eof), an error of type
;;;; `no-next-element-error' must be thrown or an eof object returned.
(in-package #:cl-user)
(defpackage #:cursor
  (:use #:common-lisp #:utils)
  (:export #:no-next-element-error
           #:map-cursors
           #:make-cursor
           #:make-mumber-cursor))
(in-package #:cursor)

(define-condition no-next-element-error (error)
  ((cursor :initarg :cursor))
  (:report (lambda (condition stream)
             (format stream "There is no next element for cursor ~A."
                     (slot-value condition 'cursor))))
  (:documentation "Condition of supertype `error' which is signaled by the cursor when no next element can be generated."))

(defun map-cursors (type fn &rest cursors)
  "Map the output of CURSORS to the function FN and return a sequence of the type TYPE containing the results of the mapping. If TYPE is nil, nil is returned. The loop terminates as soon as one of the cursors throws a `no-next-element-error'. It is therefore expected that (at least one of) the cursors throws a `no-next-element-error' to terminate the loop."
  (flet ((next-element ()
           (mapcar #'funcall cursors)))
    (ecase type
      ((vector string)
       (loop with result = (apply #'make-array '(1) :adjustable t :fill-pointer 0
                                  (case type (string (list :element-type 'character))))
          do (vector-push-extend (apply fn (next-element)) result)
          finally (return result)))
      (list
       (loop collect (apply fn (next-element))))
      ((nil)
       (loop (apply fn (next-element)))))))

(defgeneric make-cursor (collection &key eof &allow-other-keys))

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

;;;;;;; Several Ideas to Investigate
#|
(defmacro define-cursor-generator (name arglist &body body)
  (let* ((label name)
         (name (intern (concatenate 'string "MAKE-CUR-" (string label)))))
    `(defun ,name ,arglist
       (labels ((,label (,arg)
                  (ecase ,arg
                    ,@body)))
         #',label))))

(let* ((label name)
       (name (intern (concatenate 'string "MAKE-CUR-" (string label))))
       (doc (let ((doc (car body)))
              (if (stringp doc)
                  doc
                  (format nil "Cursor generator ~A~S" name arglist))))
       (body (cdr doc-body))
       (test (cdr (assoc :test body)))
       (get  (cdr (assoc :get body)))
       (step (cdr (assoc :step body))))
  `(defun ,name ,arglist
     ,doc
     (lambda ()
       (if (progn ,@test)
           (signal 'no-next-element-error :generator ,label)
           (prog1
               (progn ,@get)
             ,@step)))))

(defgen num-gen (num &key to (step 1))
  `((:test (and (numberp num)
                (numberp to)
                (<= to num)))
    (:get num)
    (:step (incf num step))))

(defun make-cur-number (n &key to (step 1))
  (let* ((n (or (when (numberp n)
                  n)
                0))
         (step (or (when (numberp step)
                     step)
                   1))
         (to (when (and (numberp to)
                        (or (< (- n to) 0 step)
                            (< step 0 (- n to))))
               to)))
    (labels ((test () (when to (<= to n)))
             (get () n)
             (step () (incf n step)))
      #'(lambda ()
          (when (test)
            (error 'no-next-element-error))
          (prog1
              (get)
            (step))))))

(defcursor make-number-cursor (n &key to (step 1))
  (with ((n (if (numberp n)
		n
		0))
	 (step (if (numberp step)
		   step
		   1))
	 (to (when (and (numberp to)
			(or (< (- n to) 0 step)
			    (< step 0 (- n to))))
	       to)))
     (define
         :test (when to '(<= to n))
       :get 'n
       :next '(incf n step))))
|#

;;;;; Database Related Code
;;;; From here on we refer to code from PLAIN-ODBC, therefore we load it at compile time.
(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:oos 'asdf:load-op 'plain-odbc-with-libs))

;;;;;; Database
(in-package #:plain-odbc)


(defun prepare-parameters (query parameter-list)
  (let ((hstmt (hstmt query)))
    (setf (parameters query)
	  (map 'vector
	       (let ((pos 0))
		 #'(lambda (param)
		     (multiple-value-bind (value type direction args)
			 (object-to-parameter-spec param)
		       (let ((parameter (create-parameter
                                          query pos
                                          type direction args)))
			 (bind-parameter hstmt pos parameter)
			 (unless (eq direction :out)
			   (set-parameter-value parameter value))
			 (incf pos)
			 parameter))))
	       parameter-list))))

(defun send-parameters (sql query connection)
  (let ((hstmt (hstmt query)))
    (when (= $SQL_NEED_DATA (%sql-exec-direct sql hstmt (henv connection)
                                              (hdbc connection)))
      (let ((parameters (slot-value query 'parameters))
            prev-pos)
        (with-temporary-allocations
            ((ptr (cffi:foreign-alloc :pointer)))
          (flet ((param-needs-data ()
                   (if (= $SQL_NEED_DATA (with-error-handling (:hstmt hstmt)
                                             (%sql-param-data hstmt ptr)))
                       (let ((pos (cffi:mem-ref (cffi:mem-ref ptr :pointer) :long)))
                         (if (eql prev-pos pos)
                             (error "Parameter ~A is not yet filled." pos)
                             (setq prev-pos pos)))
                       (error 'no-next-element-error :cursor hstmt))))
            (handler-case
                (loop (send-parameter-data (aref parameters (param-needs-data))
                                           hstmt))
              (no-next-element-error () nil))))))))

(defmethod cursor:make-cursor ((query odbc-query) &key eof row-storage)
  (with-slots (hstmt columns column-count) query
    (let ((row (or row-storage `(make-list ,column-count)))
          (eof-signal (or eof `(error 'no-next-element-error :cursor ,query))))
      (eval `(let ((query ,query))
               #'(lambda ()
                   (if (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
                       ,eof-signal
                       (map-into ,row #'get-column-value columns))))))))

(defmethod cursor:make-cursor ((sql string) &key eof row-storage
                               connection parameter-list)
  (let* ((query (make-query connection))
	 (eof-signal (or eof `(error 'no-next-element-error :cursor ,query)))
	 (row (or row-storage `(make-list ,(column-count query)))))
    (prepare-parameters query parameter-list)
    (send-parameters sql query connection)
    (eval `(let ((query ,query))
	     #'(lambda ()
		 (if (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
		     ,eof-signal
		     (map-into ,row #'get-column-value columns)))))))
(defmacro with-query ((&rest vars) (sql-string &rest parameter-list) connection &body body)
  (let ((conn (gensym "CONNECTION"))
        (query (gensym "QUERY"))
        (cursor (gensym "CURSOR"))
        (hstmt (gensym "HSTMT"))
        (bindings vars))
    `(let* ((,conn ,connection)
            (,query (make-query ,conn))
            (,hstmt (hstmt ,query)))
       (prepare-parameters ,query ,parameter-list)
       (send-parameters ,sql-string ,query ,conn)
       (flet ((,cursor ()
                (if (= (%sql-fetch ,hstmt) $SQL_NO_DATA_FOUND)
                    (error 'cursor:no-next-element-error :cursor ,query)
                    (mapcar #'get-column-value (slot-value ,query 'columns)))))
         (handler-case
             (loop (destructuring-bind ,bindings (,cursor)
                     ,@body))
           (cursor:no-next-element-error ()
             (free-query ,query)))))))


(in-package #:cl-user)
(defpackage #:bp2ntm-system
  (:use #:cl))
(defpackage #:bp2ntm
  (:use #:cl #:bp2ntm-system))
(defpackage #:bp2ntm-user
  (:use #:cl #:bp2ntm))
(in-package #:bp2ntm-system)
;;;;========================================================

;;;;; Connection

;;;;;; Connection Specifications
(defvar *connection-strings*
  '((:bp-pav    . (:Driver "{Microsoft ODBC for Oracle}" :Server "p014.swisslife.ch"   :Uid "ADAPTIV_READ" :Pwd "pada14p"))
    (:bp-uat    . (:Driver "{Microsoft ODBC for Oracle}" :Server "q014.swisslife.ch"   :Uid "ADAPTIV_READ" :Pwd "adaq14"))
    (:bp-uat-mu . (:Driver "{Microsoft ODBC for Oracle}" :Server "q014mu.swisslife.ch" :Uid "ADAPTIV_READ" :Pwd "adaq14mu"))
    (:a-pav     . (:driver "{SQL Native Client}" :server "nx3036" :uid "mgr" :pwd "pavsystem"))
    (:a-uat     . (:driver "{SQL Native Client}" :server "nx3183" :uid "mgr" :pwd "uatsystem"))
    (:a-st      . (:driver "{SQL Native Client}" :server "nx3073" :uid "mgr" :pwd "NX3073"))
    (:a-it      . (:driver "{SQL Native Client}" :server "nx3038" :uid "mgr" :pwd "itsystem"))))

(defun connection-string (server-alias)
  (cdr (assoc server-alias *connection-strings*)))

;;;;; Load BluePrint positions

(defmacro create-default-db-vars ((&rest names-docstrings))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcan #'(lambda (name-docstring)
                   (let* ((name (string (first name-docstring)))
                          (docstring (second name-docstring))
                          (var-symbol (intern (nstring-upcase (concatenate 'string "*default-" name "-db*"))))
                          (var-set-fn-sym (intern (nstring-upcase(concatenate 'string "set-" name "-default")))))
                     `((defvar ,var-symbol nil ,docstring)
                       (defun ,var-set-fn-sym (db)
                         (setq ,var-symbol db)))))
               names-docstrings)))
(create-default-db-vars ((blueprint "The BluePrint data base used by default.")
                         (adaptiv "The Adaptiv data base used by default.")
                         (mfiles "The reference data base for Adaptiv used by default.")
                         (executive "The Executive database used by default.")))
;(defvar +bp-db-class+ (find-class 'db::oracle))
;(defvar +adaptiv-db-class+ (find-class 'db::odbc))
#+nil(flet ((connect (cs)
         (connect (connection-spec cs)
                  :if-exists :old
                  :make-default nil
                  :database-type (connection-type cs)
                  :encoding :utf-8)))
  (defun connect-to-blueprint (db-alias)
    (change-class (connect (get-blueprint-db db-alias)) +bp-db-class+))
  (defun connect-to-adaptiv (alias user catalog)
    (change-class (connect (get-adaptiv-db alias user catalog)) +adaptiv-db-class+)))

#+nil0(connect '("asd" "mgr" "itsystem" :connection-string "ODBC;Driver={SQL Native Client};Server=nx3038;Database=ADAPTIV;Uid=mgr;Pwd=itsystem" :window-handle t) :if-exists :new :make-default t :database-type :odbc :encoding :utf-8)

;;;;;; Classes for holding BluePrint Positions

(in-package #:cl-user)
(defpackage #:blueprint
  (:nicknames #:bp))

(defclass bp::position ()
  ())

(defclass bp::instrument ()
  ())

#+nil
(define-instrument bond (principal-at-maturity)
  :add-used-fields (INSTRUMENTMATURITYDATE
                    ...)
    :add-slots ((maturity-date INSTRUMENTMATURITYDATE)))



