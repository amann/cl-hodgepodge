;;;; -*- outline-regexp:";;;;;+ +" -*-
;;;; $Id: bp2ntm.lisp,v 1.2 2010/05/17 16:01:44 amao Exp $
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
           #:make-list-cursor
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

(defun make-list-cursor (list &key (step 1) key eof)
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(<= (length list) ,step))
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
                           (prog1
                               ,get
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

;;;;; Database Related Code
;;;; From here on we refer to code from PLAIN-ODBC, therefore we load it at compile time.
(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:oos 'asdf:load-op 'plain-odbc-with-libs))


;;;;;; Database
(in-package #:cl-user)
(defpackage #:cldbc.database
  (:nicknames #:db #:database))
(defmacro with-query ((&rest vars)
                      (sql-string &rest parameter-list)
                      (&rest result-forms)
                      connection &body body)
  (let ((cursor (gensym "CURSOR"))
        (row-count (gensym "ROW-COUNT"))
        (results (gensym "RESULTS"))
        (parameters (gensym "PARAMETERS"))
        (bindings vars))
    `(multiple-value-bind (,row-count ,results ,parameters)
         (plain-odbc::exec-sql-statement ,connection ,sql-string ,parameter-list)
       (declare (ignore ,row-count ,parameters))
       (let* ((,cursor (cursor:make-list-cursor ,results)))
         (handler-case
             (loop (destructuring-bind ,bindings (funcall ,cursor)
                     ,@body))
           (cursor:no-next-element-error ()
             ,@result-forms))))))



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

;;;;;; Connection Strings

(defvar *connection-strings*
  '((:bp-pav    . (:Driver "{Microsoft ODBC for Oracle}"
                   :Server "p014.swisslife.ch"
                   :Uid "ADAPTIV_READ"
                   :Pwd "pada14p"))
    (:bp-uat    . (:Driver "{Microsoft ODBC for Oracle}"
                   :Server "q014.swisslife.ch"
                   :Uid "ADAPTIV_READ"
                   :Pwd "adaq14"))
    (:bp-uat-mu . (:Driver "{Microsoft ODBC for Oracle}"
                   :Server "q014mu.swisslife.ch"
                   :Uid "ADAPTIV_READ"
                   :Pwd "adaq14mu"))
    (:a-pav     . (:driver "{SQL Native Client}"
                   :server "nx3036"
                   :uid "mgr"
                   :pwd "pavsystem"))
    (:a-uat     . (:driver "{SQL Native Client}"
                   :server "nx3183"
                   :uid "mgr"
                   :pwd "uatsystem"))
    (:a-st      . (:driver "{SQL Native Client}"
                   :server "nx3073"
                   :uid "mgr"
                   :pwd "NX3073"))
    (:a-it      . (:driver "{SQL Native Client}"
                   :server "nx3038"
                   :uid "mgr"
                   :pwd "itsystem"))))

(defun connection-string (server-alias)
  (cdr (assoc server-alias *connection-strings*)))

;;;;; Default Database Connections

(defmacro create-default-db-vars (&rest names-docstrings)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcan #'(lambda (name-docstring)
                   (let* ((name (string (first name-docstring)))
                          (docstring (second name-docstring))
                          (var-symbol (intern (nstring-upcase
                                               (concatenate 'string
                                                            "*default-" name "-db*"))))
                          (var-set-fn-sym (intern (nstring-upcase
                                                   (concatenate 'string
                                                                "set-" name
                                                                "-default")))))
                     `((defvar ,var-symbol nil ,docstring)
                       (defun ,var-set-fn-sym (db)
                         (setq ,var-symbol db)))))
               names-docstrings)))

(create-default-db-vars
 (blueprint "The BluePrint data base used by default.")
 (adaptiv "The Adaptiv data base used by default.")
 (mfiles "The reference data base for Adaptiv used by default.")
 (executive "The Executive database used by default."))


;;;;; Load BluePrint Positions
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





