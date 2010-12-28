;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;$Id: oam-etl.lisp,v 1.1 2010/11/23 10:46:44 amao Exp $

;;;;* The ETL Package
;;;;
;;;; This package offers some utilities to manage easily tables (recordsets)
;;;; whose data are loaded from an sql-table or a csv-file. Such tables are
;;;; given a database and a table name which are used to create their own
;;;; name space (package) containing the names of their columns as exported
;;;; symbols.

(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.oam-etl
  (:use)
  (:nicknames #:etl))
(defpackage #:ch.amann-wolowyk.oam-etl-system
  (:use #:cl))
(in-package #:ch.amann-wolowyk.oam-etl-system)

;;;;** Datapools
;;;; Datapools are where You can fetch data but not write.
;;;; A datapool is an instance of the class ETL::DATABASE. It is labeled
;;;; by a name and creates a package (namespace) of this name. Further
;;;; it holds a collection of named readonly tables. For each of these tables an
;;;; external symbol of same name is interned into the datapool's package.
;;;; Furthermore the tables create their own packages in which are interned
;;;; the (external) symbols naming each row of the table.
;;;; The name of those latter packages is composed by the name of the
;;;; datapool and the name of the table with a slash inbetween -- datapool-
;;;; name/table-name -- hence none of the original names should contain a
;;;; slash.
(defclass etl::datapool ()
  ((package :type package :reader etl::package)
   (tables :initform nil :reader etl::tables)))

(defmethod slot-unbound ((class t) (instance etl::datapool) (slot-name (eql 'package)))
  nil)
(defmethod slot-unbound ((class t) (instance etl::datapool) (slot-name (eql 'tables)))
  nil)

(defun etl::datapool-valid-p (datapool)
  (and (slot-boundp datapool 'package)
       (packagep (etl::package datapool))
       (slot-boundp datapool 'tables)))

(defgeneric etl::emptyp (object)
  (:documentation "Return T if object is valid but contains no relevant data, and NIL else."))
(defmethod etl::emptyp ((object etl::datapool))
  (and (etl::datapool-valid-p object)
       (not (slot-value object 'tables))))

(let ((datapools (if (fboundp '%get-datapools)
                       (%get-datapools)
                       (make-hash-table :test #'eq))))
    (defun %get-datapools ()
      datapools)
    (defun %get-datapools-alist (&aux result)
      (maphash #'(lambda (k v)
                   (push (cons k v) result))
               datapools)
      result)
    (defun %add-datapool (datapool)
      (setf (gethash (etl::package datapool) datapools) datapool))
    (defun %get-datapool (package)
      (gethash package datapools))
    (defun %rem-datapool (package)
      (remhash package datapools)))

(defmethod initialize-instance :after ((self etl::datapool) &rest initargs
                                       &key name &allow-other-keys)
  (declare (ignore initargs))
  (let* ((package (make-package name)))
    (setf (slot-value self 'package) package)
    (%add-datapool self)))

(defmethod reinitialize-instance :before ((self etl::datapool)
                                          &key &allow-other-keys)
  (etl::delete-datapool self))

(defun etl::find-datapool (datapool-designator)
  "Return the datapool associated to the package or package name DATAPOOL-DESIGNATOR as first value and the associated package name as second value. If DATAPOOL-DESIGNATOR is a datapool instance, it also checks the validity of the datapool and registers it if necessary. Return NIL else."
  (etypecase datapool-designator
    (null nil)
    (etl::datapool
     (when (etl::datapool-valid-p datapool-designator)
       (%add-datapool datapool-designator)
       (values datapool-designator
               (package-name (etl::package datapool-designator)))))
    (package (multiple-value-bind (datapool foundp)
                 (%get-datapool datapool-designator)
               (when foundp (etl::find-datapool datapool))))
    ((or symbol string) (etl::find-datapool (find-package datapool-designator)))))

(defun etl::delete-datapool (datapool)
  "Delete all tables of DATAPOOL as well as its associated package and invalidates DATAPOOL."
  (with-slots (package tables) datapool
    (map nil #'etl::delete-table tables)
    (when (packagep package)
      (%rem-datapool package)
      (delete-package package)))
  (slot-makunbound datapool 'package)
  (values))

(defun etl::list-all-datapools ()
  "Return an alist (associated-package . datapool) of all registered datapools."
  (%get-datapools-alist))


;;;;;


(defun etl::make-symbol-binder (name whole symbols)
  "Define a macro named NAME and which returns an anonymous function taking as many arguments as are symbols in the list SYMBOLS and which are bound in BODY of the macro in respective order to all those symbols; The symbol at place WHOLE is bound to the whole list of all arguments."
  (eval `(defmacro ,name (&body body)
           ,(format nil "Return an anonymous function taking ~A argument~:*~Ps which are bound in BODY in respective order to the symbols ~{~S~#[~; and ~:;, ~]~}; The symbol ~S is bound to the whole list of all arguments."
                    (length symbols) symbols whole)
           `#'(lambda ,',symbols
                (declare (ignorable ,@',symbols))
                (let ((,',whole (list ,@',symbols)))
                  (declare (ignorable ,',whole))
                  ,@body)))))

;;;;** Tables
;;;; A table is a list of rows, is given a name and associated to a datapool.
(defvar etl::*row-name* "@ROW@")

(defclass etl::table ()
  ((datapool :type etl::datapool :reader etl::datapool)
   (name :type symbol :reader etl::name)
   (package :type package :reader etl::package)
   (rows :initform (error "No data given.") :initarg :rows)
   (row-name :type symbol :reader etl::row-name)
   (column-names :type list :reader etl::column-names
    :documentation "List of symbols naming the columns of the table. The symbols are in the package DATAPOOL-NAME/TABLE-NAME.")))




(defun ensure-table-package (datapool table-name)
  (multiple-value-bind (datapool datapool-name)
      (do ()
          ((etl::find-datapool datapool)
           (etl::find-datapool datapool))
        (restart-case
            (error "No such datapool: ~S." datapool)
          (use-new-datapool-name (new-datapool-name)
            :report (lambda (s) (format s "Use new datapool name instead of ~S." datapool))
            :interactive (lambda ()
                           (format t "Enter a new value: ")
                           (multiple-value-list (eval (read))))
            (setq datapool new-datapool-name))
          (make-new-datapool ()
            :report (lambda (s) (format s "Make new datapool with name ~S." datapool))
            (setq datapool (make-instance 'etl::datapool :name datapool)))))
    (restart-case
        (values (handler-case
                    (make-package (format nil "~A/~A" datapool-name table-name)
                                  :use nil)
                  (simple-error (c)
                    (error c)))
                datapool table-name)
      (use-new-table-name (new-table-name)
        :report (lambda (s) (format s "Use new table name instead of ~S." table-name))
        :interactive (lambda ()
                       (format t "Enter a new value: ")
                       (multiple-value-list (eval (read))))
        (ensure-table-package datapool new-table-name))
      (use-new-datapool-name (new-datapool-name)
        :report (lambda (s) (format s "Use new datapool name instead of ~S." datapool-name))
        :interactive (lambda ()
                       (format t "Enter a new value: ")
                       (multiple-value-list (eval (read))))
        (ensure-table-package new-datapool-name table-name)))))

(defmethod initialize-instance :before ((self etl::table) &rest initargs
                                      &key name datapool &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (table-package datapool table-name)
      (ensure-table-package datapool name)
    (let* ((db-package (etl::package datapool))
           (name (intern (string table-name) db-package)))
      (export name db-package)
      (setf (slot-value self 'datapool) datapool
            (slot-value self 'name) name
            (slot-value self 'package) table-package)
      (set name self)
      (eval `(declaim (special ,name))))
    (pushnew self (slot-value datapool 'tables))))

(defmethod shared-initialize :before ((self etl::table) slot-names
                                     &key column-names (row-name etl::*row-name*) &allow-other-keys)
  (declare (ignore slot-names))
  (let* ((package (etl::package self))
         (column-names (mapcar #'(lambda (col-name)
                                   (intern col-name package))
                               column-names)))
    (when column-names
      (export column-names package)
      (setf (slot-value self 'column-names) column-names))
    (let ((name (etl::name self)))
      (unless (slot-boundp self 'row-name)
        (let ((row-name (intern row-name package)))
          (export row-name package)
          (setf (slot-value self 'row-name) row-name)))
      (etl::make-symbol-binder name (etl::row-name self) (etl::column-names self)))))

(defun etl::empty-table (table)
  (setf (slot-value table 'rows) nil
        (slot-value table 'column-names) nil)
  (slot-makunbound table 'row-name)
  (let ((*package* (etl::package table)))
    (do-external-symbols (s)
      (unintern s))))

(defun etl::delete-table (table)
  (when (slot-boundp table 'datapool)
    (with-slots (tables) (etl::datapool table)
      (setf tables (delete table tables :test #'eq))))
  (let ((package (etl::package table)))
    (unintern (etl::name table) package)
    (delete-package package))
  (map nil #'(lambda (slot)
               (slot-makunbound table
                                #+ccl (ccl:slot-definition-name slot)
                                #+sbcl (sb-mop:slot-definition-name slot)))
       #+ccl (ccl:class-slots (class-of table))
       #+sbcl (sb-mop:class-slots (class-of table))))

(defmethod reinitialize-instance :before ((self etl::table)
                                          &key &allow-other-keys)
  (etl::empty-table self))


(defgeneric etl::nth-row (index object))

(defmethod cursor::make-cursor ((object etl::table)
                                &key eof (step 1) key
                                &allow-other-keys)
  (cursor:make-cursor (slot-value object 'rows)
                       :step step :key key :eof eof))

(defmacro etl::do-table-rows (table-name &body body)
  "Apply each row of the table TABLE-NAME to BODY where the symbols naming the columns of the table are bound to the corresponding value of the row. TABLE-NAME must be a symbol in the correct package; it is not evaluated."
  (let ((rsmap (gensym "RSMAP"))
        (next-row-fn (gensym "NEXT-ROW-FN")))
    `(let* ((,rsmap (,table-name ,@body))
            (,next-row-fn (cursor:make-cursor ,table-name)))
       (handler-case
           (loop (apply ,rsmap (etl::to-list (funcall ,next-row-fn))))
         (cursor:no-next-element-error ())))))

(labels
    ((get-map (hash-table keys)
       (if keys
           (handler-case
               (get-map (gethash (first keys) hash-table) (rest keys))
             (type-error ()))
           hash-table))
     (add-map (hash-table cur-key rest-keys-tests value)
       (if rest-keys-tests
           (destructuring-bind (key test) (first rest-keys-tests)
             (add-map (or (gethash cur-key hash-table)
                          (setf (gethash cur-key hash-table)
                                (make-hash-table :test test)))
                      key (rest rest-keys-tests) value))
           (push value (gethash cur-key hash-table)))))
  (defmacro etl::define-table-index (name (&rest columns) table)
    "Return a function NAME which defines an index on the columns given in COLUMNS of table TABLE."
    (let* ((colnames-tests (mapcar #'(lambda (col)
                                       (etypecase col
                                         (symbol (list col #'eql))
                                         (cons col)))
                                   columns))
           (args (mapcar #'car colnames-tests))
           (table (etypecase table
                    (symbol (symbol-value table))
                    (etl::table table)))
           (table-name (etl::name table))
           (datapool (etl::datapool table))
           (row-name (etl::row-name (find table (etl::tables datapool)))))
      `(let* ((hash-table (make-hash-table :test ,(cadar colnames-tests))))
         (defun ,name (,@args)
           ,(format nil "Index of table ~S on the columns ~{~A~#[~; and ~:;, ~]~} returning a list of the rows which correspond to those columns." table-name args)
           (funcall ,#'get-map hash-table (list ,@args)))
         (export ',name (symbol-package ',name))
         (etl::do-table-rows ,table-name
           (funcall ,#'add-map hash-table ,(caar colnames-tests)
                    (list ,@(mapcar #'(lambda (x)
                                        `(list ,(car x) ,(cadr x)))
                                    (rest colnames-tests)))
                    ,row-name))))))


(defgeneric etl::to-list (object &key &allow-other-keys))
(defmethod etl::to-list ((object sequence) &key key &allow-other-keys)
  (map 'list (or key #'identity) object))
(defmethod etl::to-list ((object list) &key key &allow-other-keys)
  (if key
      (call-next-method)
      object))

(defgeneric etl::map-from-to (from-domain to-domain value)
  (:documentation "Return an entity"))

(eval-when (:load-toplevel :execute)
  (let ((package (find-package '#:etl)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (export symbol package)))))