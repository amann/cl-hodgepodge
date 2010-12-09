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
  ((package :reader etl::package)
   (tables :initform nil)))

(let ((datapools (make-hash-table :test #'eq)))
  (defmethod shared-initialize :after ((self etl::datapool) slot-names &key name
                                         &allow-other-keys)
    (declare (ignore slot-names))
    (let* ((package (make-package name)))
      (setf (slot-value self 'package) package
            (gethash package datapools) self)))
  (defmethod reinitialize-instance :before ((self etl::datapool) &key
                                            &allow-other-keys)
    (etl::delete-datapool self))
  (defun find-datapool (datapool)
    (etypecase datapool
      (etl::datapool
       (setf (gethash (etl::package datapool) datapools) datapool))
      (package (nth-value 0(gethash datapool datapools)))
      ((or symbol string) (find-datapool (find-package datapool)))))
  (defun delete-datapool (datapool)
    (with-slots (package tables) datapool
      (remhash package datapools)
      (map nil #'delete-table tables)
      (delete-package package))
    (slot-makunbound datapool 'package)))



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
  (rows
   (datapool :initform (error "No datapool given.")
             :type etl::datapool :reader etl::datapool)
   (name :initform (error "No name given.")
         :type symbol :initarg :name :reader etl::name)
   (package :reader etl::package)
   (column-names :documentation "List of symbols naming the columns of the table. The symbols are in the package DATABASE-NAME/TABLE-NAME.")))


(defmethod shared-initialize :before ((self etl::table) slot-names
                                      &key name datapool &allow-other-keys)
  (declare (ignore slot-names))
  (let* ((datapool (setf (slot-value self 'datapool)
                         (or (find-datapool datapool)
                             (error "No such datapool: ~S." datapool))))
         (db-package (etl::package datapool))
         (name (if name
                   (export (intern (string name) db-package) db-package)
                   (error "No name given."))))
    (setf (slot-value self 'package)
          (make-package (format nil "~A/~A"
                                (package-name db-package) name)
                        :use nil))))
(defmethod shared-initialize :after ((self etl::table) slot-names
                                     &key column-names &allow-other-keys)
  (declare (ignore slot-names))
  (pushnew self (slot-value (etl::datapool self) 'tables))
  (let* ((package (etl::package self))
         (column-names (mapcar #'(lambda (col-name)
                                   (intern col-name package))
                               column-names)))
    (export column-names package)
    (setf (slot-value self 'column-names) column-names)
    (let ((name (etl::name self))
          (row-name (intern etl::*row-name* package)))
      (export row-name package)
      (etl::make-symbol-binder name row-name column-names)
      (set name self))))


(defun delete-table (table)
  (remove table (slot-value (etl::datapool table) 'tables)
          :test #'eq)
  (let ((package (etl::package table)))
    (unintern (etl::name table) package)
    (delete-package package))
  (slot-makunbound table 'package)
  (slot-makunbound table 'column-names))

(defmethod reinitialize-instance :before ((self etl::table)
                                          &key &allow-other-keys)
  (delete-table self))


(defgeneric etl::nth-row (object output-type))

(defmethod cursor::make-cursor (output-type (object etl::table)
                                &key eof (step 1) key &allow-other-keys)
  (cursor::make-cursor (slot-value object 'rows) output-type
                       :step step :key key :eof eof))

(defmacro etl::do-table-rows (table-name &body body)
  "Apply each row of the table TABLE-NAME to BODY where the symbols naming the columns of the table are bound to the corresponding value of the row. TABLE-NAME must be a symbol in the correct package; it is not evaluated."
  (let ((rsmap (gensym "RSMAP"))
        (next-row-fn (gensym "NEXT-ROW-FN")))
    `(let* ((,rsmap (,table-name ,@body))
            (,next-row-fn (cursor::make-cursor (find-class 'list)
                                               ,table-name)))
       (handler-case
           (loop (apply ,rsmap (funcall ,next-row-fn)))
         (cursor:no-next-element-error ())))))



#+(or)
(let ((package (find-package #:oam)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))