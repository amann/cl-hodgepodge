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

(defmethod slot-unbound (class (instance etl::datapool) (slot-name (eql 'package)))
  nil)
(defmethod slot-unbound (class (instance etl:datapool) (slot-name (eql 'tables)))
  nil)
(let ((datapools (make-hash-table :test #'eq)))
  (defmethod shared-initialize :after ((self etl::datapool) slot-names
                                       &key name &allow-other-keys)
    (declare (ignore slot-names))
    (let* ((package (make-package name)))
      (setf (slot-value self 'package) package
            (gethash package datapools) self)))
  (defun etl::find-datapool (datapool)
    (etypecase datapool
      (etl::datapool
       (setf (gethash (etl::package datapool) datapools) datapool))
      (package (nth-value 0(gethash datapool datapools)))
      ((or symbol string) (etl::find-datapool (find-package datapool)))))

  (defun etl::delete-datapool (datapool)
    (with-slots (package tables) datapool
      (map nil #'delete-table tables)
      (when (packagep package)
        (remhash package datapools)
        (delete-package package)))
    (slot-makunbound datapool 'package))
  (defmethod reinitialize-instance :before ((self etl::datapool) &key
                                            &allow-other-keys)
    (etl::delete-datapool self)))



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
  ((rows :initform (error "No data given.") :initarg :rows)
   (datapool :initform (error "No datapool given.")
             :type etl::datapool :reader etl::datapool)
   (name :initform (error "No name given!")
         :type symbol :reader etl::name)
   (row-name :type symbol :reader etl::row-name)
   (package :type package :reader etl::package)
   (column-names :type list :reader etl::column-names
    :documentation "List of symbols naming the columns of the table. The symbols are in the package DATAPOOL-NAME/TABLE-NAME.")))

;;;; TODO: Provide restarts in order to change the names if a conflict appears with the package name.
(defun ensure-table-package (datapool-name table-name)
  (restart-case
      (values (make-package (format nil "~A/~A"
                                    datapool-name table-name)
                            :use nil)
              datapool-name table-name)
    (use-new-table-name (new-table-name)
      :report "Use new table name."
      :interactive (lambda ()
                     (format t "Enter a new value: ")
                     (multiple-value-list (eval (read))))
      (ensure-table-package datapool-name new-table-name))
    (use-new-datapool-name (new-datapool-name)
      :report "Use new table name."
      :interactive (lambda ()
                     (format t "Enter a new value: ")
                     (multiple-value-list (eval (read))))
      (ensure-table-package datapool-name new-table-name))))
(defmethod shared-initialize :before ((self etl::table) slot-names
                                      &key name datapool &allow-other-keys)
  (declare (ignore slot-names))
  (let* ((datapool (setf (slot-value self 'datapool)
                         (or (etl::find-datapool datapool)
                             (error "No such datapool: ~S." datapool))))
         (db-package (etl::package datapool))
         (name (if name
                   (let ((name (intern (string name) db-package)))
                     (export name db-package)
                     (setf (slot-value self 'name) name))
                   (error "No name given."))))
    ( (slot-value self 'package)
          )))
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
      (setf (slot-value self 'row-name) row-name)
      (etl::make-symbol-binder name row-name column-names)
      (set name self))))

(defun etl::delete-table (table)
  (unless (slot-unboundp table 'datapool)
    (with-slots (tables) (etl:datapool table)
      (setf tables (delete table tables :test #'eq))))
  (let ((package (etl::package table)))
    (unintern (etl::name table) package)
    (delete-package package))
  (map nil #'(lambda (slot) (slot-makunbound table (ccl:slot-definition-name slot)))
       (ccl:class-slots (class-of table))))

(defmethod reinitialize-instance :before ((self etl::table)
                                          &key &allow-other-keys)
  (etl::delete-table self))


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
    ((get-map (hash-table columns)
       (if columns
           (handler-case
               (get-map (gethash hash-table (first columns)) (rest 
                                                              columns))
             (type-error ()))
           hash-table))
     (add-map (hash-table column value rest-columns)
       (if rest-columns
           (destructuring-bind (col test) (first rest-columns)
             (add-map (or (gethash (funcall column value) hash-table)
                          (setf (gethash (funcall column value) hash-table)
                                (make-hash-table :test test)))
                      col value (rest rest-columns)))
           (push value (gethash column hash-table)))))
  (defmacro etl::define-table-index (name (&rest columns) table)
    "Define an index on the columns given in COLUMNS of table TABLE."
    (let* ((columns (mapcar #'(lambda (col)
                                (etypecase col
                                  (symbol (list col #'eql))
                                  (cons col)))
                            columns))
           (args (mapcar #'car columns)))
      `(let* ((table ,table)
              (row-name (etl::row-name table))
              (hash-table (make-hash-table :test ,(cadar columns))))
         (defun ,name (,@args)
           (funcall ,#'get-map hash-table (list ,@args)))
         (etl::do-table-rows ,table
           (funcall ,#'add-map hash-table ,(caar columns) row-name ',(rest columns)))))))


#+ (or)
(labels
    ((get-map (hash-table columns)
       (handler-case
           (get-map (gethash hash-table (first columns)) (rest columns))
         (type-error ())))
     (add-map (hash-table columns value test)
       (if (= 1 (length columns))
           (push value (gethash (first columns) hash-table))
           (add-map (let ((col (first columns)))
                      (or (gethash col hash-table)
                          (setf (gethash col hash-table)
                                (make-hash-table :test test))))
                    (rest columns) test))))
  (defmacro define-table-index (name (&rest columns) table)
    "Define an index on the columns given in COLUMNS of table TABLE."
    (let* ((tbl (gensym "TABLE")))
      `(let ((,tbl ,table)
             (hash-table (make-hash-table))) 
         (defun ,name (,@(mapcar #'(lambda (col)
                                      (etypecase col
                                        (symbol col)
                                        (cons (first col))))
                                 columns))
           (funcall #'get-map ))))))




(defgeneric etl::to-list (object &key &allow-other-keys))
(defmethod etl::to-list ((object list) &key key &allow-other-keys)
  (if key
      (call-next-method)
      object))
(defmethod etl::to-list ((object sequence) &key key &allow-other-keys)
  (map 'list (or key #'identity) object))

(defgeneric etl::map-from-to (from-domain to-domain value))

(let ((package (find-package '#:etl)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))