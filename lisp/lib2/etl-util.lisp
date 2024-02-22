(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op 'plain-odbc-with-libs))
;;;;** The ETL Package
;;;;
;;;; This package offers some utilities to manage easily tables (recordsets)
;;;; whose data are loaded from an sql-table or a csv-file. Such tables are
;;;; given a project and a table name which are used to create their own
;;;; name space (package) containing the names of their columns as exported
;;;; symbols.

(in-package #:cl-user)
(defpackage #:etl
  (:export #:project
           #:table
           #:recordset
           #:resultset
           #:with-recordset))
(defpackage #:etl-system)

;;;;** Projects

(defclass etl::project ()
  ((etl::name :initform (error "No name given.") :type (or symbol string)
              :initarg :name :reader etl::name)
   (etl::package :reader etl::package)
   (etl::open-conns :initform (make-hash-table :test #'eq))
   (etl::conn-specs :initform (make-hash-table :test #'eq))
   (etl::tables :initform (make-hash-table :test #'eq))))

(let ((projects (make-hash-table :test #'eq)))
  (defmethod initialize-instance :after ((self etl::project) &key &allow-other-keys)
    (let ((name (setf (slot-value self 'etl::name)
                      (oam::to-keyword (string-upcase (slot-value self 'etl::name))))))
      (setf (slot-value self 'etl::package)
            (or (prog1(find-package name)
                  (warn "Problematic project name: A package with name ~A already exists." name))
                (make-package name))
            (gethash name projects) self)))
  (defun etl::find-project (project)
    (typecase project
      (etl::project (values project nil))
      (package (etl::find-project (package-name project)))
      (t (gethash (oam::to-keyword project) projects))))
  (defun etl::delete-project (project)
    (declare (ignore project))
    (error "Not implemented.")))

(defun etl::find-table (project table)
  (gethash (oam::to-keyword table) (slot-value (etl::find-project project) 'etl::tables)))

(defun etl-system::add-conn-spec (project db-alias conn-spec)
  (setf (gethash db-alias (slot-value project 'etl::conn-specs)) conn-spec))
(defun etl-system::add-table (project table)
  (setf (gethash (etl::name table) (slot-value project 'etl::tables)) table))

(defun etl-system::db-conn (project db-alias)
  (or (gethash db-alias (slot-value project 'etl::open-conns))
      (setf (gethash db-alias (slot-value project 'etl::open-conns))
            (apply #'plain-odbc::connect-generic
                   (gethash db-alias (slot-value project 'etl::conn-specs))))))

(defun etl::add-connection-spec (project db-alias conn-spec)
  (etl-system::add-conn-spec (etl::find-project project) db-alias conn-spec))

;;;;** Tables
;;;; A table is a list of rows, is given a name and associated to a project.

(defclass etl::table ()
  (etl::rows
   (etl::project :initform (error "No project given.")
                 :type etl::project :reader etl::project)
   (etl::name :initform (error "No name given.")
              :type (or symbol string) :initarg :name :reader etl::name))
  (:default-initargs :project (error "No project given.")))

(defmethod shared-initialize :before ((self etl::table) slot-names
                                      &key project &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value self 'etl::project) (or (etl::find-project project)
                                            (error "No such project: ~S." project))))
(defmethod shared-initialize :after ((self etl::table) slot-names
                                     &key &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (etl::name etl::project) self
    (setf etl::name (oam::to-keyword etl::name))
    (etl-system::add-table etl::project self)))

;;;;*** Recordset
;;;; A table with named columns and a package in which the
;;;; column names are interned as external symbols.
(defclass etl::recordset (etl::table)
  ((etl::package :initform nil :reader etl::package)
   (etl::column-names :documentation "List of symbols naming the columns of the recordset. The symbols are in the package PROJECT-NAME.RESULTSET-NAME.")))


;;TODO: When reinitialized or when finalized then unintern all related symbols
(defmethod shared-initialize :after ((self etl::recordset) slot-names
                                     &key &allow-other-keys)
  (declare (ignore slot-names))
  (let* ((table-name (etl::name self))
         (project (etl::project self))
         (macro-name (intern (string table-name) (etl::package project)))
         (package-name (format nil "~A.~A" (etl::name project) table-name))
         (package (or (etl::package self)
                      (prog1(find-package package-name)
                        (warn "Problematic table name: A package with name ~A already exists." package-name))
                      (make-package package-name)))
         (column-names (mapcar #'(lambda (col-name)
                                   (let ((symbol (intern col-name package)))
                                     (export symbol package)
                                     symbol))
                               (slot-value self 'etl::column-names)))
         (row-name (intern (string '@row@) package)))
    (setf (slot-value self 'etl::column-names) column-names
          (slot-value self 'etl::package) package)
    (etl-system::make-symbol-binder macro-name row-name column-names) ;; WARNING hard coded parameter @row@
    (export row-name package)
    (export macro-name (etl::package project))
    (set macro-name self)))

(defun etl-system::make-symbol-binder (name whole symbols)
  "Define a macro named name and which returns an anonymous function taking as many arguments as are symbols in the list SYMBOLS and which are bound in BODY in respective order to all those symbols; The symbol at place WHOLE is bound to the whole list of all arguments."
  (eval `(defmacro ,name (&body body)
           ,(format nil "Return an anonymous function taking ~A arguments which are bound in BODY in respective order to the symbols ~{~S~#[~; and ~:;, ~]~}; The symbol ~S is bound to the whole list of all arguments." (length symbols) symbols whole)
           `#'(lambda ,',symbols
                (let ((,',whole (list ,@',symbols)))
                  (declare (ignorable ,',whole))
                  ,@body)))))



(defmethod cursor::make-cursor ((object etl::table)
                                &key eof (step 1) key &allow-other-keys)
  (cursor::make-list-cursor (slot-value object 'etl::rows)
                            :step step :key key :eof eof))

(defmacro etl::with-recordset (recordset-name &body body)
  "recordset-name must be a symbol in the correct package; it is not evaluated."
  (let ((rsmap (gensym "RSMAP"))
        (next-row-fn (gensym "NEXT-ROW-FN")))
    `(let* ((,rsmap (,recordset-name ,@body))
            (,next-row-fn (cursor::make-cursor ,recordset-name)))
       (handler-case
           (loop (apply ,rsmap (funcall ,next-row-fn)))
         (cursor:no-next-element-error ())))))

;;;;**** ODBC Result Sets

(defclass etl::resultset (etl::recordset)
  ((etl::sql :initform (error "No SQL statement given.")
             :type string)
   (etl::connection :initform (error "No connection given.")))
  (:documentation "The SQL-statement should be given as format-string which can be completed with a where statement. The where statement will be added completely (i.e. as \"where ...\") either as a SQL-STMNT-COMPLETION argument of `etl::fill-table' or during initialization of the resultset instance; there the where statement is \"where 1=0\". If no other clauses like sort or group are used in the SQL-statement template, no control directives need to be added, if at the opposite, sort or group or other clauses are present in the template, a placeholder ~A for the where statement is needed; either with a leading space or with a ~% or ~& directive: i.e. ~&~A is recomended.")
  (:default-initargs
      :db-alias (error "No db-alias given.")
    :sql (error "No SQL statement given.")))

(defun etl-system::format-fill (stream format-spec fill-string &rest format-args)
  (do (done) (done done)
    (handler-case
        (setq done (or (apply #'format stream format-spec format-args) t))
      (error (e)
        (break "~A" e)
        (setq format-args (append format-args (list fill-string)))))))
(defmethod shared-initialize :before ((self etl::resultset) slot-names &rest initargs
                                      &key project db-alias sql &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let* ((connection (etl-system::db-conn (etl::find-project project) db-alias))
         (sql (concatenate 'string sql "~@{~^~&~A~}"))
         (resultset (multiple-value-list
                     (plain-odbc::exec-query
                      connection (etl-system::format-fill nil sql "" "where 1=0") nil))))
    (setf (slot-value self 'etl::sql)          sql
          (slot-value self 'etl::connection)   connection
          (slot-value self 'etl::column-names) (second resultset))))


(defmethod etl::fill-table ((table etl::resultset)
                            &optional sql-stmnt-completions parameters)
  (setf (slot-value table 'etl::rows)
        (first (multiple-value-list
                (plain-odbc::exec-query
                 (slot-value table 'etl::connection)
                 (apply #'etl-system::format-fill
                  nil (slot-value table 'etl::sql)
                  "" sql-stmnt-completions) parameters))))
  nil)


;;;;**** CSV Files
;;;; A CSV-table is a recordset filled from a CSV file whose
;;;; first row specifies the column names.

(defclass etl::csv-table (etl::recordset)
  ((etl::col-separator :initform #\, :initarg :col-separator)))

(defmethod shared-initialize :before ((self etl::csv-table) slot-names &rest initargs
                                      &key stream col-separator &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let ((csv-table (etl-system::read-csv-stream stream col-separator)))
    (setf (slot-value self 'etl::rows) (rest csv-table)
          (slot-value self 'etl::column-names) (first csv-table))))

;;;; TODO: - check the number of columns on each row: must be equal to
;;;;         the number of column names of the first row in the file. 
(defun etl-system::read-csv-stream (input-stream &optional (col-separator #\~))
  (assert (streamp input-stream) () "The value of INPUT-STREAM is ~S which is not a stream." input-stream)
  (assert (input-stream-p input-stream) () "INPUT-STREAM ~S is not an input stream." input-stream)
  (loop
     collect (handler-case
                 (etl-system::explode (read-line input-stream) col-separator)
               (end-of-file ()
                 (return result)))
     into result))
(defun etl-system::explode (string &optional (delimiter #\~))
  (labels ((explode (res string)
             (let ((pos (position delimiter string)))
               (if (null pos)
                   (cons string res)
                   (explode (cons (subseq string 0 pos) res)
                            (subseq string (1+ pos)))))))
    (nreverse (explode nil string))))


