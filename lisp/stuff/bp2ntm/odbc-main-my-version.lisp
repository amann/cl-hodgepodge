;;; -*- Mode: Lisp -*-

;; plain-odbc, ODBC module for clisp
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :plain-odbc)

(defvar *open-connections* nil)


;; the main class, this a a wrapper around the connection
;; handle from odbc
;; an instance can only be created by connecting,
;; via connect, driver-connect or the special connect functions 
;; for oracle, sql-server or access
;; (make-instance 'odbc-connection ...) is useless
;; once a connection is closed it can not be reopened

(defclass odbc-connection ()
  (;; any reason to have more than one henv?
   (henv :initform (%new-environment-handle)
         :allocation :class :reader henv)
   (connection-string :initform (error "No connection string given.")
                      :initarg :connection-string :type string)
   (hdbc :initform nil :reader hdbc)
   (connected-p :initform nil)
   (server-name :reader server-name)
   (dbms-name :reader dbms-name)
   (user-name :reader user-name)
   ;; info returned from SQLGetInfo
   (info :initform (make-hash-table) :reader db-info))
  #+cormanlisp (:metaclass cl::class-slot-class))
(defmethod shared-initialize :after ((self odbc-connection) slot-names &key &allow-other-keys)
  (with-slots (henv connection-string hdbc connected-p
                    server-name dbms-name user-name info)
      self
    (%sql-driver-connect henv hdbc connection-string)
    (push self *open-connections*)
    (setq connected-p t
          server-name (get-odbc-info self $SQL_SERVER_NAME)
          dbms-name   (get-odbc-info self $SQL_DBMS_NAME)
          user-name   (get-odbc-info self $SQL_USER_NAME))
    (when (/= (get-odbc-info self $SQL_TXN_CAPABLE)
              $SQL_TC_NONE) ;; is txn capable
      (disable-autocommit hdbc))))
(defmethod initialize-instance :before ((self odbc-connection)
                                        &rest initargs
                                        &key &allow-other-keys)
  (setq hdbc (%new-db-connection-handle (if (slot-boundp self 'henv)
                                            (henv self)
                                            (%new-environment-handle)))))

(defun get-odbc-info (con info-type)
  (with-slots (hdbc info) con
    (or (gethash info-type info)
        (setf (gethash info-type info)
              (%sql-get-info hdbc info-type)))))

(defun driver-connect (connection-string)
  (make-instance 'odbc-connection
                 :connection-string connection-string)) 

(defun build-connection-string (list)
  (format nil "~{~A=~A~^;~}" list))

(defun connect-generic (&rest attrs)
  (driver-connect (build-connection-string attrs)))

(defun connect (dsn userid password)
  (connect-generic :dsn dsn
                   :uid userid
                   :pwd password))

(defmethod print-object ((connection odbc-connection) s)
  (format s "~A SERVER=~S DBMS=~S USER=~S>"
          (string-right-trim ">" (with-output-to-string (s)
                                   (call-next-method connection s)))
          (if (slot-boundp connection 'server-name) 
              (slot-value connection 'server-name)
              "")
          (if (slot-boundp connection 'dbms-name) 
              (slot-value connection 'dbms-name)
              "")
          (if (slot-boundp connection 'user-name) 
              (slot-value connection 'user-name)
              "")))
;; TODO: Try to find a better test
(defmethod connected-p ((connection odbc-connection))
  (slot-value connection 'connected-p))

;; before the connection is closed, it is rolled back.
;; odbc complains if a connection with an uncomitted transaction
;; is closed. Committing the transaction would be wrong.
(defun close-connection (con)
  (when (connected-p con)
    ;; if there is an active transaction and we disconnect then
    ;; odbc creates an error message
    ;; anyway, a transaction must be explicitly commited
    (rollback con)
    (%disconnect (hdbc con))
    (%free-connection (hdbc con))
    (SLOT-MAKUNBOUND con 'hdbc)
    (setf (slot-value con 'connected-p) nil)
    (setf *open-connections* (remove con *open-connections*))
    nil))
(defun reconnect (con)
  (reinitialize-instance con))

(defun trace-connection (con filename)
  (%start-connection-trace (hdbc con) filename)
  nil)

(defun untrace-connection (coN)
  (%stop-connection-trace (hdbc con))
  nil) 


(defun commit (con)
  (%commit (henv con) (hdbc con))
  nil)

(defun rollback (con)
  (%rollback (henv con) (hdbc con))
  nil)

;; this class is a wrapper for odbc statement handles.
;; it is visible to the user only as prepared statement
;; the only way to create a odbc-query is to create a 
;; prepared statement

;; fixme: what is the difference between statement and prepared-statement

(defclass odbc-query ()
  ((connection :initform (error "No connection given.")
               :initarg :connection :reader connection)
   (sql :initform (error "No SQL-statement given.")
        :initarg :sql)
   (active-p :initform t :reader :active-p)
   (hstmt :reader hstmt) ; = cursor??
   (columns :initform nil)
   (column-count :initform nil :accessor column-count)
   (parameters :reader parameters))
  (:default-initargs :parameters nil))

(defmethod initialize-instance ((self odbc-query) &rest initargs
                                       &key parameters &allow-other-keys)
  (prog1
      (call-next-method)
    (setf (slot-value self 'hstmt)
          (%new-statement-handle (hdbc (connection self)))
          (slot-value self 'parameters)
	  (map 'vector
	       (let ((pos 0))
		 #'(lambda (param)
		     (let ((parameter (multiple-value-call
                                          #'create-parameter
                                        self pos
                                        (check-parameter-spec param))))
                       (bind-parameter hstmt pos parameter) 
                       (incf pos)
                       parameter)))
	       parameters))))
;;;;----------------
(defclass direct-statement (odbc-query) ())
(defmethod initialize-instance ((self direct-statement) &rest initargs
                                &key parameters &allow-other-keys)
  (apply #'call-next-method
         self (list* :parameters (mapcar #'cdr parameters) initargs)))
(defmethod initialize-instance :after ((self direct-statement)
                                       &rest initargs
                                       &key connection sql parameters
                                       &allow-other-keys)
  (map nil #'(lambda (param value)
               (unless (eql (slot-value param 'direction) :out)
                 (set-parameter-value param value)))
       (parameters self)
       (mapcar #'car parameters))
  (let ((hstmt (hstmt query)))
    (when (= (%sql-exec-direct sql hstmt (henv connection) (hdbc connection))
             $SQL_NEED_DATA)
      (send-parameters self))))
(defmethod initialize-instance :around ((self direct-statement)
                                        &rest initargs
                                        &key &allow-other-keys)
  (unwind-protect
       (call-next-method)
    (free-query self)))
;;;;----------------
(defclass prepared-statement (odbc-query) ())
(defmethod initialize-instance ((self prepared-statement) &rest initargs
                                &key &allow-other-keys)
  (prog1
      (call-next-method)
    (%sql-prepare (hstmt self) (slot-value self 'sql))))

(defun make-prepared-statement (con sql &rest param-specs)
  (make-instance 'prepared-statement
                 :connection con
                 :sql sql
                 :parameters param-specs))



(defun send-parameters (query)
  (loop
     with last-pos = nil
     and hstmt = (hstmt query)
     and parameters = (parameters query)
     for (res pos) = (multiple-value-list
                      (sql-param-data-position hstmt))
     while (= res $SQL_NEED_DATA)
     when (eql pos last-pos)
       (error "Parameter ~A is not filled yet." pos)
     do (send-parameter-data (aref parameters (setq last-pos pos))
                             hstmt)))

(defmethod set-parameters ((query odbc-query) parameter-values)
  (multiple-value-bind (in-parameters length-in-parameters)
      (loop
         for parameter across (parameters query)
         unless (eql :out (slot-value parameter 'direction))
         collect parameter into list and count parameter into nbr
         finally (return (values list nbr)))
      (let* ((length-parameter-values (length parameter-values)))
        (assert (= length-in-parameters length-parameter-values)
                (list parameter-values)
                "We need ~D parameter values; You have given ~D."
                length-in-parameters length-parameter-values)
        (map nil #'set-parameter-value in-parameters parameter-values))))

(defmethod get-parameters ((query odbc-query))
  (loop
     for parameter across (parameters query)
     when (slot-value parameter 'direction)
     collect (get-parameter-value parameter)))



(defun exec-sql-statement (connection sql parameter-list)
  (let ((query (make-instance 'direct-statement
                              :connection connection
                              :sql sql
                              :parameters parameter-list)))
    (unwind-protect
	 (with-slots (hstmt) qury
           (values (result-rows-count hstmt)
                   (loop
                      for column-count = (result-columns-count hstmt)
                      until (zerop column-count)
                      do (bind-columns query column-count)
                      collect (list (fetch-query-results query)
                                    (column-names query))
                      do (unbind-columns query)
                      while (%sql-more-results hstmt))
                   (get-parameters query)))
      (free-query query))))

(defmethod set-params-and-exec ((query odbc-query) parameters)
  (set-parameters query parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect
      (let ((res (%sql-execute (hstmt query)))
            (last-pos nil))
        (if (= res $SQL_NEED_DATA)
            (send-parameters query))))))

;; this functions works only, since we store at value-ptr the position
;; of the parameter
(defun sql-param-data-position (hstmt)
  (with-temporary-allocations
      ((ptr (cffi:foreign-alloc :pointer)))
    (let ((res (with-error-handling (:hstmt hstmt)
                   (%sql-param-data hstmt ptr))))
      (values res
              (if (= res $SQL_NEED_DATA)
                  (cffi:mem-ref (cffi:mem-ref ptr :pointer) :long))))))

(defun get-result-set (hstmt)
  (let ((column-count (result-columns-count hstmt)))
    (when (zerop column-count)
      )))

(defmethod exec-prepared-query ((query prepared-statement) &rest parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
    ;;; fixme
        ;; if this query has already been used, we reuse the old
        ;; column binding. This breaks down if the schema has been changed
        ;; in the mean time, ex. the query is "select * from table1" and then
        ;; a column of table1 is dropped.
        (let ((no-of-columns (result-columns-count hstmt)))
          (when (zerop no-of-columns )
            (error "there is no result set"))
          (if (column-count query)
            (unless (= (column-count query) no-of-columns)
              (error "the number of columns has changed"))
            (bind-columns query no-of-columns)))
        (values (fetch-query-results query)
                (coerce (column-names query) 'list)))
    (%free-statement hstmt :close))))




(defun column-names (query)
  (map 'list #'(lambda (column)
		 (slot-value column 'column-name))
       (slot-value query 'columns)))
(defun bind-columns (query columncount)
  (if (zerop columncount)
      (error "Cannot bind columns, there is no result set.")
      (let ((hstmt (hstmt query)))
	(setf (slot-value query 'columns)
	      (loop with columns = (make-array `(,columncount))
		 for pos below columncount
		 do (setf (aref columns pos) (create-column hstmt pos))
		 finally (return columns))
	      (slot-value query 'column-count)
	      columncount))))
(defun unbind-columns (query)
  (with-slots (columns hstmt) query
    (setf columns
	  (map nil #'(lambda (column)
		       (when column
			 (with-slots (value-ptr ind-ptr) column
			   (when value-ptr
			     (cffi:foreign-free value-ptr))
			   (when ind-ptr
			     (cffi:foreign-free ind-ptr)))))
	       columns))
    (when hstmt
      (%free-statement hstmt :unbind))))
  
(defmethod free-query ((query odbc-query))
  (unbind-columns query)
  (%free-statement (hstmt query) :close)
  (%free-statement (hstmt query) :drop)
  (SLOT-MAKUNBOUND query 'hstmt))

(defmethod free-query ((query prepared-statement))
  (free-parameters query)
  (call-next-method))

(defun free-statement (x)
  (free-query x))

(defmethod fetch-query-results ((query odbc-query))
  (with-slots (hstmt columns) query
    (loop
       until (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
       collect (loop for col across columns
                  collect (get-column-value col)))))

(defmethod fetch-query-results ((query odbc-query))
  ;nil
  (with-slots (hstmt columns column-count)
      query
      (let ((res nil))
        (loop
          (when (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
            (return-from fetch-query-results (nreverse res)))
          (let ((row nil))
            (dotimes (i column-count)
              ;(get-column-value (aref columns i))
              (push (get-column-value (aref columns i)) row))
            (push (nreverse row) res)))))) 

;;; on hold
#+ignore
(defmethod fetch-row ((query odbc-query))
  (with-slots (hstmt columns column-count)
      query
    (if (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
      nil
      (let ((row nil))
        (dotimes (i column-count)
          (push (get-column-value (aref columns i)) row))
        (nreverse row)))))

;; convert a obj to a parameter specification
;; a parameter specification is a list of 
;; parameter-type on of :integer :string :clob .....
;; parameter direction :in or :out :in-out
;; further parameter for the parameter, ex. for a string this is the length
;;  
;; we accept normal objects i.e. non cons the parameter specification s derived
;; or a list -->the first element is the value for the parameter
;; the cdr is (keyword direction .....) -> (keyword direction ....)
;;            (keyword )    -> (keyword :in)
;;            ()            -> (:string :in)
;;  this function returns four values: 
;; the value, parameter-type direction args 
(defun object-to-parameter-spec (obj)
  (typecase obj
    (null (values nil :string :in '(1)))
    (cons (destructuring-bind (value &rest param-spec) obj
            (if param-spec)
            (multiple-value-call #'values value
                                 (check-parameter-spec param-spec))))
    (string (values obj :string :in (list (length obj))))
    (integer (values obj :integer :in nil))
    (float (values (coerce obj 'double-float) :double :in nil))
    (array (values obj  :binary :in (list (length obj))))
    (t (if (funcall *date-type-predicate* obj)
         (values obj :date :in)
         (error "Not able to deduce parameter specification for ~A."
                obj)))))
(defun check-parameter-spec (param)
  (destructuring-bind (type &optional (direction :in) &rest args)
      (etypecase param
        (list param)
        (symbol (list param)))
    (assert (keywordp type)
            (type)
            "Parameter type must be a keyword and not ~A."
            type)
    (let ((possible-values '(:in :out :in-out)))
      (assert (member direction possible-values)
              (direction)
              "The direction must be one of ~A and not ~A."
              possible-values direction))
    (values type direction args)))



(defun exec-sql* (connection sql parameter-list)
  (exec-sql-statement connection sql parameter-list))

(defun exec-sql (connection sql &rest parameter-list)
  (exec-sql* connection sql parameter-list))


(defun exec-query* (connection sql parameter-list)
  (multiple-value-bind (rows result-sets out-params)
      (exec-sql-statement connection sql parameter-list)
    (declare (ignore rows) (ignore out-params))
    (let ((res nil))
      (dolist (result-set result-sets)
        (push (first result-set) res)
        (push (second result-set) res))
      (values-list (nreverse res)))))

(defun exec-query (connection sql &rest parameter-list)
  (exec-query* connection sql parameter-list))


(defun exec-update* (connection sql parameter-list)
  (multiple-value-bind (rows result-sets out-params)
      (exec-sql-statement connection sql parameter-list)
    (declare (ignore result-sets out-params))
    rows))

(defun exec-update (connection sql &rest parameter-list)
  (exec-update* connection sql parameter-list))


(defun exec-command* (connection sql parameter-list)
  (multiple-value-bind (rows result-sets out-params)
      (exec-sql-statement connection sql parameter-list)
    (declare (ignore rows result-sets))
    (values-list out-params)))

(defun exec-command (connection sql &rest parameter-list)
  (exec-command* connection sql parameter-list))

(defmethod exec-prepared-update ((query prepared-statement) &rest parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
         (progn
           (set-params-and-exec query parameters)
           (let ((rowcount (result-rows-count hstmt)))
             (if (= rowcount -1) nil rowcount)))
      (%free-statement hstmt :close))))

(defmethod exec-prepared-command ((query prepared-statement) &rest parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
         (progn
           (set-params-and-exec query parameters)
           (get-parameters query))
      (%free-statement hstmt :close))))



;;;;------------------------------------------------------------------
;;;;;; Metadata Functions
(defun call-metadata-func (connection fun) 
  (let ((query (make-query connection)))
    (unwind-protect 
        (progn
          (funcall fun (hstmt query))
          (let ((column-count (result-columns-count (hstmt query))))
            (unless (zerop column-count)
              (bind-columns query column-count)
              (values (fetch-query-results query)
                      (coerce (column-names query)
                              'list)))))
      (free-query query))))
(defmacro define-metadata-func (name (&rest args) %sql-metadata-fun)
  `(defun ,name (connection ,@args)
     ,@(mapcar #'(lambda (a)
		   `(check-type ,a (or null string)))
	       args)
     (call-metadata-func connection
			 #'(lambda (hstmt)
			     (,%sql-metadata-fun hstmt ,@args)))))

(define-metadata-func get-primary-keys
    (catalog-name schema-name table-name)
  %sql-primary-keys)
(define-metadata-func get-tables
    (catalog-name schema-name table-name table-type)
  %sql-tables)
(define-metadata-func get-columns
    (catalog-name schema-name table-name column-name)
  %sql-columns)
(define-metadata-func get-foreign-keys
    (catalog-name1 schema-name1 table-name1
                   catalog-name2 schema-name2 table-name2)
  %sql-foreign-keys)
;;;;------------------------------------------------------------------


