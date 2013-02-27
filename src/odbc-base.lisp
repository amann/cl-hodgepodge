;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")



;;;;* Handles

(defclass sql-handle-metaclass (standard-class)
  ())

(defmethod make-instance :around ((class sql-handle-metaclass)
                                  &key sql-handle)
  (let ((instance (call-next-method)))
    (values instance (sql-diagnostics sql-handle))))
(defmethod c2mop:validate-superclass ((class sql-handle-metaclass)
                                      (superclass standard-class))
  t)
(defclass sql-handle ()
  ((pointer :reader sql-handle-pointer)))
(defclass sql-environment (sql-handle) ())
(defclass mixin-sql-environment ()
  ((environment :initform (error "No environment handle given.")
                :initarg :environment
                :type sql-environment
                :reader sql-environment)))
(defclass mixin-sql-connection ()
  ((connection :initform (error "No connection handle given.")
               :initarg :connection
               :type sql-connection
               :reader sql-connection)))
(defclass sql-connection (sql-handle mixin-sql-environment)
  ())
(defclass sql-statement (sql-handle mixin-sql-connection)
  ())
(defclass sql-description (sql-handle mixin-sql-connection)
  ())

(defmethod initialize-instance :after ((self sql-handle)
                                       &key &allow-other-keys)
  "Register the finalizer freeing the pointer."
  (let ((pointer (slot-value self 'pointer)))
    (tg:finalize self (lambda () (cffi-sys:foreign-free pointer)))))

(defmethod initialize-instance :before ((self sql-handle)
                                        &key &allow-other-keys)
  "Allocate the handle pointer."
  )

(defgeneric sql-handle-type (sql-handle)
  (:documentation "Return the code designing the handler type.")
  (:method (sql-handle)
    (error 'type-error
           :expected-type 'sql-handle
           :datum sql-handle))
  (:method ((sql-handle symbol))
    (sql-handle-type (find-class symbol)))
  (:method ((sql-handle sql-handle))
    (sql-handle-type (class-of sql-handle)))
  (:method ((sql-handle (eql (find-class 'sql-environment))))
    #.SQL_HANDLE_ENV)
  (:method ((sql-handle (eql (find-class 'sql-connection))))
    #.SQL_HANDLE_DBC)
  (:method ((sql-handle (eql (find-class 'sql-statement))))
    #.SQL_HANDLE_STMT)
  (:method ((sql-handle (eql (find-class 'sql-description))))
    #.SQL_HANDLE_DESC))


;;;;* Diagnostics

;;;;** Diagnostics Headers and Records
(defstruct sql-diag)
(defstruct (sql-diag-header :include sql-diag
                            :prefix sql-diag)
  returncode
  number)
(defstruct (sql-stmnt-diag-header :include sql-diag-header
                                  :prefix sql-diag)
  dynamic-function
  dynamic-function-code
  cursor-row-count
  row-count)

(defstruct (sql-diag-record :include sql-diag
                            :prefix sql-diag)
  sqlstate
  native
  message-text
  class-origin
  subclass-origin
  connection-name
  server-name)

(defstruct (sql-stmnt-diag-record :include sql-diag-record
                                  :prefix sql-diag)
  row-number column-number)
(dolist (class '(sql-stmnt-diag-rec sql-stmnt-diag-header))
  (dolist (slot (c2mop:class-slots (find-class )))
    (pushnew (c2mop:slot-definition-name slot)
             (slot-value slot 'sb-pcl::initargs))))

;;;;*** Diagnostics Fields

(defstruct sql-diag-field
  name code type)
(defparameter *sql-diag-fields*
  (loop :with hash := (make-hash-table)
     :for (name code &rest type)
     :in '((returncode #.SQL_DIAG_RETURNCODE #.SQLRETURN)
           (number #.SQL_DIAG_NUMBER #.SQLINTEGER)
           (dynamic-function #.SQL_DIAG_DYNAMIC_FUNCTION :string)
           (dynamic-function-code #.SQL_DIAG_DYNAMIC_FUNCTION_CODE #.SQLINTEGER)
           (cursor-row-count #.SQL_DIAG_CURSOR_ROW_COUNT #.SQLLEN)
           (row-count #.SQL_DIAG_ROW_COUNT #.SQLLEN)
           (sqlstate #.SQL_DIAG_SQLSTATE :string)
           (native #.SQL_DIAG_NATIVE #.SQLINTEGER)
           (message-text #.SQL_DIAG_MESSAGE_TEXT :string)
           (class-origin #.SQL_DIAG_CLASS_ORIGIN :string)
           (subclass-origin #.SQL_DIAG_SUBCLASS_ORIGIN :string)
           (connection-name #.SQL_DIAG_CONNECTION_NAME :string)
           (server-name #.SQL_DIAG_SERVER_NAME :string)
           (row-number #.SQL_DIAG_ROW_NUMBER #.SQLLEN)
           (column-number #.SQL_DIAG_COLUMN_NUMBER #.SQLINTEGER))
     :do (setf (gethash name hash) (make-sql-diag-field :name name
                                                        :code code
                                                        :type type))
     :finaly (return hash)))
(defun find-sql-diag-field (name)
  (gethash name *sql-diag-fields*))

(defun sql-diag-field-value (handle-type handle rec-number field)
  (let ((field-code (sql-diag-field-code field))
        (field-type (sql-diag-field-type field)))
    (foreign-let ((string-length 0 #.SQLSMALLINT)
                  (diag-info (unless (eql field-type :string) 0) field-type))
      (case (SQLGetDiagField handle-type handle rec-number field-code
                             (& diag-info) 0 (& string-length))
        ((#.SQL_SUCCESS_WITH_INFO #.SQL_SUCCESS)
         (case field-type
           (:string
            (let ((info-length (1+ string-length)))
              (cffi:with-foreign-pointer-as-string
                  (diag-info info-length)
                (SQLGetDiagField handle-type handle
                                 rec-number field-code
                                 diag-info info-length
                                 string-length))))))
        (t diag-info)))))


(defun sql-get-diag-rec (handle-type handle rec-number diag-rec-class)
  (apply 'make-instance diag-rec-class
         (mapcan (lambda (slot &aux
                          (name (c2mop:slot-definition-name slot)))
                   (list
                    name
                    (sql-diag-field-value handle-type handle
                                          rec-number
                                          (find-sql-diag-field name))))
                 (c2mop:class-slots (find-class diag-rec-class)))))


;;;;* Diagnostics Conditions
(define-condition sql-diagnostics (condition)
  ((header :initarg :header :reader sql-diagnostics-header)
   (records :initarg :records :reader sql-condition-records))
  (:report (lambda (condition stream)
             (with-slots (header records)
                 condition
               (format stream "~&~A~{~&~A~}"
                       header records)))))

(define-condition sql-error (sql-diagnostics) ())
(define-condition sql-warning (sql-diagnostics) ())

(defun diagnostics-class (sql-return-code)
  (case sql-return-code
    ((#.SQL_SUCCESS #.SQL_SUCCESS_WITH_INFO)
     'sql-diagnostics)
    ((#.SQL_STILL_EXECUTING #.SQL_NEED_DATA #.SQL_NO_DATA
                            #.SQL_PARAM_DATA_AVAILABLE)
     'sql-warning)
    (t
     'sql-error)))

(defgeneric sql-diagnostics (sql-handle sql-return-code)
  (:method ((sql-handle sql-handle) sql-return-code)
    (get-sql-diagnostics (sql-handle-type sql-handle)
                         (sql-handle-pointer sql-handle)
                         (diagnostics-class sql-return-code)
                         sql-diag-header
                         sql-diag-record))
  (:method ((sql-handle sql-statement) sql-return-code)
    (get-sql-diagnostics (sql-handle-type sql-handle)
                         (sql-handle-pointer sql-handle)
                         (diagnostics-class sql-return-code)
                         sql-stmnt-diag-header
                         sql-stmnt-diag-record)))

(defun get-sql-diagnostics (handle-type handle diagnostics-class
                            header-class record-class)
  (let ((header (sql-get-diag-rec handle-type handle 0 header-class)))
    (when header
      (make-condition diagnostics-class
       :header header
       :records (loop
                   :for idx :upfrom 1
                   :for rec := (sql-get-diag-rec handle-type handle
                                                 idx record-class)
                   :while rec
                   :collect rec)))))


;;;;* Allocate Handle

(defun sql-allocate-handle (handle-class handle)
  (cffi:with-foreign-object (output-handle-ptr #.SQLHANDLE)
    (let* ((sql-return-code (SQLAllocHandle
                            (sql-handle-type handle-class)
                            (sql-handle-pointer (or handle
                                                    (cffi-sys:null-pointer)))
                            output-handle-ptr))
           (sql-diagnostics (when handle
                              (sql-diagnostics handle sql-return-code)))))))