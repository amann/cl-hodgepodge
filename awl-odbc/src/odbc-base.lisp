;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")

(defmacro plus-ntc (size)
  `(1+ ,size))


;;;;* Diagnostics

;;;;** Diagnostics Headers and Records
(defstruct (sql-diag (:constructor nil)))
(defstruct (sql-diag-header (:include sql-diag)
                            (:conc-name sql-diag-))
  returncode
  number)
(defstruct (sql-stmnt-diag-header (:include sql-diag-header)
                                  (:conc-name sql-diag-))
  dynamic-function
  dynamic-function-code
  cursor-row-count
  row-count)

(defstruct (sql-diag-record (:include sql-diag)
                            (:conc-name sql-diag-))
  sqlstate
  native
  message-text
  class-origin
  subclass-origin
  connection-name
  server-name)

(defstruct (sql-stmnt-diag-record (:include sql-diag-record)
                                  (:conc-name sql-diag-))
  row-number column-number)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric make-sql-diag (struct-name handle-type handle rec-number))
  (defun expand-sql-diag-constructor (struct-name)
    (let ((slot-names (loop
                         :for slot
                         :in (c2mop:class-slots (find-class struct-name))
                         :collect (c2mop:slot-definition-name slot))))
      `(defmethod make-sql-diag ((struct-name (eql ',struct-name))
                                 handle-type handle rec-number)
         (let ((record (,(awl:symb "MAKE-" struct-name))))
           (setf ,@(mapcan
                    (lambda (slot-name)
                      (let* ((field (find-sql-diag-field slot-name))
                             (field-code (sql-diag-field-code field))
                             (field-type (sql-diag-field-type field)))
                        `((,(awl:symb "SQL-DIAG-" slot-name) record)
                          (sql-diag-field-value handle-type handle
                                                rec-number
                                                ',field-code ',field-type))))
                    slot-names)))))))

(defmacro define-sql-diag-constructor (&rest struct-names)
  `(progn ,@(mapcar #'expand-sql-diag-constructor struct-names)))

;;;;*** Diagnostics Fields


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct sql-diag-field
    name code type)
  (defparameter *sql-diag-fields*
    (loop :with hash := (make-hash-table)
       :for (name code &rest type)
       :in '((returncode            #.$:SQL_DIAG_RETURNCODE            #.$:SQLRETURN)
             (number                #.$:SQL_DIAG_NUMBER                #.$:SQLINTEGER)
             (dynamic-function      #.$:SQL_DIAG_DYNAMIC_FUNCTION      :string)
             (dynamic-function-code #.$:SQL_DIAG_DYNAMIC_FUNCTION_CODE #.$:SQLINTEGER)
             (cursor-row-count      #.$:SQL_DIAG_CURSOR_ROW_COUNT      #.$:SQLLEN)
             (row-count             #.$:SQL_DIAG_ROW_COUNT             #.$:SQLLEN)
             (sqlstate              #.$:SQL_DIAG_SQLSTATE              :string)
             (native                #.$:SQL_DIAG_NATIVE                #.$:SQLINTEGER)
             (message-text          #.$:SQL_DIAG_MESSAGE_TEXT          :string)
             (class-origin          #.$:SQL_DIAG_CLASS_ORIGIN          :string)
             (subclass-origin       #.$:SQL_DIAG_SUBCLASS_ORIGIN       :string)
             (connection-name       #.$:SQL_DIAG_CONNECTION_NAME       :string)
             (server-name           #.$:SQL_DIAG_SERVER_NAME           :string)
             (row-number            #.$:SQL_DIAG_ROW_NUMBER            #.$:SQLLEN)
             (column-number         #.$:SQL_DIAG_COLUMN_NUMBER         #.$:SQLINTEGER))
       :do (setf (gethash name hash) (make-sql-diag-field :name name
                                                          :code code
                                                          :type type))
       :finally (return hash)))
  (defun find-sql-diag-field (name)
    (gethash name *sql-diag-fields*)))


(defun sql-diag-field-value (handle-type handle rec-number
                             field-code field-type)
  (foreign-let ((string-length 0 #.$:SQLSMALLINT)
                (diag-info (unless (eql field-type :string) 0) field-type))
    (case (SQLGetDiagField handle-type handle rec-number field-code
                           (& diag-info) 0 (& string-length))
      ((#.$:SQL_SUCCESS #.$:SQL_SUCCESS_WITH_INFO)
       (case field-type
         (:string
          (let ((info-length (1+ string-length)))
            (cffi:with-foreign-pointer-as-string
                (diag-info info-length)
              (SQLGetDiagField handle-type handle
                               rec-number field-code
                               diag-info info-length
                               string-length))))
         (t diag-info))))))


(define-sql-diag-constructor
    sql-diag-header
    sql-diag-record
  sql-stmnt-diag-header
  sql-stmnt-diag-record)

(defun sql-get-diag-rec (handle-type handle rec-number diag-rec-class)
  (make-sql-diag diag-rec-class handle-type handle rec-number))


;;;;** Diagnostics Conditions

(define-condition sql-diagnostics ()
  ((header :initarg :header :reader sql-diagnostics-header)
   (records :initarg :records :reader sql-condition-records))
  (:report (lambda (condition stream)
             (with-slots (header records)
                 condition
               (format stream "~&~A~{~&~A~}" header records)))))

(define-condition sql-success (sql-diagnostics) ())
(define-condition sql-success-with-info (sql-success) ())
(define-condition sql-no-data (sql-diagnostics) ())
(define-condition sql-warning (warning sql-diagnostics) ())
(define-condition sql-still-executing (sql-warning) ())
(define-condition sql-need-data (sql-warning) ())
(define-condition sql-param-data-available (sql-warning) ())
(define-condition sql-error (error sql-diagnostics) ())
(define-condition sql-invalid-handle (sql-error) ())


(defun sql-diagnostics-class (sql-return-code)
  (find-class (ecase sql-return-code
                (#.$:SQL_SUCCESS 'sql-success)
                (#.$:SQL_SUCCESS_WITH_INFO 'sql-success-with-info)
                (#.$:SQL_NO_DATA 'sql-no-data)
                (#.$:SQL_PARAM_DATA_AVAILABLE 'sql-param-data-available)
                (#.$:SQL_STILL_EXECUTING 'sql-still-executing)
                (#.$:SQL_NEED_DATA 'sql-need-data)
                (#.$:SQL_ERROR 'sql-error)
                (#.$:SQL_INVALID_HANDLE 'sql-invalid-handle))))


(defun make-sql-diagnostics (handle-type handle)
  (multiple-value-bind (header-class record-class)
      (case handle-type
        (#.$:SQL_HANDLE_STMT (values 'sql-stmnt-diag-header
                                   'sql-stmnt-diag-record))
        (t (values 'sql-diag-header 'sql-diag-record)))
    (let ((header (make-sql-diag header-class handle-type handle 0)))
      (when header
        (make-condition (sql-diagnostics-class (sql-diag-returncode header))
                        :header header
                        :records (loop
                                    :for idx :upfrom 1
                                    :for rec := (make-sql-diag record-class
                                                               handle-type
                                                               handle
                                                               idx)
                                    :while rec
                                    :collect rec))))))


(defgeneric sql-diagnostics (sql-handle)
  (:documentation "Return an sql-diagnostics condition object."))

(defun sql-diagnostics-type-p (o)
  (and (awl:type-spec-p o)
       (subtypep o 'sql-diagnostics)))
(declaim (type (satisfies sql-diagnostics-type-p)  *signal-diagnostics*))
(defvar *signal-diagnostics* 'sql-diagnostics)

(defgeneric signal-diagnostics (sql-diagnostics)
  (:method (sql-diagnostics))
  (:method :around (sql-diagnostics)
    (when (typep sql-diagnostics *signal-diagnostics*)
      (call-next-method)))
  (:method ((sql-diagnostics sql-diagnostics))
    (signal sql-diagnostics))
  (:method ((sql-diagnostics sql-warning))
    (warn sql-diagnostics))
  (:method ((sql-diagnostics sql-error))
    (error sql-diagnostics)))

;;;;* WITH-SQL-FUNCTIONS
;;;; A Facility Macro wrapping a call to an SQL Function in such a way as
;;;; to return the values from the pointers and to signal the diagnostics
;;;; of the call.
(awl:define-recursive-macro with-sql-functions
    (((sql-function diagnostic-handle)) &body body)
  "In the scope of BODY, capture the diagnostics of each call of the sql-functions given in ARGS. ARGS is a list of pairs (sql-function diagnostic-handle), where sql-function (unevaluated) is a symbol naming a sql function and diagnostic-handle evaluates to a diagnostic-handle object."
  (awl:with-actual-macros (awl:macrolet*)
    `(awl:macrolet*
         ((,sql-function (&rest args)
             `(let* ((return-code (,',sql-function ,@args))
                     (diagnostic-handle ,',diagnostic-handle)
                     (diagnostics (when diagnostics-handle
                                    (sql-diagnostics diagnostics-handle))))
                (if diagnostics
                    (signal-diagnostics diagnostics)
                    (signal-diagnostics return-code)))))
       ,@body)))


(defmacro with-sql-function (returned-values diagnostics-handle &body body)
  "RETURNED-VALUES is a list of bindings of the form ({(var value &optional ctype count)}*) where var is a symbol which in BODY will be bound to value and will maybe contain a value returned by an SQL-function; these values are returned as multiple values."
  (awl:with-actual-macros (foreign-let)
    `(foreign-let ,returned-values
       (let* ((return-code (progn ,@body))
              (diagnostics ,(when diagnostics-handle
                                  `(sql-diagnostics ,diagnostics-handle))))
         (if diagnostics
             (signal-diagnostics diagnostics)
             (signal-diagnostics return-code)))
       (values ,@(mapcar 'first returned-values)))))

(defmacro sql-collect (&body body)
  "Repeat executing BODY collecting its primary return value until SQL-NO-DATA is signalled. If BODY does not signal any SQL-DIAGNOSTICS condition, the loop stops."
  `(do (result stop no-diagnostics)
       ((or stop no-diagnostics)
        (nreverse result))
     (handler-bind ((sql-diagnostics (lambda (c)
                                       (declare (ignore c))
                                       (setq no-diagnostics nil)))
                    (sql-no-data (lambda (c)
                                   (declare (ignore c))
                                   (setq stop t))))
       (setq no-diagnostics t)
       (push (progn ,@body) result))))


;;;;* Handles

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
  "Allocate the handle and register the finalizer freeing the pointer."
  (let* ((base-handle (base-handle self))
         (handle-type (sql-handle-type self))
         (new-handle (with-sql-function SQLAllocHandle
                         base-handle
                       (foreign-let ((new-handle nil #.$:SQLHANDLE))
                         (SQLAllocHandle handle-type
                                         (sql-handle-pointer base-handle)
                                         (& new-handle))))))
    (setf (slot-value self 'pointer) new-handle)
    (tg:finalize self (lambda ()
                        (SQLFreeHandle handle-type new-handle)
                        (cffi-sys:foreign-free new-handle)))))

(defmethod sql-handle-pointer ((sql-handle null))
  "Return a null pointer for the border case where SQL-HANDLE is NIL."
  (cffi:null-pointer))

(defgeneric base-handle (sql-handle)
  (:documentation "Return the handle in context of which the handle SQL-HANDLE has been (or will be) allocated.")
  (:method ((sql-handle sql-handle))
    nil)
  (:method ((sql-handle mixin-sql-environment))
    (sql-environment sql-handle))
  (:method ((sql-handle mixin-sql-connection))
    (sql-connection sql-handle)))

(defgeneric sql-type-code (sql-handle)
  (:documentation "Return the code designing the handler type.")
  (:method ((sql-handle sql-environment))
    #.$:SQL_HANDLE_ENV)
  (:method ((sql-handle sql-connection))
    #.$:SQL_HANDLE_DBC)
  (:method ((sql-handle sql-statement))
    #.$:SQL_HANDLE_STMT)
  (:method ((sql-handle sql-description))
    #.$:SQL_HANDLE_DESC))

(awl:define-recursive-macro with-sql-handle
    (((var sql-handle) 'awl:var-binding-pair) &body body)
  (awl:with-gensyms (g!sql-handle g!sql-handle-ptr
                                  g!sql-handle-type-code
                                  g!sql-base-handle)
    (awl:with-actual-macros (awl:macrolet*)
      `(let* ((,g!sql-handle ,sql-handle)
              (,g!sql-handle-ptr (sql-handle-pointer ,g!sql-handle))
              (,g!sql-handle-type-code (sql-handle-type ,g!sql-handle))
              (,g!sql-base-handle (base-handle ,g!sql-handle))
              (,var ,g!sql-handle))
         (awl:macrolet*
             ((& (var) (if (eq var ,var)
                           g!sql-handle-ptr
                           `(& ,var)))
              (sql-type-code (var) (if (eq var ,var)
                                       g!sql-handle-type-code
                                       `(sql-type-code ,var)))
              (base-handle (var) (if (eq var ,var)
                                     g!sql-base-handle
                                     `(base-handle ,var))))
           ,@body)))))

;;;;** Get Diagnostics Condition Objects from Handles

(defmethod sql-diagnostics ((sql-handle sql-handle))
  (make-sql-diagnostics (sql-handle-type sql-handle)
                        (sql-handle-pointer sql-handle)))


;;;;* Metainformation

(declaim (ftype (function (sql-environment) list) sql-drivers))
(defun sql-drivers (sql-environment)
  (with-sql-handle (sql-environment)
    (with-sql-functions ((SQLDrivers sql-environment))
      (foreign-let ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                    (pcchDrvrAttr 0 #.$:SQLSMALLINT))
        (let ((sizes (let ((fdirection (cons #.$:SQL_FETCH_FIRST
                                             (awl:make-circle #.$:SQL_FETCH_NEXT))))
                       (sql-collect
                         (SQLDrivers (& sql-environment)
                                     (pop fdirection)
                                     (& nil)
                                     0
                                     (& pcchDriverDesc)
                                     (& nil)
                                     0
                                     (& pcchDrvrAttr))
                         (list pcchDriverDesc pcchDrvrAttr)))))
          (destructuring-bind (cchDriverDescMax cchDrvrAttrMax)
                  (reduce (lambda (max size)
                            (list (max (first max) (first size))
                                  (max (second max) (second size))))
                          (rest sizes)
                          :initial-value (first sizes))
            (foreign-let ((szDriverDesc nil #.$:SQLCHAR cchDriverDescMax)
                         (szDriverAttributes nil #.$:SQLCHAR cchDrvrAttrMax))
             (sql-collect
               (SQLDrivers (& sql-environment)
                           #.$:SQL_FETCH_NEXT
                           (& szDriverDesc)
                           (plus-ntc cchDriverDescMax)
                           (& pcchDriverDesc)
                           (& szDriverAttributes)
                           (plus-ntc cchDrvrAttrMax)
                           (& pcchDrvrAttr))
               (list szDriverDesc szDriverAttributes)))))))))



(declaim (ftype (function (sql-environment &optional
                                           (and symbol (member :all :user :system))) list)
                sql-datasources))
(defun sql-datasources (sql-environment &optional (option :all))
  (let ((fdirection (cons (case option
                            (:all #.$:SQL_FETCH_FIRST)
                            (:user #.$:SQL_FETCH_FIRST_USER)
                            (:system SQL_FETCH_FIRST_SYSTEM))
                          (awl:make-circle #.$:SQL_FETCH_NEXT))))
    (with-sql-handle (sql-environment)
      (with-sql-functions ((SQLDataSources sql-environment))
        (foreign-let ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                      (pcchDrvrAttr 0 #.$:SQLSMALLINT))
          (let ((sizes (let ((fdirection fdirection))
                         (sql-collect
                           (SQLDataSources (& sql-environment)
                                           (pop fdirection)
                                           (& nil)
                                           0
                                           (& pcchDriverDesc)
                                           (& nil)
                                           0
                                           (& pcchDrvrAttr))
                           (list pcchDriverDesc pcchDrvrAttr)))))
            (foreign-let ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                          (pcchDrvrAttr 0 #.$:SQLSMALLINT))
              (sql-collect
               (destructuring-bind (cchDriverDescMax cchDrvrAttrMax)
                   (pop sizes)
                 (SQLDataSources (& sql-environment)
                                 (pop fdirection)
                                 (& szDriverDesc)
                                 cchDriverDescMax
                                 (& pcchDriverDesc)
                                 (& szDriverAttributes)
                                 cchDrvrAttrMax
                                 (& pcchDrvrAttr))
                 (list szDriverDesc szDriverAttributes))))))))))


;;;;* Connections

(declaim (ftype (function (sql-connection string) (values string integer))
                sql-driver-connect))
(defun sql-driver-connect (sql-connection connection-string)
  (declare (type sql-connection sql-connection)
           (type string connection-string))
  (with-sql-function ((completed-conn-string nil #.$:SQLCHAR 1024)
                      (out-str-length 0 #.$:SQLSMALLINT))
      sql-connection
    (let ((length-c-str (length connection-string)))
     (foreign-let ((connection-string connection-string #.$:SQLCHAR))
       (SQLDriverConnect (sql-handle-pointer sql-connection)
                         (cffi:null-pointer)
                         (& connection-string)
                         length-c-str
                         (& completed-conn-string)
                         1024
                         (& out-str-length)
                         #.$:SQL_DRIVER_NOPROMPT)))))

(declaim (ftype (function (sql-connection)) sql-disconnect))
(defun sql-disconnect (sql-connection)
  (with-sql-function ()
      sql-connection
    (SQLDisconnect (sql-handle-pointer sql-connection))))

(declaim (ftype (function ((or sql-environment sql-connection)
                           (and symbol (member :commit :rollback))))
                sql-end-transaction))
(defun sql-end-transaction (sql-handle action)
  (with-sql-function ()
      sql-handle
    (SQLEndTran (sql-handle-type sql-handle)
                (sql-handle-pointer sql-handle)
                (ecase action
                  (:commit #.$:SQL_COMMIT)
                  (:rollback #.$:SQL_ROLLBACK)))))

(declaim (ftype (function (sql-handle)) sql-cancel))
(defun sql-cancel (sql-handle)
  (with-sql-function ()
      sql-handle
    (SQLCancelHandle (sql-handle-type sql-handle)
                     (sql-handle-pointer sql-handle))))


;;;;* SQL Buffers
;;;;** Binary Buffer
(defvar *sql-size*)
(define-ff-buffer sql-binary-buffer ()
  (ptr #.$:SQLCHAR)
  :getter (lambda ()
            (let* ((dim (min (size ptr)
                             (if (boundp '*sql-size*)
                                 *sql-size*
                                 (size ptr))))
                   (result (make-array dim)))
              (dotimes (i dim result)
                (setf (svref result i) (cref ptr i)))))
  :setter (lambda (value)
            (declare (type (vector (unsigned-byte 8)) value))
            (dotimes (i (min (size ptr) (length value)) value)
              (setf (cref ptr i) (aref value i)))))
;;;;** String Buffers
(define-ff-buffer sql-string-buffer (sql-binary-buffer)
  (ptr #.$:SQLCHAR)
  :getter (lambda (&aux (cffi:*default-foreign-encoding* :iso-8859-1))
            (cffi:foreign-string-to-lisp ptr))
  :setter (lambda (value &aux
                           (cffi:*default-foreign-encoding* :iso-8859-1))
            (cffi:lisp-string-to-foreign value ptr count)))
(defmethod initialize-instance
    ((self sql-string-buffer) &rest initargs &key size &allow-other-keys)
  (apply #'call-next-method self :size (1+ size) initargs))
;;;;*** UCS-2 String Buffers
(define-ff-buffer sql-ucs2-string-buffer (sql-string-buffer)
    (ptr #.$:SQLWCHAR)
  :getter (lambda () 
            (with-output-to-string (s)
              (let ((code 0))
                (dotimes (i (floor (size ptr) 2))
                  (let ((i (* 2 i)))
                    (setf (ldb (byte 8 0) code) (cref ptr i)
                          (ldb (byte 8 8) code) (cref ptr (1+ i))))
                  (when (zerop code) (return))
                  (write-char (code-char code) s)))))
  :setter (lambda (value)
            (loop :for char :across value
               :for i :upfrom 0 :below (1- (size ptr)) :by 2
               :do (let ((code (char-code char)))
                     (setf (cref ptr i) (ldb (byte 8 0) code)
                           (cref ptr (1+ i)) (ldb (byte 8 8) code)))
               :finally (setf (cref ptr i) (ldb (byte 8 0) 0)
                              (cref ptr (1+ i)) (ldb (byte 8 8) 0)))))
(defmethod initialize-instance
    ((self sql-ucs2-string-buffer) &rest initargs &key size &allow-other-keys)
  (apply #'call-next-method selft :size (* 2 size) initargs))

;;;;** Numeric Buffers
;;;;*** Integer
(define-ff-buffer sql-integer-buffer ()
  (ptr #.$:SQLBIGINT)
  :getter (lambda () ptr)
  :setter (lambda (value) (setf ptr value)))
;;;;*** Double Float
(define-ff-buffer sql-double-buffer ()
  (ptr #.$:SQLDOUBLE)
  :getter (lambda () ptr)
  :setter (lambda (value) (setf ptr value)))
;;;;*** Numeric Structure
(defun scale (x &aux (max (expt 2 64)))
  (if (< max x)
      (loop
         :for i :downfrom -1
         :for y := (/ x 10) :then (/ y 10)
         :do (unless (< max y)
               (return i)))
      (loop
         :for i :upfrom 0
         :for y := (* x 10) :then (* y 10)
         :do (when (< max y)
               (return i)))))
(defun fit-to-numeric (x)
  (let ((scale (scale x)))
    (values (round (* x (expt 10 scale))) scale)))

(define-ff-buffer sql-numeric-buffer ()
  (ptr SQL-NUMERIC-STRUCT)
  :getter (lambda ()
            (cffi:with-foreign-slots
                ((scale sign val)
                 (& ptr) sql-numeric-struct)
              (let ((value (/ (let ((num 0))
                                (dotimes (i #.$:SQL_MAX_NUMERIC_LEN num)
                                  (setf (ldb (byte 8 (* 8 i)) num)
                                        (cffi:mem-aref val #.$:SQLCHAR i))))
                              (expt 10 (the fixnum scale)))))
                (if (zerop (mod sign 2))
                    (- value)
                    value))))
  :setter (lambda (value)
            (cffi:with-foreign-slots
                ((scale sign val)
                 (& ptr) sql-numeric-struct)
              (setq sign (if (< 0 value) 1 #.$:SQL_NEGATIVE_VALUE))
              (multiple-value-bind (value s)
                  (fit-to-numeric (abs value))
                (setq scale s)
                (dotimes (i #.$:SQL_MAX_NUMERIC_LEN)
                  (setf (cffi:mem-aref val #.$:SQLCHAR i)
                        (ldb (byte 8 (* 8 i)) value)))))))

;;;;** Blob Buffers
;;;; They are probably covered by the binary buffers
;;(define-ff-buffer sql-blob-buffer ())
;;;;** Clob Buffers
;;;; They are probably covered by the string buffers
;;(define-ff-buffer sql-clob-buffer ())
;;(define-ff-buffer sql-uclob-buffer ())

;;;;** Date Buffer
(defgeneric timestamp-encoder (date-time-class)
  (:documentation "Return a function taking the values YEAR, MONTH, DAY, HOUR, MINUTE, SECOND and FRACTION representing a valid gregorian date, where SECOND can contain up to 2 leap seconds and FRACTION is a (real 0 1).")
  (:method ((date-time-class (eql :universal-time)))
    (lambda (year month day &optional (hour 0) (minute 0) (second 0)
             (fraction 0))
      (multiple-value-bind (second frac)
          (floor second 60)
        (let ((fraction (+ frac fraction)))
          (assert (<= 0 fraction 2))
          (values (encode-universal-time second minute hour day month year)
                  fraction))))))
(defgeneric timestamp-decoder (date-time-class)
  (:documentation "Return a function taking one or more values representing some date-time instance and returning as multiple values the year, month, day, hour minute and second as integers and fraction as a (real 0 1).")
  (:method ((date-time-class (eql :universal-time)))
    (lambda (universal-time &optional (fraction 0))
      (check-type fraction (real 0 2))
      (multiple-value-bind (sec fraction)
          (floor fraction)
        (multiple-value-bind (second minute hour day month year)
            (decode-universal-time universal-time)
          (values year month day hour minute (+ sec second) fraction))))))


(declaim (type symbol *date-time-class*))
(defvar *date-time-class* :universal-time)


(define-ff-buffer sql-date-buffer ()
  (ptr 'SQL-TIMESTAMP-STRUCT)
  :getter (let ((timestamp-encoder (if date-time-class
                                       (timestamp-encoder date-time-class)
                                       (lambda (&rest args)
                                         (apply (timestamp-encoder *date-time-class*)
                                                args)))))
            (lambda ()
              (cffi:with-foreign-slots
                  ((year month day hour minute second fraction)
                   (& ptr) SQL-TIMESTAMP-STRUCT)
                (funcall (timestamp-encoder *date-time-class*)
                         year month day
                         hour minute second fraction))))
  :setter (let ((timestamp-decoder (if date-time-class
                                       (timestamp-decoder date-time-class)
                                       (lambda (&rest args)
                                         (apply (timestamp-decoder *date-time-class*)
                                                args)))))
            (lambda (value)
              (cffi:with-foreign-slots
                  ((year month day hour minute second fraction)
                   (& ptr) SQL-TIMESTAMP-STRUCT)
                (multiple-value-setq
                    (year month day hour minute second fraction)
                  (apply (timestamp-decoder *date-time-class*)
                         (awl:ensure-list value)))))))
(defmethod initialize-instance
    ((self sql-ucs2-string-buffer)
     &key date-time-class &allow-other-keys)
  (declare (special date-time-class))
  (call-next-method))


(defun ctype-code (sql-type-code)
  "Return the C type code corresponding to the given SQL type code.")

#||

SQL_C_CHAR
SQLCHAR *
unsigned char *
SQL_C_WCHAR
SQLWCHAR *
wchar_t *
SQL_C_SSHORT[j]
SQLSMALLINT
short int
SQL_C_USHORT[j]
SQLUSMALLINT
unsigned short int
SQL_C_SLONG[j]
SQLINTEGER
long int
SQL_C_ULONG[j]
SQLUINTEGER
unsigned long int
SQL_C_FLOAT
SQLREAL
float
SQL_C_DOUBLE
SQLDOUBLE, SQLFLOAT
double
SQL_C_BIT
SQLCHAR
unsigned char
SQL_C_STINYINT[j]
SQLSCHAR
signed char
SQL_C_UTINYINT[j]
SQLCHAR
unsigned char
SQL_C_SBIGINT
SQLBIGINT
_int64[h]
SQL_C_UBIGINT
SQLUBIGINT
unsigned _int64[h]
SQL_C_BINARY
SQLCHAR *
unsigned char *
SQL_C_BOOKMARK[i]
BOOKMARK
unsigned long int[d]
SQL_C_VARBOOKMARK
SQLCHAR *
unsigned char *
SQL_C_TYPE_DATE[c]
SQL_DATE_STRUCT
struct tagDATE_STRUCT {
   SQLSMALLINT year;
   SQLUSMALLINT month;
   SQLUSMALLINT day;  
} DATE_STRUCT;[a]
SQL_C_TYPE_TIME[c]
SQL_TIME_STRUCT
struct tagTIME_STRUCT {
   SQLUSMALLINT hour;
   SQLUSMALLINT minute;
   SQLUSMALLINT second;
} TIME_STRUCT;[a]
SQL_C_TYPE_TIMESTAMP[c]
SQL_TIMESTAMP_STRUCT
struct tagTIMESTAMP_STRUCT {
   SQLSMALLINT year;
   SQLUSMALLINT month;
   SQLUSMALLINT day;
   SQLUSMALLINT hour;
   SQLUSMALLINT minute;
   SQLUSMALLINT second;
   SQLUINTEGER fraction;[b] 
} TIMESTAMP_STRUCT;[a]
SQL_C_NUMERIC
SQL_NUMERIC_STRUCT
struct tagSQL_NUMERIC_STRUCT {
   SQLCHAR precision;
   SQLSCHAR scale;
   SQLCHAR sign[g];
   SQLCHAR val[SQL_MAX_NUMERIC_LEN];[e], [f] 
} SQL_NUMERIC_STRUCT;
SQL_C_GUID
SQLGUID
struct tagSQLGUID {
   DWORD Data1;
   WORD Data2;
   WORD Data3;
   BYTE Data4[8];
} SQLGUID;[k]
All C interval data types
SQL_INTERVAL_STRUCT
See the C Interval Structure section, later in this appendix.


||#


;;;;* Columns


(defclass sql-column ()
  ((stmnt :initform (error "No statement Handle given.")
          :initarg :statement
          :type sql-statement)
   (name :reader name)
   (col-nbr :initform (error "No column number given.")
            :initarg :col-nbr)
   (sql-type-code :initform (error "No SQL type code given.")
                  :initarg :sql-type-code)
   getter
   setter)
  (:default-initargs :count 1))


(defmethod initialize-instance :after ((self sql-column)
                                       &key statement col-nbr
                                       sql-type-code count
                                       &allow-other-keys)
  (let* ((ctype-code (ctype-code sql-type-code))
         (ctype (ctype ctype-code))
         (hstmnt (sql-handle-pointer statement)))
    (let ((value-buffer (make-ff-buffer ctype :size count))
          (indic-buffer (make-ff-buffer #.$:SQLLEN)))
      (with-foreign-buffers (value-buffer indic-buffer)
        (with-sql-function ()
            hstmnt
          (SQLBindCol hstmnt col-nbr ctype-code
                      (& value-buffer) (size value-buffer)
                      (& indic-buffer)))
        (with-slots (getter setter)
            self
          (setf getter (lambda ()
                         (let ((*sql-size* indic-buffer))
                           (unless (= *sql-size* #.$:SQL_NULL_DATA)
                             value-buffer)))
                setter (lambda (value)
                         )))))
    (tg:finalize self (lambda () 
                        (SQLBindCol hstmnt col-nbr ctype-code
                                    (cffi-sys:null-pointer) 0
                                    (cffi-sys:null-pointer))))))

(defgeneric sql-get-value (column)
  (:method ((column sql-column))
    (ff-buffer-value (slot-value column 'buffer))))
(defgeneric (setf sql-get-value) (value column)
  (:method (value (column sql-column))
    (setf (ff-buffer-value (slot-value column 'buffer)) value)))

(defgeneric sql-getter (column)
  (:method ((column sql-column))
    (slot-value (slot-value column 'buffer) 'getter)))
(defgeneric sql-setter (column)
  (:method ((column sql-column))
    (slot-value (slot-value column 'buffer) 'setter)))

