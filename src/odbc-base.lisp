;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")

(defun plus-ntc (size)
  (1+ size))


;;;;* Diagnostics

;;;;** Diagnostics Headers and Records
(defstruct (sql-diag (:constructor)))
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

(defgeneric make-sql-diag (struct-name handle-type handle rec-number))
(defun expand-sql-diag-constructor (struct-name)
  (let ((slot-names (loop
                        :for slot
                        :in (c2mop:class-slots (find-class struct-name))
                        :collect (c2mop:slot-definition-name slot))))
    `(defmethod make-sql-diag ((struct-name (eql ',struct-name))
                               handle-type handle rec-number)
       (let ((record (,(oam:symb "MAKE-" struct-name))))
         (setf ,@(mapcan
                  (lambda (slot-name)
                    (let* ((field (find-sql-diag-field slot-name))
                           (field-code (sql-diag-field-code field))
                           (field-type (sql-diag-field-type field)))
                      `((,(oam:symb "SQL-DIAG-" slot-name) record)
                        (sql-diag-field-value handle-type handle
                                              rec-number
                                              ',field-code ',field-type))))
                  slot-names))))))

(defmacro define-sql-diag-constructor (&rest struct-names)
  `(progn ,@(mapcar 'expand-sql-diag-constructor struct-names)))

;;;;*** Diagnostics Fields

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
  (gethash name *sql-diag-fields*))

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

(define-condition sql-diagnostics (sql-condition)
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


(defun sql-condition-class (sql-return-code)
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
        (make-condition (diagnostics-class (sql-diag-returncode header))
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

(declaim (type 'sql-condition *signal-diagnostics*))
(defvar *signal-diagnostics* 'sql-condition)

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

;;;;* WITH-SQL-FUNCTION
;;;; A Facility Macro wrapping a call to an SQL Function in such a way as
;;;; to return the values from the pointers and to signal the diagnostics
;;;; of the call.
(defmacro with-sql-function (returned-values diagnostics-handle &body body)
  ""
  (oam:with-gensyms (return-code diagnostics)
    `(foreign-let ,returned-values
       (let* ((,return-code (progn ,@body))
              (,diagnostics (when diagnostics-handle
                              (sql-diagnostics diagnostics-handle))))
         (if ,diagnostics
             (signal-diagnostics ,diagnostics)
             (signal-diagnostics ,return-code)))
       (values ,@(mapcar 'first returned-values)))))

(defmacro sql-collect (diagnostics-handle &body body)
  "Repeat executing BODY collecting its primary return value until SQL-NO-DATA is signalled. If BODY does not signal any SQL-DIAGNOSTICS condition, the loop stops."
  (do (result stop no-diagnostics)
      ((or stop no-diagnostics)
       (nreverse result))
    (handler-bind ((sql-diagnostics (lambda (c) (setq no-diagnostics nil)))
                   (sql-no-data (lambda (c) (setq stop t))))
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
         (new-handle (with-sql-function ((new-handle nil #.$:SQLHANDLE))
                         base-handle
                       (SQLAllocHandle handle-type
                                       (sql-handle-pointer base-handle)
                                       (& new-handle)))))
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

(defgeneric sql-handle-type (sql-handle)
  (:documentation "Return the code designing the handler type.")
  (:method ((sql-handle sql-environment))
    #.$:SQL_HANDLE_ENV)
  (:method ((sql-handle sql-connection))
    #.$:SQL_HANDLE_DBC)
  (:method ((sql-handle sql-statement))
    #.$:SQL_HANDLE_STMT)
  (:method ((sql-handle sql-description))
    #.$:SQL_HANDLE_DESC))

;;;;** Get Diagnostics Condition Objects from Handles

(defmethod sql-diagnostics ((sql-handle sql-handle))
  (make-sql-diagnostics (sql-handle-type sql-handle)
                        (sql-handle-pointer sql-handle)))


;;;;* Connections

(declaim (ftype (function (sql-connection string) (values string integer))
                sql-driver-connect))
(defun sql-driver-connect (sql-connection connection-string)
  (declare (sql-connection sql-connection)
           (string connection-string))
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

;;;;* Metainformation

(declaim (ftype (function (sql-environment) list) sql-drivers))
(defun sql-drivers (sql-environment)
  (let* ((henv (sql-handle-pointer sql-environment))
         (fdirection '(#.$:SQL_FETCH_FIRST . #1=(#.$:SQL_FETCH_NEXT . #1#)))
         (sizes (sql-collect
                 (multiple-value-list
                  (with-sql-function ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                                      (pcchDrvrAttr 0 #.$:SQLSMALLINT))
                      sql-environment
                    (SQLDrivers henv
                                (pop fdirection)
                                (cffi:null-pointer)
                                0
                                (& pcchDriverDesc)
                                (cffi:null-pointer)
                                0
                                (& pcchDrvrAttr)))))))
    (let ((fdirection (cons #.$:SQL_FETCH_FIRST fdirection)))
      (mapcar
       (lambda (size)
         (let ((cchDriverDescMax (plus-ntc (first size)))
               (cchDrvrAttrMax (plus-ntc (second size))))
           (multiple-value-list
            (with-sql-function ((szDriverDesc nil #.$:SQLCHAR
                                              cchDriverDescMax)
                                (szDriverAttributes nil #.$:SQLCHAR
                                                    cchDrvrAttrMax))
                sql-environment
              (foreign-let ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                            (pcchDrvrAttr 0 #.$:SQLSMALLINT))
                (SQLDrivers henv
                            (pop fdirection)
                            (& szDriverDesc)
                            cchDriverDescMax
                            (& pcchDriverDesc)
                            (& szDriverAttributes)
                            cchDrvrAttrMax
                            (& pcchDrvrAttr)))))))))))


(declaim (ftype (function (sql-environment &optional
                           (and symbol (member :all :user :system))) list)
                sql-datasources))
(defun sql-datasources (sql-environment &optional (option :all))
  (let* ((henv (sql-handle-pointer sql-environment))
         (first-fetch (case option
                        (:all #.$:SQL_FETCH_FIRST)
                        (:user #.$:SQL_FETCH_FIRST_USER)
                        (:system SQL_FETCH_FIRST_SYSTEM)))
         (fdirection `(,first-fetch . #1=(#.$:SQL_FETCH_NEXT . #1#)))
         (sizes (sql-collect
                 (multiple-value-list
                  (with-sql-function ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                                      (pcchDrvrAttr 0 #.$:SQLSMALLINT))
                      sql-environment
                    (SQLDataSources henv
                                    (pop fdirection)
                                    (cffi:null-pointer)
                                    0
                                    (& pcchDriverDesc)
                                    (cffi:null-pointer)
                                    0
                                    (& pcchDrvrAttr)))))))
    (let ((fdirection (cons first-fetch fdirection)))
      (mapcar
       (lambda (size)
         (let ((cchDriverDescMax (plus-ntc (first size)))
               (cchDrvrAttrMax (plus-ntc (second size))))
           (multiple-value-list
            (with-sql-function ((szDriverDesc nil #.$:SQLCHAR
                                              cchDriverDescMax)
                                (szDriverAttributes nil #.$:SQLCHAR
                                                    cchDrvrAttrMax))
                sql-environment
              (foreign-let ((pcchDriverDesc 0 #.$:SQLSMALLINT)
                            (pcchDrvrAttr 0 #.$:SQLSMALLINT))
                (SQLDataSources henv
                                (pop fdirection)
                                (& szDriverDesc)
                                cchDriverDescMax
                                (& pcchDriverDesc)
                                (& szDriverAttributes)
                                cchDrvrAttrMax
                                (& pcchDrvrAttr)))))))))))



;;;;* Buffers
;;;; A thin wrapper for buffer pointers. The memory recource is automatically
;;;; freed at garbage collection.

(defclass ff-buffer ()
  (getter setter (count :initform 1
                        :type (integer 1 #.array-dimension-limit)
                        :reader ff-buffer-length)))

(defmethod initialize-instance :around ((self ff-buffer) &key
                                        &allow-other-keys)
  (prog1
      (call-next-method)
    (initialize-accessors self)))

(defgeneric initialize-accessors (ff-buffer)
  (:documentation "Methods for creating thegetter and setter methods."))

(defmacro define-ff-buffer (name (&optional superclasses slots)
                             (ptr-var ctype &optional count)
                             &body body)
  "In BODY the variables COUNT SETTER and GETTER are bound to the repective slots of the instance."
  (oam:with-gensyms (g!sql-buffer)
    (let* ((documentation (when (stringp (first body))
                            (first body)))
           (body (if documentation
                     (rest body)
                     body))
           (slots (mapcar (lambda (slot)
                            (let ((slot (if (consp slot) slot (list slot))))
                              (unless (getf slot :initform)
                                (setf (getf slot :initform) nil))
                              slot))
                          slots))
           (slot-names (mapcar 'first slots)))
      `(progn
         (defclass ,name ,(adjoin 'sql-buffer superclasses
                                  :test (lambda (x y) (subtypep y x)))
           ,slots
           ,@(when documentation `((:documentation ,documentation))))
         (defmethod initialize-accessors ((,g!sql-buffer ,name))
           (with-slots (getter setter count)
               ,g!sql-buffer
             ,(when (integerp count)
                    `(setq count ,count))
             (let ((,ptr-var (cffi:foreign-alloc ,ctype :count count)))
               (tg:finalize ,g!sql-buffer (lambda ()
                                            (cffi:foreign-free ,ptr-var)))
               (let ((count count)
                     ,@(mapcar (lambda (slot-name)
                                 `(slot-name (slot-value ,g!sql-buffer
                                                         ',slot-name)))
                               slot-names))
                 (declare (type fixnum count)
                          (ignorable count ,@slot-names))
                 (macrolet ((,ptr-var (&optional (i 0))
                              `(cffi:mem-aref ,',ptr-var ,',ctype ,i)))
                   ,@body)))))))))


(defgeneric ff-buffer-value (ff-buffer)
  (:method ((ff-buffer ff-buffer))
    (funcall (slot-value ff-buffer 'getter))))
(defgeneric (setf ff-buffer-value) (value ff-buffer)
  (:method ((ff-buffer ff-buffer))
    (funcall (slot-value ff-buffer 'setter) value)))

;;;;** Binary Buffer
(define-ff-buffer sql-binary-buffer ()
  (ptr #.$:SQL_C_CHAR)
  (setq getter (lambda ()
                 (declare (special sql-len))
                 (let* ((dim (min count sql-len))
                        (result (make-array dim)))
                   (dotimes (i dim result)
                     (setf (svref result i) (ptr i)))))
        setter (lambda (value)
                 (declare (type (vector (unsigned-byte 8)) value))
                 (check-type value '(vector (unsigned-byte 8)))
                 (dotimes (i (min count (length value)) value)
                   (setf (ptr i) (aref value i))))))
(defmethod initialize-instance :after ((self sql-binary-buffer)
                                        &key size &allow-other-keys)
  (when (integerp size)
    (setf (slot-value self 'count) size)))
;;;;** String Buffers
(define-ff-buffer sql-string-buffer (sql-binary-buffer)
  (ptr #.$:SQL_C_CHAR)
  (setq getter (lambda (&aux (cffi:*default-foreign-encoding* :iso-8859-1))
                 (cffi:foreign-string-to-lisp ptr))
        setter (lambda (value &aux
                        (cffi:*default-foreign-encoding* :iso-8859-1))
                 (cffi:lisp-string-to-foreign value ptr count))))
(defmethod initialize-instance :after ((self sql-string-buffer)
                                       &key &allow-other-keys)
  (incf (slot-value self 'count)))
;;;;*** UCS-2 String Buffers
(define-ff-buffer sql-ucs2-string-buffer (sql-string-buffer)
  (ptr #.$:SQL_C_WCHAR)
  (setq getter (lambda () 
                 (with-output-to-string (s)
                   (let ((code 0))
                     (dotimes (i (floor count 2))
                       (let ((i (* 2 i)))
                         (setf (ldb (byte 8 0) code) (ptr i)
                               (ldb (byte 8 8) code) (ptr (1+ i))))
                       (when (zerop code) (return))
                       (write-char (code-char code) s)))))
        setter (lambda (value &aux (dim (1- count)))
                 (loop :for char :across value
                    :for i :upfrom 0 :below dim :by 2
                    :do (let ((code (char-code char)))
                          (setf (ptr i) (ldb (byte 8 0) code)
                                (ptr (1+ i)) (ldb (byte 8 8) code)))
                    :finally (setf (ptr i) (ldb (byte 8 0) 0)
                                (ptr (1+ i)) (ldb (byte 8 8) 0))))))
(defmethod initialize-instance :after ((self sql-utf16-string-buffer)
                                       &key &allow-other-keys)
  (with-slots (count)
      self
    (setf count (* 2 count))))

;;;;** Numeric Buffers
;;;;*** Integer
(define-ff-buffer sql-integer-buffer ()
  (ptr #.$:SQLBIGINT)
  (setq getter (lambda () (ptr))
        setter (lambda (value) (setf (ptr) value))))
;;;;*** Double Float
(define-ff-buffer sql-double-buffer ()
  (ptr #.$:SQLDOUBLE)
  (setq getter (lambda () (ptr))
        setter (lambda (value) (setf (ptr) value))))
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
  (setq getter (lambda ()
                 (cffi:with-foreign-slots
                     ((scale sign val)
                      ptr sql-numeric-struct)
                   (let ((value (/ (let ((num 0))
                                     (dotimes (i #.$:SQL_MAX_NUMERIC_LEN num)
                                       (setf (ldb (byte 8 (* 8 i)) num)
                                             (cffi:mem-aref val #.$:SQLCHAR i))))
                                   (expt 10 (the fixnum scale)))))
                     (if (zerop (mod sign 2))
                         (- value)
                         value))))
        setter (lambda (value)
                 (cffi:with-foreign-slots
                     ((scale sign val)
                      ptr sql-numeric-struct)
                   (setq sign (if (< 0 value) 1 #.$:SQL_NEGATIVE_VALUE))
                   (multiple-value-bind (value s)
                       (fit-to-numeric (abs value))
                     (setq scale s)
                     (dotimes (i #.$:SQL_MAX_NUMERIC_LEN)
                       (setf (cffi:mem-aref val #.$:SQLCHAR i)
                             (ldb (byte 8 (* 8 i)) value))))))))

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
  (:documentation "Return a function taking one or more values representing some date-time instance and returning as multiple values the year, month, day, hour minute and second as integers and fraction as a (real 0 2).")
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


(define-ff-buffer sql-date-buffer
    (nil ((date-time-class :initform *date-time-class*)))
  (ptr 'SQL-TIMESTAMP-STRUCT)
  (let ((timestamp-encoder (if date-time-class
                               (timestamp-encoder date-time-class)
                               (lambda (&rest args)
                                 (apply (timestamp-encoder *date-time-class*)
                                        args))))
        (timestamp-decoder (if date-time-class
                               (timestamp-decoder date-time-class)
                               (lambda (&rest args)
                                 (apply (timestamp-decoder *date-time-class*)
                                        args)))))
    (setq getter (lambda ()
                   (cffi:with-foreign-slots
                       ((year month day hour minute second fraction)
                        ptr 'SQL-TIMESTAMP-STRUCT)
                     (funcall timestamp-encoder
                              year month day
                              hour minute second fraction)))
          setter (lambda (value)
                   (cffi:with-foreign-slots
                       ((year month day hour minute second fraction)
                        ptr 'SQL-TIMESTAMP-STRUCT)
                     (multiple-value-setq
                         (year month day hour minute second fraction)
                       (apply timestamp-encoder (if (listp value)
                                                    value
                                                    (list value)))))))))

(defun make-buffer sql-type-code precision scale nullable)

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
   (col-nbr :initarg :col-nbr)
   (buffer :type sql-buffer)))


(defmethod initialize-instance :after ((self sql-column)
                                       &key &allow-other-keys)
  (with-slots (col-nbr size buffer)
      self
    (let ((buffer (make-buffer column size))
          (ctype (ctype-code column))
          (hstmnt (sql-handle-pointer (slot-value column 'stmnt))))
      (setf getter (lambda ()
                     (cffi:mem-ref buffer ctype))
            setter (lambda (value)
                     (setf (cffi:mem-ref buffer ctype) value)))
      (tg:finalize self (lambda () 
                          (SQLBindCol hstmnt col-nbr ctype
                                      (cffi-sys:null-pointer) 0
                                      (cffi-sys:null-pointer))
                          (cffi-sys:foreign-free ind-ptr))))))

(defgeneric sql-get-value (column)
  (:method ((column sql-column))
    (funcall (slot-value column 'getter))))

(defgeneric (setf sql-get-value) (value column)
  (:method (value (column sql-column))
    (funcall (slot-value column 'setter) value)))

(defgeneric make-buffer (column size))
(defgeneric ctype-code (column))


