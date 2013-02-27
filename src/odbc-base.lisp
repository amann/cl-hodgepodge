;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")



(define-condition sql-condition (condition)
  ((message :initarg :message :reader sql-condition-message)
   (code :initarg :code :reader sql-condition-code)
   (state :initarg :state :reader sql-condition-state))
  (:report (lambda (condition stream)
             (with-slots (message code stream)
                 condition
               (format stream "~A; Code: ~A; State: ~A."
                       message code stream)))))

(define-condition sql-error (sql-condition) ())
(define-condition sql-warning (sql-condition) ())

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

(defstruct (sql-diag-rec :include sql-diag
                         :prefix sql-diag)
  sqlstate
  native
  message-text
  class-origin
  subclass-origin
  connection-name
  server-name)

(defstruct (sql-stmnt-diag-rec :include sql-diag-rec
                               :prefix sql-diag)
  row-number column-number)
(dolist (class '(sql-stmnt-diag-rec sql-stmnt-diag-header))
  (dolist (slot (c2mop:class-slots (find-class )))
    (pushnew (c2mop:slot-definition-name slot)
             (slot-value slot 'sb-pcl::initargs))))

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




(defun ctype-of (x)
  (typecase x
    ((signed-byte 8) :int8)
    ((unsigned-byte 8) :uint8)
    ((signed-byte 16) :int16)
    ((unsigned-byte 16) :uint16)
    ((signed-byte 32) :int32)
    ((unsigned-byte 32) :uint32)
    ((signed-byte 64) :int64)
    ((unsigned-byte 64) :uint64)
    (short-float :float)
    (double-float :double)
    (string `(:string ,(length x)))
    (vector (loop :for (lisp-type ctype) :in '(((signed-byte 8) :int8)
                                               ((unsigned-byte 8) :uint8)
                                               ((signed-byte 16) :int16)
                                               ((unsigned-byte 16) :uint16)
                                               ((signed-byte 32) :int32)
                                               ((unsigned-byte 32) :uint32)
                                               ((signed-byte 64) :int64)
                                               ((unsigned-byte 64) :uint64)
                                               (short-float :float)
                                               (double-float :double))
               :do (when (every (lambda (elt)
                                  (typep elt lisp-type))
                                x)
                     (return (list ctype (length x))))
               :finally (error "Unsupported vector type ~A" x)))
    (t (error "Unable to find ctype for ~A." x))))
(defun ensure-composed-type (x)
  (if (listp x)
      x
      (list x 1)))



(flet ((expand-getter (ptr ctype &optional count)
         (if count
             `(do ((vector (make-array ,count))
                   (pos 0 (1+ pos)))
                  ((<= ,count pos) vector)
                (setf (svref vector pos)
                      (cffi:mem-aref ,ptr ,ctype pos)))
             `(cffi:mem-ref ,ptr ,ctype)))
       (expand-setter (value ptr ctype &optional count)
         (if count
             `(do ((pos 0 (1+ pos)))
                  ((<= ,count pos) value)
                (setf (cffi:mem-aref ,ptr ,ctype pos)
                      (aref ,value pos)))
             `(setf (cffi:mem-ref ,ptr ,ctype) ,value)))
       (1< (count) (and (numberp count) (< 1 count))))
  (flet ((expand-getter (g!ptr g!ctype g!count count)
           (if (constantp count)
               (expand-getter g!ptr g!ctype (when (1< count)
                                              count))
               `(if (and (numberp ,g!count)
                         (< 1 ,g!count))
                    ,(expand-getter g!ptr g!ctype g!count)
                    ,(expand-getter g!ptr g!ctype))))
         (expand-setter (value g!ptr g!ctype g!count count)
           (if (constantp count)
               (expand-setter value g!ptr g!ctype (when (1< count)
                                                    count))
               `(if (and (numberp ,g!count)
                         (< 1 ,g!count))
                    ,(expand-setter value g!ptr g!ctype g!count)
                    ,(expand-setter value g!ptr g!ctype)))))
    (defun expand-foreign-let (var g!ptr value ctype g!ctype count body)
      (check-type var oam:variable "a valid variable")
      (oam:with-gensyms (g!value g!count)
        `(let ((,g!value ,value)
               (,g!ctype ,ctype)
               (,g!count ,count))
           (cffi:with-foreign-object (,g!ptr ,g!ctype ,g!count)
             (labels ((,g!value () ,(expand-getter g!ptr g!ctype g!count count))
                      ((setf ,g!value) (value)
                        ,(expand-setter value g!ptr g!ctype g!count count)))
               (symbol-macrolet ((,var (,g!value)))
                 (when ,g!value
                   (setq ,var ,g!value))
                 ,@body))))))))

(defun %add-ptr (var ptr ctype)
  (declare (special ptr<-var))
  (setf (getf ptr<-var var) (list ptr ctype)))
(defun %get-ptr (var)
  (declare (special ptr<-var))
  (values-list (getf ptr<-var var)))

(defmacro foreign-let (bindings &body body &environment env)
  "BINDINGS is of the form ({(var value &optional ctype count)}*). In BODY var is bound to a foreign memory allocation containing the evaluation of value"
  (declare (special ptr<-var))
  (let* ((*gensym-counter* 0)
         (ptr<-var (when (boundp 'ptr<-var) ptr<-var))
         (body (typecase (macroexpand-1 body env)
                 ((cons (eql foreign-let) list) body)
                 (t `((macrolet ((& (var) (%get-ptr var))
                                 (cref (var idx)
                                   (multiple-value-bind (ptr ctype)
                                       (%get-ptr var)
                                     `(cffi:mem-aref ,ptr ,ctype ,idx)))
                                 ((setf cref) (value var idx)
                                   (multiple-value-bind (ptr ctype)
                                       (%get-ptr var)
                                     `(setf (cffi:mem-aref ,ptr ,ctype ,idx) ,value))))
                        ,@body))))))
    (declare (special ptr<-var))
    (reduce (lambda (binding body)
              (destructuring-bind (var value &optional ctype count)
                  binding
                (let ((g!ptr (oam:mkgnsym var))
                      (g!ctype (oam:mkgnsym var "-TYPE")))
                  (%add-ptr var g!ptr g!ctype)
                  (expand-foreign-let var g!ptr value ctype g!ctype count body))))
            bindings
            :from-end t
            :initial-value body)))

#+(or)
(defmacro foreign-let (bindings &body body)
  (let* ((*gensym-counter* 0)
         (g!ptr (gensym "PTR"))
         (bindings (mapcar (lambda (b)
                             (destructuring-bind (var value &optional
                                                      ctype (count 1))
                                 b
                               (let ((g!val (gensym (string var))))
                                 (list var (gensym (string var))
                                       value g!val
                                       (if ctype
                                           `(list ,ctype ,count)
                                           `(ensure-composed-type
                                             (ctype-of ,g!val)))
                                       (gensym (string var))))))
                           bindings)))
    (flet ((var (b) (first b))
           (g!ptr (b) (second b))
           (value (b) (third b))
           (g!val (b) (fourth b))
           (ctype (b) (fifth b))
           (g!ctype (b) (sixth b)))
      (dolist (b bindings)
        (setf (get (var b) g!ptr) (g!ptr b)))
      `(let ,(mapcar (lambda (b)
                       `(,(g!ctype b) ,(ctype b)))
                     bindings)
         (cffi:with-foreign-objects ,(mapcar (lambda (b)
                                               `(,(g!ptr b)
                                                  (first ,(g!ctype b))
                                                  (second ,(g!ctype b))))
                                             bindings)
           (let ,(mapcar (lambda (b)
                           `(,(g!val b)
                              (let ((ctype (first ,(g!ctype b)))
                                    (count (second ,(g!ctype b))))
                                (if (and (numberp count)
                                         (< 1 count))
                                    (lambda ()
                                      (do ((vector (make-array count))
                                           (pos 0 (1+ pos)))
                                          ((<= count pos) vector)
                                        (setf (svref vector pos)
                                              (cffi:mem-aref ,(g!ptr b)
                                                             ctype pos))))
                                    (lambda ()
                                      (cffi:mem-ref ,(g!ptr b) ctype))))))
                         bindings)
             (flet ,(mapcar (lambda (b)
                              `(,(g!val b) () (funcall ,(g!val b))))
                            bindings)
               (let ,(mapcar (lambda (b)
                               `(,(g!val b)
                                  (let ((ctype (first ,(g!ctype b)))
                                        (count (second ,(g!ctype b))))
                                    (if (and (numberp count)
                                             (< 1 count))
                                        (lambda (value)
                                          (do ((max-pos (if (vectop value)
                                                            (min (length value)
                                                                 count)
                                                            count))
                                               (pos 0 (1+ pos)))
                                              ((<= max-pos pos)
                                               value)
                                            (setf (cffi:mem-aref ,(g!ptr b)
                                                                 ctype pos)
                                                  (if (vectop value)
                                                      (aref value pos)
                                                      value))))
                                        (lambda (value)
                                          (setf (cffi:mem-ref ,(g!ptr b)
                                                              ctype)
                                                value))))))
                             bindings)
                 (flet ,(mapcar (lambda (b)
                                  `((setf ,(g!val b)) (value)
                                    (funcall ,(g!val b) value)))
                                bindings)
                   (symbol-macrolet ,(mapcar (lambda (b)
                                               `(,(g!ptr b) (,(g!val b))))
                                             bindings)
                     (macrolet ((& (var) (get var ,g!ptr)))
                       (let ,(mapcar (lambda (b)
                                       `(,(g!val b) ,(value b)))
                                     bindings)
                         ,@(mapcar (lambda (b)
                                     `(when ,(g!val b)
                                        (setf (,(g!val b)) ,(g!val b))))
                                   bindings))
                       ,@body)))))))))))




(defun sql-get-diag-field-value (handle-type handle rec-number field)
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
        (t diag-info)))
    (cffi:with-foreign-object
        (string-length #.SQLSMALLINT)
      (cffi:with-foreign-object (diag-info field-type)
        (case (SQLGetDiagField handle-type handle
                               rec-number field-code
                               diag-info
                               0 string-length)
          ((#.SQL_SUCCESS_WITH_INFO #.SQL_SUCCESS)
           (case field-type
             (:string
              (let ((info-length (1+ (cffi:mem-ref string-length #.SQLSMALLINT))))
                (cffi:with-foreign-pointer-as-string
                    (diag-info info-length)
                  (SQLGetDiagField handle-type handle
                                   rec-number field-code
                                   diag-info info-length
                                   string-length))))
             (t
              (cffi:mem-ref diag-info field-type)))))))))


(defun sql-get-diag-rec (handle-type handle rec-number)
  (apply 'make-instance diag-rec-class
         (mapcan (lambda (slot &aux
                          (name (c2mop:slot-definition-name slot)))
                   (list
                    name
                    (sql-get-diag-field-value handle-type handle
                                              rec-number
                                              (find-sql-diag-field name))))
                 (c2mop:class-slots (find-class diag-rec-class)))))


(defun sql-get-diag (sql-handle)
  (let ((handle (sql-handle-pointer sql-handle))
        (handle-type (sql-handle-type sql-handle)))
    (cons ())))



((cffi:with-foreign-objects ((array :int 10))
   (dotimes (i 10)
     (setf (mem-aref array :int i) (random 100)))))




(defclass sql-handle ()
  (pointer (code :reader sql-handle-code)))
(defclass sql-environment (sql-handle)
  ())
(defclass sql-connection (sql-handle)
  (environment :reader sql-environment))


(defmethod initialize-instance :after ())


