;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")





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
    (sequence (loop :for (lisp-type ctype) :in '(((signed-byte 8) :int8)
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

#+ (or)
(eval-when (:compile-toplevel :load-toplevel :execute)  
  (flet ((expand-getter (ptr ctype &optional count)
           (if count
               `(do ((vector (make-array ,count))
                     (pos 0 (1+ pos)))
                    ((<= ,count pos) vector)
                  (declare (type fixnum pos))
                  (setf (svref vector pos)
                        (cffi:mem-aref ,ptr ,ctype pos)))
               `(cffi:mem-ref ,ptr ,ctype)))
         (expand-setter (value ptr ctype &optional count)
           (flet ((exp-loop (ref)
                    `(do ((pos 0 (1+ pos)))
                         ((<= ,count pos) ,value)
                       (declare (type fixnum pos))
                       (setf (cffi:mem-aref ,ptr ,ctype pos)
                             (,ref ,value pos)))))
             (if count
                 `(etypecase ,value
                    (simple-vector ,(exp-loop 'svref))
                    (vector ,(exp-loop 'aref)))
                 `(setf (cffi:mem-ref ,ptr ,ctype) ,value)))))
    (flet ((expand-getter (ptr ctype count)
             (if (numberp count)
                 (expand-getter ptr ctype (when (< 1 count) count))
                 `(if (and (numberp ,count) (< 1 ,count))
                      ,(expand-getter ptr ctype count)
                      ,(expand-getter ptr ctype))))
           (expand-setter (value ptr ctype count)
             (if (numberp count)
                 (expand-setter value ptr ctype (when (< 1 count) count))
                 `(if (and (numberp ,count) (< 1 ,count))
                      ,(expand-setter value ptr ctype count)
                      ,(expand-setter value ptr ctype)))))
      (defun expand-foreign-let (ptrs ptr/ctypes var value ctype count expr)
        (check-type var awl:variable "a valid variable")
        (let* ((g!ptr (awl:mkgnsym var "-PTR"))
               (g!ctype (awl:mkgnsym var "-TYPE")))
          (awl:with-gensyms (g!value g!count)
            (multiple-value-bind (ctype-exp ctype-bind)
                (cond ((and (not (null ctype))
                            (constantp ctype))
                       ctype)
                      ((and (not (null value))
                            (constantp value))
                       (ctype-of value))
                      (t
                       (values g!ctype `((,g!ctype (or ,ctype (ctype-of ,g!value)))))))
              (multiple-value-bind (count-exp count-bind)
                  (cond ((or (null count)
                             (and (numberp count)
                                  (<= count 1)))
                         1)
                        ((numberp count)
                         count)
                        (t
                         (values g!count `((,g!count ,count)))))
                (setf (getf (car ptrs) var) g!ptr
                      (getf (car ptr/ctypes) var) (list g!ptr ctype-exp count-exp))
                `(let* ((,g!value ,value)
                        ,@ctype-bind
                        ,@count-bind)
                   (cffi:with-foreign-object (,g!ptr ,ctype-exp ,count-exp)
                     (labels ((,g!value () ,(expand-getter g!ptr ctype-exp count-exp))
                              ((setf ,g!value) (value)
                                ,(expand-setter 'value g!ptr ctype-exp count-exp)))
                       (declare (inline ,g!value (setf ,g!value)))
                       (symbol-macrolet ((,var (,g!value)))
                         (when ,g!value
                           (setq ,var ,g!value))
                         ,expr)))))))))))

  

  (progn
    (defvar #1=#:*g!&* (list '#:&))
    (defvar #2=#:*g!cref* (list '#:cref))
    (defmacro foreign-let (bindings &body body &aux (*gensym-counter* 0))
      "BINDINGS is of the form ({(var value &optional ctype count)}*). In BODY var is bound to a foreign memory allocation containing the evaluation of value if non NIL. In BODY the macro (& var) returns the pointer to the memory allocation and, if var is a vector (i.e. if count is an integer > 1), the macro (cref var idx) returns the value at position idx of var."
      (let ((ptrs (list nil))
            (ptr/ctypes (list nil))
            (prev-g!& (first #1#))
            (curr-g!& (gensym "&"))
            (prev-g!cref (first #2#))
            (curr-g!cref (gensym "CREF")))
        (setf (first #1#) curr-g!&
              (first #2#) curr-g!cref)
        (reduce (awl:dlambda ((var value &optional ctype count) expr)
                  (expand-foreign-let ptrs ptr/ctypes var value ctype count expr))
                bindings
                :from-end t
                :initial-value 
                `(macrolet ((,curr-g!& (var)
                              (or (getf (first ',ptrs) var)
                                  `(handler-case
                                       (,',prev-g!& ,var)
                                     (unbound-variable ()
                                       (error 'foreign-let-pointer-error :format-arguments '(,var))))))
                            (& (var) `(,',curr-g!& ,var))
                            (,curr-g!cref (var idx)
                              (let ((ptr/ctype (getf (first ',ptr/ctypes) var)))
                                (if ptr/ctype
                                    (destructuring-bind (ptr ctype count)
                                        ptr/ctype
                                      (if (and (numberp idx) (numberp count)) 
                                          (if (typep idx `(mod ,count))
                                              `(cffi:mem-aref ,ptr ,ctype ,idx)
                                              (error 'foreign-let-out-of-range-error
                                                     :format-arguments (list idx var count)))
                                          `(let ((idx ,idx)
                                                 (count ,count))
                                             (if (typep idx `(mod ,count))
                                                 (cffi:mem-aref ,ptr ,ctype idx)
                                                 (error 'foreign-let-out-of-range-error
                                                        :format-arguments (list idx ',var count))))))
                                    `(handler-case
                                         (,',prev-g!cref ,var ,idx)
                                       (unbound-variable ()
                                         (error 'foreign-let-vector-error :format-arguments '(,var)))))))
                            (cref (var idx) `(,',curr-g!cref ,var ,idx)))
                   ,@body)))))

  (define-condition foreign-let-program-error (program-error simple-condition) ())
  (define-condition foreign-let-pointer-error (foreign-let-program-error) ()
    (:default-initargs :format-control "No pointer is associated with the symbol ~S."))
  (define-condition foreign-let-vector-error (foreign-let-program-error) ()
    (:default-initargs :format-control "The variable ~S does not designate a foreign vector.")))


(defun ctypespecp (o &optional env)
  "Return T if o is a valid CFFI typespec in the current environment or in ENV."
  (declare (ignore env))
  (and (not (null o))
       (constantp o)))

(flet ((expand-getter (ptr ctype &optional count)
         (if count
             `(do ((vector (make-array ,count))
                   (pos 0 (1+ pos)))
                  ((<= ,count pos) vector)
                (declare (type fixnum pos))
                (setf (svref vector pos)
                      (cffi:mem-aref ,ptr ,ctype pos)))
             `(cffi:mem-ref ,ptr ,ctype)))
       (expand-setter (value ptr ctype &optional count)
         (flet ((exp-loop (ref)
                  `(do ((pos 0 (1+ pos)))
                       ((<= ,count pos) ,value)
                     (declare (type fixnum pos))
                     (setf (cffi:mem-aref ,ptr ,ctype pos)
                           (,ref ,value pos)))))
           (if count
               `(etypecase ,value
                  (simple-vector ,(exp-loop 'svref))
                  (vector ,(exp-loop 'aref)))
               `(setf (cffi:mem-ref ,ptr ,ctype) ,value)))))
  (flet ((expand-getter (ptr ctype count)
           (if (numberp count)
               (expand-getter ptr ctype (when (< 1 count) count))
               `(if (and (numberp ,count) (< 1 ,count))
                    ,(expand-getter ptr ctype count)
                    ,(expand-getter ptr ctype))))
         (expand-setter (value ptr ctype count)
           (if (numberp count)
               (expand-setter value ptr ctype (when (< 1 count) count))
               `(if (and (numberp ,count) (< 1 ,count))
                    ,(expand-setter value ptr ctype count)
                    ,(expand-setter value ptr ctype)))))
    (defmacro foreign-let (&whole form (&rest bindings) &body body)
      "Bind locally variables to a value hold in foreign memory. BINDINGS is a list of elements of the form (name value &optional ctype count) where name is the name of the bound variable, value is the initial value - if value is null the foreign memory is not initialized - ctype is the foreign type - etc..."
      (let ((body (let ((rest-bindings (rest bindings)))
                    (if rest-bindings
                        `((,(first form) ,rest-bindings ,@body))
                        body))))
        (if bindings
            (destructuring-bind (name value &optional ctype count)
                (first bindings)
              (awl:with-gensyms (g!ptr g!value g!ctype g!count)
                (multiple-value-bind (ctype-exp ctype-bind)
                    (cond ((ctypespecp ctype) ctype)
                          ((and (not (null value))
                                (constantp value)) (ctype-of value))
                          (t (values g!ctype
                                     `((,g!ctype (or ,ctype
                                                     (ctype-of ,g!value)))))))
                  (multiple-value-bind (count-exp count-bind)
                      (etypecase count
                        (null 1)
                        ((integer 1 #.array-dimension-limit) count)
                        ((or cons symbol) (values g!count
                                                  `((,g!count ,count)))))
                    `(let* ((,g!value ,value) ,@ctype-bind ,@count-bind)
                       (cffi:with-foreign-object
                           (,g!ptr ,ctype-exp ,count-exp)
                         (labels ((,g!value ()
                                    ,(expand-getter g!ptr ctype-exp
                                                    count-exp))
                                  ((setf ,g!value) (value)
                                    ,(expand-setter 'value g!ptr ctype-exp
                                                    count-exp)))
                           (declare (inline ,g!value (setf ,g!value)))
                           (symbol-macrolet ((,name (,g!value)))
                             (when ,g!value
                               (setq ,name ,g!value))
                             (awl:macrolet*
                                 ((& (var)
                                     (if (eq var ',name)
                                         ',g!ptr
                                         `(& ,var)))
                                  (cref (var idx)
                                    (if (eq var ',name)
                                        `(cffi:mem-aref
                                          ,',g!ptr ,',ctype-exp
                                          (let ((idx ,idx)
                                                (count ,',count-exp))
                                            ,(if (and (numberp idx)
                                                      (numberp ',count-exp))
                                                 (unless (typep idx `(mod ,count-exp))
                                                   (error 'type-error :datum idx
                                                          :expected-type `(mod ,count-exp)))
                                                 `(unless (typep idx `(mod ,count))
                                                    (error 'type-error :datum idx
                                                           :expected-type `(mod ,count))))
                                            idx))
                                        `(cref ,var ,idx))))
                               ,@body)))))))))
            `(progn ,@body))))))


(defun null-pointer () (cffi:null-pointer))
(defmacro & (var) (etypecase var
                    (null `(cffi:null-pointer))))

;;;;* Buffers
;;;; A thin wrapper for buffer pointers. The memory recource is automatically
;;;; freed at garbage collection.

(defclass ff-buffer-class (c2mop:funcallable-standard-class)
  ((factory :initarg :factory :reader ff-buffer-class-factory)))
(defmethod c2mop:validate-superclass
    ((class ff-buffer-class) (superclass c2mop:funcallable-standard-class))
  t)
(defclass ff-buffer () ()
  (:metaclass ff-buffer-class))
(defmethod initialize-instance :after
    ((self ff-buffer) &key (size 1) &allow-other-keys)
  (c2mop:set-funcallable-instance-function self
   (funcall (ff-buffer-class-factory (class-of self)) size)))

(defgeneric size (o)
  (:method ((o ff-buffer))
    (funcall o :size)))
(defgeneric (setf size) (v o))
(defgeneric value (o)
  (:method ((o ff-buffer))
    (funcall (funcall o :getter))))
(defgeneric (setf value) (v o)
  (:method (value (o ff-buffer))
    (funcall (funcall o :setter) v)))

(defmacro make-ff-buffer-factory ((var ctype) &body body
                                  &key getter setter)
  (declare (ignore body))
  (awl:with-gensyms (g!size g!ctype g!ptr)
    `(lambda (&key ((size ,g!size) 1) &aux
              (,g!size ,g!size)
              (,g!ctype ,ctype)
              (,g!ptr (cffi:foreign-alloc ,(if (constantp ctype) ctype g!ctype)
                                          :count ,g!size)))
       (tg:finalize 
        (awl:macrolet*
            ((size (var) (if (eq var ',var)
                             ',g!size
                             `(size ,var)))
             (& (var) (if (eq var ',var)
                          ',g!ptr
                          `(& ,var)))
             (cref (var &optional (idx 0))
               (if (eq var ',var)
                   `(cffi:mem-aref ,',g!ptr ,',ctype
                                   (let ((idx ,idx)
                                         (in-range `(mod ,,',g!size)))
                                     (unless (typep idx in-range)
                                       (error 'type-error :datum idx
                                              :expected-type in-range))
                                     idx))
                   `(cref ,var ,idx))))
          (let ((getter ,getter)
                (setter ,setter))
            (check-type getter function)
            (check-type setter function)
            (lambda (method)
              (case method
                (:getter getter)
                (:setter setter)
                (:size ,g!size)
                (:ptr ,g!ptr)
                (:ctype ,g!ctype)))))
        (lambda ()
          (cffi:foreign-free ,g!ptr))))))



(defmacro define-ff-buffer (name (&rest superclasses) (var ctype)
                            &body body &key documentation getter setter)
  "Define a new ff-buffer class with name NAME. In BODY the symbol given in VAR is used to name the pointer of the buffer for which following key values can be accessed:
- (1) the pointer itself by (& <VAR>),
- (2) the size of the allocated foreign memory by (SIZE <VAR>) and
- (3) the value stored at the ith position in the foreign memory by (cref <VAR> &optional (i 0)).
The :SETTER and :GETTER keys must be set to respectively a thunk returning the value of the buffer as a whole and a function in one argument taking the value to be stored in the buffer. These getter and setter are metaclass properties and are not inherited from SUPERCLASSES."
  (declare (ignore body))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (prog1
         (defclass ,name ,(adjoin 'ff-buffer superclasses
                                  :test (lambda (x y) (subtypep y x)))
           () ,@(when documentation `((:documentation documentation)))
           (:metaclass ff-buffer-class))
       (setf (slot-value (find-class ',name) 'factory)
             (make-ff-buffer-factory (,var ,ctype)
               :getter ,getter :setter ,setter)))))
#+ (or)
(make-ff-buffer-factory (foo :uint8)
  :getter (lambda ())
  :setter (lambda (v)))


(defun make-ff-buffer (class &rest initargs &key (size 1) &allow-other-keys)
  (apply #'make-instance class :size size initargs))

(defmacro with-ff-buffers (&whole form (&rest buffers) &body body)
  "BUFFERS is of the form {(buffer ff-buffer-or-class &rest args)}*. buffer  are symbols which, in BODY will be bound to the buffer pointer and the length of the buffer memory in bytes respectively. ff-buffer-or-class, evaluated, is either a ff-buffer object or a symbol designating an ff-buffer class or a ff-buffer class itself. args are the initargs of the class. The return values are the created buffers in the order of apearance in BUFFERS."
  (let ((body (let ((rest (rest buffers)))
                (if rest
                    `((,(first form) ,rest ,@body))
                    body))))
    (if buffers
        (destructuring-bind (var expr)
            (awl:ensure-list (first buffers))
          (awl:with-gensyms (g!buffer g!getter g!setter g!size g!ptr)
            `(let ((,g!buffer ,expr))
               (awl:fbind ((,g!getter (funcall ,g!buffer :getter))
                           (,g!setter (funcall ,g!buffer :setter)))
                 (declare (ftype (function ()) ,g!getter)
                          (ftype (function (*)) ,g!setter))
                 (flet (((setf ,g!getter) (v) (,g!setter v)))
                   (let ((,g!size (funcall ,g!buffer :size))
                         (,g!ptr (funcall ,g!buffer :ptr))
                         (,g!ctype (funcall ,g!buffer :ctype)))
                     (awl:macrolet* ((& (var) (if (eq var ',var)
                                                  ',g!ptr
                                                  `(& ,var)))
                                     (size (var) (if (eq var ',var)
                                                     ',g!size
                                                     `(size ,var)))
                                     (ctype (var) (if (eq var ',var)
                                                      ',g!ctype
                                                      `(ctype ,var))))
                       (symbol-macrolet ((,var (g!getter)))
                         ,@body))))))))
        `(progn ,@body))))
