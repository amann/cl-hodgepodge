;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;============================================================================



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
(defun ctypespecp (o &optional env)
  "Return T if o is a valid CFFI typespec in the global environment or in ENV."
  (declare (ignore env))
  (cffi::find-type-parser (if (consp o) (car o) o)))


(cffi:define-parse-method)

(defgeneric awl::alien-pointer (object &optional (offset 0))
  (:documentation "Return an alien pointer associated with OBJECT and with its address shifted by OFFSET."))


(defun awl::& (object &optional (offset 0))
  (declare (fixnum offset))
  (awl::alien-pointer object offset))

(define-compiler-macro awl::& (&whole form object &optional (offset 0)
                                      &environment environment)
  (if (constantp object)
      (if (constantp offset)
          (awl::alien-pointer object offset)
          `(cffi-sys:inc-pointer ,(awl::alien-pointer object) ,offset))
      form))

()




;#+ (or)
(awl:define-nested-binding-macro with-ffi-var ((name ctype &optional value))
  (awl:with-gensyms (g!name g!value)
    (multiple-value-bind (ctype-exp ctype-bind)
        (cond ((ctypespecp ctype) ctype)
              ((and (not (null value))
                    (constantp value)) (ctype-of value))
              (t (values g!ctype
                         `((,g!ctype (or ,ctype
                                         (ctype-of ,g!value)))))))
      `(let* ((,g!value ,value) (,g!count ,count) ,@ctype-bind)
         (declare (ignorable ,g!count))
         (cffi:with-foreign-object
             (,g!ptr ,ctype-exp ,count-exp)
           (flet ((,g!value ()
                    ,(expand-getter g!ptr ctype-exp
                                    count-exp))
                  ((setf ,g!value) (value)
                    ,(expand-setter 'value g!ptr ctype-exp
                                    count-exp)))
             (declare (inline ,g!value (setf ,g!value)))
             (symbol-macrolet ((,name (,g!value)))
               (when ,g!value
                 (setq ,name ,g!value))
               (awl::macrolet*
                   ((awl::& (&whole form var &optional (offset 0))
                            (if (eq var ',name)
                                (if (eql offset 0)
                                    ',g!ptr
                                    `(let ((offset ,offset))
                                       (cffi-sys:inc-pointer ,g!ptr offset)))
                                form))
                    (awl::size (&whole form var)
                               (if (eq var ',name)
                                   ',g!count
                                   form))
                    (awl::mref (var &optional idx)
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
                 (declare (inline awl::ref))
                 ,@body))))))))



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
                  (let ((count-exp (etypecase count
                                     (null 1)
                                     ((integer 1 #.array-dimension-limit) count)
                                     ((or cons symbol) g!count))))
                    `(let* ((,g!value ,value) (,g!count ,count) ,@ctype-bind)
                       (declare (ignorable ,g!count))
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
                                 ((& (var) (if (eq var ',name)
                                               ',g!ptr
                                               `(& ,var)))
                                  (size (var) (if (eq var ',name)
                                                  ',g!count
                                                  `(size ,var)))
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