;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")







(defun canonize-slot-specification (class-name slots env)
  "Return canonized slot specifications suitable as :direct-slots argument for ensure-class or make-instance."
  (let (sb-pcl::*readers-for-this-defclass*
        sb-pcl::*writers-for-this-defclass*
        sb-pcl::*slot-names-for-this-defclass*
        sb-pcl::*initfunctions-for-this-defclass*)
    (sb-pcl::canonize-defclass-slots class-name slots env)))


;;;;* Property Structure
;;;; A Property Structure is an object composed of predefined properties.
;;;; Each property associates a name (symbol) to a value of a precise type.
;;;; The predefined properties are stored in a property set.  Multiple
;;;; property sets can be defined.  In principle, they are named and accessed
;;;; by their name.  A property set is an instance of AWL:PROPERTY-SET and
;;;; can be defined using DEFCLASS with the :metaclass option AWL:PROPERTY-SET.
;;;; Property structures are instances of such property sets as defined above
;;;; and can be defined using DEFCLASS with the :metaclass option the name of the
;;;; property set.
;;;;
;;;; The property structure objects hierarchy is defined on three levels:
;;;; 1. The object level: Property structure objects inherit of the class
;;;;    STANDARD-PROPERTY-STRUCTURE.
;;;; 2. The class level: Property structure classes inherit of the class
;;;;    STANDARD-PROPERTY-SET.
;;;; 3. The metaclass level: the metaclass of property structure classes
;;;;    is the class PROPERTY-SET.
;;;;
;;;; The class hierarchy is
;;;;
;;;;           STANDARD-OBJECT
;;;;                 U
;;;;       AWL:STANDARD-PROPERTY-STRUCTURE   STANDARD-CLASS
;;;;                 |                          U    |
;;;;                 U    AWL:STANDARD-PROPERTY-SET  U
;;;;                 |              U                |
;;;; #<p-struct> € p-struct   €   p-set   €   AWL:PROPERTY-SET
;;;;
;;;; where € signifies "is instance of" and U signifies "is subtype of".

;;;;** Property Structure Objects
;;;;
;;;; All property structure objects are of a subtype of AWL::STANDARD-PROPERTY-STRUCTURE
(defclass awl::standard-property-structure () ()
  (:metaclass awl:abstract-class))
;;;;*** Initialization of Property Structure Objects
;;;;
;;;; The only particularity in the initialization process of property structure objects
;;;; is the binding of the special variable PROPERTY-STRUCTURE to the instance to be
;;;; initialized. This special variable is used for storing the instance into the error
;;;; object which is signaled in case a mandatory property has not received any value.

(defmethod shared-initialize :around ((instance awl::standard-property-structure) slot-names
                                      &key &allow-other-keys)
  "Bind the special variable PROPERTY-STRUCTURE to the instance and call next method. The special variable PROPERTY-STRUCTURE is used for the thunk returned by MANDATORY-PROPERTY-INITFUNCTION."
  (let ((property-structure instance))
    (declare (special property-structure))
    (call-next-method)))

;;;;*** Error Condition for non-initialised Mandatory Properties
;;;;
;;;; Properties for which no default value has been given by mean of
;;;; an :initform or some :default-initargs directive are considered
;;;; mandatory and will signal an error if no value is passed to them
;;;; via MAKE-INSTANCE.
(define-condition awl::mandatory-property-error (error)
  ((property-slot :initarg property-slot
                  :reader awl::mandatory-property-error-slot)
   (property-definition :initarg property-definition
                        :reader awl::mandatory-property-error-definition)
   (property-structure :initarg property-structure
                       :reader awl::mandatory-property-error-structure))
  (:report (lambda (condition stream)
             (format stream "~@<Property ~2I~_~S~I~_ in structure ~2I~_~S~I~_ is manadory.~:>"
                     (c2mop:slot-definition-name (awl::mandatory-property-error-slot condition))
                     (class-name (class-of (awl::mandatory-property-error-structure condition))))))
  (:documentation "Error condition signalled when a mandatory property has not received any value."))

(defun mandatory-property-initfunction (property-slot property-definition)
  "Return a thunk without arguments closing over the variables PROPERTY-SLOT and PROPERTY-DEFINITION and using the special variable PROPERTY-STRUCTURE which will be bound to the property-structure which is initialised in SHARED-INITIALIZE."
  (lambda ()
    (declare (special property-structure))
    (error 'awl::mandatory-property-error
           'property-slot property-slot
           'property-definition property-definition
           'property-structure property-structure)))

;;;;** Classes of Property-Structure Objects (Standard-Property-Set)
;;;;
;;;; Classes of property-structure objects are a subtype of AWL:STANDARD-PROPERTY-SET and
;;;; an instance of a property set (c.f. next section).
;;;; The slots of those classes are initialised with the particularity that
;;;; those slots whose name is associated to a property description are of the
;;;; type STRUCTURE-PROPERTY and are initialized using the information held in
;;;; the property description object (:initfunction, :type, :allocation, etc.).
;;;;
;;;; The association (slot-name . property-description) is held in the property set
;;;; metaclass of which the standard-property-set object is an instance.
(defclass awl::standard-property-set (standard-class) ()
  (:metaclass awl:abstract-class)
  (:documentation "Property Sets contain the property definitions in :class allocated slots."))

(defmethod c2mop:validate-superclass ((class awl::standard-property-set) (superclass awl:abstract-class))
  (or (eq superclass (find-class 'awl::standard-property-structure))
      (call-next-method)))
(defmethod c2mop:validate-superclass ((class awl::standard-property-set) (superclass standard-class)) t)
;;;;*** Initialization of Classes of Property-Structure Objects (Standard-Property-Set)
;;;;
;;;; The initialization of a standard-property-set class must insure that it
;;;; inherits of AWL:STANDARD-PROPERTY-STRUCTURE. The initialization of the
;;;; slots is decribed in the next section "Slots of Property Structures."
(defun adjoin-to-superclasses (class-name superclasses)
  "Return the list SUPERCLASSES maybe augmented by the class denominated by CLASS-NAME if CLASS-NAME is not a supertype of any of the classes in SUPERCLASSES."
  (adjoin (find-class class-name) superclasses
          :test (lambda (class superclass)
                  (subtypep superclass class))))

(defmethod initialize-instance :around ((instance awl::standard-property-set) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  "Ensure that INSTANCE inherits from AWL::STANDARD-PROPERTY-STRUCTURE."
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :direct-superclasses
         (adjoin-to-superclasses 'awl::standard-property-structure direct-superclasses)
         initargs))
(defmethod reinitialize-instance :around ((instance awl::standard-property-set) &rest initargs
                                          &key (direct-superclasses nil direct-superclasses-p)
                                          &allow-other-keys)
  "Ensure that INSTANCE inherits from AWL::STANDARD-PROPERTY-STRUCTURE."
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (apply #'call-next-method instance
             :direct-superclasses
             (adjoin-to-superclasses 'awl::standard-property-structure direct-superclasses)
             initargs)
      (call-next-method)))

;;;;** Slots of Property Structures
;;;;*** Property Definitions
;;;;
;;;; The information about the initialization of property slots is
;;;; contained in a PROPERTY-DEFINITION object. The association
;;;; (property-name . property-definition) is stored in a AWL:PROPERTY-SET
;;;; metaobject of which the standard-property-set is an instance.
(defclass property-definition () 
  ((name :initarg :name
         :reader property-definition-name)
   (namestring :initarg :namestring
                :reader property-definition-namestring)
   (initargs :initform nil
             :initarg :initargs 
             :reader property-definition-initargs)
   (type :initform t
         :initarg :type
         :reader property-definition-type)
   (initform :initform nil
             :initarg :initform
             :reader property-definition-initform)
   (initfunction :initform nil
                 :initarg :initfunction
                 :reader property-definition-initfunction)
   (allocation :initform :instance
               :initarg :allocation
               :reader property-definition-allocation)
   (documentation :initform nil
                  :initarg :documentation
                  :reader property-definition-documentation))
  (:documentation "Objects containing the specifications of the properties."))

(defmethod shared-initialize :after ((instance property-definition) slot-names
                                     &key name &allow-other-keys)
  (unless (slot-boundp instance 'namestring)
    (setf (slot-value instance 'namestring) (string name))))

(defun make-property-definition (property-name canonized-direct-slot)
  (apply 'make-instance 'property-definition
         :name property-name
         (mapcan (lambda (initarg)
                   (let ((value (getf canonized-direct-slot initarg)))
                     (when value
                       (list initarg value))))
                 '(:namestring :initargs :initform :initfunction
                   :type :allocation :documentation))))

(defgeneric find-property-definition (object name)
  (:documentation "Return the property definition object associated to NAME for the object OBJECT.")
  (:method (object name))
  (:method ((object awl::standard-property-set) name)
    (let ((slot-value (slot-value object name)))
      (when (typep slot-value 'property-definition)
        slot-value)))
  (:method ((object awl::standard-property-structure) name)
    (find-property-definition (class-of object) name)))
(declaim (ftype (function (* *) (or null property-definition)) find-property-definition))

;;;;*** Effective Slot Definition of the Property Slots
(defclass structure-property (c2mop:standard-effective-slot-definition) (namestring)
  (:documentation "The class of those effective slot-definitions which define the properties of the property structure."))

(defmethod c2mop:compute-effective-slot-definition :around ((class awl::standard-property-set)
                                                            name direct-slot-definitions)
  "Bind the property-definition named NAME of CLASS to the special variable PROPERTY-DEFINITION and call next method."
  (let ((property-definition (find-property-definition class name)))
    (declare (special property-definition))
    (call-next-method)))

(defmethod c2mop:effective-slot-definition-class ((class awl::standard-property-set)
                                                  &key &allow-other-keys)
  "If the special variable PROPERTY-DEFINITION is non nil, then the slot definition class must be the class STRUCTURE-PROPERTY."
  (declare (special property-definition))
  (if property-definition
      (find-class 'structure-property)
      (call-next-method)))

(defmethod initialize-instance :after ((instance structure-property)
                                       &key &allow-other-keys)
  "Fill the slots of the structure-property INSTANCE with the information contained in the special variable PROPERTY-DEFINITION."
  (declare (special property-definition))
  ;;(declare (type property-definition property-definition))
  (with-accessors ((namestring property-definition-namestring)
                   (initargs property-definition-initargs)
                   (type property-definition-type)
                   (allocation property-definition-allocation)
                   (initform property-definition-initform)
                   (initfunction property-definition-initfunction)
                   (documentation property-definition-documentation))
      property-definition
    (with-slots ((slot-namestring namestring))
        instance
      (with-accessors ((slot-name c2mop:slot-definition-name)
                       (slot-initargs c2mop:slot-definition-initargs)
                       (slot-type c2mop:slot-definition-type)
                       (slot-allocation c2mop:slot-definition-allocation)
                       (slot-initform c2mop:slot-definition-initform)
                       (slot-initfunction c2mop:slot-definition-initfunction))
          instance
        ;;======================================================
        (setf slot-namestring namestring
              slot-initargs (adjoin slot-name (union initargs slot-initargs))
              slot-type type)
        (unless (eq slot-allocation :class)
          (setf slot-allocation allocation))
        (unless slot-initfunction
          (setf slot-initform (or initform :mandatory)
                slot-initfunction (or initfunction
                                      (mandatory-property-initfunction instance
                                                                       property-definition))))))
    (unless (documentation instance t)
      (setf (documentation instance t) documentation))))

;;;;** Property Set
;;;;
;;;; Property Set is the metaclass of the Property Structure classes.

(defclass awl::property-set (standard-class) ()
  (:metaclass standard-class)
  (:documentation "Metaclass of the property structure classes. The slots are :class allocated and contain the property-definitions."))

(defmethod c2mop:validate-superclass ((class awl::property-set) (superclass awl:abstract-class))
  (or (eq superclass (find-class 'awl::standard-property-set))
      (call-next-method)))
(defmethod c2mop:validate-superclass ((class awl::property-set) (superclass standard-class)) t)

(defun initialize-property-definitions (direct-slots)
  (mapcar (lambda (slot)
            (let* ((property-name (getf slot :name))
                   (property-definition (make-property-definition property-name slot)))
              (list :name property-name
                    :initform `(quote ,property-definition)
                    :initfunction (constantly property-definition)
                    :allocation :class)))
          direct-slots))

(defmethod initialize-instance :around ((instance awl::property-set) &rest initargs
                                        &key direct-superclasses direct-slots &allow-other-keys)
  "Ensure that INSTANCE inherits from AWL::STANDARD-PROPERTY-SET."
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :direct-superclasses
         (adjoin-to-superclasses 'awl::standard-property-set direct-superclasses)
         :direct-slots
         (initialize-property-definitions direct-slots)
         initargs))
(defmethod reinitialize-instance :around ((instance awl::property-set) &rest initargs
                                          &key (direct-superclasses nil direct-superclasses-p)
                                          direct-slots &allow-other-keys)
  "Ensure that INSTANCE inherits from AWL::STANDARD-PROPERTY-SET."
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (apply #'call-next-method instance
             :direct-superclasses
             (adjoin-to-superclasses 'awl::standard-property-set direct-superclasses)
             :direct-slots
             (initialize-property-definitions direct-slots)
             initargs)
      (call-next-method)))


;;;;** User Interface
(define-condition awl::not-found-property-error (error)
  ((property-name :initarg :name
                  :reader awl::property-error-name)
   (property-structure :initarg :structure
                       :reader awl::property-error-structure)
   (property-set :initarg :set
                 :reader awl::property-error-set))
  (:report (lambda (condition stream)
             (format stream "~@<Property ~2I~_~S~I~_ could not be found in property structure ~2I~_~S.~:>"
                     (awl::property-error-name condition) (awl::property-error-structure condition)))))
(define-condition awl::undefined-property-error (awl::not-found-property-error) ()
  (:report (lambda (condition stream)
             (format stream "~@<Property ~2I~_~S~I~_ is undefined in property set ~2I~_~S.~:>"
                     (awl::property-error-name condition) (awl::property-error-set condition)))))

(defgeneric find-property-slot (object property-name)
  (declare (optimize speed))
  (:method ((object awl::standard-property-set) (property-name symbol))
    (let ((value (find property-name (c2mop:class-slots object)
                       :key 'c2mop:slot-definition-name)))
      (if (typep value 'structure-property)
          value
          (error (if (find-property-definition object property-name)
                     'awl::not-found-property-error
                     'awl::undefined-property-error)
                 :set (class-of object)
                 :structure object
                 :name property-name))))
  (:method ((object awl::standard-property-structure) (property-name symbol))
    (find-property-slot (class-of object) property-name)))
(declaim (ftype (function (* *) structure-property) find-property-slot))

(defgeneric get-property-slots (object)
  (declare (optimize speed))
  (:method ((object awl::standard-property-set))
    (remove-if-not (lambda (s)
                     (typep s 'structure-property))
                   (c2mop:class-slots object)))
  (:method ((object awl::standard-property-structure))
    (get-property-slots (class-of object))))

(defun properties (object)
  "Return an association list of conses (property-name . property-value)."
  (let ((class (class-of object)))
    (mapcar (lambda (slotd)
              (declare (type c2mop:standard-effective-slot-definition slotd))
              (cons (c2mop:slot-definition-name slotd)
                    (c2mop:slot-value-using-class class object slotd)))
            (get-property-slots class))))
(declaim (inline awl::properties))

(defun awl::property-names (object)
  "Return a list of the names of the properties defined in object."
  (mapcar 'c2mop:slot-definition-name (get-property-slots object)))
(defun awl::properties (object)
  "Return an association list of conses (property-name . property-value)."
  (properties object))
(defmacro awl::do-properties ((key value object &optional result) &body body)
  (awl:with-gensyms (g!property g!next-p)
    `(let (,g!next-p)
       (declare (type boolean ,g!next-p))
       (dolist (,g!property (properties ',object) ,result)
         (let ((,key (car ,g!property))
               (,value (cdr ,g!property)))
           (symbol-macrolet ((awl::first-property-p (or ,g!next-p (setq ,g!next-p t))))
             . ,body))))))

(defun awl::property-type (object property)
  "Return the type specification of the value of the property PROPERTY contained in the object OBJECT."
  (c2mop:slot-definition-type (find-property-slot object property)))
(defun awl::property-allocation (object property)
  "Return the allocation type of the property of the property-structure object."
  (c2mop:slot-definition-allocation (find-property-slot object property)))
(defun awl::property-namestring (object property)
  "Return the namestring of the property of the property-structure object."
  (slot-value (find-property-slot object property) 'namestring))

(declaim (inline awl::property-value)
         (ftype (function (awl::standard-property-structure symbol) *) awl::property-value))
(defun awl::property-value (object property)
  (slot-value object property))

(defmethod documentation ((object awl::standard-property-structure) (doc-type symbol))
  (let ((property-slot (find-property-slot object doc-type)))
    (when property-slot
      (documentation property-slot t))))

(defmacro awl::with-property-set (property-set (&rest parents) (&rest property-definitions)
                                  &body property-structures &environment environment)
  `(progn
     ,(unless (typep (find-class property-set nil environment) 'awl::property-set)
        `(defclass ,property-set ,parents ,property-definitions (:metaclass awl::property-set)))
     . ,(mapcar (lambda (property-structure)
                  (if (eq :documentation (first property-structure))
                      `(setf (documentation ,property-set 'type) (second property-structure))
                      (destructuring-bind (name (&rest parents) (&rest slots) &rest options)
                          property-structure
                        `(defclass ,name ,parents ,slots (:metaclass ,property-set) . ,options))))
                property-structures)))



;;;;============================================================================
;;;;* Cached Structures
(defmacro awl::define-cached-struct (name &rest slots)
  "Like CL:DEFSTRUCT but defines the constructors in such a way that when called twice with same args it returns the same instance. The args are hereby tested by the equality test given with the option :test (possible values EQ EQL EQUAL EQUALP). Copier function definitions are not allowed, since it would be contrary to the principle of caching instances."
  (assert (and (not (null name))
               (or (symbolp name)
                   (listp name))))
  (assert (or (symbolp name)
              (notany (lambda (x) (or (eq :copier x)
                                      (and (consp x) (eq :copier (car x)))))
                      (cdr name))))
  (let* ((slots (mapcar (lambda (slot)
                          (setq slot (etypecase slot
                                       (symbol (list slot nil))
                                       (cons slot)))
                          (setf (getf (cddr slot) :read-only) t)
                          slot)
                        slots))
         (constructors (mapcar (lambda (constructor)
                                 (when (symbolp constructor)
                                   (setq constructor (list constructor)))
                                 (unless (second constructor)
                                   (setf (cdr constructor) (list (intern (format nil "MAKE-~A" name)))))
                                 (unless (third constructor)
                                   (setf (cddr constructor) (list (list* '&key (mapcar #'car slots)))))
                                 constructor)
                               (if (listp name)
                                   (remove-if (lambda (option)
                                                (etypecase option
                                                  (symbol (not (eq :constructor option)))
                                                  (cons (destructuring-bind (option-type &rest option-spec)
                                                            option
                                                          (or (not (eq :constructor option-type))
                                                              (and option-spec
                                                                   (null (car option-spec))))))))
                                              (cdr name))
                                   (list (list :constructor)))))
         (structure-name (typecase name
                           (symbol name)
                           (list (car name)))))
    (multiple-value-bind (name test)
        (typecase name
          (symbol (list structure-name constructors))
          (t (cons (car name)
                   (remove-if (lambda (x) (eq (car x) :test))
                                   (cdr name)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,name
           ,@(mapcar (lambda (slot)
                       (when (symbolp slot)
                         (setq slot (list slot nil)))
                       (setf (getf (cddr slot) :read-only) t)
                       slot)
                     slots))
         (let ((cache (awl::make-hash-tree ,@(car test))))
           (let (,@(mapcar (lambda (constructor)
                             (let ((constructor-name (second constructor)))
                               `(,constructor-name (symbol-function ',constructor-name))))
                           constructors))
             ,@(mapcar (lambda (constructor)
                         `(symbol-macrolet (,@(mapcar (lambda (slot)
                                                        `(,(first slot) ',(second slot)))
                                                      slots))
                            (defun ,@(cdr constructor)
                                (or (awl::get-multi-hash cache ,@(mapcar #'car slots))
                                 (setf (awl::get-multi-hash cache ,@(mapcar #'car slots)) (apply ,@(cdr constructor)))))))
                       constructors)))
         ',name))))





(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))
