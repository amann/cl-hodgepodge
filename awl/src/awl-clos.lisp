;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Abstract Class
;;;; Classes of this type cannot be instanciated.
(defclass abstract-class (standard-class) ()
  (:documentation "Classes of this type cannot be instanciated."))
(defmethod c2mop:validate-superclass ((class abstract-class) (superclass standard-class)) t)
(defmethod c2mop:validate-superclass ((class standard-class) (superclass abstract-class)) t)
(defmethod allocate-instance :around ((class abstract-class) &key &allow-other-keys)
  (error "The class ~A is abstract and cannot be instanciated." (class-name class)))
;;;;* Registered Classes
;;;; Registered classes are classes which, when created, are registered into a registry
;;;; under the key NAME, which is contained in the slot NAME inherited of standard-class.
;;;; The idea is to create classes which are anonymous under the normal naming system, i.e.
;;;; not available through cl:find-class, but available under another class-registry.  This
;;;; class-registry is like a system which is parallel to the standard naming system.  It is
;;;; also intended that classes registered under this system be registered excusively in this
;;;; class-registry and having as few contact points to classes outside this system.
;;;;
;;;; The function analogous to cl:find-class is
;;;; (find-rclass class &optional (errorp t) (registry *class-registry*))
;;;; The globally special variable *class-registry* contains the current registry.

;;;;** Registry
;;;; We allow two implementations of a registry: hash-table and plist
(defun make-registry (registry-type &rest options &key &allow-other-keys)
  "Return a closure representing a registry object of type REGISTRY-TYPE. REGISTRY-TYPE is one of :PLIST or :HASH-TABLE. The returned closure has lambdalist (MSG &OPTIONAL KEY VALUE). MSG can be one of:
- :get : return the value associated to KEY;
- :set : set the registered value associated to KEY to VALUE; remove the association if VALUE is null;
- :clr : clear the registry, deleting all entries;
- :exp : expose the complete content of the registry in form of a plist key-value."
  (ecase registry-type
    (:plist (let (registry)
              (lambda (msg &optional key value)
                (ecase msg
                  (:get (getf registry key))
                  (:set (if value
                            (setf (getf registry key) value)
                            (remf registry key)))
                  (:clr (setq registry nil))
                  (:exp (copy-list registry))))))
    (:hash-table (let ((registry (apply 'make-hash-table options)))
                   (lambda (msg &optional key value)
                     (ecase msg
                       (:get (gethash key registry))
                       (:set (if value
                                 (setf (gethash key registry) value)
                                 (remhash key registry)))
                       (:clr (clrhash registry))
                       (:exp (let (copy)
                               (maphash (lambda (key val)
                                          (push val copy)
                                          (push key copy))
                                        registry)
                               copy))))))))


(declaim (ftype (function (function symbol) *) get-register-entry)
         (inline get-register-entry))
(defun get-register-entry (registry key)
  "Return the value associated to KEY; return NIL if no association exist for KEY."
  (funcall registry :get key))
(declaim (ftype (function (function symbol *) *) set-register-entry)
         (inline set-register-entry))
(defun set-register-entry (registry key value)
  (funcall registry :set key value))
(defsetf get-register-entry set-register-entry
  "Set the registered value associated to KEY to VALUE; remove the association if VALUE is null.")


;;;;** Registry Class
;;;;
;;;;                          class-registry
;;;;                               ||
;;;;                               ··
;;;;        registered-class := registry < registry-class
;;;;
;;;;  registered-class is registered in the metaclass object registry which is
;;;;  of subtype registry-class; i.e. registered-class is of type registry-class.
;;;;
;;;; A := B : A is instance of B
;;;; A <  B : A is subtype of B

(defclass class-registry (standard-class c2mop:funcallable-standard-class) ()
  (:documentation "All classes which are registered in a registry are instances of a metaclass R which is an instance of (a subtype of) this metaclass.  The metaclass R implements the registry of those classes. Hence, class registries are of type class-registry."))
(defmethod initialize-instance :after ((instance class-registry) slot-names &key (registry-type :plist) &allow-other-keys)
  (c2mop:set-funcallable-instance-function instance (make-registry registry-type)))
(defclass registry-class (standard-class c2mop:funcallable-standard-object)
  ((class-name :type symbol :initform nil :initarg :name :accessor class-name))
  (:documentation "All classes which are registered in a registry are instances of a metaclass R which is a subtype of this metaclass.  The metaclass R implements the registry of those classes. Hence, registered classes are of type registry-class.")
  (:metaclass abstract-class))
(defmethod c2mop:validate-superclass ((class class-registry) (superclass standard-class)) t)
(defmethod c2mop:validate-superclass ((class standard-class) (superclass class-registry)) t)


(defmethod c2mop:validate-superclass ((class registry-class) (superclass standard-class))
  "The only superclass which may not be in the registry-class system is STANDARD-OBJECT."
  (eql superclass (find-class 'standard-object)))

(declaim (ftype (function (*) class-registry)))
(defgeneric registry (class)
  (:method ((class registry-class))
    (class-of class)))



;;;;** Class Registry
(defvar *class-registry*)
(declaim (type class-registry *class-registry*))
(defun ensure-class-registry (name &rest args)
  (let ((metaclass (or (let ((metaclass (getf args :metaclass)))
                         (when (subtypep metaclass 'class-registry)
                           metaclass))
                       'class-registry))
        (direct-superclasses (let ((direct-superclasses (getf args :direct-superclasses)))
                               (append (unless (some (lambda (dsc) (subtypep dsc 'registry-class))
                                                     direct-superclasses)
                                         (list 'registry-class))
                                       direct-superclasses))))
   (apply 'c2mop:ensure-class-using-class (find-class name nil) name
          :metaclass metaclass :direct-superclasses direct-superclasses
          args)))

(defun canonize-defclass-slots (class-name slots env)
  (let (sb-pcl::*readers-for-this-defclass*
        sb-pcl::*writers-for-this-defclass*
        sb-pcl::*slot-names-for-this-defclass*
        sb-pcl::*initfunctions-for-this-defclass*)
    (sb-pcl::canonize-defclass-slots class-name slots env)))
(defmacro define-class-registry (name (&rest direct-superclasses) (&rest slots) &body options &environment env)
  (let ((direct-slots (canonize-defclass-slots name slots env))
        (direct-default-initargs (let ((direct-default-initargs (getf options :default-initargs)))
                                   (when direct-default-initargs
                                     `(list ,@(awl:map-plist (lambda (key value)
                                                               `(list ',key ',value (lambda () ,value)))
                                                             direct-default-initargs)))))
        (documentation (getf options :documentation))
        (metaclass (getf options :metaclass))
        (other-options (mapcan (lambda (option)
                                 (unless (member (car option) '(:default-initargs :documentation :metaclass))
                                   (list `',(car option) `',(cdr option))))
                               options)))
    `(ensure-class-registry ',name
                            :direct-superclasses ',direct-superclasses
                            :direct-slots ',direct-slots
                            ,@(when direct-default-initargs
                                `(:direct-default-initargs ',direct-default-initargs))
                            ,@(when documentation `(:documentation ',documentation))
                            ,@(when metaclass `(:metaclass ',metaclass))
                            ,@other-options)))

(define-condition class-registry-error (error)
  ((registry :initarg registry :reader class-registry-error-registry)
   (datum :initarg datum :reader class-registry-error-datum)))
(define-condition class-registry-unknown-entry-error (class-registry-error) ()
  (:report (lambda (c s)
             (format s "Unknown class ~A in registry ~A."
                     (class-registry-error-datum c)
                     (class-registry-error-registry c)))))
(define-condition class-registry-invalid-class-error (class-registry-error) ()
  (:report (lambda (c s)
             (format s "Class class ~A is not registered in registry ~A."
                     (class-registry-error-datum c)
                     (class-registry-error-registry c)))))
(define-condition class-registry-error-name-conflict (class-registry-error) ()
  (:report (lambda (c s)
             (let* ((class (class-registry-error-datum c))
                    (name (class-name class)))
               (format s "The symbol ~A is already a proper name in registry ~A."
                       name (class-registry-error-registry c))))))

;;;;** Setf Class-Name
(defmethod (setf class-name) :before (new-name (class registry-class))
  (let ((old-name (class-name class)))
    (unless (eq new-name old-name)
      (let* ((registry (registry class)) 
             (other-class (get-register-entry registry new-name)))
        (unless (eq other-class class)
          (when (and other-class
                     (eq new-name (class-name other-class)))
            (error 'class-registry-error-name-conflict other-class registry))
          (setf (get-register-entry registry new-name) class
                (get-register-entry registry old-name) nil))))))

;;;;** Find Registered Class
(defun find-rclass (name &optional (errorp t) (registry *class-registry*))
  "Return "
  (multiple-value-bind (class name)
      (find-registered-class-generic name registry)
    (when errorp
      (unless (and class name)
        (error 'class-registry-unknown-entry-error 'registry registry 'datum name)))
    (values class name)))

(defsetf find-rclass set-find-registered-class-generic)

(defun ensure-registry-class (name &rest args)
  (apply 'ensure-registry-class-using-class (find-rclass name nil) name args))

(defgeneric ensure-registry-class-using-class (class name &key direct-default-initargs direct-slots
                                                            direct-superclasses name &allow-other-keys)
  (:method ((class null) name &rest args &key &allow-other-keys)
    (setf (find-rclass name) (apply 'make-instance *class-registry* :name name args)))
  (:method ((class registry-class) name &rest args &key &allow-other-keys)
    (apply 'reinitialize-instance class args)))


(defgeneric find-registered-class-generic (name registry)
  (declare (optimize speed))
  (:method :around ((name symbol) registry)
    (values (call-next-method) name))
  (:method ((name class-registry) registry)
    (find-registered-class-generic (class-name name) registry))
  (:method (name registry)
    (find-registered-class-generic name (find-class registry)))
  (:method (name (registry class-registry))
    (get-register-entry registry name))
  (:method (name (registry null))
    (find-class name)))

(defgeneric set-find-registered-class-generic (name registry value)
  (:method :around (name registry value)
    (call-next-method) value)
  (:method ((name class) registry value)
    (set-find-registered-class-generic (class-name name) registry value))
  (:method (name registry value)
    (set-find-registered-class-generic name (find-class registry) value))
  (:method (name (registry class-registry) (value registry-class))
    (let ((registered-class (get-register-entry registry name)))
      (unless (eql value registered-class)
        (when (eql name (class-name registered-class))
          (error 'class-registry-error-name-conflict registered-class registry))
        (unless (class-name value)
          (setf (class-name registered-class) name))
        (set-register-entry registry name value)))))

;;;;** Cached Object
(defclass memoized-object-class (standard-class)
  ((instances :initform (make-hash-table :test 'equal))))

(defmethod allocate-instance ((class memoized-object-class) &key instance-key &allow-other-keys)
  (or (gethash instance-key (slot-value class 'instances))
      (setf (gethash instance-key (slot-value class 'instances)) (call-next-method))))

