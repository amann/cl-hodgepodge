;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")

;;;;* Some Stuff

(defun destructure (tree variablep)
  "Expand and return a form which"
  (labels ((destr (item)
             (cond ((consp item) (cons 'list (mapcar #'destr item)))
                   ((funcall variablep item) item)
                   (t `',item))))
    (destr tree)))

(declaim (ftype (function ((awl:list-of list)) list)))
(defun cartesian-product* (lists)
  (labels ((product (lists &aux (rest-lists (rest lists)))
             (if rest-lists
                 (loop :for elt :in (the list (first lists))
                       :nconc (loop :for other-elt :in (product rest-lists)
                                    :collect (cons elt other-elt)))
                 (mapcar 'list (first lists)))))
    (if (rest lists)
        (product lists)
        (first lists))))
(defun cartesian-product (&rest lists)
  (declare (inline cartesian-product*))
  (cartesian-product* lists))


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


;;;;* Domains
;;;;
;;;; We call Domain the types defined in our type system. These domains are considered as
;;;; sets. An object is of a domain, if it can be considered as an element of this domain.
;;;; The binary relation "is subtype" (<=) is therefore equivalent to the inclusion of sets
;;;; and the whole set of domains with this relation is a lattice.  We make of it a bounded
;;;; lattice by adding the top domain OBJECT (1) and the bottom domain VOID (0).
;;;; We distinguish basically two kinds of domains: classes and other, "derived" domains.
;;;; The classes have the property, that they are either included one into another or disjoint.
;;;; Said in an other way, the meet (greatest lower bound) a ^ b of two classes a and b is
;;;; a, b or direct-subclasses(a) ^ direct-subclasses(b).  Those classes are implemented as
;;;; CLOS-classes with a metaclass of DOMAIN-CLASS.  The derived domains are those constructed
;;;; as a join, a meet or the complement of given domains or explicit enumerations of objects.
;;;; For domains on which exist an algebraic structure, like an order relation or internal
;;;; compositions, subdomains may be defined.
;;;; Other derived domains will be "parameterized" domains, like polynomials over a ring or
;;;; maps from one domain to another.
;;;; 
;;;;** Domain Metaclass
;;;;
;;;; 



(defclass domain (standard-class) ()
  (:documentation "Metaclass for the two domains OBJECT and VOID."))
(defclass domain-class (domain) ()
  (:documentation "Every domain which is a class in the sense that for all such classes a and b their meet a ^ b is a, b or 0."))
(defclass top-domain-class (domain-class) ()
  (:documentation "The latice of the domains is bounded at the top by an instance of TOP-DOMAIN-CLASS."))
(defclass bottom-domain-class (domain-class) ()
  (:documentation "The latice of the domains is bounded at the bottom by an instance of BOTTOM-DOMAIN-CLASS."))

(defmethod c2mop:validate-superclass ((class top-domain-class) (superclass standard-class))
  "Only STANDARD-OBJECT can be a superclass of a class of metaclass TOP-DOMAIN-CLASS."
  (eql superclass (find-class 'standard-object)))
(defmethod c2mop:validate-superclass ((class domain-class) (superclass top-domain-class)) t)
(defmethod c2mop:validate-superclass ((class bottom-domain-class) (superclass domain-class)) t)

(defclass deleted-domain-class () ()
  (:documentation "Class of deleted domain-classes which might be garbage collected."))

;;;;** Domain Set
;;;; 
(defclass domain-set (registry-class) ())

(define-condition domain-set-error (error)
  ((domain-set :initarg domain-set :reader domain-set-error-domain-set)))
(define-condition undefined-domain-error (domain-set-error)
  ((domain-name :initarg domain-name :reader domain-set-error-domain-name))
  (:report (lambda (c s)
             (format s "No domain of name ~A in domain set ~A."
                     (domain-set-error-domain-name c)
                     (domain-set-name (domain-set-error-domain-set c))))))
(define-condition invalid-domain-error (domain-set-error)
  ((domain :initarg domain :reader domain-set-error-invalid-domain))
  (:report (lambda (c s)
             (format s "Domain ~A is not defined in the domain set ~A."
                     (domain-set-error-invalid-domain c)
                     (domain-set-name (domain-set-error-domain-set c))))))

;;;;*** Access and register domains in domain sets
;;;;**** Find-domain
(defgeneric find-domain-generic (domain)
  (:documentation "Lookup in *DOMAIN-SET* for DOMAIN and return as values the corresponding domain object and the domain name. If DOMAIN does not belong to the domain set *DOMAIN-SET*, one of the two return values is null.")
  (:method ((name symbol))
    (with-slots (name->domain) *domain-set*
      (values (gethash name name->domain) name)))
  (:method ((domain domain))
    (with-slots (domain->name) *domain-set*
      (values domain (gethash domain domain->name)))))

(defgeneric remove-domain-generic (domain name)
  (:method :around ((domain domain-set) name)
    (prog1 (let ((name-removed-p (remhash name name->domain)))
             (or (remhash domain domain->name) name-removed-p)))
    (prog1 (with-slots (name->domain domain->name)
               *domain-set*
             (let ((name-removed-p (remhash name name->domain)))
               (or (remhash domain domain->name) name-removed-p)))
      (call-next-method)))
  (:method (domain name))
  (:method ((domain domain-class) name)
    (let ((direct-superdomains (domain-class-direct-superclasses domain)))
      (dolist (subdomain (domain-class-direct-subclasses domain))
        (setf (domain-class-direct-superclasses subdomain)
              (delete-duplicates (mapcan (lambda (direct-superdomain)
                                           (if (eq domain direct-superdomain)
                                               direct-superdomains
                                               (list direct-superdomain)))
                                         (domain-class-direct-superclasses subdomain)))))))
  (:method :after ((domain domain-class) name)
    (change-class domain 'deleted-domain)))

;;;;TODO: Is this really enough? I think we have to ensure that, when reinitializing, the
;;;; compound domains drop out all deleted domains from their arguments list.
(defmethod update-instance-for-different-class :after ((previous domain) current &key &allow-other-keys)
  (make-instances-obsolete 'compound-domain))


(defun find-domain (domain &optional (errorp t) (domain-set *domain-set*))
  "Return the domain object specified by DOMAIN in the domain set DOMAIN-SET.  DOMAIN-SET defaults to the content of the dynamic variable *domain-set*.  If no domain could be found in the domain set, signal an error of type DOMAIN-SET-ERROR if ERRORP is non NIL (the default) or one of the return values is NIL else."
  (let ((*domain-set* domain-set))
    (if errorp
        (multiple-value-bind (domain name)
            (find-domain-generic domain)
          (unless domain
            (error 'invalid-domain-error 'domain domain 'domain-set *domain-set*))
          (unless name
            (error 'undefined-domain-error 'domain-name name 'domain-set *domain-set*))
          (values domain name))
        (find-domain-generic domain))))

(defgeneric change-domain-generic (old-domain old-name new-domain new-name)
  ;; We check if one of the new values differ from the corresponding old value
  ;; and remove the old values if so.
  (:method :before (old-domain old-name new-domain new-name)
    (unless (and (eq old-domain new-domain)
                 (eq old-name new-name))
      (remove-domain-generic old-domain old-name)))
  ;; If one on the new values is null, we do not continue, i.e. the old values are simply removed.
  (:method (old-domain old-name (new-domain null) (new-name symbol)))
  (:method (old-domain old-name (new-domain domain) (new-name null)))
  ;; Regular case, where we register NEW-DOMAIN under NEW-NAME
  (:method (old-domain old-name (new-domain domain) (new-name symbol))
    (unless (and (eq old-domain new-domain)
                 (eq old-name new-name))
      (with-slots (name->domain domain->name) *domain-set*
        (setf (gethash new-name name->domain) new-domain
              (gethash new-domain domain->name) new-name)))))

(defsetf find-domain (domain &optional (errorp t) (domain-set *domain-set*)) (new-domain)
  "Register a new domain object to the domain set DOMAIN-SET (defaulting to *DOMAIN-SET*).  If DOMAIN is a symbol, the value set will be registered under this symbol as name.  If DOMAIN is a domain registered in DOMAIN-SET, it is replaced by value under the old name of DOMAIN; if DOMAIN is not registered an error of type DOMAIN-SET-ERROR is signalled if ERRORP is true or nothing is done if ERRORP is false.  If value is NIL the registration is removed."
  `(let ((*domain-set* ,domain-set))
     (multiple-value-bind (domain name)
         (find-domain ,domain nil)
       (when (and errorp (null name))
         (error 'invalid-domain-error 'domain domain 'domain-set *domain-set*))
       (change-domain-generic domain name ,new-domain name))
     ,new-domain))

(defun remove-domain (domain &optional (errorp t) (domain-set *domain-set*))
  "Remove the domain object specified by DOMAIN from the domain set DOMAIN-SET.  DOMAIN-SET defaults to the content of the dynamic variable *domain-set*.  If errorp is true (the default), signal an error of type DOMAIN-SET-ERROR; if ERRORP is false, return true if DOMAIN has been effectively removed and false else."
  (let ((*domain-set* domain-set))
    (multiple-value-bind (domain name)
        (find-domain domain errorp)
      (remove-domain-generic domain name))))

;;;;**** Domain-name
(defun domain-name (domain &optional (errorp t) (domain-set *domain-set*))
  "Return as primary value the name of the domain object DOMAIN and as secondary value the domain, as registered in the domain set DOMAIN-SET (defaulting to *DOMAIN-SET*).  If the domain is not registered in the domain set, signal an error of type DOMAIN-SET-ERROR if ERRORP is true; if ERRORP is false, one of the return values is null."
  (let ((*domain-set* domain-set))
    (multiple-value-bind (domain domain-name)
        (find-domain domain errorp)
      (values domain-name domain))))
(defsetf domain-name (domain &optional (errorp t) (domain-set *domain-set*)) (new-name)
  "Set the domain name of the domain object DOMAIN in the domain set DOMAIN-SET (defaulting to *DOMAIN-SET*).  If DOMAIN is a domain, replace the old name to new name, if DOMAIN is a symbol, replace the name of the domain registered under this symbol by new name; If no domain is registered under this symbol, signal an error of type DOMAIN-SET-ERROR if ERRORP is true, or do nothing if ERRORP is false.  If new name is NIL the domain is removed from the domain set."
  `(let ((*domain-set* ,domain-set))
     (multiple-value-bind (domain old-name)
         (find-domain ,domain nil)
       (when (and errorp (null domain))
         (error 'undefined-domain-error 'domain-name old-name 'domain-set *domain-set*))
       (change-domain-generic domain old-name domain ,new-name))
     ,new-name))


;;;;*** Constructor of Domain Set
(defmethod shared-initialize :after ((domain-set domain-set) slot-names &key &allow-other-keys)
  (let* ((*domain-set* domain-set)
         (object (make-instance 'top-domain-class))
         (void (make-instance 'bottom-domain-class :direct-superdomains (list object))))
    (setf (find-domain 'object) object
          (find-domain 'void) void)))
(defun make-domain-set () (make-instance 'domain-set))
  
;;;;** Constructor of Domain-Classes

(defgeneric ensure-domain-using-domain (domain name &key &allow-other-keys)
  (:method ((domain null) (name symbol) &rest options &key &allow-other-keys)
    (setf (find-domain name) (apply 'make-instance 'domain options)))
  (:method ((domain domain-class) (name symbol) &rest options &key &allow-other-keys)
    (apply 'reinitialize-instance domain options)))
(defun ensure-domain (name &rest options &key &allow-other-keys)
  (apply 'ensure-domain-using-domain (find-domain name nil) name options))

(defun canonize-define-domain-slots (class-name slots env)
  (let (sb-pcl::*readers-for-this-defclass*
        sb-pcl::*writers-for-this-defclass*
        sb-pcl::*slot-names-for-this-defclass*
        sb-pcl::*initfunctions-for-this-defclass*)
    (sb-pcl::canonize-defclass-slots class-name slots env)))
(defmacro define-domain-class (name ((&rest superdomains) (&rest subdomains))
                               (&rest slots) &rest options &environment env)
  `(ensure-domain ',name
                  :slots ',(canonize-define-domain-slots name slots env)
                  :metaclass 'domain-class
                  :direct-superdomains ',superdomains
                  :direct-subdomains ',subdomains
                  ,@(loop :for (key . value) :in options
                          :collect `',key :collect `',value)))

(defmethod domain-class-id (domain)
  (domain-class-id (find-domain domain nil)))

(defgeneric domain-class-direct-superclasses (domain)
  (declare (optimize speed))
  (:documentation "Return a list of the direct superdomain-classes of DOMAIN.")
  (:method (domain)
    (domain-class-direct-superclasses (find-domain domain)))
  (:method ((domain domain-class))
    (c2mop:class-direct-superclasses domain))
  (:method ((domain top-domain-class))
    nil))
(defgeneric (setf domain-class-direct-superclasses) (direct-superdomains domain)
  (:method (direct-superdomains (domain top-domain-class))
    (when direct-superdomains
      (error "The top domain-class ~A cannot have superdomains; tried setting ~A."
             domain direct-superdomains)))
  (:method (direct-superdomains (domain domain-class))
    (reinitialize-instance domain :direct-superclasses (or direct-superdomains
                                                           (list (find-domain 'object))))
    (c2mop:finalize-inheritance domain)))
(defgeneric domain-class-direct-subclasses (domain)
  (declare (optimize speed))
  (:documentation "Return a list of the direct subdomain-classes of DOMAIN.")
  (:method (domain)
    (domain-class-direct-subclasses (find-domain domain)))
  (:method ((domain domain-class))
    (c2mop:class-direct-subclasses domain))
  (:method ((domain bottom-domain-class))
    nil))

;;;;*** Initialization of Domain Classes
(defmethod shared-initialize :after ((domain domain-class) slot-names
                                     &key direct-superdomains direct-subdomains &allow-other-keys
                                     &aux (direct-superdomains (or (mapcar 'find-domain direct-superdomains)
                                                                   (list (find-domain 'object)))))
  (dolist (subdomain (or (mapcar 'find-domain direct-subdomains)
                         (list (find-domain 'void))))
    (setf (domain-class-direct-superclasses subdomain)
          (set-difference (substitute-if domain
                                         (lambda (superclass)
                                           (member superclass direct-superdomains))
                                         (domain-class-direct-superclasses subdomain)
                                         :count 1)
                          direct-superdomains))))

;;;;** Lattices
(defun dag-border (predicate node successors-fn)
  "Follow the successors of NODE given by SUCCESSORS-FN, and return for each followed path the last node for which PREDICATE returns true.  In other words, return a list of the nodes succeding NODE for which PREDICATE is true and having a direct successor for which PREDICATE is false.  The list can contain duplicate entries. The graph must be acyclic (at least on the domain where PREDICATE is true)."
  (labels ((follow-path (current precedent)
             (if (funcall predicate current) 
                 (loop :for successor :in (funcall successors-fn current)
                       :nconc (follow-path successor (list current)))
                 (list precedent))))
    (follow-path node nil)))

(defun least-common-super-domain-classes (classes)
  (labels ((common-superdomain (domain)
             (every (lambda (class)
                      (subdomainp class domain))
                    classes)))
    (awl:find-extrema (dag-border #'common-superdomain (find-domain 'object) #'domain-class-direct-subclasses)
                      #'subdomainp)))

(defgeneric join2 (d1 d2)
  (:method (d1 d2)
    (join2 (find-domain d1) (find-domain d2)))
  (:method ((d1 domain-class) (d2 domain-class))
    (cond ((subdomainp d1 d2) d2)
          ((subdomainp d2 d1) d1)
          (t (join* (remove-duplicates (append (domain-class-direct-superclasses d1)
                                               (domain-class-direct-superclasses d2))))))))
(defgeneric meet2 (d1 d2)
  (:method (d1 d2)
    (meet2 (find-domain d1) (find-domain d2)))
  (:method ((d1 domain-class) (d2 domain-class))
    (cond ((subdomainp d1 d2) d1)
          ((subdomainp d2 d1) d2)
          (t (meet* (remove-duplicates (append (domain-class-direct-subclasses d1)
                                               (domain-class-direct-subclasses d2))))))))

(defun join* (domains)
  "Return the domain which is currently the join of DOMAINS in the current domain set."
  (if domains
      (reduce 'join2 (rest domains) :initial-value (first domains))
      (find-domain 'void)))
(declaim (inline join))
(defun join (&rest domains)
  "Return the domain which is currently the join of DOMAINS in the current domain set."
  (join* domains))

(defun meet* (domains)
  "Return the domain which is currently the meet of DOMAINS in the current domain set."
  (if domains
      (reduce 'meet2 (rest domains) :initial-value (first domains))
      (find-domain 'object)))
(declaim (inline meet))
(defun meet (&rest domains)
  "Return the domain which is currently the meet of DOMAINS in the current domain set."
  (meet* domains))

;;;;** Compound Domains
;;;; Compound domains are domains which are combined from other domains by using an operation.
;;;; Typically they are defined using an s-expr of the form (operation &rest args).
(defclass compound-domain (domain memoized-object-class)
  ((subdomainp :type function :documentation "Contains the predicate function testing if this domain is a subdomain of the argument.")
   (superdomainp :type function :documentation "Contains the predicate function testing if this domain is a superdomain of the argument.")
   (containsp :type function :documentation "Contains the predicate function testing for appartenance of an object."))
  (:metaclass registry-class))

(let ((count 0))
  (defun new-domain-specification-id () (decf count)))

(defmethod make-instance ((class compound-domain) &rest initargs &key args &allow-other-keys)
  (let ((args (sort (mapcar 'find-domain args) '< :key 'domain-class-id)))
    (apply #'call-next-method class :args args :instance-key args initargs)))

(defmethod c2mop:validate-superclass ((class compound-domain) (superclass standard-class))
  (eql class (find-class 'domain-specification))) 
(defclass domain-specification (registered-class)
  ((args :initarg :args)) 
  (:metaclass compound-domain))

(defgeneric make-compound-domain-using-class (domain-class spec)
  (:method ((domain-class null) spec))
  (:method (domain-class spec)
    (make-instance (find-registered-class 'compound-domain domain-class) spec)))

(declaim (ftype (function (cons) compound-domain) make-compound-domain))
(defun make-domain-specification (domain-spec)
  (make-domain-specification-using-class (car domain-spec) (cdr domain-spec)))





;;;;*** Subdomainp
(defgeneric subdomainp-using-classes (domain1 domain2)
  (:method ((domain1 domain-class) (domain2 domain-class))
    (subtypep domain1 domain2))
  (:method ((domain1 compound-domain) domain2)
    (funcall (slot-value domain1 'subdomainp) domain2))
  (:method (domain1 (domain2 compound-domain))
    (funcall (slot-value domain2 'superdomainp) domain1)))
(defun subdomainp (domain1 domain2 &optional (*domains* *domains*))
  (subdomainp-using-classes (find-domain domain1) (find-domain domain2)))
(defun containsp (domain object &optional (*domains* *domains*))
  (containsp-using-class object (find-domain domain)))

;;;;*** Combined Domains AND, OR, NOT, MEMBER, etc.

(defclass join-domain (domain-specification) ()
  (:metaclass compound-domain)
  (:key or))
(defmethod shared-initialize :after ((instance join-domain) slot-names &key args &allow-other-keys)
  (with-slots (superdomainp subdomainp containsp)
      instance
    (setf superdomainp (compile nil     ; c <= a v b <=> c <= ^{ x | (a <= x) v (b <= x)}
                                        ;            <=> ^{ (c <= x) | (a <= x) v (b <= x)}
                                `(lambda (domain)
                                   ,(when args
                                      `(and ,@(mapcar (lambda (a)
                                                        `(subdomainp domain ,a))
                                                      (reduce 'union args
                                                              :key 'domain-class-direct-superclasses))))))
          subdomainp (compile nil ;; a v b <= c <=> (a <= c) v (b <= c)
                              `(lambda (domain)
                                 (or ,@(mapcar (lambda (arg)
                                                  `(subdomainp ,arg domain))
                                                args))))
          containsp (compile nil ;; e :in a v b <=> (e :in a) v (e :in b)
                             `(lambda (object)
                                (or ,@(mapcar (lambda (arg)
                                                `(containsp ,arg object))
                                              args)))))))

(defclass meet-domain (combined-domain) ()
  (:metaclass compound-domain-class)
  (:key and))
(defmethod shared-initialize :after ((instance meet-domain) slot-names &key args &allow-other-keys)
  (with-slots (superdomainp subdomainp containsp)
      instance
    (setf superdomainp (compile nil ;; c <= a ^ b <=> (c <= a) ^ (c <= b)
                                `(lambda (domain)
                                   (and ,@(mapcar (lambda (arg)
                                                    `(subdomainp domain ,arg))
                                                  args))))
          subdomainp (compile nil       ; a ^ b <= c <=> V{ x | (x <= a) ^ (x <= b)} <= c
                                        ;            <=> V{ (x <= c) | (x <= a) ^ (x <= b)}
                              `(lambda (domain)
                                 ,(when args
                                    `(or ,@(mapcar (lambda (a)
                                                     `(subdomainp ,a domain))
                                                   (reduce 'intersection args
                                                           :key 'domain-class-direct-subclasses))))))
          containsp (compile nil ;; e :in a ^ b <=> (e :in a) ^ (e :in b)
                             `(lambda (object)
                                (and ,@(mapcar (lambda (arg)
                                                 `(containsp ,arg object))
                                               args)))))))

(defclass complementary-domain (combined-domain) ()
  (:metaclass compound-domain-class)
  (:key not))
(defmethod shared-initialize :after ((instance complementary-domain) slot-names &key args &allow-other-keys)
  (let ((arg (first args)))
    (with-slots (superdomainp subdomainp containsp)
        instance
      (setf superdomainp (compile nil ;; c <= ¬a <=> a ^ c = void
                                  `(lambda (domain)
                                     (subdomainp (meet domain ,arg) 'void)))
            subdomainp (compile nil ;; ¬a <= c <=> a v c = object
                                `(lambda (domain)
                                   (subdomainp 'object (join domain ,arg))))
            containsp (compile nil
                               `(lambda (object)
                                  (not (containsp ,arg object))))))))

(defclass enumeration-domain (combined-domain) ()
  (:metaclass compound-domain-class)
  (:key member))
(defmethod shared-initialize :after ((instance enumeration-domain) slot-names &key args &allow-other-keys)
  (with-slots (superdomainp subdomainp containsp)
      instance
    (setf superdomainp (compile nil
                                `(lambda (domain)
                                   (and (typep domain 'enumeration-domain)
                                        (subsetp (slot-value domain 'args) ',args))))
          subdomainp (compile nil
                              `(lambda (domain)
                                 (and ,@(mapcar (lambda (arg)
                                                  (containsp domain ,arg))
                                                args))))
          containsp (compile nil
                             `(lambda (object)
                                (or ,@(mapcar (lambda (arg)
                                                (eql ,arg object))
                                              args)))))))



(defmethod make-compound-domain-using-class ((domain-class domain-specification) spec)
  (make-instance domain-class :args args))

;;;;*** "Signature" Domains

(defclass signature-domain (compound-domain)
  (mandatory optional rest)
  (:documentation "Metaclass representing the category of operation signature. It can be seen as a generalization of the cartesian-product: e.g. (A B &rest C) can be seen as (A x B) u (A x B x C) u (A x B x C x C) u ...")
  (:metaclass compound-domain-class)
  (:key signature))

(defun parse-signature-domain (lambda-list)
  (multiple-value-bind (mandatory optional restp rest keyp)
      (awl:parse-lambda-list lambda-list)
    (assert (not keyp) () "Keys are not yet implemented.")
    (when (and optional restp)
      (warn "Dangerous lambda list: ~A. Use of &OPTIONAL and &REST features are discouraged."))
    (values (mapcar 'expand-domain mandatory)
            (mapcar (lambda (opt &aux (opt (awl:ensure-list opt)))
                      (cons (expand-domain (first opt)) (rest opt)))
                    optional)
            (expand-domain rest))))
(defmethod shared-initialize :after ((instance signature-domain) slot-value
                                     &key lambda-list &allow-other-keys)
  (multiple-value-bind (mandatory optional rest)
      (parse-signature-domain lambda-list)
    (setf (slot-value instance 'mandatory) mandatory
          (slot-value instance 'optional) optional
          (slot-value instance 'rest) rest)))

(defmethod make-compound-domain-using-class ((domain-class signature-domain) spec)
  (make-instance domain-class :lambda-list spec))


;;;;** Methods on Domains
;;;;*** Subdomains

(defun subdomains (domain &optional (*domains* *domains*))
  (subdomains-using-domain (find-domain domain)))

(defgeneric subdomains-using-domain (domain)
  (:method ((domain domain))
    (labels ((subdomains (domain)
               (declare (type domain domain))
               (let ((direct-superdomains (domain-class-direct-subclasses domain)))
                 (append direct-superdomains
                         (mapcan (lambda (subdomain)
                                   (subdomains subdomain))
                                 direct-superdomains)))))
      (delete-duplicates (subdomains domain)))))


;;;;*** Subtypes
;;;; In order to include other lisp types, like numbers or arrays etc., into
;;;; the system of domains, we introduce the function SUBTYPES which, for a given
;;;; domain, returns a list of all lisp types which are considered of being a
;;;; subdomain of DOMAIN.

(defun subtypes (domain)
  (delete-duplicates (subtypes-using-class domain)))

(defgeneric subtypes-using-class (domain)
  (:documentation "Return a list of all lisp types which are not subclasses of DOMAIN but whose elements are considered of being in this domain.")
  (:method ((domain domain))
    (append (direct-subtypes domain)
            (mapcan 'subtypes-using-class (direct-subdomains domain)))))


;;;;*** Domainp


(defgeneric containsp-using-class (domain object)
  (:method ((domain domain-class) (object object))
    (typep object domain))
  (:method ((domain cons) object)
    (domainp-using-class object (expand-domain domain)))
  (:method ((domain intersection-domain) object)
    (every (lambda (arg) (domainp-using-class object arg)) (slot-value domain 'args)))
  (:method ((domain union-domain) object)
    (some (lambda (arg) (domainp-using-class object arg)) (slot-value domain 'args)))
  (:method ((domain complementary-domain) object)
    (not (domainp-using-class object (first (slot-value domain 'args)))))
  (:method ((domain enumeration-domain) object)
    (every (lambda (arg) (eql object arg)) (slot-value domain 'args))))

;;;;*** find-constants

(defun find-constants (name &optional domain (*domains* *domains*))
  (find-constants-using-class name (find-domain domain)))

(defgeneric find-constants-using-class (name domain)
  (:documentation "Return a list of all constants in domain DOMAIN of name NAME.")
  (:method :before (name (domain domain))
    (domain-name domain))
  (:method ((name symbol) (domain domain))
    (append (gethash name (slot-value domain 'constants))
            (mapcan (lambda (subdomain) (find-constants-using-class name subdomain))
                    (domain-class-direct-subclasses domain)))))
(defgeneric (setf find-constants) (value name domain)
  (:method :around (value name domain)
    (typecase value
      (cons (call-next-method))
      (t (setf (find-constants name domain) (list value)))))
  (:method :around ((value cons) name (domain domain))
    (if (every (lambda (val) (in-domain-p val domain)) value)
        (call-next-method)
        (error 'type-error :expected-type domain :datum value)))
  (:method (value (name symbol) (domain domain))
    (setf (gethash name (slot-value domain 'constants)) value)))


(defclass signature (domain)
  ((signature :initarg :signature :reader signature)))


(defclass operation-domain (domain)
  ((signature :initarg :signature :reader signature)
   (codomain :initarg :codomain :reader codomain))
  (:documentation "Metaclass representing the domains of operations. The start domain of the operations is described by the signature which is a lambda-list like list accepting the keys &OPTIONAL and &REST and which takes instead of variable names placeholders for domains in which the arguments of a operation must be."))

(defgeneric match-signature (operation args)
  (:documentation "Return true if the list of objects ARGS matches the signature of OPERATION as arguments.")
  (:method :around (operation args)
    (when (consp args)
      (call-next-method)))
  (:method ((operation operation-domain) args)
    (funcall (second (slot-value operation 'signature)) args)))



(defclass operation (object) ()
  (:metaclass operation-domain))
(defmethod slot-unbound ((class domain) instance (slot-name (eql 'list-of-domain)))
  (setf (slot-value instance slot-name) (make-instance 'list-domain :domain instance)))
(defgeneric operationp (object)
  (:documentation "Return true if OBJECT is an operation object.")
  (:method ((object operation)) t))
(defgeneric domain-of (object)
  (:documentation "Return the domain of OBJECT.")
  (:method ((object object))
    (class-of object)))
(defmethod match-signature ((operation operation) args)
  (match-signature (domain-of operation) args))



;;;;============================================================================
;;;;* Expressions
;;;;
;;;; Expressions are s-expresions of the form expression == atom | (operation . list-of-expressions)
;;;; operation is an object of class operation and standard-math-object, atom is an object of some
;;;; math-object type
;;;;
;;;; Expressions are built by parsing s-expressions of the form atom | (operation-name . list-of-expressions)
;;;; where operation-name is a symbol which stands for an operation and atom is some object which can
;;;; be assigned to a domain by calling (domain-of atom).
;;;; The parsing of an expression (op . args) proceeds by first determining the domains of args, possibly
;;;; by parsing recursively the expressions in args, and then trying to match for the operation name
;;;; op one of the associated signatures stored in the hash-table at slot operations of the current
;;;; axiomset object. The first matching signature returns the associated operation object #<op>. The resulting
;;;; expression is given by (cons #<op> args).

;;;;** Type Expression
;;;;
(deftype compound-expression ()
  'cons)
(deftype variable-expression ()
  'symbol)
(deftype atomic-expression ()
  'atom)
(deftype expression ()
  '(or atomic-expression compound-expression))

(declaim (inline expr-op expr-args make-expr)
         (ftype (function (compound-expression) expression) expr-op expr-args)
         (ftype (function (expression (awl:list-of expression)) compound-expression) make-expr))
(defun expr-op (expression)
  (car expression))
(defun expr-args (expression)
  (cdr expression))
(defun make-expr (op args)
  (cons op args))

;;;;** Parser of Expressions
;;;;
;;;;*** Parse Failure Conditions
(define-condition parse-failure (error)
  ((expression :initarg :expression :reader parse-failure-expression)))
(define-condition parse-failure-unknown-operation (parse-failure)
  ((operation :initarg :operation :reader parse-failure-unknown-operation-operation)
   (codomain :initarg :codomain :reader parse-failure-unknown-operation-codomain)
   (args :initarg :args :reader parse-failure-unknown-operation-args)))
(defmethod initialize-instance :after ((instance parse-failure-unknown-operation)
                                       &key operation args)
  (setf (slot-value instance 'expression) (make-expr operation args)))
(define-condition parse-failure-unknown-constant (parse-failure)
  ((domain :initarg :domain :reader parse-failure-unknown-constant-domain)))

;;;;*** Parser Implementation
(defun parse-atom (atom &optional domain)
  (if domain
      (or (when (domainp atom domain)
            atom)
          (find-constants atom domain)
          (when (symbolp atom)
            (make-instance domain :name atom))
          (error 'parse-failure-unknown-constant
                 :expression atom :domain domain))
      (or (find-constants atom)
          (error 'parse-failure-unknown-constant
                 :expression atom))))

(defun parse-operator (operator args &optional codomain
                       &aux (domain (operation (mapcar 'domain-of args) codomain)))
  "Parse the operator of name OPERATOR with codomain CODOMAIN and arguments ARGS matching the signature of OPERATOR. CODOMAIN can be NIL in which case it is supposed unknown."
  (etypecase operator
    (atomic-expression (parse-atom operator domain))
    (expression (parse-topdown operator domain))))

(defun parse-topdown (expression domain)
  "Parse EXPRESSION \"top-down\" by first finding the toplevel object matching DOMAIN."
  (etypecase expression
    (atomic-expression (parse-atom expression domain))
    (expression (let ((expr-args (expr-args expression)))
                  (mapcan (lambda (op)
                            (mapcar (lambda (args) (make-expr op args))
                                    (cartesian-product* (mapcar 'parse-topdown
                                                                expr-args
                                                                (signature op expr-args)))))
                          (parse-operator (expr-op expression) expr-args domain))))))

(defun parse-bottomup (expression)
  (etypecase expression
    (atomic-expression (parse-atom expression))
    (expression (let ((expr-op (expr-op expression)))
                  (mapcan (lambda (args)
                            (mapcar (lambda (op) (make-expr op args))
                                    (parse-operator expr-op args)))
                          (cartesian-product* (mapcar 'parse-bottomup (expr-args expression))))))))

(defun parse-expression (expression &optional domain)
  "Return a list of parsed expressions. EXPRESSION is of type expression."
  (if domain
      (parse-topdown expression domain)
      (parse-bottomup expression)))

;;;;

(define-compiler-macro parse-expression (expression &optional domain)
  (if domain
      `(parse-topdown ,expression ,domain)
      `(parse-bottomup ,expression)))

(define-compiler-macro parse-atom (atom &optional domain)
  `(let ((atom ,atom))
     ,(if domain
          `(let ((domain ,domain))
             (or (when (and domain (in-domain-p atom domain))
                   atom)
                 (find-constants atom domain)
                 (when (symbolp atom)
                   (make-instance domain :name atom))
                 (error 'parse-failure-unknown-constant
                        :expression atom :domain domain)))
          `(or (find-constants atom)
               (error 'parse-failure-unknown-constant
                      :expression atom :domain nil)))))

(define-compiler-macro parse-operator (operator args &optional codomain)
  (let ((domain `(operation ,(when args `(list ,@(mapcar (lambda (arg) `(domain-of ,arg)) args)))
                            ,@(when codomain (list codomain)))))
    `(let ((operator ,operator))
       (etypecase operator
         (atomic-expression (parse-atom operator ,domain))
         (expression (parse-topdown operator ,domain))))))

;;;;* Axiom Set
;;;;
;;;; An axiomset is an object holding the defined categories, domains and the rules
;;;; for operations.
(defvar *axioms*)

(defclass axiomset ()
  ((categories :initform (make-hash-table))
   (domains :initform (make-hash-table))
   (operations :initform (make-hash-table))
   (constants :initform (make-hash-table))))

(defun find-domain (domain)
  (etypecase domain
    (symbol (gethash domain (slot-value *axioms* 'domains)))
    (category domain)))
(defun (setf find-domain) (value domain)
  (let ((name (etypecase domain
                (symbol domain)
                (domain (class-name domain)))))
    (check-type value domain)
    (setf (gethash name (slot-value *axioms* 'domains)) value)))

(defmethod find-constants (name (domain symbol))
  (find-constants name (find-domain domain)))
(defmethod (setf find-constants) (value name (domain symbol))
  (setf (find-constants name (find-domain domain)) value))

(defmethod find-constants ((name symbol) (domain null))
  "Return the constants in domain DOMAIN whose name is NAME. DOMAIN may be NIL, in which case return all constants with name NAME."
  (gethash name (slot-value *axioms* 'constants)))
(defmethod (setf find-constants) (value (name symbol) (domain null))
  (setf (gethash name (slot-value *axioms* 'constants)) value))

(defun find-operations (name &key codomain (args nil argsp) signature)
  "Return a list of all operations matching the specified keys as follows:
- NAME matches the name of the operation;
- CODOMAIN: if CODOMAIN is a domain, the codomains of the returned operations are subdomains of CODOMAIN, if CODOMAIN is a function, it is a predicate function on a domain as single argument -- those operations for which the predicate returns true are taken;
- SIGNATURE: the signatures of the returned operations have the same number of arguments and the domains in SIGNATURE are subdomains of the respective domains in the signatures of the returned operations,
- ARGS: the list ARGS is a valid argument list for the returned operations."
  (declare (type domain codomain) (ignore signature))
  (let ((ops (gethash name (slot-value (or codomain *axioms*) 'operations))))
    (if argsp (awl:filter (lambda (op) (match-signature op args)) ops) ops)))

(defun find-category (category)
  (etypecase category
    (symbol (gethash category (slot-value *axioms* 'categories)))
    (category category)))
(defun (setf find-category) (value category)
  (let ((name (etypecase category
                (symbol category)
                (category category))))
    (check-type value category)
    (setf (gethash name (slot-value *axioms* 'categories)) value)))

(defmethod initialize-instance :after ((instance axiomset) &key &allow-other-keys)
  (let ((*axioms* instance))
    (setf (find-category 't) (find-class 'domain)
          (find-domain 't) (find-class 'top-object))))

(defmacro with-axiomset (axiomset &body body)
  `(let ((*axioms* ,axiomset))
     ,@body))

(defun find-operation-signature-maps (operation-name)
  (gethash operation-name (slot-value *axioms* 'operations)))
(defun (setf find-operation-signature-maps) (value operation-name)
  (setf (gethash operation-name (slot-value *axioms* 'operations)) value))

(defmethod list-of ((domain symbol))
  (list-of (find-domain domain)))

;;;;** Initialization and Reinitialization of Categories

(defmethod category-parameters ((category cons))
  (check-type (car category) category)
  (rest category))
(defun parameter-name (parameter)
  (car parameter))
(defun parameter-axioms (parameter)
  (cdr parameter))
(defun (setf parameter-axioms) (value parameter)
  (setf (cdr parameter) value))

(defun compute-supercategories (category)
  "Return a list of all list of conses (supcat . params) where supcat is among all supercategories of CATEGORY and params is the list of parameter names coming from CATEGORY applied to that supercategory; e.g. for the category (abelian + 0) a supercategory is (group + 0) whereas the category group was defined multiplicatively, i.e. with the parameters * and 1."
  (declare (type category category))
  (let ((parameters (category-parameters category))
        (orig-param-names (mapcar 'parameter-name (category-parameters category))))
    (remove-duplicates (mapcan (lambda (supcat)
                                 (let ((category (funcall (compile nil `(lambda ,orig-param-names
                                                                          (declare (ignorable ,@orig-param-names))
                                                                          ,(destructure supcat (lambda (x)
                                                                                                 (member x orig-param-names)))))
                                                          parameters)))
                                   (cons category (compute-supercategories (car category)))))
                               (slot-value category 'direct-supercategories))
                       :test 'equalp)))

(defun compute-axioms (category)
  (declare (type category category))
  (dolist (parameter (category-parameters category))
    (dolist (supercategory (compute-supercategories category))
      (ignore-errors (extend-axioms (cdr parameter) (nth (position parameter (cdr supercategory))
                                                         (category-parameters (car supercategory))))))))


(defun make-signature-matchers (signature operation)
  "Return a function matching arguments of an operation against a pattern. The list of arguments is destructured using a lambdalist of similar structure as SIGNATURE. SIGNATURE is like an ordinary lambdalist except that, instead of symbols naming variables, the domains in which the arguments have to reside are listed."
  (multiple-value-bind (required optional restp rest keyp)
      (awl:parse-lambda-list signature) 
    (assert (not keyp) () "Keys are not allowed in signatures of operations.") 
    (let* ((g!required (mapcar 'awl:g!sym required))
           (g!optional (mapcar (lambda (o) (if (consp o) `(,(awl:g!sym (first o)) (get-instance ',(first o) ',(second o))) (awl:g!sym o))) optional))
           (g!rest (when restp (awl:g!sym rest)))
           (g!vars (append g!required (mapcar 'car (mapcar 'awl:ensure-list g!optional))))
           (domains (append required (mapcar 'car (mapcar 'awl:ensure-list optional)))))
      (flet ((compute-matchers (body)
               (compile nil `(lambda (args)
                               (ignore-errors
                                (destructuring-bind (,@g!required ,@(when optional (list* '&optional g!optional)) ,@(when restp (list '&rest g!rest)))
                                    args
                                  ,@body))))))
        (list signature
              (compile nil (compute-matchers `((when (and ,@(mapcar (lambda (var domain) `(in-domain-p ,var ',domain)) g!vars domains)
                                                          ,@(when restp `((every (lambda (var) (in-domain-p var ',rest)) ,g!rest))))
                                                 ',operation))))
              (compile nil (compute-matchers `((when (and ,@(mapcar (lambda (var domain) `(subdomainp ,var ',domain)) g!vars domains)
                                                          ,@(when restp `((every (lambda (var) (subdomainp var ',rest)) ,g!rest))))
                                                 ',operation))))
              (compile nil (compute-matchers `((list* ,@(mapcar (lambda (domain) `',domain) domains)
                                                      ,(when restp `(mapcar (constantly ',rest) ,g!rest)))))))))))
;;;;*** Axioms
(defclass axioms () ())
(defclass operation-axioms (axioms)
  ((signature :initarg :signature :reader operation-signature)
   (domain :initarg :domain :reader operation-domain)
   (equivalence-rules :initarg :equivalence-rules :accessor operation-equivalence-rules)
   (inference-rules :initarg :inference-rules :accessor operation-inference-rules)))

(defgeneric copy-axioms (axioms)
  (:documentation "Return an instance of a subtype of the class AXIOMS which, in substance, is equivalent to the axioms given in AXIOMS.")
  (:method ((axioms operation-axioms))
    (make-instance 'operation-axioms
                   :signature (operation-signature axioms)
                   :domain (operation-domain axioms)
                   :equivalence-rules (operation-equivalence-rules axioms)
                   :inference-rules (operation-inference-rules axioms))))

(defgeneric extend-axioms (axioms other-axioms)
  (:method ((axioms operation-axioms) (other-axioms operation-axioms))
    (pushnew (operation-equivalence-rules other-axioms) (operation-equivalence-rules axioms))
    (pushnew (operation-inference-rules other-axioms) (operation-inference-rules axioms))
    axioms)
  (:method ((axioms null) other-axioms)
    (copy-axioms other-axioms)))


(defmethod shared-initialize :around ((instance category) slot-names &key parameters direct-supercategories &allow-other-keys)
  (let* ((direct-superclasses (mapcar (awl:f-comp 'find-category 'car) direct-supercategories))
         (direct-supercategories (mapcar (lambda (class category) (setf (car category) class))
                                         direct-superclasses direct-supercategories)))
    (call-next-method instance slot-names :direct-superclasses direct-superclasses
                                          :direct-supercategories direct-supercategories
                                          :parameters parameters)))
(defmethod shared-initialize :after ((instance category) slot-names &key &allow-other-keys)
  (compute-axioms instance))


(defun ensure-category (name parameters super-categories)
  (let ((category (find-category name)))
    (if category
        (reinitialize-instance category :direct-supercategories super-categories
                                        :parameters parameters)
        (setf (find-category name) (make-instance 'standard-class :direct-supercategories super-categories
                                                                  :parameters parameters :name name)))))


(defmethod shared-initialize ((instance domain) slot-names &key &allow-other-keys)
  ())

(defun ensure-domain (name category parameters)
  (declare (type category category))
  (let ((domain (find-domain name)))
    (if domain
        (let ((domain-category (class-of domain)))
          (if (eq domain-category category)
              (reinitialize-instance domain :parameters parameters)
              (change-class domain category))))
    (let ((domain (make-instance domain
                                 :name name
                                 :parameters (mapcan 'list (category-parameters category) parameters))))
      (setf (find-domain name) domain)
      name)))










(defmethod initialize-instance :after ((instance operation-axioms) &key signature &allow-other-keys)
  (setf (slot-value instance 'signature) (make-signature-matcher signature)))

(defun match-signature-p (operation args)
  (declare (type operation operation))
  (funcall (slot-value operation 'signature) args))


(defun expand-domaine-definer (name category &key &allow-other-keys)
  ()
  (awl:with-gensyms (g!*)
    `(progn
       (defclass ,name (domaine) ()
         (:metaclass ,category))
       (defclass ,g!* (,name operator) () (:metaclass ,category))
       (defun ,* (&rest args)
         (apply #'make-expression ',g!* args))
       (defmethod equiv ((op ,g!*) (arg1 ,name) (arg2 ,g!*))
         (with-slots ((arg2.1 arg1) (arg2.2 arg2))
             arg2
           (,* (,* arg1 arg2.1) arg2.2))))))
(defun expand-operator-definer (name signature type rules)
  )




;;;;** Some Appartenance Tests
(defun of-category-p (object category)
  (let ((category (etypecase category
                    (cons (find-category (car category)))
                    (symbol (find-category category))
                    (category category)))
        (parameters (when (consp category) (cdr category))))
    (if parameters
        (and (typep object category)
             (every 'eql (parameters category object) parameters))
        (typep object category))))


(defgeneric in-domain-of-category-p (object category)
  (:method (object (category category))
    (of-category-p (class-of object) category)))


;;;;* Transformation Rules

(defun apply-rule (expr rule)
  "Return the transformation of the expression EXPR by the rule RULE, if possible, NIL else."
  (let ((env (ignore-errors (awl:unify expr (rule-pattern rule)
                                       (lambda (s)
                                         (and (symbolp s)
                                              (not (member s (constants rule)))))
                                       'eq))))
    (when env
      (apply (rule-template rule)
             (mapcar (lambda (s) (awl:lookup s env))
                     (rule-symbols rule))))))

(defun get-toplevel-transforms (expr type ruleset)
  "Return a list of all direct transformations of the expression EXPR using the ruleset RULESET on top level, i.e. without applying transformations on the arguments of EXPR."
  (awl:filter (lambda (rule) (apply-rule expr rule))
              (get-equivalence-rules expr type ruleset)))


(defun get-direct-transforms (expr ruleset &aux (op (expr-op expr)) (args (expr-args expr)))
  "Return a list of all direct transformations of the expression EXPR using the ruleset RULESET."
  (append (get-toplevel-transforms expr ruleset)
          (mapcan (let ((arg-pos 0))
                    (lambda (transforms)
                      (let ((first-args (subseq args 0 arg-pos))
                            (last-args (subseq args (incf arg-pos))))
                        (mapcar (lambda (trans) (make-comp-expr op (append first-args (list trans) last-args))) transforms))))
                  (mapcar (lambda (arg) (get-direct-transforms arg ruleset)) args))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (symb "AWL")
    (export symb "AWL")))
