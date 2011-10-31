;;;; $Source: H:/cvsroot/common-lisp/lib/oam-clos.lisp,v $
;;;; $Revision: 1.2 $

(oam:define-project-package #:ch.amann-wolowyk.oam-clos #:oam-clos)
(in-package #:ch.amann-wolowyk.oam-clos-system)

(provide :ch.amann-wolowyk.oam-clos.abstract-class)
(provide :ch.amann-wolowyk.oam-clos.final-class)
(provide :ch.amann-wolowyk.oam-clos.standard-cached-class)
(provide :ch.amann-wolowyk.oam-clos.singleton-class)




(defclass oam-clos::abstract-class (standard-class)
  ()
  (:documentation "The class of abstract classes."))

(defmethod make-instance ((c oam-clos::abstract-class) &rest rest)
  (declare (ignore rest))
  (error "Trying to make an instance of ~A which is an abstract class."
	 (class-name c)))

(defmethod c2mop:validate-superclass ((class oam-clos::abstract-class) 
				(superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class standard-class)
				(superclass oam-clos::abstract-class))
  t)




(defclass oam-clos::final-class (standard-class)
  ()
  (:documentation "The class of classes which may not be subclassed."))

(defmethod c2mop:validate-superclass ((class oam-clos::final-class) 
				(superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class oam-clos::final-class #+nil standard-class)
				(superclass oam-clos::final-class))
  (error "Attempting to subclass a final class"))



;;;;** Cached Class
;;;;

;;(defclass standard-cached-class-slot-definition (c2mop:slot-definition) ())

(defclass oam-clos::standard-cached-class (standard-class)
  (cache-fn)
  (:documentation "Instances of classes of this class are cached which means that if when calling make-instance the initargs satisfy a certain predicate a cached (and maybe reinitialized) instance is returned instead of a freshly created one. The cache function is a form which evaluates to a function with arglist (mode key &optional value) and which returns the cached instance or nil.
Arguments and values:
mode --- ( :get | :set )
key ---  the key; the key used is the initialization argument list as given with MAKE-INSTANCE
value --- the value, i.e. a fresh instance, to be associated with key when mode == :set; is ignored in the other modes
instance --- the cached instance of the class associated to key or nil

This function is a map key -> instance."))

(defmethod initialize-instance :after ((self oam-clos::standard-cached-class)
                                       &key cache-fn &allow-other-keys)
  (let* ((cache-fn (or (typecase cache-fn
                         (null nil)
                         (function cache-fn)
                         (symbol (symbol-function cache-fn))
                         (cons (let ((first-item (first cache-fn)))
                                 (typecase first-item
                                   (function first-item)
                                   (symbol (symbol-function first-item))
                                   (cons (eval first-item))))))
                       (let ((cache (apply #'oam:make-fixed-multi-hash-table
                                           (mapcar (lambda (s)
                                                     (declare (ignore s))
                                                     (append (list :test #'eql)
                                                             #+(or clisp openmcl) '(:weak :value)
                                                             #+sbcl '(:weakness :value)))
                                                   (c2mop:class-slots self)))))
                         (lambda (mode key &optional value)
                           (case mode
                             (:get (oam:get-multi-hash* cache key))
                             (:set (setf (oam:get-multi-hash* cache key) value)))))))) 
    (setf (slot-value self 'cache-fn) cache-fn)))

(oam:with-slot-definition-locations (cache-fn)
    oam-clos::standard-cached-class
  (let ((unbound-slot (gensym "UNBOUND-SLOT")))
    (defmethod make-instance :around ((class oam-clos::standard-cached-class) &key &allow-other-keys)
      (oam:fbind ((cache (c2mop:standard-instance-access class cache-fn)))
        (or (let* ((candidate (call-next-method))
                   (slot-values (oam:get-slot-values candidate unbound-slot)))
              (or (cache :get slot-values)
                  (cache :set slot-values candidate))))))))

(defmethod c2mop:validate-superclass ((class oam-clos::standard-cached-class)
                                (superclass standard-class))
  t)
(defmethod c2mop:validate-superclass ((class oam-clos::standard-cached-class)
                                (superclass standard-object))
  t)
(defmethod c2mop:validate-superclass ((class standard-class)
                                (superclass oam-clos::standard-cached-class))
  t)

(defclass oam-clos::singleton-class (oam-clos::standard-cached-class)
  ()
  (:default-initargs :cache-fn (let (cache)
                                 #'(lambda (mode key &optional value)
                                     (declare (ignore key))
                                     (case mode
                                       (:get cache)
                                       (:set (setq cache value))))))
  (:documentation "The class of classes of which exactly one instance exists and is stored in the slot single-instance."))

(defmethod shared-initialize :after ((class oam-clos::singleton-class) slot-names
                                     &key instance-initargs
                                     &allow-other-keys)
  (declare (ignore slot-names))
  (apply #'make-instance class instance-initargs))


;;;;** Methods
;;;; We define here some utilities for methods.
(defun oam-clos::get-effective-method (gf args)
  "Return the effective method of the generic function GF which is used when applying the mandatory arguments ARGS."
  (c2mop:compute-effective-method
   gf
   (c2mop:generic-function-method-combination gf)
   (c2mop:compute-applicable-methods-using-classes gf (mapcar #'find-class
                                                              args))))

;;;;** Some other utilities
(defun oam-clos::destroy-object (object)
  (dolist (slot (c2mop:class-slots (class-of object)))
    (slot-makunbound object (c2mop:slot-definition-name slot))))


(oam:export-interface '#:oam-clos)


