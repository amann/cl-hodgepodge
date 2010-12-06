;;;; $Source: H:/cvsroot/common-lisp/lib/oam-clos.lisp,v $
;;;; $Revision: 1.2 $

(in-package :cl)
(defpackage :ch.amann-wolowyk.oam-clos
  (:nicknames #:oam-clos)
  (:use :cl
	#+allegro :clos
	#+cmu :pcl
	#+lispworks :hcl
	#+sbcl :sb-mop
	#+clisp :mop
        #+openmcl :openmcl-mop)
  ;; CMU needs to get these things from PCL, not CL, as they are
  ;; different because it has this weirdo wrapper stuff such that
  ;; cl:standard-class is not pcl::standard-class & so on.  This will,
  ;; I hope, go away at some point, but it's true for cmucl 18c
  ;; 2000-09-07.
  #+cmu
  (:shadowing-import-from :pcl #:standard-class #:built-in-class
			  #:find-class #:class-name #:class-of)
  #+genera
  (:import-from :clos-internals #:validate-superclass)
  (:export #:abstract-class
	   #:final-class
           #:cached-class
           #:singleton-class
           #:get-effective-method))

(in-package :ch.amann-wolowyk.oam-clos)
(provide :ch.amann-wolowyk.oam-clos.abstract-class)
(provide :ch.amann-wolowyk.oam-clos.final-class)
(provide :ch.amann-wolowyk.oam-clos.cached-class)
(provide :ch.amann-wolowyk.oam-clos.singleton-class)




(defclass abstract-class (standard-class)
  ()
  (:documentation "The class of abstract classes."))

(defmethod make-instance ((c abstract-class) &rest rest)
  (declare (ignore rest))
  (error "Trying to make an instance of ~A which is an abstract class."
	 (class-name c)))

(defmethod validate-superclass ((class abstract-class) 
				(superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass abstract-class))
  t)




(defclass final-class (standard-class)
  ()
  (:documentation "The class of classes which may not be subclassed."))

(defmethod validate-superclass ((class final-class) 
				(superclass standard-class))
  t)

(defmethod validate-superclass ((class final-class #+nil standard-class)
				(superclass final-class))
  (error "Attempting to subclass a final class"))



(defclass cached-class (standard-class)
  ((cache-fn :initform (error "No caching function given.")
             :initarg :cache-fn
             :documentation "A function with arglist (mode key &optional value) => instance.
Arguments and values:
mode --- ( :get | :set )
key ---  the key; the key used is the initialization argument list as given with MAKE-INSTANCE
value --- the value, i.e. a fresh instance, to be associated with key when mode == :set; is ignored in the other modes
instance --- the cached instance of the class associated to key or nil

This function is a map key -> instance."))
  (:documentation "Instances of classes of this class are cached which means that if when calling make-instance the initargs satisfy a certain predicate a cached (and maybe reinitialized) instance is returned instead of a freshly created one."))
(defmethod shared-initialize :after ((self cached-class) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (with-slots (cache-fn) self
    (when (consp cache-fn)
      (setf cache-fn (car cache-fn)))))


(defmethod make-instance :around ((class cached-class) &rest rest)
  (or (let ((old-instance (funcall (slot-value class 'cache-fn) :get rest)))
        (when old-instance
          (apply #'reinitialize-instance old-instance rest)))
      (let ((new-instance (call-next-method)))
        (funcall (slot-value class 'cache-fn) :set rest new-instance))))

(defmethod validate-superclass ((class cached-class)
                                (superclass standard-class))
  t)
(defmethod validate-superclass ((class standard-class)
                                (superclass cached-class))
  t)

(defclass singleton-class (cached-class)
  ((cache-fn :initform (let (cache)
                         (lambda (mode key &optional value)
                           (declare (ignore key))
                           (ecase mode
                             (:get cache)
                             (:set (setq cache value)))))))
  (:documentation "The class of classes of which exactly one instance exists and is stored in the slot single-instance."))

(defmethod shared-initialize :after ((class singleton-class) slot-names
                                     &key instance-initargs
                                     &allow-other-keys)
  (declare (ignore slot-names))
  (apply #'make-instance class instance-initargs))


;;;;** Methods
;;;; We define here some utilities for methods.
(defun get-effective-method (gf args)
  "Return the effective method of the generic function GF which is used when applying the mandatory arguments ARGS."
  (compute-effective-method gf (generic-function-method-combination gf) (compute-applicable-methods gf args)))





