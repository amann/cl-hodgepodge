;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")



;;;;* Metaclasses
;;;;** Abstract Class
(defclass awl::abstract-class (standard-class) ())
(defmethod c2mop:validate-superclass ((class awl::abstract-class) (superclass standard-class)) t)
(defmethod c2mop:validate-superclass ((class standard-class) (superclass awl::abstract-class)) t)
(defmethod allocate-instance ((class awl::abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "The class ~A is abstract and cannot be instanciated." (class-name class)))

;;;;** Wrapper Class
(defclass wrapper () ()
  (:metaclass awl::abstract-class))
(defmethod (setf c2mop:slot-value-using-class) :around (new-value class (object wrapper) slotd)
  (let ((slot-type (or (c2mop:slot-definition-type slotd) t)))
    (unless (typep new-value slot-type)
      (error 'type-error :expected-type slot-type :datum new-value)))
  (call-next-method))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))