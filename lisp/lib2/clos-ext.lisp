;;;; $Source: /home/remote/oam/CVSrepository/lisp/fsl/clos-ext.lisp,v $
;;;; $Revision: 1.2 $

(in-package :cl)
(defpackage :ch.amann-wolowyk.clos-ext
  (:nicknames #:clos-ext)
  (:use :cl
	#+allegro :clos
	#+cmu :pcl
	#+lispworks :hcl
	#+sbcl :sb-mop
	#+clisp :mop)
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
	   #:function-arg-list
	   #:get-parameter-list))

(in-package :ch.amann-wolowyk.clos-ext)
(provide :ch.amann-wolowyk.abstract-classes)
(provide :ch.amann-wolowyk.final-classes)


(defclass abstract-class (standard-class)
  ()
  (:documentation "The class of abstract classes"))

(defmethod make-instance ((c abstract-class) &rest rest)
  (declare (ignore rest))
  (error "Trying to make an instance of ~A which is an abstract class"
	 (class-name c)))

(defmethod validate-superclass ((class abstract-class) 
				(superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass abstract-class))
  t)




(defclass final-class (standard-class)
  ()
  (:documentation "The class of classes which may not be subclassed"))

(defmethod validate-superclass ((class final-class) 
				(superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass final-class))
  (error "Attempting to subclass a final class"))











;;;;--------------------------------------------------------------------------------------
#+clisp
(defun function-arg-list (function)
  (ext:arglist function))

#+sbcl
(defun function-arg-list (function)
  (sb-introspect:function-arglist function))

#+lucid
  (defun function-arg-list (fn)
    (system::arglist fn))


#+allegro
  (defun function-arg-list (fn)
    (excl::arglist fn))

;;; GCLisp 1.1 version
#+gclisp
  (defun function-arg-list (fn)
    (if (macro-function fn)
	'(&rest "Form =")
	(lambda-list fn)))

;;; KCL version
#+kcl
(defun function-arg-list (fn)
  (let ((x (symbol-function fn)))
    (cond ((atom x) nil)
	  ((eq (first x) 'macro) (list '&rest "Form ="))
	  (t (third x)))))

;;; CMU Common Lisp version.  This version looks in a symbol's
;;; function cell and knows how to take apart lexical closures
;;; and compiled code objects found there.
#+cmu
  (defun function-arg-list (x &optional original-x)
    (cond ((symbolp x) (function-arg-list (symbol-function x) x))
	  ((compiled-function-p x)
	   (read-from-string
	    (lisp::%primitive header-ref x
			      lisp::%function-arg-names-slot)))
	  ((listp x) (case (first x)
		       (lambda (second x))
		       (lisp::%lexical-closure% (function-arg-list (second x)))
		       (system:macro '(&rest "Form ="))
		       (t '(&rest "Arglist:"))))
	  (t (cerror (format nil
                        "Use a reasonable default argument list for ~S"
		        original-x)
		"Unkown object in function cell of ~S:  ~S" original-x x)
	     '())))



(defun get-parameter-list (ordinary-lambda-list)
  (mapcan (let ((state '&ordinary))
	    (lambda (x)
	      (if (member x lambda-list-keywords)
		  (progn
		    (setq state x)
		    nil)
		  (case state
		    (&ordinary
		     (list x))
		    (&optional
		     (list (or (and (consp x) (car x)) x)))
		    ((&rest &aux  &whole) 
		     nil)
		    (&key
		     (let ((x (or (and (consp x) (car x)) x)))
		       (list (intern (symbol-name x) :keyword) x)))))))
	  ordinary-lambda-list))



;;;;==============================================================================
