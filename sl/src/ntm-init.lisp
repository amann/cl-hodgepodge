;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: ics-ntm.lisp,v 1.2 2010/05/17 16:01:44 amao Exp $
;;;;* Module NTM
(oam:define-project-package #:ch.swisslife.ntm #:ntm)

(in-package #:ch.swisslife.ntm-system)
;;;;** Utilities for NTM processing
;;;;*** NTM-Node
;;;; A NTM-node is a structure inheriting of the oxml:xml-element
(defstruct (ntm::ntm-node (:include oxml:xml-element)))

(defclass ntm::ntm-element ()
  ((ntm-node :type ntm::ntm-node :accessor ntm-node)))

(defun ntm::make-node (name &optional attributes &rest values)
  (make-ntm-node :name (oxml:check-xml-name name) :attributes (apply #'oxml:check-xml-attributes attributes)
                    :children (oxml:check-xml-value values)))

(defgeneric ntm::get-nodes* (node path))

(defmethod ntm::get-nodes* ((node ntm::ntm-node) path)
  (apply #'oxml:find-xml-elements node path))
(defmethod ntm::get-nodes* ((node ntm::ntm-element) path)
  (ntm::get-nodes* (ntm-node node) path))

(defun ntm::get-nodes (node &rest path)
  (ntm::get-nodes* node path))

(defmethod (setf ntm::get-nodes*) (value (ntm-node ntm::ntm-node) path)
  (let ((nodes (apply #'oxml:find-xml-elements ntm-node path)))
    (dolist (node nodes)
      (setf (oxml:xml-element-value node) (ntm::format-ntm-node-value (ntm-node-name ntm-node) (ntm-node-name node) value))))
  value)

(defmethod (setf ntm::get-nodes*) (value (node ntm::ntm-element) path)
  (setf (ntm::get-nodes* (ntm-node node) path) value))

(defun (setf ntm::get-nodes) (value node &rest path)
  (setf (ntm::get-nodes* node path) value))

(defgeneric ntm::insert-values* (ntm-node path place values))

(defmethod ntm::insert-values* ((ntm-node ntm::ntm-node) path place values)
  (dolist (node (apply #'oxml:find-xml-elements ntm-node path) ntm-node)
    (apply #'oxml:insert-values node place (ntm::format-ntm-node-value (ntm-node-name ntm-node) (ntm-node-name node) values))))

(defmethod ntm::insert-values* ((node ntm::ntm-element) path place values)
  (ntm::insert-values* (ntm-node node) path place values))

(defun ntm::insert-values (node path place &rest values)
  (ntm::insert-values* node path place values))

(defgeneric ntm::rename-node (node path new-name))

(defmethod ntm::rename-node ((ntm-node ntm::ntm-node) path new-name)
  (setf (oxml:name (apply #'oxml:find-xml-elements ntm-node path)) new-name))

(defmethod ntm::rename-node ((node ntm::ntm-element) path new-name)
  (ntm::rename-node (ntm-node node) path new-name))

(defgeneric ntm::set-values-of-sub-nodes* (node common-path path-values))

(defmethod ntm::set-values-of-sub-nodes* ((ntm-node ntm::ntm-node) common-path path-values)
  (dolist (node (apply #'oxml:find-xml-elements ntm-node common-path))
    (dolist (class (oam:classify (loop
                                    :for (path . values) :in (oam:alist<-plist path-values)
                                    :nconc (mapcar (lambda (subnode)
                                                     (cons subnode values))
                                                   (apply #'oxml:find-xml-elements node path)))
                                 :key #'car))
      (setf (oxml:xml-element-value (car (first class))) (mapcan #'cdr class)))))

(defmethod ntm::set-values-of-sub-nodes* ((node ntm::ntm-element) common-path path-values)
  (ntm::set-values-of-sub-nodes* (ntm-node node) common-path path-values))

(defun ntm::set-values-of-sub-nodes (node common-path &rest path-values)
  (ntm::set-values-of-sub-nodes* node common-path path-values))


;;;;**** Node parsing/instanciating macro
(defvar ntm::*default-cons-dom-structure* :sxml)

(eval-when (:compile-toplevel)
  (setq ntm::*default-cons-dom-structure* :sxml))

(defun destructure-node (node &key (input-type ntm::*default-cons-dom-structure*))
  (destructuring-bind (name (&rest attributes &key &allow-other-keys) &rest values)
      (ecase input-type
        (:lxml (etypecase node
                 (symbol (list node nil))
                 (cons (let ((name (first node)))
                         (list (if (consp name)
                                   (first name)
                                   name)
                               (when (consp name)
                                 (rest name))
                               (rest node))))))
        (:sxml (list* (first node)
                      (multiple-value-bind (values attributes)
                          (oam:remove-if (lambda (value) (and (consp value)
                                                              (eq :@ (car value)))) (rest node))
                        (list* (mapcan #'rest attributes) values)))))
    (values name attributes values)))

(defun ntm::parse-node (node &key ntm-context (input-type ntm::*default-cons-dom-structure*))
  (multiple-value-bind (name attributes values)
      (destructure-node node :input-type input-type)
    (apply #'ntm::make-node name (oam:mapcan-plist (lambda (key value)
                                                     (list key (ntm::format-ntm-attribute-value ntm-context name key value)))
                                                   attributes)
           (mapcar (lambda (value)
                     (ntm::format-ntm-node-value ntm-context name value))
                   values))))
(defmacro ntm::node ((&key ntm-context-form (input-type ntm::*default-cons-dom-structure*)) node)
  "Macro parsing a cons representing an xml-dom of type *DEFAULT-CONS-DOM-STRUCTURE* and returning an NTM-NODE."
  (multiple-value-bind (name attributes children)
        (destructure-node node :input-type input-type)
      (let* ((ntm-context (gensym "NTM-CONTEXT"))
             (attributes-bindings (oam:map-plist (lambda (key value)
                                                   (list (gensym (string key))
                                                         `(ntm::format-ntm-attribute-value ,ntm-context ',name ',key ,value)))
                                                 attributes))
             (children-bindings (mapcar (lambda (child)
                                          (list (gensym) (typecase child
                                                           (cons `(ntm::node (:ntm-context-form ,ntm-context) ,child))
                                                           (t `(ntm::format-ntm-node-value ,ntm-context ',name ,child)))))
                                      children)))
        `(let* ((,ntm-context (or ,ntm-context-form ',name))
                ,@attributes-bindings
                ,@children-bindings)
           (declare (ignorable ,ntm-context))
           (ntm::make-node ',name
                           (list ,@(mapcan (lambda (key binding)
                                             `(',key ,(first binding)))
                                           (oam:keys+values<-plist attributes)
                                           attributes-bindings))
                           ,@(mapcar #'first children-bindings))))))




(defconstant ntm::date-format :iso8601)

(defgeneric ntm::format-ntm-node-value (ntm-context node-name value)
  (:documentation "Return a string representing the value VALUE of the NTM-node NODE in the NTM context NTM-CONTEXT as given in the macro NODE.")
  (:method (ntm-context node-name (value null)) (declare (ignore ntm-context node-name)) nil)
  (:method (ntm-context node-name (value string)) (declare (ignore ntm-context node-name)) value)
  (:method (ntm-context node-name (value symbol)) (declare (ignore ntm-context node-name)) (string value))
  (:method (ntm-context node-name (value real)) (declare (ignore ntm-context node-name)) (format nil "~F" value))
  (:method (ntm-context node-name (value integer)) (declare (ignore ntm-context node-name)) (format nil "~D" value))
  (:method (ntm-context node-name (value date:date-time)) (declare (ignore ntm-context node-name)) (date::format-date value ntm::date-format))
  (:method (ntm-context node-name (value cons)) (declare (ignore node-name)) (ntm::parse-node value :ntm-context ntm-context))
  (:method (ntm-context node-name (value ntm::ntm-node)) (declare (ignore ntm-context node-name)) value)
  (:method (ntm-context node-name (value ntm::ntm-element)) (declare (ignore ntm-context node-name)) (ntm-node value)))



(defgeneric ntm::format-ntm-attribute-value (ntm-context node-name key value)
  (:documentation "Return a string representing the value VALUE of the attribute KEY of an NTM-node NODE in the NTM context NTM-CONTEXT as given in the macro NODE.")
  (:method (ntm-context node-name key (value null)) (declare (ignore ntm-context node-name key)) "")
  (:method (ntm-context node-name key (value string))(declare (ignore ntm-context node-name key))  value)
  (:method (ntm-context node-name key (value symbol)) (declare (ignore ntm-context node-name key)) (string value))
  (:method (ntm-context node-name key (value real)) (declare (ignore ntm-context node-name key)) (format nil "~F" value))
  (:method (ntm-context node-name key (value integer)) (declare (ignore ntm-context node-name key)) (format nil "~D" value))
  (:method (ntm-context node-name key (value date:date-time)) (declare (ignore ntm-context node-name key)) (date:format-date value ntm::date-format)))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern "space" '#:XML)
  (export 'XML::|space| '#:XML)
  #+ (or) (ntm::enable-ntm-syntax))

(define-condition ntm::missing-mandatory-field-error (error)
  ((ntm::field-name :initarg :field-name :reader ntm::field-name))
  (:report (lambda (e s)
             (format s "The field ~A is mandatory." (ntm::field-name e)))))
(defun ntm::mandatory (field-name)
  (error 'ntm::missing-mandatory-field-error :field-name field-name))
;;;;** Basic NTM-elements



(defun ntm-element-name (ntm-element)
  (ntm-node-name (ntm-node ntm-element)))

(oam:export-interface "NTM")