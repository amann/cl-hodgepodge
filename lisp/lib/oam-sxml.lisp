;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;$Id: oam-sxml.lisp,v 1.1 2010/08/26 11:25:42 amao Exp $
(eval-when (:compile-toplevel :load-toplevel :execute)
           (asdf:oos 'asdf:load-op 's-xml))
;;;;* Extensions to S-XML
;;;;** Error conditions
(defpackage #:ch.amann-wolowyk.s-xml-error
  (:nicknames #:s-xml-error)
  (:export #:node-not-found
           #:immutable-node))
(define-condition s-xml-error:node-not-found (error)
  ((s-xml-error::parent-node :initarg :parent-node :reader s-xml-error::parent-node)
   (s-xml-error::target-tag :initarg :target-tag :reader s-xml-error::target-tag))
  (:report (lambda (e s)
             (with-slots (s-xml-error::parent-node s-xml-error::target-tag) e
               (format s "The node ~S contains ~[no~:;at most ~:*~D~] node~:P with name ~S."
                       s-xml-error::parent-node
                       (or (when (consp s-xml-error::target-tag)
                             (cadr s-xml-error::target-tag)) 0)
                       (or (when (consp s-xml-error::target-tag)
                             (car s-xml-error::target-tag))
                           s-xml-error::target-tag))))))
(define-condition s-xml-error:immutable-node (error)
  ((s-xml-error::node :initarg :node :reader s-xml-error::node))
  (:report (lambda (e s)
             (format s "Node ~S immutable." (s-xml-error::node e)))))

;;;;** API to manipulate xml nodes in the sxml or lxml format
;;;; For the moment they are implemented using a case switch on
;;;; the :type key. This should be changed; maybe using a wrapper
;;;; class.
(defvar s-xml::*default-dom-type* :lxml)

(defun s-xml::node (tag attributes values &key (type s-xml::*default-dom-type*))
  (case type
    (:lxml (if attributes
               (cons (cons tag attributes) values)
               (if values
                   (cons tag values)
                   tag)))
    (:sxml (list* tag (cons :@ (oam::alist2<-plist attributes)) values))))

(defun s-xml::get-tag (node &key (type s-xml::*default-dom-type*)) 
  (case type
    (:lxml (if (consp node)
               (if (consp (car node))
                   (caar node)
                   (car node))
               node))
    (:sxml (car node))))

(defun s-xml::set-tag (node new-tag &key (type s-xml::*default-dom-type*))
  (case type
    (:lxml (if (consp node)
               (if (consp (car node))
                   (rplaca (car node) new-tag)
                   (rplaca node new-tag))
               (error 's-xml-error::immutable-node :node node)))
    (:sxml (rplaca node new-tag)))
  node)

(defun s-xml::get-attributes (node &key (type s-xml::*default-dom-type*))
  "Return a property list representing the attributes of NODE."
  (case type
    (:lxml (when (consp node)
             (when (consp (car node))
               (cdar node))))
    (:sxml (oam::plist<-alist2 (mapcan #'(lambda (v)
                                           (when (and (consp v)
                                                      (eq :@ (car v)))
                                             (cdr v)))
                                       (cdr node))))))
(defun s-xml::set-attributes (node attributes &key (type s-xml::*default-dom-type*))
  (case type
    (:lxml (if (consp node)
               (rplaca node (cons (s-xml::get-tag node) attributes))
               (error 's-xml-error::immutable-node :node node)))
    (:sxml (rplacd node (cons attributes (s-xml::get-values node)))))
  node)
(defun s-xml::add-attributes (node attributes &key (type s-xml::*default-dom-type*))
  (let ((attrs (oam::alist<-plist (s-xml::get-attributes node :type type))))
    (map nil #'(lambda (attr)
                 (pushnew attr attrs :key #'car))
         (oam::alist<-plist attributes))
    (s-xml::set-attributes node (oam::plist<-alist attrs) :type type))
  node)

(defun s-xml::get-values (node &key (type s-xml::*default-dom-type*))
  (case type
    (:lxml (when (consp node)
             (cdr node)))
    (:sxml (remove :@ (cdr node) :key #'(lambda (n) (s-xml::get-tag n :type :sxml))))))
(defun s-xml::set-values (node values &key (type s-xml::*default-dom-type*))
  (case type
    (:lxml (if (consp node)
               (rplacd node values)
               (error 's-xml-error::immutable-node :node node)))
    (:sxml (rplacd node (append (s-xml::get-attributes node) values))))
  node)
(defun s-xml::insert-values (node values place
                             &key (type s-xml::*default-dom-type*))
  (case type
    (:lxml (if (consp node)
               (oam::insert* values (cdr node) place)
               (error 's-xml-error::immutable-node :node node)))
    (:sxml (s-xml::set-values node (oam::insert* values (s-xml::get-values node) place))))
  node)

;;;;** Some node manipulation facilities
;;;; They (should) depend only on the API defined in previous section
;;;; without worrying about the effective type of the underlying dom
;;;; format. However the key argument :type is still present. This
;;;; should be changed.
(defun s-xml::find-child (node child-path &key (type s-xml::*default-dom-type*))
  "Return as values the child-node and its position in the list of values of NODE if this child-node exists else throw an error of type s-xml-error:node-not-found. Child-path is  a list containing the tag of the child node to find and the occurence or the tag only, in which case the occurence is 0."
  (let* ((s-xml::*default-dom-type* type)
         (node-values (s-xml::get-values node))
         child-node pos)
    (destructuring-bind (child-tag child-occurence)
        (if (consp child-path) child-path `(,child-path 0))
     (when node-values
       (flet ((find-pos (start)
                (position child-tag node-values
                          :start start :key #'s-xml::get-tag)))
         (let* ((repeat child-occurence))
           (setq pos (loop with pos = -1 repeat repeat
                        do (setq pos (or (find-pos (1+ pos)) pos))
                        finally (return (find-pos (1+ pos))))
                 child-node (when pos
                              (nth pos node-values)))))))
    (unless child-node
      (error 's-xml-error:node-not-found :parent-node node :target-tag child-path))
    (values child-node pos)))

(defun s-xml::delete-child (node child-path &key (type s-xml::*default-dom-type*))
  (let* ((s-xml::*default-dom-type* type))
    (multiple-value-bind (child-node child-pos)
        (s-xml::find-child node child-path)
     (values (s-xml::set-values node (delete (s-xml::get-tag child-node)
                                             (s-xml::get-values node)
                                             :start child-pos :count 1
                                             :key #'s-xml::get-tag))
             child-pos))))

(defun s-xml::replace-child (node child-path new-child
                             &key (type s-xml::*default-dom-type*))
  (let* ((s-xml::*default-dom-type* type))
    (multiple-value-bind (node child-pos)
        (s-xml::delete-child node child-path)
     (values (s-xml::insert-values node `(,new-child) child-pos)
             child-pos))))

(defun s-xml::find-node (node-path xml-dom &key (type s-xml::*default-dom-type*))
  (values-list (reduce #'(lambda (result node-tag)
                           (let ((parent-node (car result)))
                             (multiple-value-bind (child-node pos)
                                 (s-xml::find-child parent-node node-tag :type type)
                               (list child-node parent-node (cons pos (caddr result))))))
                       node-path
                       :initial-value (list xml-dom))))


(defun s-xml::node-set-values (node-path values xml-dom
                               &key (type s-xml::*default-dom-type*))
  (let ((s-xml::*default-dom-type* type))
    (multiple-value-bind (target parent)
        (s-xml::find-node node-path xml-dom)
      (handler-case
          (s-xml::set-values target values)
        (s-xml-error::immutable-node ()
          (let ((target (s-xml::node (s-xml::get-tag target)
                                     (s-xml::get-attributes target)
                                     values)))
            (s-xml::replace-child parent (car (last node-path)) target))))))
  xml-dom)
(defun s-xml::node-insert-values (node-path values place xml-dom
                                  &key (type s-xml::*default-dom-type*))
  (let ((s-xml::*default-dom-type* type))
    (multiple-value-bind (target parent)
        (s-xml::find-node node-path xml-dom)
      (handler-case
          (s-xml::insert-values target values place)
        (s-xml-error::immutable-node ()
          (let ((target (s-xml::node (s-xml::get-tag target)
                                     (s-xml::get-attributes target)
                                     (oam::insert* values (s-xml::get-values target)
                                                   place))))
            (s-xml::replace-child parent target (car (last node-path))))))))
  xml-dom)
(defun s-xml::rename-node (node-path new-name xml-dom
                           &key (type s-xml::*default-dom-type*))
  (let ((s-xml::*default-dom-type* type))
    (multiple-value-bind (target parent)
        (s-xml::find-node node-path xml-dom)
      (handler-case
          (s-xml::set-tag target new-name)
        (s-xml-error::immutable-node ()
          (let ((target (s-xml::node new-name
                                     (s-xml::get-attributes target)
                                     (s-xml::get-values target))))
            (s-xml::replace-child parent target (car (last node-path))))))))
  xml-dom)

;;;;** @-xml DOM
;;;;
;;;; This DOM is intended to be useful as a lisp serialization of XML in for of
;;;; s-expressions. It follows the syntax of s-xml except that the first element
;;;; of each node is the symbol @. This symbol is bound to a macro in such a way
;;;; that, when reading, the @-xml expression is transformed to another DOM.