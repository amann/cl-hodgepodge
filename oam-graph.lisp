

(oam:define-project-package #:ch.amann-wolowyk.oam-graph #:graph)
(in-package #:ch.amann-wolowyk.oam-graph-system)

(defstruct (cell (:copier) (:conc-name) (:constructor))
  wrapper properties)

(defstruct (abstract-graph
             (:include cell) (:copier) (:conc-name) (:constructor)
             (:print-object (lambda (self stream)
                              (print-unreadable-object (self stream :type t :identity t)
                                (format stream "~@[ Properties: ~A~] Vertices: ~A~
                                                                     Edges: ~A"
                                        (properties self) (vertices self) (edges self))))))
  (vertices nil :read-only t)
  (edges nil :read-only t))
(defstruct (graph::weak-graph
             (:include abstract-graph
                       (vertices (make-hash-table :test #'eq
                                                  #+(or clisp openmcl) :weak
                                                  #+sbcl :weakness :key) :read-only t)
                       (edges (make-hash-table :test #'eq
                                               #+(or clisp openmcl) :weak
                                               #+sbcl :weakness :key) :read-only t))
             (:copier) (:conc-name)
             (:constructor graph::make-weak-graph ())))
(defstruct (graph::graph
             (:include abstract-graph
                       (vertices (make-hash-table :test #'eq) :read-only t)
                       (edges (make-hash-table :test #'eq) :read-only t))
             (:copier) (:conc-name)
             (:constructor graph::make-graph ())))

(defstruct (graph-element (:include cell)
                          (:copier) (:conc-name) (:constructor))
  (containing-graphs (make-hash-table :test #'eq) :read-only t))
(defstruct (graph::vertex
             (:include graph-element)
             (:copier) (:conc-name)
             (:constructor %make-vertex (wrapper properties))
             (:print-object (lambda (self stream)
                              (print-unreadable-object (self stream :type t :identity t)
                                (format stream "~@[ Properties: ~A~] In: ~A Out: ~A"
                                        (properties self) (in self) (out self))))))
  (in (make-hash-table :test #'eq) :read-only t)
  (out (make-hash-table :test #'eq) :read-only t))


(defstruct (graph::edge
             (:include graph-element)
             (:copier) (:conc-name)
             (:constructor %make-edge (origin terminus wrapper properties))
             (:print-object (lambda (self stream)
                              (print-unreadable-object (self stream :type t :identity t)
                                (format stream "~@[ Properties: ~A~] Origin: ~A~
                                                                     Terminus: ~A"
                                        (properties self) (origin self) (terminus self))))))
  (origin nil)
  (terminus nil))

(defun graph::contains-p (graph object)
  (handler-case
      (nth-value 1 (gethash object (etypecase object
                                     (graph::vertex (vertices graph))
                                     (graph::edge (edges graph)))))
    (error ())))
(defun graph::element-of-p (object graph)
  (handler-case
      (nth-value 1 (gethash graph (containing-graphs object)))
    (error ())))
(defun graph::element-of (object)
  (loop :for key :being :each :hash-key :of (containing-graphs object)
     :collect key))
(defun graph::wrapper (object)
  (wrapper object))
(defun (setf graph::wrapper) (wrapper object)
  (setf (wrapper object) wrapper))

(defun graph::properties (object &optional complement)
  (handler-case
      (if complement
          (gethash object (etypecase complement
                            (abstract-graph (etypecase object
                                              (graph::vertex (vertices complement))
                                              (graph::edge (edges complement))))
                            (graph-element (containing-graphs complement))
                            (symbol (ecase complement
                                      (:origin (out (origin object)))
                                      (:terminus (in (terminus object)))))))
          (values (properties object) t))
    (error () (values nil nil))))

;;;; TODO: Not sure if it's correct!!!!
(defsetf graph::properties (object &optional complement) (properties)
  `(handler-case
       (values
        (setf ,(if complement
                   (gethash object
                            (etypecase complement
                              (abstract-graph (if (graph::element-of-p object complement)
                                                  (etypecase object
                                                    (graph::vertex (vertices complement))
                                                    (graph::edge (edges complement)))
                                                  (error "Not applicable.")))
                              (graph-element (if (graph::element-of-p complement object)
                                                 (containing-graphs complement)
                                                 (error "Not applicable.")))
                              (symbol (ecase complement
                                        (:origin (out (origin object)))
                                        (:terminus (in (terminus object)))))))
                   (properties object))
              ,properties)
               t)
     (error () (values ,properties nil))))

(oam:fbind ((make-vertex (symbol-function '%make-vertex)))
  (defun graph::make-vertex (&key wrapper properties)
    (make-vertex wrapper properties))
  (fmakunbound '%make-vertex)
  (unintern '%make-vertex))

(defun graph::add-vertex (graph vertex
                          &key vertex-graph-properties graph-vertex-properties)
  (setf (gethash vertex (vertices graph)) graph-vertex-properties
        (gethash graph (containing-graphs vertex)) vertex-graph-properties))

(oam:fbind ((make-edge (symbol-function '%make-edge)))
  (defun graph::make-edge (origin terminus &key wrapper properties
                           origin-properties terminus-properties)
    (assert (vertex-p origin))
    (assert (vertex-p terminus))
    (let ((new-edge (make-edge origin terminus wrapper properties)))
      (setf (gethash new-edge (out origin)) origin-properties
            (gethash new-edge (in terminus)) terminus-properties)
      new-edge))
  (fmakunbound '%make-edge)
  (unintern '%make-edge))

(defun graph::add-edge (graph edge &key edge-graph-properties graph-edge-properties)
  (setf (gethash edge (edges graph)) graph-edge-properties
        (gethash graph (containing-graphs edge)) edge-graph-properties))

(defun graph::rem-edge (graph edge)
  (remhash edge (edges graph))
  (remhash graph (containing-graphs edge)))


(defun make-iterator (iterator-name list body)
  `(macrolet ((,iterator-name ()
                `(let ((list ,',list))
                   (pop list))))
     ,@body))

(defmacro graph::with-vertex-iterator ((iterator graph &key (eof-value nil eof-value-p))
                                       &body body)
  (oam:with-gensyms (hashtable-iterator)
    `(macrolet
         ((,iterator ()
            `(multiple-value-bind (foundp key value)
                 (,',hashtable-iterator)
               (declare (ignore value))
               (if foundp
                   key
                   ,,(if eof-value-p
                         eof-value
                         (error "No next element."))))))
       (with-hash-table-iterator (,hashtable-iterator (vertices ,graph))
         ,@body))))

(defmacro graph::with-edge-iterator ((iterator object &key (in-out :all)
                                               (eof-value nil eof-value-p))
                                     &body body)
  (oam:with-gensyms (ob io hashtable-iterator)
    `(let ((,ob ,object)
           (,io ,in-out))
       (case ,io
         (:all (macrolet
                   ((,iterator ()
                      `(multiple-value-bind (foundp key value)
                           (,',hashtable-iterator)
                         (declare (ignore value))
                         (if foundp
                             key
                             (multiple-value-bind (foundp key value)
                                 (,',io)
                               (declare (ignore value))
                               (if foundp
                                   key
                                   ,,(if eof-value-p
                                         eof-value
                                         (error "No next element."))))))))
                 (with-hash-table-iterator (,hashtable-iterator (in ,ob))
                   (with-hash-table-iterator (,io (out ,ob))
                     ,@body))))
         (t (macrolet
                ((,iterator ()
                   `(multiple-value-bind (foundp key value)
                        (,',hashtable-iterator)
                      (declare (ignore value))
                      (if foundp
                          key
                          ,,(if eof-value-p
                                eof-value
                                (error "No next element."))))))
              (with-hash-table-iterator
                  (,hashtable-iterator (etypecase ,ob
                                         (abstract-graph (edges ,ob))
                                         (graph::vertex (ecase ,in-out
                                                          (:in (in ,ob))
                                                          (:out (out ,ob))))))
                ,@body)))))))
(defmacro graph::with-containing-graph-iterator
    ((iterator graph-element &key (eof-value nil eof-value-p)) &body body)
  (oam:with-gensyms (hashtable-iterator)
    `(macrolet
         ((,iterator ()
            `(multiple-value-bind (foundp key value)
                 (,',hashtable-iterator)
               (declare (ignore value))
               (if foundp
                   key
                   ,,(if eof-value-p
                         eof-value
                         (error "No next element."))))))
       (with-hash-table-iterator (,hashtable-iterator (containing-graphs ,graph-element))
         ,@body))))



(defun graph::del-edge (edge)
  (graph::with-containing-graph-iterator (next-graph edge :eof-value nil)
    (loop :for graph := (next-graph) :while graph
       :do (graph::rem-edge graph edge)))
  (remhash edge (out (origin edge)))
  (setf (origin edge) nil)
  (remhash edge (in (terminus edge)))
  (setf (terminus edge) nil))

(defun graph::edge-valid-p (edge)
  (and (origin edge) (terminus edge) t))

(defun graph::rem-vertex (graph vertex)
  (graph::with-edge-iterator (next-edge vertex :eof-value nil)
    (loop :for edge := (next-edge) :while edge
       :do (graph::rem-edge graph edge)))
  (remhash vertex (vertices graph))
  (remhash graph (containing-graphs vertex)))

(defun graph::del-vertex (vertex)
  (graph::with-containing-graph-iterator (next-graph vertex :eof-value nil)
    (loop :for graph := (next-graph) :while graph
       :do (graph::rem-vertex graph vertex)))
  (graph::with-edge-iterator (next-edge vertex :eof-value nil)
    (loop :for edge := (next-edge) :while edge
       :do (graph::del-edge edge))))


;;;;*** Wrappers 
(defclass graph::wrapper-mixin ()
  ((pointer :initarg :graph-element :accessor graph::graph-element)))
(defclass graph::vertex-mixin (graph::wrapper-mixin) ())
(oam:with-slot-definition-locations (pointer) graph::vertex-mixin
  (defmethod graph::vertex ((wrapper graph::vertex-mixin))
    (c2mop:standard-instance-access wrapper pointer)))
(defclass graph::edge-mixin (graph::wrapper-mixin) ())
(oam:with-slot-definition-locations (pointer) graph::edge-mixin
  (defmethod graph::edge ((wrapper graph::edge-mixin))
    (c2mop:standard-instance-access wrapper pointer)))
(defclass graph::graph-mixin (graph::wrapper-mixin) ())
(oam:with-slot-definition-locations (pointer) graph::graph-mixin
  (defmethod graph::graph ((wrapper graph::graph-mixin))
    (c2mop:standard-instance-access wrapper pointer)))

(defmacro define-symmetric-binary-method (gf ((var1 spec1) (var2 spec2)) &body body)
  `(progn
     (defmethod ,gf ((,var1 ,spec1) (,var2 ,spec2))
       ,@body)
     (defmethod ,gf ((,var2 ,spec2) (,var1 ,spec1))
       ,@body)))

(defgeneric graph::detach (object1 object2)
  (:documentation "Generic function to detach a graph element from another object as the wrapper or a property. This operator should be symmetric. For convenience wrapper and properties mixin-classes are already defined which detach the graph element from the wrapper resp. the properties."))
(define-symmetric-binary-method graph::detach
    ((graph graph::graph) (edge graph::edge))
  (graph::rem-edge graph edge))
(define-symmetric-binary-method graph::detach
    ((graph graph::graph) (vertex graph::vertex))
  (graph::rem-vertex graph vertex))
(define-symmetric-binary-method graph::detach
    ((wrapper graph::graph-mixin) (object cell))
  (setf (wrapper object) nil)
  (slot-makunbound wrapper 'pointer))




(oam:export-interface '#:graph)