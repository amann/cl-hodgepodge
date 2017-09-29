(asdf:oos 'asdf:load-op "closer-mop")
(asdf:oos 'asdf:load-op "oam-util")

(oam:define-project-package #:ch.amann-wolowyk.oam-graph #:graph)
(in-package #:ch.amann-wolowyk.oam-graph-system)

(defclass graph::graph () ())

(defun make-graph ()
  (let ((edges ()))))

(defclass graph::edge () () (:metaclass c2mop:funcallable-standard-class))

(defun add-vertex (graph))
(defun add-edge (graph))

(defmethod initialize-instance :after ((self graph::edge) &key origin terminus)
  (c2mop:set-funcallable-instance-function self
                                           #'(lambda (vertex)
                                               (ecase vertex
                                                 (:origin origin)
                                                 (:terminus terminus))))
  (funcall origin :out self)
  (funcall terminus :in self))
(defmethod graph::origin ((edge graph::edge))
  (funcall edge :origin))
(defmethod graph::terminus ((edge graph::edge))
  (funcall edge :terminus))


(defclass graph::vertex () () (:metaclass c2mop:funcallable-standard-class))
(defmethod initialize-instance :after ((self graph::vertex) &key node incoming-edges outgoing-edges)
  (c2mop:set-funcallable-instance-function self
                                           #'(lambda (get &optional new-edge)
                                               (ecase get
                                                 (:node node)
                                                 (:in (if new-edge
                                                          (setf incoming-edges new-edge)
                                                          (reverse incoming-edges)))
                                                 (:out (if new-edge
                                                           (setf outgoing-edges new-edge)
                                                           (reverse outgoing-edges)))))))
(defmethod graph::in ((vertex graph::vertex))
  (funcall vertex :in))
(defmethod graph::out ((vertex graph::vertex))
  (funcall vertex :out))


(defun graph::incomming-edges (vertex))
(defun graph::outgoing-edges (vertex))
(defun graph::preceeding-vertices (vertex))
(defun graph::following-vertices (vertex))



(oam:export-interface '#:graph)