
#|----------------------------------------------------------------------------
Artificial Intelligence, Second Edition
Elaine Rich and Kevin Knight
McGraw Hill, 1991

This code may be freely copied and used for educational or research purposes.
All software written by Kevin Knight.
Comments, bugs, improvements to knight@cs.cmu.edu
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------
			    A-STAR SEARCH
			    "a-star.lisp"
----------------------------------------------------------------------------|#

;; -------------------------------------------------------------------------

(defstruct sn		; search node
 f 
 g 
 h 
 parent 
 children 
 state)

(defvar *open*)
(defvar *closed*)

(defvar *nodes-expanded*)


;; -------------------------------------------------------------------------
;; Functions for manipulating the open and closed lists.  The implementation
;; is straightforward; the open list is kept ordered by the f values of its
;; nodes, smallest first. A more efficient implementation would use priority 
;; queues to allow fast insertion and deletion of best nodes.

(defun make-empty-nodelist ()
  nil)

(defun empty-nodelist? (nodelist)
  (null nodelist))

(defun add-to-nodelist (node nodelist)
  (cond ((eq nodelist 'closed)
	 (setq *closed* (cons node *closed*)))
	((eq nodelist 'open)
	 (cond ((or (null *open*)
		    (< (sn-f node) (sn-f (car *open*))))
	        (setq *open* (cons node *open*)))
	       (t
		(do ((x *open*)
		     (y (cdr *open*)))
		    ((or (null y)
			 (< (sn-f node) (sn-f (car y))))
		     (cond ((null y) 
			    (setf (cdr x) (list node)))
		     	   (t
			    (setf (cdr x) (cons node y)))))
		   (setq x y)
		   (setq y (cdr y))))))))

(defun remove-front-of-nodelist (nodelist)
   (cond ((eq nodelist 'open)
	  (let ((firstnode (car *open*)))
	     (setq *open* (cdr *open*))
	     firstnode))
	 (t
	  (error "Error. Can only remove from open list."))))

(defun find-node-in-nodelist (state nodelist)
  (find-if #'(lambda (n)
		  (eq-states state (sn-state n)))
	(if (eq nodelist 'open) *open* *closed*)))

(defun change-priority-of-node (node nodelist)
  (cond ((eq nodelist 'open)
	 (setq *open* (delete node *open*))
	 (add-to-nodelist node 'open))
	(t
	 (error "Error. Can only change priorities of nodes on open list."))))


;; -------------------------------------------------------------------------
;; Function A-STAR does a heuristic search from the start to the goal state,
;; returning a solution path.  It maintains two lists of nodes, called open
;; and closed, and expands most promising nodes first.

(defun a-star (start &optional verbose)
   (setq *nodes-expanded* 0)
   (setq *open* (make-empty-nodelist)) 
   (setq *closed* (make-empty-nodelist))
   (let ((start-node (make-sn :state start
			      :g 0
			      :h (heuristic start)
			      :f (heuristic start)
			      :children nil
			      :parent nil)))
     (add-to-nodelist start-node 'open))
   (do ((failure (empty-nodelist? *open*)) 
	(goal-found nil))
       ((or failure goal-found)
	(if failure "No solution." (extract-a-star-path goal-found)))
;     (print (mapcar #'(lambda (s) (list (sn-state s) (sn-f s))) *open*))
      (let ((bestnode (remove-front-of-nodelist 'open)))
	 (add-to-nodelist bestnode 'closed)
	 (cond ((goal-state?  (sn-state bestnode))
	 	(setq goal-found bestnode))
	       (t (expand-bestnode bestnode verbose))))))


;; -------------------------------------------------------------------------
;; Function EXPAND-BEST-NODE takes the node with the best heuristic score 
;; and expands it.  If the successors are already on the open or closed
;; lists, it updates heuristic scores; otherwise, it adds them to the open
;; list.

(defun expand-bestnode (bestnode verbose)
  (when verbose (format t "Expanding node ~d~%" (sn-state bestnode)))
  (setf *nodes-expanded* (1+ *nodes-expanded*))
  (let ((successors (expand (sn-state bestnode))))
     (do ((s successors (cdr s)))
	 ((null s) nil)
       (let* ((succ (car s))
	      (old-open (find-node-in-nodelist succ 'open)))
	  (cond (old-open 		;  step 2c
		 (add-child bestnode old-open)
		 (possibly-change-parent old-open bestnode 'open))
		(t
		 (let ((old-closed (find-node-in-nodelist succ 'closed)))
		   (cond (old-closed	;  step 2d
			  (add-child bestnode old-closed)
			  (possibly-change-parent old-closed bestnode 
						  'closed))
			 (t
			  (let ((new-node (make-sn :state succ
					      	   :g (+ (sn-g bestnode) 
						         (cost-of-move 
							     (sn-state bestnode)
							     succ))
					           :h (heuristic succ)
					           :parent bestnode
					           :children nil)))
			     (setf (sn-f new-node) (+ (sn-g new-node) 
						      (sn-h new-node)))
			     (add-child bestnode new-node)
			     (add-to-nodelist new-node 'open)))))))))))


;; -------------------------------------------------------------------------
;; Function ADD-CHILD modifies a node to have a new successor.

(defun add-child (parent child)
   (setf (sn-children parent) (cons child (sn-children parent))))


;; -------------------------------------------------------------------------
;; Function POSSIBLY-CHANGE-PARENT takes a new node that is already on the 
;; open or closed lists, and changes the old node's g, f, and parent fields
;; if the new path is shorter than the old one.

(defun possibly-change-parent (old bestnode nodelist)
  (let ((old-cost (sn-g old))
	(cost-through-bestnode (+ (sn-g bestnode) 
				  (cost-of-move (sn-state bestnode)
						(sn-state old)))))
     (when (> old-cost cost-through-bestnode)
	   (setf (sn-parent old) bestnode)
	   (setf (sn-g old) cost-through-bestnode)
	   (setf (sn-f old) (+ (sn-g old) (sn-h old)))
	   (cond ((eq nodelist 'open)
		  (change-priority-of-node old 'open))
		 ((eq nodelist 'closed)
		  (propagate-cost old))))))

;; -------------------------------------------------------------------------
;; Function PROPAGATE-COST takes a node and ensures that the heuristic 
;; estimates of its descendant nodes are accurate.

(defun propagate-cost (old)
    (do ((children (sn-children old) (cdr children)))
	((null children) nil)
      (let ((child (car children)))
	(when (or (eq old (sn-parent child))
		  (< (+ (sn-g old) 
			(cost-of-move (sn-state old)
				      (sn-state child)))
		     (sn-g child)))
	   (setf (sn-g child) (+ (sn-g old) 1))
	   (setf (sn-f child) (+ (sn-g child) (sn-h child)))
	   (setf (sn-parent child) old)
	   (when (member child *open*)
	       (change-priority-of-node child *open*))
	   (propagate-cost child)))))


;; -------------------------------------------------------------------------
;; Function EXTRACT-PATH retrieves a solution path by tracing parent pointers
;; of a node.

(defun extract-a-star-path (node)
  (do ((path nil) 
       (n node (sn-parent n)))
      ((null n) path)
    (setq path (cons (sn-state n) path))))

