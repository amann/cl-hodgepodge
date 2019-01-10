;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;* Search Algorithms
;;;;** A* Search
;;;;
;;;;*** Priority Queue
;;;;
;;;; We implement a priority queue as a binary heap.
(define-condition empty-queue-error (error) ())
(defun awl::make-priority-queue (order-predicate &key key
                                                   (element-type t) initial-contents)
  "Return a priority queue implemented as a binary heap on an array. The priority of the items of the queue is determined by comparing with ORDER-PREDICATE the results obtained when applying KEY to the items (when KEY is NIL the items are compared directly). The priority queue supports following methods:
- push item: add item into the queue;
- peek: get the item with highest priority without removing it from the queue;
- pop:  remove the item with the highest priority;
- remove item &key key test test-not: remove an item I from the queue satisfying (test item (key I)) or (not (test-not item (key I))) -- if key is NIL I is tested directly. Return NIL if none is removed;
- size: get the number of items in the queue."
  (declare (type (or symbol (function (* *) *)) order-predicate)
           (type (or null symbol (function (*) *)) key))
  (let ((content (make-array (length initial-contents) :element-type element-type :adjustable t
                                                       :fill-pointer t :initial-contents initial-contents))
        (test (compile nil `(lambda (x y)
                              (funcall ,(typecase order-predicate
                                          (symbol `',order-predicate)
                                          (t order-predicate))
                                       ,@(if key
                                             `((funcall ,key x) (funcall ,key y))
                                             `(x y)))))))
    (flet ((%item (n)
             (if (< -1 n (length content))
                 (aref content n) ;; AREF ignores fill-pointer !!
                 (error "Out of boundary."))))
      (declare (inline %item))
      (flet ((%< (m n)
               (ignore-errors (funcall test (%item m) (%item n)))))
        (declare (inline %<))
        (labels ((%bubble-up (n)
                   (declare (type (mod #.array-dimension-limit) n))
                   (let ((parent-pos (1- (floor (1+ n) 2))))
                     (if (%< n parent-pos)
                         (progn
                           (rotatef (aref content n) (aref content parent-pos))
                           (%bubble-up parent-pos))
                         n)))
                 (%sink-down (n &aux (child-pos-1 (* 2 (1+ n)))
                                  (child-pos-2 (1- child-pos-1)))
                   (let* ((swap-pos (when (%< child-pos-1 n)
                                      child-pos-1))
                          (swap-pos (if (%< child-pos-2 (or swap-pos n))
                                        child-pos-2
                                        swap-pos)))
                     (if swap-pos
                         (progn
                           (rotatef (aref content n) (aref content swap-pos))
                           (%sink-down swap-pos))
                         n))))
          (flet ((%find (item &rest options)
                   (apply #'position item content options))
                 (%push (item)
                   (%bubble-up (vector-push-extend item content)))
                 (%remove (n)
                   (when n
                     (handler-case (prog1 (aref content n)
                                     (setf (aref content n) (vector-pop content))
                                     (%bubble-up (%sink-down n)))
                       (error (e) (error (if (zerop (fill-pointer content))
                                             'empty-queue-error
                                             e)))))))
            (loop :for n :from (1- (floor (length content) 2)) :downto 0
                  :do (%sink-down n))
            (lambda (msg &optional arg &rest options &key key test test-not)
              (declare (ignore key test test-not))
              (ecase msg
                (:content content)
                (:push (%push arg) t)
                (:peek (if (zerop (fill-pointer content))
                           (error 'empty-queue-error)
                           (aref content 0)))
                (:pop (%remove 0))
                (:contains (apply #'%find arg options))
                (:remove (%remove (apply #'%find arg options)))
                (:size (length content))))))))))

;;;;*** A* Search Implementation
(defstruct %node state score cost precedent)
(defun awl::a*-search-factory (cost-fn heuristic-fn neighbours-fn &optional (states-equal-fn #'eql))
  "Return a function taking an initial state and a goal predicate and searching for a path starting from initial state to a state satisfying the goal predicate using the A* algorithm."
  (flet ((cost (state neighbour)
           (funcall cost-fn state neighbour))
         (heuristic (state)
           (funcall heuristic-fn state)) 
         (neighbours (state)
           (funcall neighbours-fn state))
         (states-equal (state1 state2)
           (funcall states-equal-fn state1 state2)))
    (declare (inline cost heuristic neighbours states-equal))
    (lambda (start goalp)
      "Return, if succeding, a path of states going from START to a state satisfying the predicate GOALP. If failing, return NIL."
      (let (visited)
        ;; visited contains all visited states n as key mapped to the backward
        ;; direction (neigbor nearest towards start), the corresponding
        ;; cost g(n) which is up to now minimal and the score f(n).
        (flet ((get-visited (state)
                 (find state visited :key '%node-state :test states-equal-fn)))
          (declare (inline get-visited))
          (flet (((setf get-visited) (value state)
                   (let ((node (get-visited state)))
                     (if node
                         (setf (%node-score node) (%node-score value)
                               (%node-cost node) (%node-cost value)
                               (%node-precedent node) (%node-precedent value))
                         (push value visited))
                     value)))
            (declare (inline (setf get-visited)))
            (flet ((add-visited (state &key score cost precedent)
                     (setf (get-visited state)
                           (make-%node :state state :score  score
                                       :cost cost :precedent precedent))) 
                   (current-cost (state) (let ((node (get-visited state)))
                                           (when node (%node-cost node))))
                   (precedent (state) (let ((node (get-visited state)))
                                        (when node (%node-precedent node)))))
              (declare (inline add-visited current-cost precedent))
              (let ((openset (awl::make-priority-queue #'< :key #'%node-score)))
                (macrolet ((with-state (state (&key score cost precedent) &body body)
                             (let ((g!state (gensym "STATE"))
                                   (g!node (gensym "NODE")))
                               `(let* ((,g!state ,state)
                                       (,g!node (get-visited ,g!state)))
                                  (let ,(mapcan (lambda (v a) (when v `((,v (when ,g!node (,a ,g!node))))))
                                                (list score cost precedent) '(%node-score %node-cost %node-precedent))
                                    (flet ((add-open (state &key score cost precedent)
                                             (assert (eq state ,g!state))
                                             (if ,g!node
                                                 (setf (%node-score ,g!node) score
                                                       (%node-cost ,g!node) cost
                                                       (%node-precedent ,g!node) precedent)
                                                 (setq ,g!node (add-visited state :score score
                                                                                  :cost cost
                                                                                  :precedent precedent)))
                                             (funcall openset :remove ,g!node :test 'eq)
                                             (funcall openset :push ,g!node)))
                                      ,@body))))))
                  (flet ((pop-lowest-scored-open ()
                           (%node-state (funcall openset :pop))))
                    (declare (inline pop-lowest-scored-open))
                    (labels ((extract-path (state path)
                               (if (states-equal state start)
                                   (cons start path)
                                   (extract-path (precedent state) (cons state path))))
                             (expand (state)
                               (let ((current-cost (current-cost state)))
                                 (dolist (neighbour (neighbours state))
                                   (let* ((new-cost (+ (or current-cost 0) (cost state neighbour)))
                                          (new-score (+ new-cost (heuristic neighbour))))
                                     (with-state neighbour (:score score)
                                       (when (or (null score) (< new-score score))
                                         (add-open neighbour :score new-score
                                                             :cost new-cost
                                                             :precedent state)))))
                                 (let ((next-state (handler-case (pop-lowest-scored-open)
                                                     (empty-queue-error ()
                                                       (return-from expand (values (extract-path state nil) current-cost))))))
                                   (if (funcall goalp next-state)
                                       (values (extract-path next-state nil) (current-cost next-state) t)
                                       (expand next-state))))))
                      (if (funcall goalp start)
                          (list start)
                          (progn (add-visited start :score (heuristic start) :cost 0)
                                 (expand start))))))))))))))

(defun awl::breadth-first-search (start goalp children-fn &key (test #'eql))
  (let ((visited (make-hash-table :test test)))
    (flet ((visitedp (node) (nth-value 1 (gethash node visited)))
           (precedent (node) (gethash node visited))
           ((setf precedent) (precedent node) (setf (gethash node visited) precedent)) 
           (goalp (x) (funcall goalp x))
           (children (x) (funcall children-fn x)))
      (declare (inline visitedp precedent (setf precedent) goalp children))
      (labels ((extract-path (node path)
                 (if node
                     (extract-path (precedent node) (cons node path))
                     path))
               (expand (fringe) 
                 (let ((node (first fringe)))
                   (if (goalp node)
                       (extract-path node nil)
                       (let ((fringe (append (rest fringe)
                                             (mapcan (lambda (child)
                                                       (unless (visitedp child)
                                                         (setf (precedent child) node)
                                                         (list child)))
                                                     (children node)))))
                         (when fringe
                           (expand fringe)))))))
        (setf (precedent start) nil)
        (expand (list start))))))


;;;;* Tests

;;;;** A* Search

(defun search-test (start goal &key
                                 (adjacent-fn (lambda (point) (loop :for i :from -1 :upto 1
                                                                    :nconc (loop :for j :from -1 :upto 1
                                                                                 :for p := (+ point i (* j #c(0 1)))
                                                                                 :unless (= point p)
                                                                                   :collect p))))
                                 (equality 'eql)
                                 (obstacles (append (loop :for i :from 10 :upto 20 :collect (complex i 20))
                                                    (loop :for i :from 10 :upto 20 :collect (complex 20 i))))
                                 (domainp (lambda (point) (and (<= 0 (realpart point) 30)
                                                               (<= 0 (imagpart point) 30))))
                                 (cost-fn (lambda (point neighbour) (abs (- neighbour point))))
                                 (heuristic-fn (lambda (point) (abs (- goal point)))))
  (declare (ignorable cost-fn heuristic-fn))
  (flet ((domainp (point) (funcall domainp point)))
    (let ((neighbours-fn (lambda (point) (remove-if (lambda (p) (or (not (domainp p))
                                                                    (member p obstacles :test equality)
                                                                    (= p point)))
                                                    (funcall adjacent-fn point))))) (values
      (awl::breadth-first-search start (lambda (p) (= p goal)) neighbours-fn)
      (let ((get-path (awl::a*-search-factory cost-fn heuristic-fn neighbours-fn '=)))
        (funcall get-path start (lambda (p) (= p goal))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (symb "AWL")
    (export symb "AWL")))
