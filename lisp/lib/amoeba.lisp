

(defpackage :amoeba
  (:use #:cl)
  (:export #:add
	   #:sub
	   #:mul
	   #:div
	   #:amoeba))

(in-package :amoeba)

(defvar *operator*)
(defvar *reducer*)


(defmacro def-generic-combination (name operator default-reducer)
  (let ((operator (eval `(function ,operator)))
	(default-reducer (eval `(function ,default-reducer))))
    `(progn
       (defmethod ,name :around ((a t) &rest rest)
         (declare (ignorable rest))
	 (let ((*operator* ,operator)
	       (*reducer* ,default-reducer))
	   (declare (special *operator* *reducer*))
	   (call-next-method)))
       (defmethod ,name ((a t) &rest rest)
	 (reduce* a rest)))))

(defmacro def-specific-combination (name type reducer)
  (let ((reducer (eval `(function ,reducer))))
    `(defmethod ,name :before ((a ,type) &rest rest)
       (declare (ignore rest))
       (setq *reducer* ,reducer))))

(defun sequence-combine (a b &optional (operator *operator*))
  (let ((n-a (length a))
	(n-b (length b)))
    (when (<= n-a n-b)
      (return-from sequence-combine
	(map (type-of a)
	     operator
	     a b)))
    (sequence-combine b a)))
(defun matrix-combine (a b &optional (operator *operator*))
  (declare (ignore operator))
  (sequence-combine a b #'sequence-combine))
(defun reduce* (a rest)
  (reduce *reducer*
	  rest
	  :initial-value a))
(defun dummy-reducer (x y)
  (declare (ignore y))
  x)

(def-generic-combination add + dummy-reducer)
(def-specific-combination add sequence sequence-combine)
(def-specific-combination add number +)

(def-generic-combination sub - dummy-reducer)
(def-specific-combination sub sequence sequence-combine)
(def-specific-combination sub number -)

(defgeneric scale (a &rest factors))
(defmethod scale ((a sequence) &rest rest)
  (let ((factor (apply #'* rest)))
    (map (type-of a) (lambda (x)
		     (* x factor))
	 a)))
(defmethod scale ((a number) &rest rest)
  (apply #'* a rest))

(defgeneric div (a &rest dividends))
(defmethod div ((a sequence) &rest rest)
  (let ((factor (apply #'* rest)))
    (map (type-of a) (lambda (x)
		     (/ x factor))
	 a)))
(defmethod div ((a number) &rest rest)
  (apply #'/ a rest))

(defvar *alpha* 1)
(defvar *beta* 1)
(defvar *zeta* 1/2)
(defvar *eta* 1/2)
(defvar *n* 20)

(labels
    ((funct (point)
       (declare (special funct cache))
       (or (cdr (assoc point cache :test #'equalp))
	   (cdar (push (cons point (funcall funct point)) cache))))
     (-<= (x y)
       (declare (special order-p))
       (let ((f-x (funct x))
	     (f-y (funct y)))
	 (or (funcall order-p f-x f-y)
	     (equalp f-x f-y))))
     (-< (x y)
       (not (-<= y x)))
     (iteration (worst points best n)
       (declare (special *alpha* *beta* *eta* *zeta*))
       (let* ((centroid (div (apply #'add points) (length points)))
	      (second-worst (car points))
	      (reflexion (add centroid (scale (sub centroid worst) *alpha*))))
	 (cond
	   ((-< best reflexion)
	    (setq worst second-worst
		  best (let ((expansion (add centroid
					     (scale (sub reflexion centroid)
                                                    *beta*))))
			 (if (-< reflexion expansion)
			     expansion
			     reflexion))
		  points (cdr (rplacd (last points) (list best)))))
	   ((and (-< second-worst reflexion)
		 (-<= reflexion best))
	    (setq worst second-worst
		  points (sort (cons reflexion (cdr points)) #'-<=)))
	   (t                           ;(-<= reflexion second-worst)
	    (let ((contraction (add centroid (scale (sub centroid worst) *zeta*))))
	      (cond
		((and (-< worst contraction)
		      (-<= contraction second-worst))
		 (setq worst contraction))
		((and (-< second-worst contraction)
		      (-<= contraction best))
		 (setq worst second-worst
		       points (sort (cons contraction (cdr points)) #'-<=)))
		((-< best contraction)
		 (setq worst second-worst
		       best contraction
		       points (cdr (rplacd (last points) (list best)))))
		(t                      ;(-<= contraction worst)
		 (setq points (sort (cons best
					  (loop
                                             for point in (cdr (reverse (cons worst
                                                                              points)))
                                             collect (add best
                                                          (scale (sub point best)
                                                                 *eta*))))
				    #'-<=)
		       worst (car points)))))))
	 (if (<= n 0)
	     (list best (funct best))
	     (iteration worst points best (1- n))))))
      
  (defun amoeba (funct order-p &rest points)
    (declare (special *n* funct order-p))
    (destructuring-bind (worst &rest points)
	(sort points #'-<=)
      (let ((best (car (last points))))
	(iteration worst points best *n*)))))

