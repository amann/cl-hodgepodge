(in-package #:cl-user)
(defpackage #:generator
  (:use #:common-lisp))
(in-package #:generator)

(defun make-num-generator (from &key to (step 1))
  (labels ((generator (arg)
	     (ecase arg
	       (:test (and (numberp from)
			   (<= to from)))
	       (:get from)
	       (:next (unless (generator :test)
			(prog1
			    from
			  (incf from step)))))))
    generator))

(defmacro defgen (name arglist &body body)
  (let* ((label name)
	 (name (intern (concatenate 'string "MAKE-" (string label))))
	 (arg (gensym "ARG")))
    `(defun ,name ,arglist
      (labels ((,label (,arg)
		 (ecase ,arg
		   ,@body)))
	#',label))))

(defgen num-gen (from &key to (step 1))
  (:test (and (numberp from)
	      (<= to from)))
  (:get from)
  (:next (unless (num-gen :test)
	   (prog1
	       from
	     (incf from step)))))

(and (alistp body)
     (= 3 (length body))
     (let ((keys (mapcar #'car body)))
       (and (member :test keys)
	    (member :get  keys)
	    (member :next keys))))

(loop for a in '(a s d f a s d f)
	       when (eql a 's) count a into s
	       when (eql a 'd) count a into d
	       when (eql a 'f) count a into f
	       finally (return (list s d f)))

(defun alistp (object)
  (and (listp object)
       (progn
	 (map nil #'(lambda (x)
	     (unless (consp x)
	       (return-from 'alistp nil)))
	   object))))
