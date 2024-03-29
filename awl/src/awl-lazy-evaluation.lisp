;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;============================================================================
;;;;* Lazy Evaluation
;;;;** Delay/Force
;;;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro awl::delay (&body exprs &environment env)
    "Return a delay for the expressions EXPRS. The expressions as a whole are guaranteed to be evaled once at most when forced. The result when forcing the delay is the result of the last expression of EXPRS as of with PROGN."
    (if (notevery (lambda (expr) (constantp expr env)) exprs)
        `(let* ((delay (cons '#1=#:delay nil))
                (funct (lambda () (setf (car delay) '#2=#:forced
                                        (cdr delay) (progn ,@exprs)))))
           (setf (cdr delay) funct)
           delay)
        `(cons '#2# (progn ,@exprs))))
  (declaim (inline awl::force))
  (defun awl::force (o)
    "Return the result of the expressions delayed with the macro DELAY."
    #+sbcl (declare (optimize speed))
    (if (consp o)
        (let ((car (car o)))
          (cond ((eq car '#2#) (cdr o))
                ((eq car '#1#) (funcall (the function (cdr o))))
                (t o)))
        o))
  (declaim (inline awl::force-delay)
           (ftype (function (cons)) awl::force-delay))
  (defun awl::force-delay (delay)
    "Return the result of the expressions delayed with the macro DELAY. The result is unspecified and most probably an error if the input is not a delay produced with the macro DELAY."
    (declare (optimize speed (safety 1)))
    (if (eq (car delay) '#2#) (cdr delay) (funcall (the function (cdr delay)))))
  (deftype awl::delay ()
    `(cons (or (eql #1#) (eql #2#)) *)))

;;;;** Pipes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro awl::make-pipe (head &body tail)
    "Return a pipe (a lazy list) containing as first element HEAD and in which the elements in the tale are generated by successively evaluating (progn . tail) in the current lexical environment."
    `(cons ,head (cons (lambda () ,@tail) '#1=#:tail)))
  (defun awl::tail (pipe)
    "Return a pipe representing the tail of PIPE."
    (let ((tail (rest pipe)))
      (when (and (consp tail) (eq '#1# (cdr tail)))
        (let ((fn (car tail)))
          (setf (car tail) (funcall fn)
                (cdr tail) (cons fn '#1#))))
      tail)))
(setf (fdefinition 'awl::head) (fdefinition 'first)
      (documentation 'awl::head 'function) (documentation 'first 'function))
(defun awl::pipe-empty-p (pipe)
  (null pipe))



