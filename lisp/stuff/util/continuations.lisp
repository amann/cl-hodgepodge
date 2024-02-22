(defun divide (num denum)
  (restart-case (/ num denum)
    (use-denum (new-denum)
       :report "Specify a value to use this time."
       :interactive (lambda () (format t "Enter a new value for denum: ") (multiple-value-list (eval (read)))) 
      (/ num new-denum))))

(defun careful-symbol-value (symbol)
  (check-type symbol symbol)
  (restart-case (if (boundp symbol)
                    (return-from careful-symbol-value 
                      (symbol-value symbol))
                    (error 'unbound-variable
                           :name symbol))
    (use-value (value)
      :report "Specify a value to use this time."
      :interactive (lambda () (format t "Enter a new value: ") (multiple-value-list (eval (read)))) 
      value)
    (store-value (value)
      :report "Specify a value to store and use in the future."
      :interactive (lambda () (format t "Enter a new value: ") (eval (read))) 
      (setf (symbol-value symbol) value))))

(define-condition food-error (error) ())

(define-condition bad-tasting-sundae (food-error) 
  ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
   (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
   (topping :initarg :topping :reader bad-tasting-sundae-topping))
  (:report (lambda (condition stream)
             (format stream "Bad tasting sundae with ~S, ~S, and ~S"
                     (bad-tasting-sundae-ice-cream condition)
                     (bad-tasting-sundae-sauce condition)
                     (bad-tasting-sundae-topping condition)))))

(defun all-start-with-same-letter (symbol1 symbol2 symbol3)
  (let ((first-letter (char (symbol-name symbol1) 0)))
    (and (eql first-letter (char (symbol-name symbol2) 0))
         (eql first-letter (char (symbol-name symbol3) 0)))))

(defun read-new-value ()
  (format t "Enter a new value: ")
  (multiple-value-list (eval (read))))

(defun verify-or-fix-perfect-sundae (ice-cream sauce topping)
  (do ()
      ((all-start-with-same-letter ice-cream sauce topping))
    (restart-case
        (error 'bad-tasting-sundae
               :ice-cream ice-cream
               :sauce sauce
               :topping topping)
      (use-new-ice-cream (new-ice-cream)
        :report "Use a new ice cream."
        :interactive read-new-value  
        (setq ice-cream new-ice-cream))
      (use-new-sauce (new-sauce)
        :report "Use a new sauce."
        :interactive read-new-value
        (setq sauce new-sauce))
      (use-new-topping (new-topping)
        :report "Use a new topping."
        :interactive read-new-value
        (setq topping new-topping))))
  (values ice-cream sauce topping))

(verify-or-fix-perfect-sundae 'vanilla 'caramel 'cherry)



(defun add (cont &rest args)
  (funcall cont (apply #'+ args )))
(defmacro cps (fun cont))

(flet ((=+ (*cont* &rest args)
         (apply #'+ args)))
  (macrolet ((+ (&rest args)
               ))))
(progn
  (defmacro add (a b c d &optional (e 3) &rest args)
    (cons '=add (cons '*cont* ('a 'b 'c 'd 'e 'args))))
 (defun =add (*cont* a b c d &optional (e 3) &rest args) (apply #'+ args)))


(defun cons-call-arglist (arglist)
  (loop with status = nil
     for arg in arglist
     until (eq arg '&aux)
     do (case arg
          (&rest (setq status = &rest))
          (&key )
          (&optional )
          (t ))))



(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f *cont* ,@',(extract-var-symbols parms)))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))








(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))

(setq *saved* nil)

(=defun dft-node (tree)
	(cond ((null tree) (restart))
	      ((atom tree) (=values tree))
	      (t (push #'(lambda () (dft-node (cdr tree)))
		       *saved*)
		 (dft-node (car tree)))))

(=defun restart ()
	(if *saved*
	    (funcall (pop *saved*))
	  (=values 'done)))

(=defun dft2 (tree)
	(setq *saved* nil)
	(=bind (node) (dft-node tree)
	       (cond ((eq node 'done) (=values nil))
		     (t (princ node)
			(restart)))))

