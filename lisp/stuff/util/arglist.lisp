(define-condition arg-condition ()
  ((arg-type :initform "argument" :reader arg-type)
   (arg-name :initarg :arg-name :reader arg-name)
   (arg-desc :initarg :arg-desc :reader arg-desc)))
(define-condition keyword-mixin ()
  ((arg-type :initform "keyword")))
(define-condition missing-arg-condition (arg-condition)
  () (:report (lambda (condition stream)
             (with-slots (arg-name arg-type arg-desc) condition
               (format stream "The ~A ~A is missing. Description: ~A"
                       arg-type arg-name arg-desc)))))
(define-condition invalid-arg-condition (arg-condition)
  ((arg-value :initarg :arg-value :reader arg-value)
   (arg-expect :initarg :arg-expect :reader arg-expect))
  (:report (lambda (condition stream)
             (with-slots (arg-name arg-type arg-desc arg-value arg-expect) condition
               (format stream "The ~A ~A is invalid. Current value: ~A~&Expected value: ~A~&Description: ~A"
                       arg-type arg-name arg-value arg-expect arg-desc)))))
(define-condition missing-arg-error (error missing-arg-condition)
  ())
(define-condition missing-keyword-error (keyword-mixin missing-arg-error)
  ())
(define-condition invalid-arg-error (error invalid-arg-condition)
  ())
(define-condition invalid-keyword-error (keyword-mixin invalid-arg-error)
  ())
(defun signal-missing-keyword (arg-name arg-desc)
  (let ((err (make-condition 'missing-keyword-error :arg-name arg-name :arg-desc arg-desc)))
    (restart-case (error err)
      (use-value (new-value)
        :interactive (lambda ()
                       (format t "Enter a new value for the ~A ~A: " (arg-type err) (arg-name err))
                       (multiple-value-list (eval (read))))
        :report (lambda (err stream) (format nil "Use a value for the ~A ~A." (arg-type err) (arg-name err)))
        (setq bar new-value)))
    (format t "The bar is ~A." bar)))


(defun foo (&key (bar (signal-missing-keyword 'bar "Enter the bar and take a beer.")))
  bar)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-list-keyword-p (arg)
    (member arg lambda-list-keywords))
  (defun extract-mandatory-args (arglist)
    (loop for arg in arglist until (lambda-list-keyword-p arg) collect arg))
  (defun extract-optional-args (arglist)
    (loop for arg in (cdr (member '&optional arglist))
       until (lambda-list-keyword-p arg)
       collect (typecase arg
                 (symbol (list arg nil))
                 (cons arg))))
  (defun extract-keyword-args (arglist)
    (loop for arg in (cdr (member '&key arglist))
       until (lambda-list-keyword-p arg)
       collect (typecase arg
                 (symbol (list (intern (symbol-name arg) :keyword) arg))
                 (cons (destructuring-bind (a &optional init-form &rest rest) arg
                         (let ((key (etypecase a
                                      (symbol (intern (symbol-name a) :keyword))
                                      (cons (car a))))
                               (var (etypecase a
                                      (symbol a)
                                      (cons (cadr a)))))
                           (append (list key (if init-form
                                                 (list var init-form)
                                                 var))
                                   rest)))))))
  (defun extract-rest-arg (arglist)
    (second (member '&rest arglist)))
  (defun extract-aux-args (arglist)
    (loop for arg in (cdr (member '&aux arglist))
       until (lambda-list-keyword-p arg)
       collect (typecase arg
                 (symbol (list arg nil))
                 (cons arg))))
  (defun extract-var-symbols (arglist)
    (append (extract-mandatory-args arglist)
            (mapcar (lambda (arg)
                      (etypecase arg
                        (symbol arg)
                        (cons (car arg))))
                    (extract-optional-args arglist))
            (mapcar (lambda (arg)
                      (let ((arg (second arg)))
                        (etypecase arg
                          (symbol arg)
                          (cons (car arg)))))
                    (extract-keyword-args arglist))
            (mapcar (lambda (arg)
                      (etypecase arg
                        (symbol arg)
                        (cons (car arg))))
                    (extract-aux-args arglist))
            (list (extract-rest-arg arglist)))))



(defun add-new-property (plist key val)
  "Destructively add the key `key' and the value `val' to the property list `plist' if `key' is not already a key in `plist'. Return the resulting property list."
  (unless (get-properties plist (list key))
    (setf (getf plist key) val))
  plist)