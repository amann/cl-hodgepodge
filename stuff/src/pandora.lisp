

(oam:define-project-package "PANDORA" "PAN")
(in-package "PANDORA-SYSTEM")


(defun let-binding-transform (bindings)
  (mapcar (lambda (binding)
            (etypecase binding
              (symbol (list binding))
              ((cons symbol (cons t null)) binding)))
          bindings))

(defmacro pan::pandoriclet (letargs &body body)
  (let ((letargs (cons '(this) (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (lambda (&rest args)
         (case (first args)
           ((:pandoric-get)
            (destructuring-bind (sym) (rest args)
              (case sym
                ,@(mapcar (lambda (arg) `((,(car arg)) ,(car arg))) letargs)
                (t (error "Unknown pandoric get: ~a" sym)))))
           ((:pandoric-set)
            (destructuring-bind (sym val) (rest args)
              (case sym
                ,@(mapcar (lambda (arg) `((,(car arg)) (setq ,(car arg) val))) letargs)
                (t (error "Unknown pandoric set: ~a ~a" sym val)))))
           (t
            (apply this args)))))))


(defmacro pan::plambda (largs pargs &body body)
  (let ((pargs (let-binding-transform pargs)))
    `(let (this self)
       (declare (ignorable self))
       (setq this (lambda ,largs ,@body)
             self (lambda (&rest args)
                    (case (first args)
                      ((:pandoric-get)
                       (destructuring-bind (sym) (rest args)
                         (case sym
                           ,@(mapcar (lambda (arg) `((,(car arg)) ,(car arg))) pargs)
                           (t (error "Unknown pandoric get: ~a" sym)))))
                      ((:pandoric-set)
                       (destructuring-bind (sym val) (rest args)
                         (case sym
                           ,@(mapcar (lambda (arg) `((,(car arg)) (setq ,(car arg) val))) pargs)
                           (t (error "Unknown pandoric set: ~a ~a" sym val)))))
                      (t
                       (apply this args))))))))

(declaim (inline get-pandoric))
(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))
(defsetf get-pandoric (box sym) (val)
  `(funcall ,box :pandoric-set ,sym ,val))
(defmacro pan::with-pandoric (syms box &body body)
  (let ((g!box (gensym "BOX")))
    `(let ((,g!box ,box))
       (symbol-macrolet (,@(mapcar (lambda (sym) `(,sym (get-pandoric ,g!box ',sym))) syms))
         ,@body))))

(defmacro pan::pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel (pan::plambda () ,vars t)))
     (declare (special pandoric-eval-tunnel))
     (eval `((lambda ()
               (declare (special pandoric-eval-tunnel))
               (pan::with-pandoric ,',vars pandoric-eval-tunnel ,,expr))))))

(defmacro pan::defpan (name args &body body)
  `(defun ,name (self)
     ,(if args
          `(pan:with-pandoric ,args self
             ,@body)
          `(progn ,@body))))



(oam:export-interface "PAN")
