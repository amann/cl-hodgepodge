(defun read-csv-file (filename &optional (col-separator #\~))
  (with-open-file (f filename :direction :input)
    (loop
       collect (handler-case
                   (explode (read-line f) col-separator)
                 (end-of-file ()
                   (return result)))
       into result)))

(defun explode (string &optional (delimiter #\~))
  (let ((pos (position delimiter string)))
    (if (null pos)
        (list string)
        (cons (subseq string 0 pos)
              (explode (subseq string (1+ pos))
                       delimiter)))))
(defmacro with-each-row-of-table (table &body body)
  (let ((tbl (gensym "TBL"))
        (next-row-fn (gensym "NEXT-ROW-FN"))
        (args (gensym "ARGS"))
        (fn (gensym "FN")))
    `(let* ((,tbl ,table)
            (,next-row-fn (cursor::make-list-cursor (table-rows ,tbl)))
            (,args (table-columns ,tbl))
            (,fn (eval `(lambda ,,args
                          (declare (ignorable ,@,args))
                          ,@',body))))
       (handler-case
           (loop
              (apply ,fn
                     (funcall ,next-row-fn)))
         (cursor:no-next-element-error ())))))

(defmacro %lexically-bind-symbols ((symbols values) &body body &environment env) 
  (let ((vars (gensym "VARS"))
        (vals (gensym "VALS")))
    `(let ((,vars ,symbols)
           (,vals ,values))
       (eval `(let ,(mapcar #'list
                            ,vars
                            ,vals)
                ,@',body)))))

(defun make-symbol-binder (name whole symbols)
  (eval `(defmacro ,name (&body body)
           `#'(lambda ,',symbols
                (let ((,',whole (list ,@',symbols)))
                  (declare (ignorable ,',whole))
                  ,@body)))))



(defmacro make-table-col-binding-macro (table-name table-cols)
  `(defmacro ,(intern (format nil "bind-cols-of-~A" table-name)) (row &body body)
     (let ((r (gensym "R")))
       `(let ((,r ,row))
          (let ,(mapcar #'list ,table-cols r)
            ,@body)))))

(defmacro $lexically-bind-symbols ((symbols values) &body body) 
  (let ((vars (gensym "VARS"))
        (vals (gensym "VALS")))
    `(let* ((,vars ,symbols)
            (,vals ,values))
       ,(eval `(let ,(mapcar #'list
                             ,vars
                             ,vals)
                 ,@body)))))


(defmacro $lexically-bind-symbols ((symbols values) &body body)
  (let ((syms (gensym "SYMS"))
        (vals (gensym "VALS"))
        (make-bindings (gensym "MAKE-BINDINGS"))
        (do-it (gensym "DO-IT")))
    `(let ((,syms ,symbols)
           (,vals ,values))
       (eval `(let ,(mapcar #'list ,syms ,vals)
                ,',@body)))))

(defmacro foo (vars vals &body body)
  ``(lambda ()
      (let ,(mapcar #'list ,vars ,vals)
        ,',@body)))

(macrolet ((,make-bindings (symbols values &body body)
             `(let ((,',syms ,symbols)
                    (,',vals ,values))
                (macrolet ((,',do-it (symbols values &body body)
                             `(let ,(mapcar #'list symbols values)
                                ,@body)))
                  (,',do-it ,',syms ,',vals ,@body)))))
  (,make-bindings (eval ,syms) (eval ,vals) ,@body))


(defmacro bind (((&rest symbols) values) &body body)
  ``(let ,(mapcar #'list ,symbols ,values)
      ,@',body))


(defun y-combinator (f)
  ((lambda (x) (funcall f (funcall x x))) (lambda (x) (funcall f (funcall x x)))))


(let ((syms '(a b c d))
      (vals '(1 2 3 4))
      (x 9))
  (apply (step `(lambda (,@syms)
                  (list a b c d x)))
         vals))

(let ((syms '(a b c d))
      (vals '(1 2 3 4))
      (x 9))
  (macrolet ((foo (&environment env)
               env))
    (foo)))



