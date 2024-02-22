(in-package #:cl-user)
(defpackage #:sql-util
  (:use #:cl #:string-util)
  (:export #:to-sql))
(defpackage #:sql-util-user
  (:use #:cl #:sql-util))

(in-package #:sql-util)

;(defconstant +this-package+ )

(defgeneric to-sql (object)
  (:documentation "return a string which represents OBJECT in SQL syntax.")
  (:method ((object null)) "null")
  (:method ((object string)) (format nil "'~A'" (string-replace "" "''" "'"  object)))
  (:method ((object real)) (format nil "~F" object))
  (:method ((object cons)) (dispatch-expr object))
  (:method ((object symbol)) (symbol-name object))
  #+IGNORE
  (:method ((object date)) (to-sql (subseq (format-time nil object) 0 23)))
  #+IGNORE
  (:method ((object wall-time)) (to-sql (subseq (format-time nil object) 0 23))))

(let ((key->funct (make-hash-table :key-type 'symbol :value-type 'function))
      (default-dispatch #'(lambda (expr)
                            (to-sql-infix (cons ", " expr)))))
  (defun set-sql-keyword-dispatch (key dispatch-fun)
    (map 'nil #'(lambda (key)
                  (setf (gethash (etypecase key
                                   (keyword key)
                                   (symbol (intern (symbol-name key) :keyword))
                                   (string (intern key :keyword)))
                                 key->funct) dispatch-fun))
         (typecase key
           (cons key)
           (t (list key)))))
  (defun set-default-dispatch (dispatch-fun)
    (setq default-dispatch dispatch-fun))
  (defun get-sql-keyword-dispatch (key)
    (gethash key key->funct default-dispatch))
  (defun dispatch-expr (expr)
    (let ((key (find-symbol (symbol-name (car expr)) :keyword)))
      (funcall (get-sql-keyword-dispatch key) expr))))
(set-sql-keyword-dispatch '(select delete update insert) #'to-sql-select-statement)
(set-sql-keyword-dispatch '(and or = < > /= <= >=) #'to-sql-infix)
(set-sql-keyword-dispatch '(like count sum max min) #'to-sql-funct)
(set-sql-keyword-dispatch '(from where not) #'to-sql-prefix)
(set-sql-keyword-dispatch 'group-by #'(lambda (expr)
                                        (to-sql-prefix (cons '|GROUP BY| (cdr expr)))))
(set-sql-keyword-dispatch 'order-by #'(lambda (expr)
                                        (to-sql-prefix (cons '|ORDER BY| (cdr expr)))))
(defun to-sql-infix (expr)
  (let* ((format-string (format nil "~~[~~;~~:;(~~]~~@[~~{~~A~~^ ~A ~~}~~]~~[~~;~~:;)~~]" (car expr)))
	 (args (mapcar #'to-sql (cdr expr)))
	 (n (length args)))
    (format nil format-string n args n)))
(defun to-sql-funct (expr)
  (let* ((format-string (format nil "~A (~~{~~A~~^, ~~})" (car expr)))
	 (args (mapcar #'to-sql (cdr expr))))
    (format nil format-string args)))
(defun to-sql-prefix (expr)
  (format nil "~A ~{~A~^, ~}" (car expr) (mapcar #'to-sql (cdr expr))))
(defun to-sql-select-statement (expr)
  (destructuring-bind (select column &rest rest) expr
   (format nil "~A~{~^ ~A~^,~}~{ ~@[~A~]~}" select column (mapcar #'to-sql rest))))
#+IGNORE
(defun sql-select (&key from where group-by order-by &rest columns)
  (format nil "select~{ ~A~^,~}~{ ~@[~A~]~}" (mapcar #'to-sql columns) (mapcar #'(lambda (x y)
										   (to-sql (cons x y)))
									       '(from where ))))

#+IGNORE
(select (a b c)
	(from q w)
	(where (and (= e t)
		    (not (< g h))
		    (or (= a s)
			(= c v))))
	(group-by a b)
	(order-by a))