(defun define-helper (item value)
  (etypecase item
    (cons `(defun ,(car item) ,(cdr item) ,@value))
    (symbol `(setq ,item (progn ,@value)))))



(defmacro define (item &rest value)
  (define-helper item value))



(defmacro define (item &body body)
  (let* ((to-define item)
         (defun (gensym "DEFUN"))
         (defun-arg-list '(spec (&rest args) &body body)))
    (etypecase to-define
       (cons `(macrolet
                  ((,defun ,defun-arg-list
                       `(defun  ,@,(get-parameter-list defun-arg-list))))
                (,defun ,(car to-define) ,(cdr to-define) . ,body)))
       (symbol `(set ,to-define ,(car body))))))

(get-parameter-list '(spec (&rest args) &body body))

(list* 'defun (list* 'spec '(args) 'body))