(in-package #:blueprint)

(defun sql-name<-lisp-symbol (symbol)
  (substitute #\_ #\- (ctypecase symbol
                        ((symbol (symbol-name symbol))
                         (string symbol)))))

(defun lisp-symbol<-sql-name (sql-name &optional &key (package *package*) (string-transformation-fun 'string-upcase))
  (intern (funcall string-transformation-fun (substitute #\- #\_ sql-name)) package))

(defun clsql-type<-sql-type (sql-type)
  (destructuring-bind (type precision scale) sql-type
    (case type
      (:date 'date)
      ((:varchar :varchar2) `(string ,precision))
      (:number (if  (and (numberp scale) (= 0 scale))
                    'integer
                    'real)))))

(defmacro generate-view-class (sql-table sql-schema &optional lisp-name)
  (let ((sql-name sql-table)
        (sql-schema sql-schema)
        (lisp-name (or lisp-name (lisp-symbol<-sql-name sql-table))))
    `(def-view-class ,lisp-name ()
       ,(mapcar #'(lambda (attribute)
                    (let* ((column-name (first attribute))
                           (slot-name (lisp-symbol<-sql-name column-name))
                           (type (clsql-type<-sql-type (list (second attribute) (third attribute) (fourth attribute))))
                           (is-nullable (= 1 (fifth attribute))))
                      (append (list slot-name
                                    :type type
                                    :column column-name)
                              (unless is-nullable
                                (list :db-constraints :not-null)))))
                (list-attribute-types sql-name :owner :all))
       (:base-table ,(concatenate 'string sql-schema "." sql-name)))))


(generate-view-class "DM_AEXP_SIGNATURE" "BLUEPRINT" signature-view)

