(in-package #:adaptiv)
(defconstant +connection-specs+ '((:adaptiv ("Adaptiv" "mgr" "system") :if-exists :old :database-type :odbc :make-default nil)
                                  (:executive ("Executive" "mgr" "system") :if-exists :old :database-type :odbc :make-default nil)
                                  (:mfiles ("Mfiles" "mgr" "system") :if-exists :old :database-type :odbc :make-default nil)
                                  (:prod-adaptiv ("ProdAdaptiv" "mgr" "productionsystem") :if-exists :old :database-type :odbc :make-default nil)
                                  (:prod-executive ("ProdExecutive" "mgr" "productionsystem") :if-exists :old :database-type :odbc :make-default nil)
                                  (:prod-mfiles ("ProdMfiles" "mgr" "productionsystem") :if-exists :old :database-type :odbc :make-default nil)
                                  (:blueprint ("p014.swisslife.ch" "AEXP_READ" "p14aexpqry") :if-exists :old :database-type :oracle :make-default nil)
                                  (:blueprint-adaptiv-view-uat-mu ("q014mu.swisslife.ch" "ADAPTIV_READ" "adaq14mu") :if-exists :old :database-type :oracle :make-default nil)
                                  (:blueprint-adaptiv-view-pav ("p014.swisslife.ch" "ADAPTIV_READ" "pada14p") :if-exists :old :database-type :oracle :make-default nil)))

(defvar *environment* :test
  "Indicate which to environment to connect. Possible values: :test and :production.")
(defconstant +environments+ '(:test :production)
  "Contains the possible values of the variable *environment*.")
(defmacro in-environment (environment &body body)
  (if (member environment '(:test :production))
      `(let ((*environment* ,environment))
         ,@body)
      (error "The environment must be one of ~{~S~#[~; and ~:;, ~]~}; not ~S." +environments+ environment)))

(let (catalog
      (connection-pool '((:adaptiv)
                         (:executive)
                         (:mfiles)
                         (:prod-adaptiv)
                         (:prod-executive)
                         (:prod-mfiles)
                         (:blueprint)
                         (:blueprint-adaptiv-view-uat-mu)
                         (:blueprint-adaptiv-view-pav))))
  (declare (special catalog))
  (labels ((get-connection ()
             (car (member  (cdr (assoc catalog connection-pool))(connected-databases))))
           (set-connection (connection)
             (cdr (rplacd (assoc catalog connection-pool) connection))))
    (defun connect-to (catalog)
      (declare (special catalog))
      (when (and (eq *environment* :production) (member catalog '(:adaptiv :executive :mfiles)))
        (setq catalog (intern (concatenate 'string "PROD-" (symbol-name catalog)) :keyword)))
      (or (get-connection) (set-connection (apply 'clsql:connect (cdr (assoc catalog +connection-specs+))))))))







#|

A database error occurred: NIL / 01000
  [Microsoft][ODBC SQL Server Driver][DBNETLIB]ConnectionRead (recv()).
   [Condition of type SQL-DATABASE-ERROR]



|#