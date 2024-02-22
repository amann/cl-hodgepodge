(in-package #:bp-to-ntm-system)
;;;;========================================================
(defstruct (connection-spec (:conc-name connection-))
  (spec nil :type cons :read-only t)
  (type :odbc :type keyword :read-only t))
(let ((+adaptiv-catalogs+ '(:adaptiv :mfiles :executive))
      (adaptiv-db (make-hash-table))
      (blueprint-db (make-hash-table)))
  (labels ((make-key (name)
             (intern (string name) :keyword))
           (add-alias (alias)
             (declare (special *dbs*))
             (setf (gethash (make-key alias) adaptiv-db) *dbs*)))
    (defun add-adaptiv-server (server-name user password &rest aliases)
      (let ((*dbs* (loop :with server = (string server-name)
                      :for catalog :in +adaptiv-catalogs+
                      :collect (cons catalog
                                     (let ((data-source (concatenate 'string (string catalog) "-" server)))
                                       (make-connection-spec :spec (list data-source user password) :type :odbc))))))
        (declare (special *dbs*))
        (setf (gethash (make-key server-name) adaptiv-db) *dbs*)
        (map nil #'add-alias aliases)))
    (defun add-aliases-to-adaptiv-server (name new-names)
      (let ((*dbs* (get-adaptiv-catalogs-from-server name)))
        (declare (special *dbs*))
        (map nil #'add-alias new-names)))
    (defun remove-alias (alias)
      (remhash (make-key alias) adaptiv-db))
    (defun get-adaptiv-catalogs-from-server (name)
      (gethash (make-key name) adaptiv-db))
    (defun get-adaptiv-db (name catalog)
      (cdr (assoc (make-key catalog) (get-adaptiv-catalogs-from-server name))))
    (defun get-adaptiv-catalog (catalog)
      (declare (special *adaptiv-server*))
      (get-adaptiv-db *adaptiv-server* catalog))
    (defun get-all-adaptiv-catalogs ()
      (copy-list +adaptiv-catalogs+))
    (defun add-blueprint-server (name server user password)
      (setf (gethash (make-key name) blueprint-db) (make-connection-spec :spec (list server user password) :type :oracle)))
    (defun get-blueprint-db (name)
      (gethash (make-key name) blueprint-db))))
(defmacro define-adaptiv-dbs (&body connection-specs)
  `(map nil #'(lambda (cs) (apply #'add-adaptiv-server cs)) ',connection-specs))
(defmacro define-blueprint-dbs (&body connection-specs)
  `(map nil #'(lambda (cs) (apply #'add-blueprint-server cs)) ',connection-specs))
(define-adaptiv-dbs 
  (:nx3036 "mgr" "pavsystem" :pav)
  (:nx3038 "mgr" "")
  (:nx3049 "mgr" "system")
  (:nx3062 "mgr" "productionsystem")
  (:nx3073 "mgr" "NX3073" :st)
  (:nx3163 "mgr" "december")
  (:nx3183 "mgr" "uatsystem" :uat))
(define-blueprint-dbs
  (:pav "p014.swisslife.ch" "ADAPTIV_READ" "pada14p")
  (:uat "q014.swisslife.ch" "ADAPTIV_READ" "adaq14")
  (:uat-mu "q014mu.swisslife.ch" "ADAPTIV_READ" "adaq14mu"))

()










#|

A database error occurred: NIL / 01000
  [Microsoft][ODBC SQL Server Driver][DBNETLIB]ConnectionRead (recv()).
   [Condition of type SQL-DATABASE-ERROR]



|#