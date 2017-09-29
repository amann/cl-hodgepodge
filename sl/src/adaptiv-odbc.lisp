(oam:define-project-package #:ch.swisslife.adaptiv-odbc #:ad-odbc)
(in-package #:ch.swisslife.adaptiv-odbc-system)

(defparameter *connection-specs*
  '((:pav      (:driver "{SQL Server}"
                :hostname "NX3036"
                :server "NX3036"
                :port 1433
                :uid "mgr"
                :pwd "pavsystem"))
    (:uat      (:driver "{SQL Server}"
                :hostname "NX3183"
                :server "NX3183"
                :port 1433
                :uid "mgr"
                :pwd "uatsystem"))
    (:st       (:driver "{SQL Server}"
                :hostname "NX3073"
                :server "NX3073"
                :port 1433
                :uid "mgr"
                :pwd "NX3073"))
    (:it       (:driver "{SQL Server}"
                :hostname "NX3038"
                :server "NX3038"
                :port 1433
                :uid "mgr"
                :pwd "itsystem"))))


(defun ad-odbc::get-connection-spec (level)
  (cadr (assoc level *connection-specs*)))
(defun ad-odbc::get-all-known-levels ()
  (mapcar #'car *connection-specs*))
(defun ad-odbc::get-all-known-catalogs ()
  (list :adaptiv :mfiles :executive))

(defun check-level (level)
  (let ((allowed-levels (ad-odbc::get-all-known-levels)))
    (assert (member level allowed-levels :test #'eq) (level)
            "Enter one of the possible values ~{~S~#[~; or ~:;, ~]~}: " allowed-levels)))
(defun check-catalog (catalog)
  (let ((allowed-catalogs (ad-odbc::get-all-known-catalogs)))
    (assert (member catalog allowed-catalogs :test #'eq) (catalog)
            "Enter one of the possible values ~{~S~#[~; or ~:;, ~]~}: " allowed-catalogs)))

(let ((cache (oam:make-hash-tree :test #'eq)))
  (defun ad-odbc::connect (level catalog)
    (check-level level)
    (check-catalog catalog)
    (let ((keys (list* level (when catalog (list catalog)))))
      (or (oam:get-hash* cache keys)
          (setf (oam:get-hash* cache keys)
                (apply #'(lambda (&key driver server uid pwd &allow-other-keys)
                           (apply #'plain-odbc:connect-generic
                                  :driver driver :server server :uid uid :pwd pwd
                                  (when catalog (list :database (string catalog)))))
                       (ad-odbc::get-connection-spec level))))))
  (defun ad-odbc::close (level catalog)
    (check-level level)
    (check-catalog catalog)
    (let* ((keys (list* level (when catalog (list catalog))))
           (connection (oam:get-hash* cache keys)))
      (when connection
        (plain-odbc:close-connection connection)
        (oam:rem-hash* cache keys)))))

(oam:defmacro! ad-odbc::with-open-connection ((conn o!level o!catalog) &body body)
  `(let ((,conn (ad-odbc::connect ,g!level ,g!catalog)))
     (unwind-protect
          (progn ,@body)
       (ad-odbc::close ,g!level ,g!catalog))))

(defun ad-odbc::get-level (connection)
  (loop :with host-name := (plain-odbc::server-name connection)
     :for level :in (ad-odbc::get-all-known-levels)
     :when (destructuring-bind (&key hostname &allow-other-keys)
               (ad-odbc::get-connection-spec level)
             (string= hostname host-name))
     :do (return level)))

;;;;* Stored Procedures on Adaptiv


(defmacro define-adaptiv-stored-procedure-call (catalog stored-name arglist &key name documentation gives-results 
                                                (var-symbols (oam:get-lambda-variables arglist) var-symb-p))
  "Define a function named NAME with args ARGLIST calling the stored procedure STORED-NAME on catalog CATALOG on the Adaptiv database. NAME if not given explicitly is the symbol interned in the current package whose symbol-name is constructed by replacing the characters #\_ and #\; of STORED-NAME by #\- und upcase everything. The key VAR-SYMBOLS allow to define which args are and in which order used to call the procedure; the default is those as given by ARGLIST. If symbols apear in VAR-SYMBOLS but not in ARGLIST, they are declared as special."
  `(defun ,(or name (intern (string-upcase (oam:string-replace #\- '(#\_ #\;) stored-name))))
       ,(list* 'level arglist)
     ,(format nil "Stored procedure ~A on database ~A.~@[~&~A~]" stored-name catalog documentation)
     ,@(when var-symb-p
             (let* ((declared-vars (oam:get-lambda-variables arglist))
                    (special-vars (set-difference var-symbols declared-vars))
                    (ignored-vars (set-difference declared-vars var-symbols)))
               `((declare ,@(when ignored-vars
                                  `((ignorable ,@ignored-vars)))
                          ,@(when special-vars
                                  `((special ,@special-vars)))))))
     (let ((connection (ad-odbc:connect level ',catalog)))
       (values-list (prog1
                        (multiple-value-list (funcall ,(if gives-results #'plain-odbc:exec-query #'plain-odbc:exec-command)
                                                      connection
                                                      ,(format nil "exec ~A ~{?~*~^, ~}" stored-name var-symbols)
                                                      ,@var-symbols))
                      (plain-odbc:commit connection))))))


(oam:export-interface '#:ad-odbc)