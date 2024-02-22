(in-package #:blueprint)
(eval-when (:compile-toplevel)
  (defconstant +blueprint-connection-specs+ '(("p014.swisslife.ch" "AEXP_READ" "p14aexpqry") :database-type :oracle))
  (with-database (db '("p014.swisslife.ch" "AEXP_READ" "p14aexpqry") :database-type :oracle)
    (with-default-database (db)
     (generate-view-class "DM_AEXP_SIGNATURE" "BLUEPRINT" signature-table)
     (generate-view-class "DM_AEXP_POSITIONS" "BLUEPRINT" position-table)
     (generate-view-class "DM_AEXP_FEAT_AMO" "BLUEPRINT" amortization-feature-table)
     (generate-view-class "DM_AEXP_FEAT_COUPON" "BLUEPRINT" coupon-feature-table)
     (generate-view-class "DM_AEXP_FEAT_CP" "BLUEPRINT" call-put-feature-table)
     (generate-view-class "DM_AEXP_FEAT_LEG" "BLUEPRINT" leg-feature-table))))

(def-view-class position-view2 (position-table)
  ((signature-table
    :db-kind :join
    :db-info (:join-class signature-table
                          :home-key signature
                          :foreign-key id
                          :retrieval :immediate
                          :set nil)))
  (:base-table :BLUEPRINT.DM_AEXP_POSITIONS))
(def-view-class position-view (position-table)
  ((signature-table
    :db-kind :join
    :db-info (:join-class signature-table
                          :home-key signature
                          :foreign-key id
                          :retrieval :immediate
                          :set nil)))
  (:base-table :BLUEPRINT.DM_AEXP_POSITIONS))

(defmethod initialize-instance :after ((instance position-view) &key))
(defmethod initialize-instance :around ((instance position-view) &key) (call-next-method))
(defmethod make-instance :after ((class standard-db-object) &key) (break))


(def-view-class swapir (position-view)
  ((pay-leg :db-kind :virtual)
   (rec-leg :db-kind :virtual)))
#.(locally-enable-sql-reader-syntax)

(defmethod update-instance-for-different-class :before ((old position-view) (new swapir) &key)
  (setf (slot-value new 'pay-leg) (select 'leg-feature-table
                                          :where [and [= [signature] (slot-value old 'signature) ]
                                          [= [position-rn] (slot-value old 'position-rn)]
                                          [= [legpaymenttype] "PAY"] ]
                                          :flatp t)
        (slot-value new 'rec-leg) (select 'leg-feature-table
                                          :where [and [= [signature] (slot-value old 'signature) ]
                                          [= [position-rn] (slot-value old 'position-rn)]
                                          [= [legpaymenttype] "REC"] ]
                                          :flatp t)))

(defmacro position-result-set (situationdate &optional &rest where-statement)
  `(select 'position-view
           :where [and [= [load-status] "AVAILABLE"] [= [closing] 0] [= [pfportfoliotype] "OR"]
           [= [situationdate] ,situationdate] ,@where-statement]))

#.(restore-sql-reader-syntax-state)


(defmethod get-situationdate ((position position-view))
  (slot-value (slot-value position 'signature-table) 'situationdate))




(defun connect-to-blueprint ()
  (apply 'clsql:connect +blueprint-connection-specs+))



#|
 (locally-enable-sql-reader-syntax)
 (start-sql-recording)
 (setq *annuity* (select 'position-view
                        :where [= [mappingtype] "MORTGAGEANNUITYFLOATING" ]))


  (coupon-features
   :reader coupon-features
   :db-kind :join
   :db-info (:join-class coupon-feature-table
                         :home-key (signature position-rn)
                         :foreign-key (p-dmasi-signature-id s-dmaph-pos-rn)
                         :set t))
  (call-put-features
   :reader call-put-features
   :db-kind :join
   :db-info (:join-class call-put-feature-table
                         :home-key (signature position-rn)
                         :foreign-key (p-dmasi-signature-id s-dmaph-pos-rn)
                         :set t))
  (amortization-features
   :reader amortization-features
   :db-kind :join
   :db-info (:join-class amortization-feature-table
                         :home-key (signature position-rn)
                         :foreign-key (p-dmasi-signature-id s-dmaph-pos-rn)
                         :set t))
  (leg-features
   :reader leg-features
   :db-kind :join
   :db-info (:join-class leg-feature-table
                         :home-key (signature position-rn)
                         :foreign-key (signature position-rn)
                         :set t))

|#

