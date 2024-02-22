;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: bp2ntm.lisp,v 1.2 2010/05/17 16:01:44 amao Exp $
;;;;* The BluePrint to NTM Mapping Program
;;;;
;;;;

(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (push #p"h:/My Documents/programming/lisp/lib/" asdf:*central-registry*)
  (asdf:oos 'asdf:load-op 'oam-util))
(setq plain-odbc::*universal-time-to-date-dataype*  #'(lambda (x) (make-instance 'date:date-time :universal-time x))
      plain-odbc::*date-datatype-to-universal-time* #'(lambda (x) (date::universal-time<-date-time x)))


;;;;================================================================
;;;;* The BluePrint Specific Part

(in-package #:cl-user)
(defpackage #:blueprint)
(defparameter blueprint::*project*
  (let ((project (make-instance 'etl::project
                                :name :bp)))
    (map nil #'(lambda (c-s)
                 (destructuring-bind (db-alias conn-spec) c-s
                   (etl::add-connection-spec project db-alias conn-spec)))
         '((:bp-pav     (:Driver "{Microsoft ODBC for Oracle}"
                         :Server "p014.swisslife.ch"
                         :Uid "ADAPTIV_READ"
                         :Pwd "pada14p"))
           (:bp-uat     (:Driver "{Microsoft ODBC for Oracle}"
                         :Server "q014.swisslife.ch"
                         :Uid "ADAPTIV_READ"
                         :Pwd "adaq14"))
           (:bp-uat-mu  (:Driver "{Microsoft ODBC for Oracle}"
                         :Server "q014mu.swisslife.ch"
                         :Uid "ADAPTIV_READ"
                         :Pwd "adaq14mu"))))
    project))

(defun blueprint::prepare-tables (db-alias)
  (map nil
       #'(lambda (rs-spec)
           (destructuring-bind (name sql) rs-spec
             (make-instance 'etl::resultset
                            :project blueprint::*project*
                            :name name :sql sql :db-alias db-alias)))
       '((:pos "select *
from (select * from BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos
	where pos.ISREXP_POS = 1 and (pos.PFPORTFOLIOTYPE = 'OR' or pos.BASKETTYPE_LOCAL_CODE = 'SLINDEX'))
join (select sig.ID as SIGNATURE,ORGUNIT,COMPANYCODE,SITUATIONDATE,VERSION_ASSET,IS_PRECUTOFF,DELIVERY,
             MESSAGE_ID,LOADDATE,LOADEDBY,DELIVERYTYPE,MART_VERSION,ORGUNIT_ID,cur.CODE as LOCAL_CURRENCY
	from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig
	join BLUEPRINT.CURRENCY cur on cur.ID = sig.CURRENCY
	where sig.CLOSING = 0) using (SIGNATURE)
left join (select org.code as orgunit, bbt.name as s_undrl_name, bbt.bloombergticker
           from BLUEPRINT.ORGUNIT org
           left join BLUEPRINT.MD_BLOOMBERG_NAMETRANS bbt on bbt.ORGUNIT = org.ID) using (orgunit, s_undrl_name)
left join BLUEPRINT.ADAPTIV_DMAEXP_UPIDTRANS using (UPID, COMPANYCODE)")
         (:leg "select leg.* from BLUEPRINT.ADAPTIV_DMAEXP_LEG leg
join BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos on pos.POSITION_RN = leg.POSITION_RN and pos.SIGNATURE = leg.SIGNATURE
join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig on sig.ID = pos.SIGNATURE")
         (:amo "select amo.* from BLUEPRINT.ADAPTIV_DMAEXP_AMO amo
join BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos on pos.POSITION_RN = amo.S_DMAPH_POS_RN and pos.SIGNATURE = amo.P_DMASI_SIGNATURE_ID
join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig on sig.ID = pos.SIGNATURE")
         (:cap "select cap.* from BLUEPRINT.ADAPTIV_DMAEXP_CALLPUT cap
join BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos on pos.POSITION_RN = cap.S_DMAPH_POS_RN and pos.SIGNATURE = cap.P_DMASI_SIGNATURE_ID
join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig on sig.ID = pos.SIGNATURE")
         (:cpn "select cpn.* from BLUEPRINT.ADAPTIV_DMAEXP_COUPON cpn
join BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos on pos.POSITION_RN = cpn.S_DMAPH_POS_RN and pos.SIGNATURE = cpn.P_DMASI_SIGNATURE_ID
join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig on sig.ID = pos.SIGNATURE"))))





#.(unless (every #'find-package '(bp.pos bp.leg bp.amo bp.cap bp.cpn))
    (blueprint::prepare-tables :bp-pav))


(map nil (lambda (feature)
           (etl::fill-table feature '("where situationdate = ?") `(,(date:make-date-time 2010 5 31) :date)))
     (list bp:leg bp:amo bp:cap bp:cpn))


(defmacro blueprint::make-feature-map (feature signature-key pos-rn-key)
  (let* ((package (symbol-package feature))
         (map-symbol (intern (format nil "~A-MAP" (symbol-name feature)) package)))
    (export map-symbol package)
    `(let ((hash-table (make-hash-table :test #'eql)))
       (defun ,map-symbol
           (signature pos-row-number)
           (handler-case
               (gethash pos-row-number (gethash signature hash-table))
             (type-error ())))
       (labels
           ((add-leg-map (signature pos-row-number value)
              (push value (gethash pos-row-number (or (gethash signature hash-table)
                                                      (setf (gethash signature hash-table)
                                                            (make-hash-table :test #'eql)))))))
         (etl:with-recordset ,feature
           (add-leg-map ,signature-key ,pos-rn-key ,(intern (string '@row@) (etl::package (eval feature)))))))))

(blueprint::make-feature-map bp:leg bp.leg:signature bp.leg:position_rn)
(blueprint::make-feature-map bp:amo bp.amo:p_dmasi_signature_id bp.amo:s_dmaph_pos_rn)
(blueprint::make-feature-map bp:cap bp.cap:p_dmasi_signature_id bp.cap:s_dmaph_pos_rn)
(blueprint::make-feature-map bp:cpn bp.cpn:p_dmasi_signature_id bp.cpn:s_dmaph_pos_rn)






#+nil(sort (with-package-iterator (next-symbol '(bp.pos) :external)
        (loop
           collect
           (multiple-value-bind (more? symbol) (next-symbol)
             (if more? 
                 symbol
                 (return symbols))) into symbols))
      #'string<)







#+nil(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-instance 'etl::project
                 :name :ntm
                 :conn-specs '((:a-pav      (:driver "{SQL Native Client}"
                                             :server "nx3036"
                                             :uid "mgr"
                                             :pwd "pavsystem") :adaptiv)
                               (:a-uat      (:driver "{SQL Native Client}"
                                             :server "nx3183"
                                             :uid "mgr"
                                             :pwd "uatsystem") :adaptiv)
                               (:a-st       (:driver "{SQL Native Client}"
                                             :server "nx3073"
                                             :uid "mgr"
                                             :pwd "NX3073") :adaptiv)
                               (:a-it       (:driver "{SQL Native Client}"
                                             :server "nx3038"
                                             :uid "mgr"
                                             :pwd "itsystem") :adaptiv))))


