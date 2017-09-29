;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: bp2ntm.lisp,v 1.2 2010/05/17 16:01:44 amao Exp $
;;;;* The BluePrint to NTM Mapping Program
;;;;
;;;;
#|

;;;; Trial for forcing namespaces
(let ((orig-find-pkg #'ccl::%find-pkg))
           (defun ccl::%find-pkg (name &optional (len (length name)))
             (declare (fixnum len)
                      (special ccl::prefix))
             (if (and (boundp 'ccl::prefix) ccl::prefix)
                 (funcall orig-find-pkg (concatenate 'string ccl::prefix name)
                          (the fixnum (+ (the fixnum (length ccl::prefix)) len)))
                 (funcall orig-find-pkg name len))))

|#


(in-package #:cl-user)

(setq plain-odbc::*universal-time-to-date-dataype*
      #'date::date-time<-universal-time
      plain-odbc::*date-datatype-to-universal-time*
      #'date::universal-time<-date-time)

;;;;================================================================
;;;;* The BluePrint Specific Part

(in-package #:cl-user)
(defpackage #:ch.swisslife.blueprint
  (:use)
  (:nicknames #:blueprint))
(defpackage #:ch.swisslife.blueprint-system
  (:use #:cl))
(in-package #:ch.swisslife.blueprint-system)


(defvar blueprint::*datapool*
  (make-instance 'etl::datapool :name :bp))
(defvar blueprint::*connection-specs*
  '((:bp-pav    . (:Driver "{Microsoft ODBC for Oracle}"
                   :Server "p014.swisslife.ch"
                   :Uid "ADAPTIV_READ"
                   :Pwd "pada14p"))
    (:bp-uat    . (:Driver "{Microsoft ODBC for Oracle}"
                   :Server "q014.swisslife.ch"
                   :Uid "ADAPTIV_READ"
                   :Pwd "adaq14"))
    (:bp-uat-mu . (:Driver "{Microsoft ODBC for Oracle}"
                   :Server "q014mu.swisslife.ch"
                   :Uid "ADAPTIV_READ"
                   :Pwd "adaq14mu"))))

(defvar blueprint::*queries* '((:pos "select *
from (select * from BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos
	where pos.ISREXP_POS = 1 and (pos.PFPORTFOLIOTYPE = 'OR' or pos.BASKETTYPE_LOCAL_CODE = 'SLINDEX'))
join (select sig.ID as SIGNATURE,ORGUNIT,COMPANYCODE,SITUATIONDATE,VERSION_ASSET,IS_PRECUTOFF,DELIVERY,MESSAGE_ID,LOADDATE,LOADEDBY,DELIVERYTYPE,MART_VERSION,ORGUNIT_ID,cur.CODE as LOCAL_CURRENCY
	from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig
	join BLUEPRINT.CURRENCY cur on cur.ID = sig.CURRENCY
	where sig.CLOSING = 0) using (SIGNATURE)
left join (select org.code as orgunit, bbt.name as s_undrl_name, bbt.bloombergticker
           from BLUEPRINT.ORGUNIT org
           left join BLUEPRINT.MD_BLOOMBERG_NAMETRANS bbt on bbt.ORGUNIT = org.ID) using (orgunit, s_undrl_name)
left join BLUEPRINT.ADAPTIV_DMAEXP_UPIDTRANS using (UPID, COMPANYCODE)
where ~A")
                               (:leg "select * from BLUEPRINT.ADAPTIV_DMAEXP_LEG
where SIGNATURE in (select ID
                    from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE s
                    join BLUEPRINT.ADAPTIV_DMAEXP_POSITION p on p.SIGNATURE = s.ID
                    where ~A);")
                               (:amo "select * from BLUEPRINT.ADAPTIV_DMAEXP_AMO
where P_DMASI_SIGNATURE_id in (select ID
                    from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE s
                    join BLUEPRINT.ADAPTIV_DMAEXP_POSITION p on p.SIGNATURE = s.ID
                    where ~A);")
                               (:cap "select * from BLUEPRINT.ADAPTIV_DMAEXP_CALLPUT
where P_DMASI_SIGNATURE_id in (select ID
                    from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE s
                    join BLUEPRINT.ADAPTIV_DMAEXP_POSITION p on p.SIGNATURE = s.ID
                    where ~A);")
                               (:cpn "select * from BLUEPRINT.ADAPTIV_DMAEXP_COUPON
where P_DMASI_SIGNATURE_id in (select ID
                    from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE s
                    join BLUEPRINT.ADAPTIV_DMAEXP_POSITION p on p.SIGNATURE = s.ID
                    where ~A);")))

(defvar blueprint::*bp-connection* nil)

(defun blueprint::make-sql-table (name sql parameters)
  (multiple-value-bind (rows columns)
      (plain-odbc:exec-query* blueprint::*bp-connection* sql parameters)
    (make-instance 'etl::table
                   :datapool blueprint::*datapool*
                   :name (string name)
                   :rows rows
                   :columns columns)))
(defun blueprint::reload-table (table sql parameters)
  (multiple-value-bind (rows columns)
      (plain-odbc:exec-query* blueprint::*bp-connection* sql parameters)
    (reinitialize-instance table
                   :rows rows
                   :columns columns)))

(defun blueprint::load-data (connection where-clause parameters)
  (handler-case
      (progn
        (plain-odbc:exec-command connection "set TRANSACTION read only")
        (dolist (query blueprint::*queries*)
           (destructuring-bind (name sql) query
             (let ((table (find name (etl:tables blueprint::*datapool*)
                                :test #'string= :key #'(lambda (tbl) (string (etl:name tbl)))))
                   (sql (format nil sql where-clause)))
              (if table
                  (blueprint::reload-table table sql)
                  (blueprint::make-sql-table name sql parameters)))))
        (plain-odbc:commit connection))
    (error (c)
      (plain-odbc:rollback connection))))



(defun blueprint::make-feature-indices ()
  (etl:define-table-index bp::leg-sig-posrn (bp.leg:signature bp.leg:position_rn) bp::leg)
  (etl:define-table-index bp:amo-sig-posrn (bp.amo:p_dmasi_signature_id bp.amo:s_dmaph_pos_rn) bp:amo)
  (etl:define-table-index bp:cap-sig-posrn (bp.cap:p_dmasi_signature_id bp.cap:s_dmaph_pos_rn) bp:cap)
  (etl:define-table-index bp:cpn-sig-posrn (bp.cpn:p_dmasi_signature_id bp.cpn:s_dmaph_pos_rn)bp:cpn))









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


