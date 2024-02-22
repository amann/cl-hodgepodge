;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: bp2ntm.lisp,v 1.2 2010/05/17 16:01:44 amao Exp $
;;;;* NTM
(defpackage #:ntm
  (:export #:@))
(defpackage #:ntm-user
  (:use #:cl #:ntm))

;;;;** Utilities for NTM processing
(defconstant ntm::+default-dom-type+ :lxml)

(let ((s-xml::*default-dom-type* ntm::+default-dom-type+))
  (defun ntm::make-node (name &optional attributes &rest values)
    (s-xml::node name attributes values))
  (defun ntm::set-values (ntm-message node-path &rest values)
    (s-xml::node-set-values node-path values (ntm::message ntm-message))
    ntm-message)
  (defun ntm::insert-values (ntm-message node-path place &rest values)
    (s-xml::node-insert-values node-path values place (ntm::message ntm-message))
    ntm-message)
  (defun ntm::rename-node (ntm-message node-path new-name)
    (s-xml::rename-node node-path new-name (ntm::message ntm-message))
    ntm-message)
  (defun ntm::set-values-of-sub-nodes (ntm-message node-path &rest tag-values)
    (let ((node (s-xml::find-node node-path (ntm::message ntm-message))))
      (map nil #'(lambda (subnode-values)
                   (s-xml::node-set-values `(,(car subnode-values))
                                           (cdr subnode-values) node))
           (oam::alist<-plist tag-values)))))

(defmacro ntm::node (name (&rest attributes &key &allow-other-keys) &body values)
  (let ((bindings (oam::map-plist #'(lambda (k v)
                                      (declare (ignore k))
                                      (list (gensym) v))
                                  attributes)))
   `(ntm::make-node ,name (let ,bindings
                            ,(cons 'list (mapcan #'(lambda (p b)
                                                     `(',(car p) (car (ntm::value ,(car b)))))
                                                 (oam::alist<-plist attributes)
                                                 bindings))) ,@(apply #'ntm::value values))))
(defmacro ntm::@ (name &body values)
  (let* ((attributes (mapcan #'(lambda (x)
                                 (when (and (consp x) (eq :@ (car x)))
                                   (cdr x)))
                             values))
         (a-bindings (oam::map-plist #'(lambda (k v)
                                       (declare (ignore k))
                                       (list (gensym) v))
                                   attributes))
         (values (delete :@ values :key #'(lambda (x)
                                            (when (and (consp x) (eq :@ (car x)))
                                              (car x)))))
         (v-bindings (mapcar #'(lambda (v)
                                 (list (gensym) (if (and (consp v) (eq 'ntm::@ (car v)))
                                                    v
                                                    `(ntm::to-string ,v))))
                             values)))
   `(apply #'ntm::make-node ',name
           (let ,a-bindings
             ,(cons 'list (mapcan #'(lambda (p b)
                                      `(',(car p) (ntm::to-string ,(car b))))
                                  (oam::alist<-plist attributes)
                                  a-bindings)))
           (let ,v-bindings
             (delete nil (list ,@(mapcar #'car v-bindings)))))))


#+nil(eval-when (:compile-toplevel :load-toplevel :execute)
  (let (saved-readtable
        (modified-readtable (copy-readtable)))
    (set-dispatch-macro-character #\# #\@ #'(lambda (s c n) (declare (ignore c n)) `(ntm::@ ,@(read s nil (values) t))) modified-readtable)
    (defun ntm::enable-ntm-syntax ()
      (unless saved-readtable
        (setq saved-readtable *readtable*
              *readtable* modified-readtable)))
    (defun ntm::disable-ntm-syntax ()
      (when saved-readtable
        (setq *readtable* saved-readtable
              saved-readtable nil)))))


(define-condition ntm::missing-mandatory-field-error (error)
  ((ntm::field-name :initarg :field-name :reader ntm::field-name))
  (:report (lambda (e s)
             (format s "The field ~A is mandatory." (ntm::field-name e)))))
(defun ntm::mandatory (field-name)
  (error 'ntm::missing-mandatory-field-error :field-name field-name))

(defvar ntm::*date-format* :iso8601)
(defgeneric ntm::to-string (o &key &allow-other-keys))

(defmethod ntm::to-string ((o cons) &key &allow-other-keys)
  o)
(defmethod ntm::to-string ((o null) &key &allow-other-keys)
  (values))
(defmethod ntm::to-string ((o string) &key &allow-other-keys)
  o)
(defmethod ntm::to-string ((o symbol) &key &allow-other-keys)
  (string o))
(defmethod ntm::to-string ((o real) &key &allow-other-keys)
  (format nil "~F" o))
(defmethod ntm::to-string ((o integer) &key &allow-other-keys)
  (format nil "~D" o))
(defmethod ntm::to-string ((o date::date-time) &key full (format ntm::*date-format*))
  (date::format-date o format :full full))
(defun ntm::value (&rest o)
  (mapcan #'(lambda (&rest args)
              (let ((v (apply #'ntm::to-string args)))
                (when v (list v))))
          o))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern "space" '#:XML)
  (export 'XML::|space| '#:XML)
  #+nil(ntm::enable-ntm-syntax))
(in-package #:ntm-user)
;;;;** NTMMessage
(defclass ntm::ntm-message () ((ntm::xml-dom :reader ntm::message)
                               (ntm::xml-dom-type :allocation :class
                                                  :initform ntm::+default-dom-type+
                                                  :reader ntm::xml-dom-type)))
(defmethod initialize-instance
    ((self ntm::ntm-message) &key
     (message-id "")
     (system-id (ntm::mandatory "SystemID"))
     (location (ntm::mandatory "Location"))
     (generated-time (date::now)) &allow-other-keys)
  (setf (slot-value self 'ntm::xml-dom)
        `(:|NTMMessage| (:@ :|version| "1.03")
           (:|NTMHeader|
             (:|MessageID| ,message-id)
             (:|MessageSource|
               (:|SystemID| ,system-id)
               (:|Location| ,location))
             (:|Timezone| "GMT")
             (:|DateFormat| (:@ :|format| "ISO8601"))
             (:|GeneratedTime| ,(ntm::to-string generated-time :full t))
             (:|PublishResponse| (:@ :|value| "JustLog"))))))

;;;;*** TermsAndConditions
(defclass ntm::terms-and-conditions (ntm::ntm-message) ())
(defmethod initialize-instance :after
    ((self ntm::terms-and-conditions) &key &allow-other-keys)
  (ntm::insert-values self '() -1 `(:|TermsAndConditions|)))

;;;;**** rio-equity-type
(defclass ntm::rio-equity-type (ntm::terms-and-conditions) ())

;;;;***** Index
(defclass ntm::index (ntm::rio-equity-type) ())
(export 'ntm::index '#:ntm)
(defmethod initialize-instance :after
    ((self ntm::rio-equity-type) &key
     (issue-id    (ntm::mandatory "IssueID"))
     description
     (currency    (ntm::mandatory "Currency"))
     (underlying (unless (typep self 'ntm::index)
                   (ntm::mandatory "Underlying")))
     (contract-size 1)
     (audit-time-stamp (date::now)) &allow-other-keys)
  (ntm::set-values self '(:|TermsAndConditions|)
                   `(:rio-equity-type
                     (:|IssueDefinition|
                       (:|IssueID| ,issue-id)
                       (:|Description| ,description)
                       (:|Underlying| ,underlying)
                       (:|AmountIssued| "0")
                       (:|ConversionRatio| "1")
                       (:|Calendar|)
                       (:|Currency| ,currency)
                       (:|TradingOffset| "0")
                       (:|ExchangeID|)
                       (:|IssuerCode|)
                       (:|InitialPrice| "0")
                       (:|Market| "MARKET")
                       (:|PriceIncrement| "0.01")
                       (:|SettleMethod| "Bus")
                       (:|TickValue| "0.01")
                       (:|ContractSize| ,contract-size)
                       (:|PriceFactor| "1")
                       (:|BasketDivisor| "1")
                       (:|PremiumType| "Prc")
                       (:|Sector|)
                       (:|SettlementRule| "0d")
                       (:|Holidays|)
                       (:|CompoOrQuanto| "0")
                       (:|PayoutCurrency|)
                       (:|ExerciseType| "1")
                       (:|OtherStyles|)
                       (:|SegmentID|)
                       (:|CountryOfRegister|)
                       (:|ExerciseDate| "1900-01-01")
                       (:|PutOrCall| "Call")
                       (:|DeliveryDate| "1900-01-01")
                       (:|Strike| "0")
                       (:FISS "1")
                       (:|ClassID|)
                       (:|UseAccountPeriod| "No")
                       (:|AccountPeriod|)
                       (:|IssueDate| "1900-01-01")
                       (:|UseIssueVolatility| "No")
                       (:|Volatility| "0")
                       (:|Margined| "No")
                       (:|CleanDirtyFlag| "Cln")
                       (:|PayoutMarket|)
                       (:|NumberOfSteps| "0")
                       (:|VolatilityLowerBound| "0")
                       (:|VolatilityUpperBound| "0")
                       (:|LastImpliedVolatilityUpdate| "1900-01-01")
                       (:|BackOfficeContractCode|)
                       (:|PriceOverride| "No")
                       (:|LSEReportFlag| "No")
                       (:|Status| "A")
                       (:|FaceValue| "100")
                       (:|Compound| "No")
                       (:|CompoundPutCall| "Call")
                       (:|CompoundExpiry| "1900-01-01")
                       (:|CompoundStrike| "0")
                       (:|WarrantDiscountOption| "No")
                       (:|WarrantDiscount| "0")
                       (:|WarrantRmin| "0")
                       (:|WarrantRmax| "0")
                       (:|TaxRegime|)
                       (:|BestOf| "No")
                       (:|Lookback| "No")
                       (:|Barrier| "No")
                       (:|Digital| "No")
                       (:|FwdStart| "No")
                       (:|FwdStartDate| "1900-01-01")
                       (:|Chooser| "No")
                       (:|ChooserDate| "1900-01-01")
                       (:|OnFuture| "No")
                       (:|ComponentsAmount| "0")
                       (:|PseudoProductType|)
                       (:|UseWeights| "No")
                       (:|WhenRebatePaid| "0")
                       (:|Ladder| "0")
                       (:|CurveType| "0")
                       (:|CurveID|)
                       (:|BondCurrency|)
                       (:|BondMarket|)
                       (:|BondSpread| "0")
                       (:|CalibrateModel| "No")
                       (:|PricingRule| "SL")
                       (:|AdjustmentMethod| "0")
                       (:|MaturityDateUnadj| "1900-01-01")
                       (:|Coupon| "0")
                       (:|Frequency|)
                       (:|InterestCalcBasis|)
                       (:|AccrualCalcBasis|)
                       (:|DateStubAbsStart| "1900-01-01")
                       (:|DateStubAbsEnd| "1900-01-01")
                       (:|DatesAdjMethod|)
                       (:|MaturityPrice| "0")
                       (:|UnitAmount| "No")
                       (:|CouponMonthSameDay| "No")
                       (:|FrnMargin| "0")
                       (:|AuditTimeStamp| ,audit-time-stamp)
                       (:|AuditUserName| "MGR")))))
(defmethod initialize-instance :after ((self ntm::index)
                                       &key &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :rio-equity-type)
                    :|Index|))

;;;;***** Equity
(defclass ntm::equity (ntm::rio-equity-type) ())
(export 'ntm::equity '#:ntm)
(defmethod initialize-instance :after ((self ntm::equity)
                                       &key &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :rio-equity-type)
                    :|Equity|))

;;;;***** Warrant
(defclass ntm::warrant (ntm::rio-equity-type) ())
(export 'ntm::warrant '#:ntm)
(defmethod initialize-instance :after
    ((self ntm::warrant)
     &key exercise-type exercise-date put-or-call strike &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :rio-equity-type)
                    :|Warrant|)
  (ntm::set-values-of-sub-nodes self '(:|TermsAndConditions| :|Warrant| :|IssueDefinition|)
                                :|ExerciseType| exercise-type
                                :|ExerciseDate| exercise-date
                                :|PutOrCall| put-or-call
                                :|Strike| strike))

;;;;***** EquityFuture / EquityOption
(defun ntm::position-id (issue-id maturity-date)
  (format nil "~A:~A" (ntm::to-string issue-id)
          (ntm::to-string maturity-date :format :MonYY)))
(defun ntm::security-series
    (&key
     (issue-id (ntm::mandatory "IssueID"))
     put-or-call
     (strike (when put-or-call
               (ntm::mandatory "Strike")))
     (maturity-date (ntm::mandatory (if put-or-call "ExerciseDate" "DeliveryDate"))))
  `(:|SecuritySeries|
     (:|SecurityID| "36328")
     (:|PositionID|   ,(ntm::position-id issue-id maturity-date))
     (:|ExerciseDate| ,(or (when put-or-call
                             maturity-date) '("1900-01-01")))
     (:|DeliveryDate| ,(or (unless maturity-date) '("1900-01-01")))
     (:|Strike|       ,(or strike '("0")))
     (:|PutOrCall|    ,(or put-or-call '("Put")))
     (:|PriceOffMarket|)
     (:|ServiceName|)
     (:|RecordName|)
     (:|CacheDelay| "0")
     (:|PriceType|)
     (:|Tolerance| "0")
     (:|Price| "0")
     (:|LastPriceUpdate| "1900-01-01")
     (:|PriceSourceFlag| "0")
     (:|FullPriceFactor| "0")
     (:|RtPrice| "0")
     (:|Status| "0")
     (:|LastRtPriceUpdate| "1900-01-01")))

;;;;****** EquityFuture
(defclass ntm::equity-future (ntm::rio-equity-type) ())
(export 'ntm::equity-future '#:ntm)
(defmethod initialize-instance :after
    ((self ntm::equity-future) &key issue-id delivery-dates-series &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :rio-equity-type) :|EquityFuture|)
  (ntm::insert-values self '(:|TermsAndConditions| :|EquityFuture|) -1
                      (mapcar #'(lambda (delivery-date)
                                  (ntm::security-series :issue-id issue-id
                                                        :maturity-date delivery-date))
                              delivery-dates-series)))


;;;;***** Equity Swaps
(defclass ntm::equity-swap (ntm::rio-equity-type) ())
(export 'ntm::equity-swap '#:ntm)
(defmethod initialize-instance :after ((self ntm::equity-swap) &key
                                       currency
                                       (start-date (ntm::mandatory "StartDate"))
                                       (maturity (ntm::mandatory "Maturity"))
                                       (ref-index (ntm::mandatory "Equity/InterestFixIndex")) &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :rio-equity-type) :|EquitySwap|)
  (ntm::insert-values self '(:|TermsAndConditions| :|EquitySwap|) -1
                      `(:|IssueOTC|
                         (:|SharesNotional| "Cash")
                         (:|StartDate| ,start-date)
                         (:|Maturity| ,maturity)
                         (:|OpenEnded| "No")
                         (:|EquityExchange| "Notional")
                         (:|EquityPayFreq| "Single")
                         (:|EquityResetFreq| "Single")
                         (:|EquityObsFreq| "0")
                         (:|PaymentOffset| "0")
                         (:|EquityCurrency| ,currency)
                         (:|EquityMarket| "MARKET")
                         (:|EquityFixIndex| ,ref-index)
                         (:|EquityInitialFixing| "0")
                         (:|EquityCalcType| "Stock")
                         (:|EquityDividend| "0")
                         (:|EquityPaymentHols|)
                         (:|EquityResetHols|)
                         (:|InterestFloatFix| "Floating")
                         (:|InterestMargin| "1")
                         (:|InterestExchange| "Notional")
                         (:|InterestPayFreq| "Single")
                         (:|InterestResetFreq| "Single")
                         (:|InterestObsFreq| "0")
                         (:|InterestPayOffset| "0")
                         (:|InterestCurrency| ,currency)
                         (:|InterestMarket| "MARKET")
                         (:|InterestFixIndex| ,ref-index)
                         (:|LegNInterestBasisumber| "1")
                         (:|InterestLag| "0")
                         (:|InterestRateSpread| "0")
                         (:|InterestPaymentHols|)
                         (:|InterestResetHols|)
                         (:|InterestDateAdjustmentMethod| "1")
                         (:|EquityDateAdjustmentMethod| "16")
                         (:|InterestFXRate| "1")
                         (:|EquityFXRate| "1"))))

;;;;**** Bonds
(defclass ntm::bond-issue (ntm::terms-and-conditions) ())
(defmethod initialize-instance :after ((self ntm::bond-issue) &key
                                       (issue-id (ntm::mandatory "IssueID"))
                                       (currency (ntm::mandatory "CCY"))
                                       description
                                       (issue-date (ntm::mandatory "IssueDate"))
                                       (issuer-code (ntm::mandatory "IssuerCode"))
                                       (dual-currency currency)
                                       (dual-fx-rate (if (equal currency dual-currency)
                                                         1
                                                         (ntm::mandatory "DualFXRate")))
                                       (maturity-date (ntm::mandatory "MaturityDateUnadj"))
                                       (frequency "12m")
                                       (basis (ntm::mandatory "CalcBasis"))
                                       &allow-other-keys)
  (ntm::set-values self '(:|TermsAndConditions|)
                   `(:bond
                     (:|IssueID| ,issue-id)
                     (:|AmountIssued| "0")
                     (:CCY currency) 
                     (:|CleanDirtyFlag| "Clean")
                     (:|DayOffset| "0")
                     (:|Description| ,description)
                     (:|IssueDate| ,issue-date)
                     (:|IssuerCode| ,issuer-code)
                     (:|IssuePrice| "100")
                     (:|Market| "MARKET")
                     (:|MinimumTradeQty| "0")
                     (:|PrcYldType| "Price")
                     (:|PriceIncrement| "0.01")
                     (:|PriceInputBidOfferSpread| "0")
                     (:|SettleMethod| "Business")
                     (:|TaxMethod| "NONE")
                     (:|YieldMethod| "Isma")
                     (:|AccrualAccuracyMode| "Accrual Rounding")
                     (:|AccrualAccuracyDp| "0")
                     (:|BasePrice| "0")
                     (:|DefaultSettleMethod| "Offset")
                     (:|DualCurrency| ,dual-currency)
                     (:|DualFXRate| ,dual-fx-rate)
                     (:|DualMarket| "MARKET")
                     (:|DualType| "Coupons")
                     (:|MaturityDateUnadj| ,maturity-date)
                     (:|MaturityPrice| "100")
                     (:|PriceYieldCalcBasis| ,basis)
                     (:|QuoteAccuracyMode| "Quote Rounding")
                     (:|QuoteAccuracyDp| "0")
                     (:|RedenomAccruals| "None")
                     (:|RedenominationDate| "1900-01-01")
                     (:|RedenominationMethod| "None")
                     (:|UnitAmount| "100")
                     (:|AccrualCalcBasis| ,basis)
                     (:|AccrualUsingAdjOrUnadj| "Unadjusted")
                     (:|AccrualExDivMethod| "None")
                     (:|ActiveRedemption| "Maturity")
                     (:|CapGainAccretionMethod| "None")
                     (:|CouponDenomination| "100")
                     (:|CouponMonthSameDay| "No")
                     (:|CouponPrecision| "10")
                     (:|CouponRounding| "None")
                     (:|DatesAdjMethod| "None")
                     (:|DatesExtraPayAdjMethod| "None")
                     (:|ExDivDays| "0")
                     (:|Frequency| ,frequency)
                     (:|InterestCalcBasis| ,basis)
                     (:|InterestCalcToAdjOrUnadjDates| "Unadjusted")
                     (:|JgbRoundingMethod| "None"))))
;;;;***** Fixed Issues
(defclass ntm::fixed-issue (ntm::bond-issue) ())
(export 'ntm::fixed-issue '#:ntm)
(defmethod initialize-instance :after ((self ntm::fixed-issue)
                                       &key (coupon 0) &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :bond) :|FixedIssue|)
  (ntm::insert-values self '(:|TermsAndConditions| :|FixedIssue|) 39
                      `(:|Coupon| ,coupon)))

;;;;***** Floating Issues
(defclass ntm::floating-issue (ntm::bond-issue) ())
(export 'ntm::floating-issue '#:ntm)
(defmethod initialize-instance :after ((self ntm::floating-issue) &key
                                       (floating-index-id (ntm::mandatory "FloatingIndexID"))
                                       (margin 0) &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :bond) :|FloatingIssue|)
  (ntm::insert-values self '(:|TermsAndConditions| :|FloatingIssue|) 25
                      '(:|ExDivDaysType| "Business")
                      '(:|Flavour| "None"))
  (ntm::insert-values self '(:|TermsAndConditions| :|FloatingIssue|) 48
                      (:|FloatingIndexID| floating-index-id))
  (ntm::insert-values self '(:|TermsAndConditions| :|FloatingIssue|) 50
                      '(:|FrnCap| "0")
                      '(:|FrnFloor| "0")
                      '(:|FrnMargin| margin)
                      '(:|FrnSimpleComplex| "No")
                      '(:|FrnUseDefaultInterp| "No"))
  (ntm::insert-values self '(:|TermsAndConditions| :|FloatingIssue|) 57
                      '(:|IsRateCapped| "No")
                      '(:|IsRateFloored| "No"))
  (ntm::insert-values self '(:|TermsAndConditions| :|FloatingIssue|) 60
                      '(:|RateConst| "0")
                      '(:|RateMult| "1")
                      '(:|UseRateMult| "No")))

;;;;**** Futures on Bonds
(defclass ntm::bond-future (ntm::terms-and-conditions) ())
(export 'ntm::bond-future '#:ntm)
(defmethod initialize-instance :after ((self ntm::bond-future) &key
                                       (contract-id (ntm::mandatory "ContractID"))
                                       (currency (ntm::mandatory "Currency"))
                                       (contract-size (ntm::mandatory "ContractSize"))
                                       (price-factor 1)
                                       (settlement-date (ntm::mandatory "SettlementDate"))
                                       (index-term (ntm::mandatory "IndexTerm"))
                                       (delivery-date (ntm::mandatory "Delivery"))
                                       (coupon (ntm::mandatory "Coupon"))
                                       (frequency (ntm::mandatory "Frequency"))
                                       &allow-other-keys)
  (ntm::set-values self '(:|TermsAndConditions|)
                   `(:|BondFutureContract|
                      (:|CtrtDefinition|
                        (:|ContractID| ,contract-id)
                        (:|Description|)
                        (:CCY currency)
                        (:|Extensions|
                          (:|Extension|
                            (:|ExtName| "Market1")
                            (:|ExtValue| (:@ :|datatype| "String") "MARKET"))))
                      (:|CtrtStructure|
                        (:|ContractSize| ,contract-size)
                        (:|TickValue| "10")
                        (:|PriceIncrement| "0.01")
                        (:|PriceFactor| ,price-factor)
                        (:|SettlementDate| ,settlement-date)
                        (:|NumCtrtTraded| "1")
                        (:|SettlementLag| (:@ :|dayType| "Business") "0"))
                      (:|CtrtRateDetails|
                        (:|Term|
                          (:|IndexTerm| (:@ :|value| ,index-term)))
                        (:|ParOrDiscounted| (:@ :|value| "Par"))
                        (:|DayCount| (:@ :|value| "ACT:ACT"))
                        (:|AdjustCouponDates| (:@ :|value| "No")))
                      (:|BondFutType| (:@ :|Type| "Generic"))
                      (:|Coupons|
                        (:|Delivery| ,(ntm::to-string delivery-date :format :monyy))
                        (:|Coupon| ,coupon))
                      (:|Frequency| (:@ :|value| frequency))
                      (:|PriceMethod| (:@ :|YieldOrPrice| "Price"))
                      (:|CTDMethod| (:@ :CTD "None")))))



;;;;*** Trades
(defclass ntm::trade (ntm::ntm-message) ())
(defmethod initialize-instance :after ((self ntm::trade) &key
                                       system-id location
                                       (trade-id (ntm::mandatory "TradeID"))
                                       (trade-date (ntm::mandatory "TradeDate"))
                                       (trader (ntm::mandatory "Trader"))
                                       (counterparty (ntm::mandatory "Counterparty"))
                                       description (notes "")
                                       (trading-area (ntm::mandatory "TradingArea"))
                                       (accounting-area (ntm::mandatory "AccountingArea"))
                                       (accounting-group (ntm::mandatory "AccountingGroup"))
                                       (pseudo-product-type (ntm::mandatory "PseudoProductType"))
                                       (security-type (ntm::mandatory "SecurityType"))
                                       net-pv
                                       currency
                                       &allow-other-keys)
  (ntm::insert-values self '() -1
                      `(:|NTMTrade|
                         (:|TradeHeader|
                           (:|SysTradeID|
                             (:|SystemID| ,system-id)
                             (:|Location| ,location)
                             (:|TradeID| ,trade-id))
                           (:|TradeDate| ,(ntm::to-string trade-date :full t))
                           (:|OriginEntity|
                             (:|Entity| (:@ :|role| "Owner")
                               (:|Organization|)
                               (:|Trader| ,trader)))
                           (:|OtherEntity|
                             (:|Entity| (:@ :|role| "Counterparty")
                               (:|Organization| ,counterparty)))
                           (:|TradeDescription| ,description)
                           (:|TradeRole| (:@ :|value| "Actual"))
                           (:|TradeMessageRole| (:@ :|value| "New"))
                           (:|Notes| (:@ XML:|space| "preserve") ,notes))
                         (:|TradeTags|
                           (:|TradingArea| ,trading-area)
                           (:|AccountingArea| ,accounting-area)
                           (:|AccountingGroup| ,accounting-group)
                           (:|Extensions|
                             (:|Extension|
                               (:|ExtName| "CalculationAgent")
                               (:|ExtValue| (:@ :|datatype| "String") "Trade Owner"))
                             (:|Extension|
                               (:|ExtName| "CloseDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "FinalDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "SettleDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "AgreementDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "InterestThruDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "ParentAgreeDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "ParentCloseDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime")))
                             (:|Extension|
                               (:|ExtName| "UserTradeDate")
                               (:|ExtValue| (:@ :|datatype| "Datetime") ,(ntm::to-string trade-date :full t)))))
                         (:trade-body)
                         (:|Extensions|
                           (:|Extension|
                             (:|ExtName| "Market1")
                             (:|ExtValue| (:@ :|datatype| "String") "MARKET"))
                           (when net-pv
                             (:|Extension|  
                               (:|ExtName|  "NetPVTrade") 
                               (:|ExtValue| (:@ :|datatype| "Float") ,net-pv)) 
                             (:|Extension|  
                               (:|ExtName|  "NetPVCurrency") 
                               (:|ExtValue| (:@ :|datatype| "String") ,currency)))
                           (:|Extension|
                             (:|ExtName| "PseudoProduct")
                             (:|ExtValue| (:@ :|datatype| "String") "BankingInstrument"))
                           (:|Extension|
                             (:|ExtName| "PseudoProductType")
                             (:|ExtValue| (:@ :|datatype| "String") ,pseudo-product-type))
                           (:|Extension|
                             (:|ExtName| "BookValueLocal")
                             (:|ExtValue| (:@ :|datatype| "Float") "0"))
                           (:|Extension|
                             (:|ExtName| "BookValueIFRS")
                             (:|ExtValue| (:@ :|datatype| "Float") "0"))
                           (:|Extension|
                             (:|ExtName| "SecurityType")
                             (:|ExtValue| (:@ :|datatype| "String") ,security-type))
                           (:|Extension|
                             (:|ExtName| "Note")
                             (:|ExtValue| (:@ :|datatype| "String") ,notes))))))


;;;;**** Bonds
(defclass ntm::bond (ntm::trade) ())
(export 'ntm::bond '#:ntm)
(defmethod initialize-instance :after ((self ntm::bond) &key
                                       (issue-id (ntm::mandatory "IssueId"))
                                       (settlement-date (ntm::mandatory "SettlementDate"))
                                       (buy-or-sell (ntm::mandatory "BuyOrSell"))
                                       (principal (ntm::mandatory "Principal"))
                                       (price 100)
                                       &allow-other-keys)
  (ntm::rename-node self '(:|NTMTrade| :trade-body) :|Bond|)
  (ntm::set-values self '(:|NTMTrade| :|Bond|)
                   `(:|IssueID| ,issue-id)
                   `(:|SettlementDate| ,settlement-date)
                   `(:|BuyOrSell| (:@ :|value| ,buy-or-sell))
                   `(:|PriceOrYield| (:@ :|value| "Price") ,price)
                   `(:|Principal| ,principal)))

;;;;***** Floating Rate Notes
(defclass ntm::frn (ntm::bond) ())
(export 'ntm::frn '#:ntm)
(defmethod initialize-instance :after ((self ntm::frn) &key
                                       (assumed-libor-rate 0)
                                       &allow-other-keys)
  (ntm::rename-node self '(:|NTMTrade| :|Bond|) :FRN)
  (ntm::set-values self '(:|NTMTrade| :FRN)
                   `(:|FundRate| (:@ :|edited| "No") 0)
                   `(:|AssumedLiborRate| (:@ :|edited| "ManualOverride") ,assumed-libor-rate)))





;;;;**** Payments
(defclass ntm::payment (ntm::trade) ())
(export 'ntm::payment '#:ntm)
(defmethod initialize-instance :after ((self ntm::payment) &key
                                       (pay-or-receive (ntm::mandatory "PayOrReceive"))
                                       (pay-date (ntm::mandatory "PayDate"))
                                       (currency (ntm::mandatory "Currency"))
                                       (amount (ntm::mandatory "Amount"))
                                       &allow-other-keys)
  (ntm::rename-node self '(:|NTMTrade| :trade-body) :|Payment|)
  (ntm::set-values self '(:|NTMTrade| :|Payment|)
                   `(:|Payment|  
                      (:|PayOrReceive| (:@ :|value| ,pay-or-receive)) 
                      (:|Cashflow| (:@ :|CFLType| "Principal") 
                        (:|CashflowID|  "{0EFF8420-12D6-40BB-8594-DC5DD8A55AC0}") 
                        (:|CashflowPayment|  
                          (:|PayDate|  ,pay-date) 
                          (:|Amount|  ,amount) 
                          (:CCY  ,currency))))))


;;;;***** Analytics Representations 
(defclass ntm::analytics-representation (ntm::payment)
  ((ntm::analytics-representation-string :initform (error "No Analytics Representation String given.")
                                         :type string)))
(defmethod initialize-instance :after ((self ntm::analytics-representation) &key
                                       &allow-other-keys)
  (with-slots (ntm::analytics-representation-string) slef
    (ntm::insert-values self '(:|NTMTrade| :|Extensions|) 1
                        (:|Extension|  
                            (:|ExtName|  "OverrideMapping") 
                            (:|ExtValue| (:@ :|datatype| "Integer") "1")) 
                        (:|Extension|  
                            (:|ExtName|  "AnalyticsRepresentationStr") 
                            (:|ExtValue| (:@ :|datatype| "String") ,ntm::analytics-representation-string)))))





;;;;****** Equity Variance Swaps
(defclass ntm::equity-variance-swap (ntm::analytics-representation) ())
(export 'ntm::equity-variance-swap '#:ntm)
(defmethod initialize-instance :before ((self ntm::equity-variance-swap) &key
                                        (reference (ntm:mandatory "Reference"))
                                        (buy-sell (ntm:mandatory "Buy_Sell"))
                                        (mtm (ntm:mandatory "MtM"))
                                        (tags (ntm:mandatory "Tags"))
                                        (currency (ntm:mandatory "Currency"))
                                        (maturity-date (ntm:mandatory "Maturity_Date"))
                                        (pay-receive (ntm:mandatory "Pay_Receive"))
                                        (strike (ntm:mandatory "Strike"))
                                        (realised-volatility (ntm:mandatory "Realised_Volatility"))
                                        (effective-date (ntm:mandatory "Effective_Date"))
                                        (vega-notional (ntm:mandatory "Vega_Notional"))
                                        (equity (ntm:mandatory "Equity"))
                                        &allow-other-keys)
  (setf (slot-value self 'ntm::analytics-representation-string)
        (format nil "Object=StructuredDeal,Reference=~0@*~A,MtM=~2@*~:[<undefined>~:;~2@*~A~],Description=Structured Deal,Buy_Sell=Buy,Currency=
  Object=StructuredDeal,Reference=61465,MtM=<undefined>,Description=Structured Deal,Buy_Sell=Buy,Currency=EUR
    Object=EquityVarianceSwapDeal,Reference=,MtM=<undefined>,Currency=EUR,Maturity_Date=23Apr2010,Pay_Receive=Pay,Strike=17%,Realised_Volatility=28.365%,Effective_Date=15Feb2010,Vega_Notional=340,Equity=SX5E
"
                reference
                buy-sell
                mtm
                tags
                currency
                maturity-date
                pay-receive
                strike
                realised-volatility
                effective-date
                vega-notional
                equity)))




;;;;==================================================================
#+nil(eval-when (:compile-toplevel :load-toplevel :execute)
  (ntm::disable-ntm-syntax))





(defun ntm::readable-print-lsxml (lsxml s &key (type s-xml::*default-dom-type*))
  (if (stringp lsxml)
      (format nil "~S" lsxml)
      (let ((tag (s-xml::get-tag lsxml :type type))
            (attributes (s-xml::get-attributes lsxml :type type))
            (values (mapcar #'(lambda (x) (ntm::readable-print-lsxml x nil))
                            (s-xml::get-values lsxml :type type))))
        (format s
           "~%(~S ~@[(:@~{ ~S~})~]~{ ~A~})" tag attributes values))))


#+nil
(ntm::readable-print-lsxml (s-xml:parse-xml-file
                            #p"h:/temp/61465.xml"
                            :output-type :lxml) t :type :lxml)




#+nil(:|NTMMessage| (:@ :|version| "1.03") 
    (:|NTMHeader|  
        (:|MessageID|  "91fa29d8-676c-426c-b729-8b45ba9ae3f3") 
        (:|MessageSource|  
            (:|SystemID|  "Panorama") 
            (:|Location| )) 
        (:|Timezone|  "GMT") 
        (:|DateFormat| (:@ :|format| "ISO8601")) 
        (:|GeneratedTime|  "2010-07-22T11:43:28") 
        (:|PublishResponse| (:@ :|value| "JustLog"))) 
    (:|NTMTrade|  
        (:|TradeHeader|  
            (:|SysTradeID|  
                (:|SystemID|  "Panorama") 
                (:|Location| ) 
                (:|TradeID|  "61465")) 
            (:|TradeDate|  "2010-07-21T11:07:27") 
            (:|OriginEntity|  
                (:|Entity| (:@ :|role| "Owner") 
                    (:|Organization| ) 
                    (:|Trader|  "MGR"))) 
            (:|OtherEntity|  
                (:|Entity| (:@ :|role| "Counterparty") 
                    (:|Organization|  "DERIV"))) 
            (:|TradeDescription| ) 
            (:|TradeRole| (:@ :|value| "Potential")) 
            (:|TradeMessageRole| (:@ :|value| "New"))) 
        (:|TradeTags|  
            (:|TradingArea|  "A") 
            (:|AccountingArea|  "CH") 
            (:|AccountingGroup|  "CH") 
            (:|Extensions|  
                (:|Extension|  
                    (:|ExtName|  "UserTradeDate") 
                    (:|ExtValue| (:@ :|datatype| "Datetime") "2010-02-25T00:00:00"))) 
            (:|InstrumentID|  "61465")) 
        (:|Payment|  
            (:|PayOrReceive| (:@ :|value| "Pay")) 
            (:|Cashflow| (:@ :|CFLType| "Principal") 
                (:|CashflowID|  "{595C6CFB-B437-445C-B8BB-12EE20E47519}") 
                (:|CashflowPayment|  
                    (:|PayDate|  "2010-03-01") 
                    (:|Amount|  "1000000") 
                    (:CCY  "EUR")))) 
        (:|Extensions|  
            (:|Extension|  
                (:|ExtName|  "Market1") 
                (:|ExtValue| (:@ :|datatype| "String") "MARKET")) 
            (:|Extension|  
                (:|ExtName|  "OverrideMapping") 
                (:|ExtValue| (:@ :|datatype| "Integer") "1")) 
            (:|Extension|  
                (:|ExtName|  "AnalyticsRepresentationStr") 
                (:|ExtValue| (:@ :|datatype| "String") "Object=StructuredDeal,Reference=61465,MtM=<undefined>,Description=Structured Deal,Buy_Sell=Buy,Currency=
  Object=StructuredDeal,Reference=61465,MtM=<undefined>,Description=Structured Deal,Buy_Sell=Buy,Currency=EUR
    Object=EquityVarianceSwapDeal,Reference=,MtM=<undefined>,Currency=EUR,Maturity_Date=23Apr2010,Pay_Receive=Pay,Strike=17%,Realised_Volatility=28.365%,Effective_Date=15Feb2010,Vega_Notional=340,Equity=SX5E
")) 
            (:|Extension|  
                (:|ExtName|  "NetPVFees") 
                (:|ExtValue| (:@ :|datatype| "Float") "0")) 
            (:|Extension|  
                (:|ExtName|  "NetPVPremium") 
                (:|ExtValue| (:@ :|datatype| "Float") "0")) 
            (:|Extension|  
                (:|ExtName|  "NetPVTrade") 
                (:|ExtValue| (:@ :|datatype| "Float") "3076.21020507813")) 
            (:|Extension|  
                (:|ExtName|  "NetPVCurrency") 
                (:|ExtValue| (:@ :|datatype| "String") "EUR")))))NIL











#+nil((:|NTMMessage| :|version| "1.03")
 (:|NTMHeader|
   (:|MessageID| "87a1291e-85e0-4e4e-8c8b-bcbfaafcb96b")
   (:|MessageSource| (:|SystemID| "Panorama") :|Location|)
   (:|Timezone| "GMT")
   ((:|DateFormat| :|format| "ISO8601"))
   (:|GeneratedTime| "2010-07-14T18:09:45")
   ((:|PublishResponse| :|value| "JustLog")))
 (:|NTMTrade|
   (:|TradeHeader|
     (:|SysTradeID|
       (:|SystemID| "BP")
       (:|Location| "0912")
       (:|TradeID| "128189"))
     (:|TradeDate| "2009-12-28T12:00:00")
     (:|OriginEntity|
       ((:|Entity| :|role| "Owner")
        :|Organization|
        (:|Trader| "ALL")))
     (:|OtherEntity|
       ((:|Entity| :|role| "Counterparty")
        (:|Organization| "A")))
     (:|TradeDescription| "TELEF.EM.4.375%16")
     ((:|TradeRole| :|value| "Actual"))
     ((:|TradeMessageRole| :|value| "New"))
     ((:|Notes| XML:|space| "preserve") "1051023186XS024194663002"))
   (:|TradeTags|
     (:|TradingArea| "A")
     (:|AccountingArea| "FR")
     (:|AccountingGroup| "SLAP")
     (:|Extensions|
       (:|Extension|
         (:|ExtName| "CalculationAgent")
         ((:|ExtValue| :|datatype| "String") "Trade Owner"))
       (:|Extension|
         (:|ExtName| "CloseDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "FinalDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "SettleDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "AgreementDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "InterestThruDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "ParentAgreeDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "ParentCloseDate")
         ((:|ExtValue| :|datatype| "Datetime")))
       (:|Extension|
         (:|ExtName| "UserTradeDate")
         ((:|ExtValue| :|datatype| "Datetime") "2009-12-28T12:00:00")))
     (:|InstrumentID| "18973"))
   (:|Bond|
     (:|IssueID| "XS0241946630")
     (:|SettlementDate| "2009-12-28")
     ((:|BuyOrSell| :|value| "Buy"))
     ((:|PriceOrYield| :|value| "Price") "102.28")
     (:|Principal| "5000000"))
   (:|Extensions|
     (:|Extension|
       (:|ExtName| "Market1")
       ((:|ExtValue| :|datatype| "String") "MARKET"))
     (:|Extension|
       (:|ExtName| "PseudoProduct")
       ((:|ExtValue| :|datatype| "String") "BankingInstrument"))
     (:|Extension|
       (:|ExtName| "PseudoProductType")
       ((:|ExtValue| :|datatype| "String") "BONDFIXED"))
     (:|Extension|
       (:|ExtName| "BookValueLocal")
       ((:|ExtValue| :|datatype| "Float") "0"))
     (:|Extension|
       (:|ExtName| "BookValueIFRS")
       ((:|ExtValue| :|datatype| "Float") "0"))
     (:|Extension|
       (:|ExtName| "SecurityType")
       ((:|ExtValue| :|datatype| "String") "Bonds"))
     (:|Extension|
       (:|ExtName| "Note")
       ((:|ExtValue| :|datatype| "String") "1051023186XS024194663002")))))