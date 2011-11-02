;;;; -*- outline-regexp:";;;;[*]+ +" -*-

;;;;** NTM Configuration
(in-package #:ch.swisslife.ntm-system)


;;;;*** NTMHeader
(defclass ntm::ntm-header (ntm::ntm-element) ())
(defmethod initialize-instance ((self ntm::ntm-header) &key
                                (message-id "")
                                (system-id (ntm::mandatory "SystemID"))
                                (location (ntm::mandatory "Location"))
                                (generated-time (date::now)) &allow-other-keys)
  (setf (ntm-node self)
        (ntm::node ()
                   (:|NTMHeader|
                     (:|MessageID| message-id)
                     (:|MessageSource|
                       (:|SystemID| system-id)
                       (:|Location| location))
                     (:|Timezone| "GMT")
                     (:|DateFormat| (:@ :|format| ntm::date-format))
                     (:|GeneratedTime| generated-time) ;full
                     (:|PublishResponse| (:@ :|value| "JustLog"))))))
(defmethod ntm::format-ntm-node-value ((ntm-context (eql :|NTMHeader|)) (node-name (eql :|GeneratedTime|)) (value date:date-time))
  (date:format-date value ntm::date-format :full t))

;;;;*** Trade
(defclass ntm::trade (ntm::ntm-element) ())
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
                                       net-pv currency
                                       &allow-other-keys)
  (setf (ntm-node self)
        (ntm::node ()
                   (:|NTMTrade|
                     (:|TradeHeader|
                       (:|SysTradeID|
                         (:|SystemID| system-id)
                         (:|Location| location)
                         (:|TradeID| trade-id))
                       (:|TradeDate| trade-date)
                       (:|OriginEntity|
                         (:|Entity| (:@ :|role| "Owner")
                           (:|Organization|)
                           (:|Trader| trader)))
                       (:|OtherEntity|
                         (:|Entity| (:@ :|role| "Counterparty")
                           (:|Organization| counterparty)))
                       (:|TradeDescription| description)
                       (:|TradeRole| (:@ :|value| "Actual"))
                       (:|TradeMessageRole| (:@ :|value| "New"))
                       (:|Notes| (:@ XML:|space| "preserve") notes))
                     (:|TradeTags|
                       (:|TradingArea| trading-area)
                       (:|AccountingArea| accounting-area)
                       (:|AccountingGroup| accounting-group)
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
                           (:|ExtValue| (:@ :|datatype| "Datetime") trade-date)))) ;full
                     (:trade-body)
                     (:|Extensions|
                       (:|Extension|
                         (:|ExtName| "Market1")
                         (:|ExtValue| (:@ :|datatype| "String") "MARKET"))
                       (:|Extension|
                         (:|ExtName| "PseudoProduct")
                         (:|ExtValue| (:@ :|datatype| "String") "BankingInstrument"))
                       (:|Extension|
                         (:|ExtName| "PseudoProductType")
                         (:|ExtValue| (:@ :|datatype| "String") pseudo-product-type))
                       (:|Extension|
                         (:|ExtName| "BookValueLocal")
                         (:|ExtValue| (:@ :|datatype| "Float") "0"))
                       (:|Extension|
                         (:|ExtName| "BookValueIFRS")
                         (:|ExtValue| (:@ :|datatype| "Float") "0"))
                       (:|Extension|
                         (:|ExtName| "SecurityType")
                         (:|ExtValue| (:@ :|datatype| "String") security-type))
                       (:|Extension|
                         (:|ExtName| "Note")
                         (:|ExtValue| (:@ :|datatype| "String") notes))))))
  (when net-pv
    (ntm:insert-values self '(:|Extensions|) 1
                       (ntm:node ()
                                 (:|Extension|  
                                   (:|ExtName|  "NetPVTrade") 
                                   (:|ExtValue| (:@ :|datatype| "Float") net-pv)))
                       (ntm:node ()
                                 (:|Extension|  
                                   (:|ExtName|  "NetPVCurrency") 
                                   (:|ExtValue| (:@ :|datatype| "String") currency))))))

(defmethod ntm::format-ntm-node-value (ntm-context (node-name (eql :|ExtValue|)) (value date:date-time))
  (declare (ignore ntm-context))
  (date:format-date value ntm::date-format :full t))

;;;;*** TermsAndConditions
(defclass ntm::terms-and-conditions (ntm::ntm-element) ())
(defmethod initialize-instance :after ((self ntm::terms-and-conditions)
                                       &key &allow-other-keys)
  (setf (slot-value self 'ntm-node) (ntm:node ()
                                              (:|TermsAndConditions|))))

;;;;*** ExternalResultsCalibration
(defclass ntm::external-results-calibration (ntm::ntm-element) ())

;;;;** NTMMessage
(defclass ntm::ntm-message (ntm::ntm-element) ())


(labels ((priority (ntm-element)
           (values-list (cdr (assoc (class-of ntm-element)
                                    (load-time-value (list (list (find-class 'ntm::ntm-header) 0 1)
                                                           (list (find-class 'ntm::trade) 1)
                                                           (list (find-class 'ntm::terms-and-conditions) 2)
                                                           (list (find-class 'ntm::external-results-calibration) 3))))))))
  (defmethod initialize-instance :after ((self ntm::ntm-message) &rest ntm-elements &key
                                         ntm-header trade terms-and-conditions external-results-calibration)
    (declare (ignore ntm-elements ntm-header trade terms-and-conditions external-results-calibration))
    (setf (ntm-node self) (ntm::node ()
                                     (:|NTMMessage| (:@ :|version| "1.03")))
          (ntm:get-nodes self) (mapcan (lambda (class)
                                         (let ((max-nbr-elts (nth-value 1 (priority (first class)))))
                                           (assert (or (null max-nbr-elts)
                                                       (< (length class) max-nbr-elts))
                                                   () "Maximally ~A ~A element~P allowed."
                                                   max-nbr-elts (ntm-element-name (first class))))
                                         (mapcar #'ntm-node class))
                                       (sort (oam:classify (remove-if (complement #'priority) ntm-elements)
                                                           :key #'class-of)
                                             #'priority :key #'first)))))



;;;;**** rio-equity-type
(defclass ntm::rio-equity-type (ntm::terms-and-conditions) ())

(defclass ntm::index (ntm::rio-equity-type) ())

(defmethod initialize-instance :after ((self ntm::rio-equity-type) &key
                                       (issue-id    (ntm:mandatory "IssueID"))
                                       description
                                       (currency    (ntm:mandatory "Currency"))
                                       (underlying (unless (typep self 'ntm::index)
                                                     (ntm:mandatory "Underlying")))
                                       (contract-size 1)
                                       (audit-time-stamp (date::now)) &allow-other-keys)
  (setf (ntm-node self)
        (ntm:node ()
                  (:rio-equity-type
                   (:|IssueDefinition|
                     (:|IssueID| issue-id)
                     (:|Description| description)
                     (:|Underlying| underlying)
                     (:|AmountIssued| "0")
                     (:|ConversionRatio| "1")
                     (:|Calendar|)
                     (:|Currency| currency)
                     (:|TradingOffset| "0")
                     (:|ExchangeID|)
                     (:|IssuerCode|)
                     (:|InitialPrice| "0")
                     (:|Market| "MARKET")
                     (:|PriceIncrement| "0.01")
                     (:|SettleMethod| "Bus")
                     (:|TickValue| "0.01")
                     (:|ContractSize| contract-size)
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
                     (:|AuditTimeStamp| audit-time-stamp)
                     (:|AuditUserName| "MGR"))))))

;;;;***** Index
(defmethod initialize-instance :after ((self ntm::index)
                                       &key &allow-other-keys)
  (ntm::rename-node self '(:rio-equity-type) :|Index|))

;;;;***** Equity
(defclass ntm::equity (ntm::rio-equity-type) ())
(defmethod initialize-instance :after ((self ntm::equity)
                                       &key &allow-other-keys)
  (ntm::rename-node self '(:rio-equity-type) :|Equity|))

;;;;***** Warrant
(defclass ntm::warrant (ntm::rio-equity-type) ())
(defmethod initialize-instance :after
    ((self ntm::warrant)
     &key exercise-type exercise-date put-or-call strike &allow-other-keys)
  (ntm::rename-node self '(:rio-equity-type) :|Warrant|)
  (ntm::set-values-of-sub-nodes self '(:|Warrant| :|IssueDefinition|)
                                :|ExerciseType| exercise-type
                                :|ExerciseDate| exercise-date
                                :|PutOrCall| put-or-call
                                :|Strike| strike))

;;;;***** EquityFuture / EquityOption
(defun ntm::position-id (issue-id maturity-date)
  (format nil "~A:~A" issue-id (date:format-date maturity-date :MonYY)))
(defun ntm::security-series
    (&key
     (issue-id (ntm::mandatory "IssueID"))
     put-or-call
     (strike (when put-or-call (ntm::mandatory "Strike")))
     (maturity-date (ntm::mandatory (if put-or-call "ExerciseDate" "DeliveryDate"))))
  (ntm::node ()
             (:|SecuritySeries|
               (:|SecurityID| "36328")
               (:|PositionID|   (ntm::position-id issue-id maturity-date))
               (:|ExerciseDate| (when put-or-call maturity-date))
               (:|DeliveryDate| (unless put-or-call maturity-date))
               (:|Strike|       strike)
               (:|PutOrCall|    put-or-call)
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
               (:|LastRtPriceUpdate| "1900-01-01"))))
(defmethod ntm:format-ntm-node-value ((ntm-context (eql :|SecuritySeries|)) node-name (value null))
  (case node-name
    ((:|ExerciseDate| :|DeliveryDate| :|LastPriceUpdate| :|LastRtPriceUpdate|)
     "1900-01-01")
    (:|PutOrCall| "Put")
    (t 0)))

;;;;****** EquityFuture
(defclass ntm::equity-future (ntm::rio-equity-type) ())
(defmethod initialize-instance :after ((self ntm::equity-future) &key
                                       issue-id delivery-dates-series &allow-other-keys)
  (ntm::rename-node self '(:rio-equity-type) :|EquityFuture|)
  (ntm::insert-values* self '(:|EquityFuture|) -1
                       (mapcar (lambda (delivery-date)
                                 (ntm::security-series :issue-id issue-id
                                                       :maturity-date delivery-date))
                               delivery-dates-series)))


;;;;***** Equity Swaps
(defclass ntm::equity-swap (ntm::rio-equity-type) ())
(defmethod initialize-instance :after ((self ntm::equity-swap) &key
                                       currency
                                       (start-date (ntm::mandatory "StartDate"))
                                       (maturity (ntm::mandatory "Maturity"))
                                       (ref-index (ntm::mandatory "Equity/InterestFixIndex")) &allow-other-keys)
  (ntm::rename-node self '(:|TermsAndConditions| :rio-equity-type) :|EquitySwap|)
  (ntm::insert-values self '(:|EquitySwap|) -1
                      (ntm:node ()
                                (:|IssueOTC|
                                  (:|SharesNotional| "Cash")
                                  (:|StartDate| start-date)
                                  (:|Maturity| maturity)
                                  (:|OpenEnded| "No")
                                  (:|EquityExchange| "Notional")
                                  (:|EquityPayFreq| "Single")
                                  (:|EquityResetFreq| "Single")
                                  (:|EquityObsFreq| "0")
                                  (:|PaymentOffset| "0")
                                  (:|EquityCurrency| currency)
                                  (:|EquityMarket| "MARKET")
                                  (:|EquityFixIndex| ref-index)
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
                                  (:|InterestCurrency| currency)
                                  (:|InterestMarket| "MARKET")
                                  (:|InterestFixIndex| ref-index)
                                  (:|LegNInterestBasisumber| "1")
                                  (:|InterestLag| "0")
                                  (:|InterestRateSpread| "0")
                                  (:|InterestPaymentHols|)
                                  (:|InterestResetHols|)
                                  (:|InterestDateAdjustmentMethod| "1")
                                  (:|EquityDateAdjustmentMethod| "16")
                                  (:|InterestFXRate| "1")
                                  (:|EquityFXRate| "1")))))

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
  (setf (ntm:get-nodes self) (ntm:node ()
                                       (:bond
                                        (:|IssueID| issue-id)
                                        (:|AmountIssued| "0")
                                        (:CCY currency) 
                                        (:|CleanDirtyFlag| "Clean")
                                        (:|DayOffset| "0")
                                        (:|Description| description)
                                        (:|IssueDate| issue-date)
                                        (:|IssuerCode| issuer-code)
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
                                        (:|DualCurrency| dual-currency)
                                        (:|DualFXRate| dual-fx-rate)
                                        (:|DualMarket| "MARKET")
                                        (:|DualType| "Coupons")
                                        (:|MaturityDateUnadj| maturity-date)
                                        (:|MaturityPrice| "100")
                                        (:|PriceYieldCalcBasis| basis)
                                        (:|QuoteAccuracyMode| "Quote Rounding")
                                        (:|QuoteAccuracyDp| "0")
                                        (:|RedenomAccruals| "None")
                                        (:|RedenominationDate| "1900-01-01")
                                        (:|RedenominationMethod| "None")
                                        (:|UnitAmount| "100")
                                        (:|AccrualCalcBasis| basis)
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
                                        (:|Frequency| frequency)
                                        (:|InterestCalcBasis| basis)
                                        (:|InterestCalcToAdjOrUnadjDates| "Unadjusted")
                                        (:|JgbRoundingMethod| "None")))))
;;;;***** Fixed Issues
(defclass ntm::fixed-issue (ntm::bond-issue) ())
(defmethod initialize-instance :after ((self ntm::fixed-issue)
                                       &key (coupon 0) &allow-other-keys)
  (ntm::rename-node self '(:bond) :|FixedIssue|)
  (ntm::insert-values self '(:|FixedIssue|) 39
                      (ntm:node () (:|Coupon| coupon))))

;;;;***** Floating Issues
(defclass ntm::floating-issue (ntm::bond-issue) ())
(export 'ntm::floating-issue '#:ntm)
(defmethod initialize-instance :after ((self ntm::floating-issue) &key
                                       (floating-index-id (ntm::mandatory "FloatingIndexID"))
                                       (margin 0) &allow-other-keys)
  (ntm::rename-node self '(:bond) :|FloatingIssue|)
  (ntm::insert-values self '(:|FloatingIssue|) 25
                      '(:|ExDivDaysType| "Business")
                      '(:|Flavour| "None"))
  (ntm::insert-values self '(:|FloatingIssue|) 48
                      (ntm:node () (:|FloatingIndexID| floating-index-id)))
  (ntm::insert-values self '(:|FloatingIssue|) 50
                      '(:|FrnCap| "0")
                      '(:|FrnFloor| "0")
                      (ntm:node () (:|FrnMargin| margin))
                      '(:|FrnSimpleComplex| "No")
                      '(:|FrnUseDefaultInterp| "No"))
  (ntm::insert-values self '(:|FloatingIssue|) 57
                      '(:|IsRateCapped| "No")
                      '(:|IsRateFloored| "No"))
  (ntm::insert-values self '(:|FloatingIssue|) 60
                      '(:|RateConst| "0")
                      '(:|RateMult| "1")
                      '(:|UseRateMult| "No")))

;;;;**** Futures on Bonds
(defclass ntm::bond-future (ntm::terms-and-conditions) ())
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
  (setf (ntm::get-nodes self :|TermsAndConditions|) (ntm:node ()
                                                              (:|BondFutureContract|
                                                                (:|CtrtDefinition|
                                                                  (:|ContractID| contract-id)
                                                                  (:|Description|)
                                                                  (:CCY currency)
                                                                  (:|Extensions|
                                                                    (:|Extension|
                                                                      (:|ExtName| "Market1")
                                                                      (:|ExtValue| (:@ :|datatype| "String") "MARKET"))))
                                                                (:|CtrtStructure|
                                                                  (:|ContractSize| contract-size)
                                                                  (:|TickValue| "10")
                                                                  (:|PriceIncrement| "0.01")
                                                                  (:|PriceFactor| price-factor)
                                                                  (:|SettlementDate| settlement-date)
                                                                  (:|NumCtrtTraded| "1")
                                                                  (:|SettlementLag| (:@ :|dayType| "Business") "0"))
                                                                (:|CtrtRateDetails|
                                                                  (:|Term|
                                                                    (:|IndexTerm| (:@ :|value| index-term)))
                                                                  (:|ParOrDiscounted| (:@ :|value| "Par"))
                                                                  (:|DayCount| (:@ :|value| "ACT:ACT"))
                                                                  (:|AdjustCouponDates| (:@ :|value| "No")))
                                                                (:|BondFutType| (:@ :|Type| "Generic"))
                                                                (:|Coupons|
                                                                  (:|Delivery| delivery-date) ;monyy
                                                                  (:|Coupon| coupon))
                                                                (:|Frequency| (:@ :|value| frequency))
                                                                (:|PriceMethod| (:@ :|YieldOrPrice| "Price"))
                                                                (:|CTDMethod| (:@ :CTD "None"))))))

(defmethod ntm:format-ntm-node-value ((ntm-context (eql :|BondFutureContract|)) (node-name (eql :|Delivery|)) (value date:date-time))
  (date:format-date value :monyy))

;;;;*** Trades


;;;;**** Bonds
(defclass ntm::bond (ntm::trade) ())
(defmethod initialize-instance :after ((self ntm::bond) &key
                                       (issue-id (ntm::mandatory "IssueId"))
                                       (settlement-date (ntm::mandatory "SettlementDate"))
                                       (buy-or-sell (ntm::mandatory "BuyOrSell"))
                                       (principal (ntm::mandatory "Principal"))
                                       (price 100)
                                       &allow-other-keys)
  (ntm::rename-node self '(:trade-body) :|Bond|)
  (setf (ntm::get-nodes self :|Bond|) (list
                                       (ntm:node () (:|IssueID| issue-id))
                                       (ntm:node () (:|SettlementDate| settlement-date))
                                       (ntm:node () (:|BuyOrSell| (:@ :|value| buy-or-sell)))
                                       (ntm:node () (:|PriceOrYield| (:@ :|value| "Price") price))
                                       (ntm:node () (:|Principal| principal)))))

(defmethod ntm::format-ntm-attribute-value (ntm-context (node-name (eql :|Buy-Or-Sell|)) (key (eql :|value|)) (value real))
  "Return \"Buy\" if VALUE is non negative and \"Sell\" else."
  (declare (ignore ntm-context node-name key))
  (if (<= 0 value) "Buy" "Sell"))

;;;;***** Floating Rate Notes
(defclass ntm::frn (ntm::bond) ())
(defmethod initialize-instance :after ((self ntm::frn) &key
                                       (assumed-libor-rate 0)
                                       &allow-other-keys)
  (ntm::rename-node self '(:|Bond|) :FRN)
  (setf (ntm::get-nodes self :FRN) (list (ntm:node () (:|FundRate| (:@ :|edited| "No") 0))
                                         (ntm:node () (:|AssumedLiborRate| (:@ :|edited| "ManualOverride") assumed-libor-rate)))))





;;;;**** Payments
(defclass ntm::payment (ntm::trade) ())
(export 'ntm::payment '#:ntm)
(defmethod initialize-instance :after ((self ntm::payment) &key
                                       (pay-or-receive (ntm::mandatory "PayOrReceive"))
                                       (pay-date (ntm::mandatory "PayDate"))
                                       (currency (ntm::mandatory "Currency"))
                                       (amount (ntm::mandatory "Amount"))
                                       &allow-other-keys)
  (ntm::rename-node self '(:trade-body) :|Payment|)
  (setf (ntm::get-nodes self :|Payment|)
        (list
          (ntm:node () (:|PayOrReceive| (:@ :|value| pay-or-receive))) 
          (ntm:node () (:|Cashflow| (:@ :|CFLType| "Principal") 
                         (:|CashflowID|  "{0EFF8420-12D6-40BB-8594-DC5DD8A55AC0}") 
                         (:|CashflowPayment|  
                           (:|PayDate|  pay-date) 
                           (:|Amount|  amount) 
                           (:CCY  currency)))))))


;;;;***** Analytics Representations 
(defclass ntm::analytics-representation (ntm::payment ad:ntm-analytics-representation-string))
(defmethod initialize-instance :after ((self ntm::analytics-representation) &key
                                       analytics-deal
                                       &allow-other-keys)
  (when analytics-representation-string
    (ntm::insert-values self '(:|Extensions|) 1
                        (ntm:node () (:|Extension|  
                                       (:|ExtName|  "OverrideMapping") 
                                       (:|ExtValue| (:@ :|datatype| "Integer") "1"))) 
                        (ntm:node (:ntm-context self)
                                  (:|Extension|  
                                    (:|ExtName|  "AnalyticsRepresentationStr") 
                                    (:|ExtValue| (:@ :|datatype| "String") analytics-deal))))))

(defmethod ntm::format-ntm-node-value ((ntm-context analytics-representation) node-name (value ad:deal))
  (with-output-to-string (out)
    (setf (ad:output-stream ntm-context) out)
    (oam:map-value value nil ntm-context)))






(oam:export-interface '#:ntm)



