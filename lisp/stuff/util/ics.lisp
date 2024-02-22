(asdf:oos 'asdf:load-op 's-xml)
(in-package :s-xml)
(defun trace-xml-new-element-hook (name attributes seed)
  (let ((new-seed (cons (1+ (car seed)) (1+ (cdr seed)))))
    (trace-xml-log (car seed) 
                   "(new-element :name ~s :attributes ~:[()~;~:*~s~] :seed ~s) => ~s" 
                   name attributes seed new-seed)
    new-seed))

(defun trace-xml-finish-element-hook (name attributes parent-seed seed)
  (let ((new-seed (cons (1- (car seed)) (1+ (cdr seed)))))
    (trace-xml-log (car parent-seed)
                   "(finish-element :name ~s :attributes ~:[()~;~:*~s~] :parent-seed ~s :seed ~s) => ~s" 
                   name attributes parent-seed seed new-seed)
    new-seed))

(defun trace-xml-text-hook (string seed)
  (let ((new-seed (cons (car seed) (1+ (cdr seed)))))
    (trace-xml-log (car seed) 
                   "(text :string ~s :seed ~s) => ~s" 
                   string seed new-seed)
    new-seed))

(defun trace-xml (in)
  "Parse and trace a toplevel XML element from stream in"
  (start-parse-xml in
                   (make-instance 'xml-parser-state
                                  :seed (cons 0 0) 
                                  ;; seed car is xml element nesting level
                                  ;; seed cdr is ever increasing from element to element
                                  :new-element-hook #'trace-xml-new-element-hook
                                  :finish-element-hook #'trace-xml-finish-element-hook
                                  :text-hook #'trace-xml-text-hook)))
(defun trace-xml-log (level format-string &rest args)
  (dotimes (i level) (princ " "))
  (princ "
")
  (apply #'format t format-string args))













((:|NTMMessage| :|version| "1.03")
 (:|NTMHeader|
   (:|MessageID| "598089a3-347c-42b2-b61b-8c72975c38f0")
   (:|MessageSource|
     (:|SystemID| "Panorama")
     :|Location|)
   (:|Timezone| "GMT")
   ((:|DateFormat| :|format| "ISO8601"))
   (:|GeneratedTime| "2009-04-02T16:59:03")
   ((:|PublishResponse| :|value| "JustLog")))
 (:|TermsAndConditions|
   (:|EquityFuture|
     (:|IssueDefinition|
       (:|IssueID| "EQFUT007")
       (:|Underlying| "CH0002")
       (:|AmountIssued| "0")
       (:|ConversionRatio| "1")
       :|Calendar|
       (:|Currency| "CHF")
       (:|TradingOffset| "0")
       :|ExchangeID|
       :|IssuerCode|
       (:|InitialPrice| "0")
       (:|Market| "MARKET")
       (:|PriceIncrement| "1")
       (:|SettleMethod| "Bus")
       (:|TickValue| "1")
       (:|ContractSize| "1")
       (:|PriceFactor| "1")
       (:|BasketDivisor| "1")
       (:|PremiumType| "Prc")
       :|Sector|
       (:|SettlementRule| "0d")
       :|Holidays|
       (:|CompoOrQuanto| "0")
       :|PayoutCurrency|
       (:|ExerciseType| "1")
       :|OtherStyles|
       :|SegmentID|
       :|CountryOfRegister|
       (:|ExerciseDate| "1899-12-30")
       (:|PutOrCall| "Put")
       (:|DeliveryDate| "2009-03-31")
       (:|Strike| "0")
       (:FISS "1")
       :|ClassID|
       (:|UseAccountPeriod| "No")
       :|AccountPeriod|
       (:|IssueDate| "2009-03-31")
       (:|UseIssueVolatility| "No")
       (:|Volatility| "0")
       (:|Margined| "No")
       (:|CleanDirtyFlag| "Cln")
       :|PayoutMarket|
       (:|NumberOfSteps| "0")
       (:|VolatilityLowerBound| "0")
       (:|VolatilityUpperBound| "0")
       (:|LastImpliedVolatilityUpdate| "2009-03-31")
       :|BackOfficeContractCode|
       (:|PriceOverride| "No")
       (:|LSEReportFlag| "No")
       (:|Status| "A")
       (:|FaceValue| "100")
       (:|Compound| "No")
       (:|CompoundPutCall| "Call")
       (:|CompoundExpiry| "2009-03-31")
       (:|CompoundStrike| "0")
       (:|WarrantDiscountOption| "No")
       (:|WarrantDiscount| "0")
       (:|WarrantRmin| "0")
       (:|WarrantRmax| "0")
       :|TaxRegime|
       (:|BestOf| "No")
       (:|Lookback| "No")
       (:|Barrier| "No")
       (:|Digital| "No")
       (:|FwdStart| "No")
       (:|FwdStartDate| "1899-12-30")
       (:|Chooser| "No")
       (:|ChooserDate| "2009-03-31")
       (:|OnFuture| "No") 
       (:|ComponentsAmount| "0")
       :|PseudoProductType|
       (:|UseWeights| "No")
       (:|WhenRebatePaid| "0")
       (:|Ladder| "0")
       (:|CurveType| "0")
       :|CurveID|
       :|BondCurrency|
       :|BondMarket| 
       (:|BondSpread| "0")
       (:|CalibrateModel| "No")
       (:|PricingRule| "DEFAULT")
       (:|AdjustmentMethod| "0")
       (:|MaturityDateUnadj| "1899-12-30")
       (:|Coupon| "0")
       :|Frequency|
       :|InterestCalcBasis|
       :|AccrualCalcBasis|
       (:|DateStubAbsStart| "1899-12-30")
       (:|DateStubAbsEnd| "1899-12-30")
       :|DatesAdjMethod|
       (:|MaturityPrice| "0")
       (:|UnitAmount| "No")
       (:|CouponMonthSameDay| "No")
       (:|FrnMargin| "0")
       (:|AuditTimeStamp| "2009-03-31")
       (:|AuditUserName| "mgr"))
     (:|SecuritySeries|
       (:|SecurityID| "45")
       (:|PositionID| "EQFUT007:Mar09")
       (:|ExerciseDate| "1899-12-30")
       (:|DeliveryDate| "2009-03-31")
       (:|Strike| "0")
       (:|PutOrCall| "Put") 
       :|PriceOffMarket|
       :|ServiceName|
       :|RecordName|
       (:|CacheDelay| "0")
       :|PriceType|
       (:|Tolerance| "0")
       (:|Price| "0")
       (:|LastPriceUpdate| "2009-03-31")
       (:|PriceSourceFlag| "0")
       (:|FullPriceFactor| "0")
       (:|RtPrice| "0")
       (:|Status| "0")
       (:|LastRtPriceUpdate| "1899-12-30")))))

(defpackage #:ics
  (:use :cl :clsql-user :s-xml))

(in-package :ics)
(sb-mop:class-slots (find-class 'database))
(connect '("localhost" "clsql-test" "oli" "qwer")
	 :database-type :postgresql)
