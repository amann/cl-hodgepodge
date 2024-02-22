(in-package #:adaptiv)
(define-stored-procedure browse-all-currencies () :stored-name "BrowseAllCurrencies;1" :gives-results t :catalog :mfiles)
(define-stored-procedure get-all-currencies () :stored-name "GetAllCurrencies;1" :gives-results t :catalog :mfiles)
(define-stored-procedure del-currency (currency-id) :stored-name "Del_CURRENCY;1" :catalog :mfiles)
(define-stored-procedure ins-currency (@CurrencyID
                                       &key
                                       @HolidayCalendarID
                                       (@Description "")
                                       (@UnitsDescription "")
                                       (@UnitsPrecision 2)
                                       (@ISOSwiftName @CurrencyID)
                                       (@Symbol "")
                                       (@MinVal 0.01)
                                       (@SwiftMinVal 0.0)
                                       (@MinDisp 0.0)
                                       (@IgnoreNonSpotHolidays 0)
                                       (@Owner "Common")
                                       (@Pmt_rndg_code "S")
                                       (@Doc_Lead_days 0)
                                       (@Day_count "Act/360")
                                       @ApprovalStatus)
  :stored-name "Ins_CURRENCY;1" :catalog :mfiles)
(define-stored-procedure upd-currency (@CurrencyID
                                       &key
                                       @HolidayCalendarID
                                       (@Description "")
                                       (@UnitsDescription "")
                                       (@UnitsPrecision 2)
                                       (@ISOSwiftName @CurrencyID)
                                       (@Symbol "")
                                       (@MinVal 0.01)
                                       (@SwiftMinVal 0.0)
                                       (@MinDisp 0.0)
                                       (@IgnoreNonSpotHolidays 0)
                                       (@Owner "Common")
                                       (@Pmt_rndg_code "S")
                                       (@Doc_Lead_days 0)
                                       (@Day_count "Act/360")
                                       @ApprovalStatus)
  :stored-name "Upd_CURRENCY;1" :catalog :mfiles)
(define-stored-procedure ins-rdm-country (@CountryID
                                          @CountryDesc
                                          @CountryCcy
                                          &key
                                          @CountrySwiftCode
                                          (@CountryClearing 0)
                                          @Owner
                                          (@EntryOrigin "Client")
                                          @ApprovalStatus)
  :stored-name "Ins_RDM_COUNTRY;1" :gives-results t :catalog :mfiles)
(defun get-currencies-from-blueprint ()
  (query "select code, description from blueprint.currency" :database (connect-to :blueprint) :flatp t))
(defconstant +currencies+ '(:CHF :EUR :GBP :AUD :DKK :JPY :NZD :SEK
                            :USD :CAD :CZK :HUF :ISK :NOK :PLN :BRL
                            :CNY :HKD :INR :ILS :MYR :MXN :RUB :SGD
                            :ZAR :KRW :TWD :TRY ))
(defun write-currencies-to-mfiles ()
  (let ((bp-currencies (get-currencies-from-blueprint)))
    (dolist (cur +currencies+)
      (destructuring-bind (cur-id descr) (assoc cur bp-currencies :test #'(lambda (a b) (eq a (intern b :keyword))))
        (destructuring-bind (country-code units-descr symbol &optional dcm) (get-currency cur-id)
          (ignore-errors
            (ins-currency cur-id
                          :@Description (format nil "~A, ~A" (get-country-name-from-code country-code) descr)
                          :@UnitsDescription units-descr
                          :@Symbol symbol
                          :@Day_count dcm)))))))
(defun write-countries-to-mfiles ()
  (dolist (cur-id (mapcar #'car (browse-all-currencies)))
    (let* ((country-code (get-country-code-from-currency-code cur-id))
           (country-name (get-country-name-from-code country-code)))
      (ins-rdm-country (string-upcase country-name) country-name cur-id :@countryswiftcode (string-upcase country-code)))))

;;;; Market

(define-stored-procedure ins-market (@CurrencyID
                                     @MarketID
                                     &key
                                     (@Description "")
                                     (@Type "Bth")
                                     @Owner
                                     @ApprovalStatus)
  :stored-name "Ins_MARKET;1" :catalog :mfiles)
(define-stored-procedure upd-market (@CurrencyID
                                     @MarketID
                                     &key
                                     (@Description "")
                                     (@Type "Bth")
                                     @Owner
                                     @ApprovalStatus)
  :stored-name "Upd_MARKET;1" :catalog :mfiles)
(defun write-markets-to-mfiles ()
  (dolist (cur-id (mapcar #'car (browse-all-currencies)))
    (ins-market cur-id "MARKET" :@description (format nil "~A, ~A"
                                                      (get-country-name-from-code (get-country-code-from-currency-code cur-id))
                                                      (get-currency-name-from-code cur-id)))))
;;;; Reference Index
(define-stored-procedure ins-rdm-refidxattr  (@IndexID
                                              @CurrencyID
                                              &key
                                              (@ObservationFreq "DAILY")
                                              (@Description "")
                                              (@BOContractCode "")
                                              @Holiday1           
                                              @Holiday2           
                                              @Holiday3           
                                              @Holiday4           
                                              (@SpotOffset 0)     
                                              (@CalcPrdBusDayConv "MBw")
                                              (@Basis "Act/360")
                                              (@SpotOr1stOfMonth "No")
                                              (@QuoteMethod "Par")    
                                              @QuoteDate              
                                              (@MarketID "MARKET")
                                              @PublicationTime    
                                              @PublicationTimeZone
                                              @FloatingRateOption 
                                              @ApprovalStatus)    
  :stored-name "Ins_RDM_REFIDXATTR;1" :catalog :mfiles)
(define-stored-procedure ins-rdm-refidxdflt (@IndexID
                                             @CurrencyID
                                             &key
                                             (@DateAdjMethod "MBw")
                                             (@PaymentOffset 0)
                                             (@PayUpfrontOrArrears "Arrears")
                                             (@ResetRateRound "ISDA")
                                             (@ResetUpfrontOrArrs "Advance")
                                             (@CompoundingMethod "None")
                                             (@SourceRndPrecision 0)
                                             (@SourceRndMethod "None")
                                             (@AverageRndPrecision 0)
                                             (@AverageRndMethod "None")
                                             (@FinalRateRndPrecision 0)
                                             (@FinalRateRndMethod "None")
                                             (@CashflowRndPrecision 0)
                                             (@CashflowRndMethod "None")
                                             (@Cutoff "false")
                                             (@CutoffDays 0))
  :stored-name "Ins_RDM_REFIDXDFLT;1" :catalog :mfiles)
(define-stored-procedure ins-rdm-refidxsrc (@IndexID
                                            @CurrencyID
                                            @DesignatedMaturity ;(or "1m" "3m" "6m" "12m")
                                            &key
                                            (@IndexSource "PANORAMA")
                                            (@PanoramFloatIdx (concatenate 'string (string-upcase @CurrencyID) (string-upcase @DesignatedMaturity)))
                                            (@BOContractCode "")
                                            (@PaymentFreq @DesignatedMaturity)
                                            (@CompoundingFreq @PaymentFreq)
                                            (@ResetFreq @DesignatedMaturity)
                                            (@ResetTerm @ResetFreq)
                                            (@CCTMethod "Non")
                                            (@CCTType "Non")
                                            (@CCTObs 0)
                                            (@IndexType "Flt")
                                            (@Interpolate "true")
                                            (@ExternalReference "")
                                            @ApprovalStatus)
  :stored-name "Ins_RDM_REFIDXSRC;1" :catalog :mfiles)


(defun write-rdm-refidx (&optional currency-list)
  (dolist (currency-id (or currency-list
                           (mapcar #'(lambda (x) (intern (car x) :keyword))
                                   (browse-all-currencies))))
    (let ((index-id '#:libor)
          (basis "Act/360"))
      (case currency-id
        ((:gbp :aud :cad :jpy :sgd) (setq basis "A365Fix"))
        (:hkd (setq basis "Act/Act"))
        (:eur (setq index-id '#:euribor)))
      (let ((index-id (symbol-name index-id))
            (currency-id (symbol-name currency-id)))
        (ins-rdm-refidxattr index-id currency-id
                            :@description (format nil "~A ~A" index-id (get-currency-name-from-code currency-id))
                            :@basis basis)
        (ins-rdm-refidxdflt index-id currency-id)
        (dolist (designated-maturity '("12m" "6m" "3m" "1m" "1d"))
          (ins-rdm-refidxsrc index-id currency-id designated-maturity))))))


;;;; Counterparties
(defun get-ratings-from-blueprint ()
  (query "select overallratingcode from blueprint.dm_aexp_md_rating_map" :database (connect-to :blueprint) :flatp t))
(define-stored-procedure ins-entity (@EntityID
                                     &key
                                     @DefaultCurrencyID
                                     @RevalCurrencyID
                                     (@Description                "")
                                     (@Type                       "COUNTERPARTY")
                                     (@ExternalID                 "")
                                     (@SwiftBankCode              "")
                                     (@SwiftBICCode               "")
                                     (@SwiftRole                  "")
                                     (@FSAMask                    "")
                                     @FSADate
                                     (@CalcAgent                  "")
                                     (@Domicile                   0.0)
                                     (@FedBIS                     0.0)
                                     @MasterDate
                                     (@AccNumber                  "")
                                     (@Miscellaneous              "")
                                     (@MultiStatement             0)
                                     (@PhysPLStatement            0)
                                     (@BrokerStatement            0)
                                     (@CreateUnderOpt             0)
                                     @Cat1Code
                                     @Cat2Code
                                     @Cat3Code
                                     @Cat4Code
                                     @LocationID
                                     @LegalCodeVers
                                     @CountryID    
                                     @DomesticCurrency
                                     (@LongID                     @EntityID)
                                     (@BrokeragePeriod            0)
                                     (@EntityNumber               "")
                                     (@AutoSprdSame               0)
                                     (@AutoSprdDiff               0)
                                     (@AutoNetOut                 0)
                                     (@SgrdResv                   0)
                                     (@ConfClassCode              "")
                                     (@HypValFlag                 0)
                                     (@HypVal                     0)
                                     (@BnkDepRole                 0)
                                     (@IssuerRole                 0)
                                     (@BranchRole                 0)
                                     (@TaxAgentRole               0)
                                     @Category5
                                     @Category6
                                     @Category7
                                     @Category8
                                     @Category9
                                     @Category10
                                     @Category11
                                     @Category12
                                     @Category13
                                     @Category14
                                     @Category15
                                     @Category16
                                     @Category17
                                     @Category18
                                     @Category19
                                     (@ReHypoAccept               0)
                                     (@ReHypoGrant                0)
                                     (@ISOBIC                     "")
                                     (@BISExpLimit                0.0)
                                     (@BISJurd                    0)
                                     (@NGRBasis                   0)
                                     (@IncFX                      0)
                                     (@Owner                      "")
                                     (@CustodianRole              0)
                                     (@Inf_PBE                    0)
                                     (@Inf_OrgLegality            1)
                                     @Inf_ParentID
                                     @Inf_AcctArea
                                     @Inf_TradArea
                                     @Inf_Root
                                     (@InfProcOrg                 0)
                                     (@InfLanguage                "")
                                     (@InfBusinessType            "")
                                     @InfDomicile
                                     (@InfTimeZone                "")
                                     (@InfDayLightSavings         "")
                                     (@InfGLMSCode                "")
                                     (@InfUENnumber               "")
                                     (@TradingRoleCpty            1)
                                     (@TradingRoleClient          0)
                                     (@TradingRoleBroker          0)
                                     (@TradingRoleFlrBroker       0)
                                     (@TradingRoleExchange        0)
                                     (@TradingRoleInternalCpty    0)
                                     (@TradingRoleNonTrading      0)
                                     @CountryOfRisk
                                     @ApprovalStatus)
  :stored-name "Ins_ENTITY;1" :catalog :mfiles)

(defun write-counterparties (extra-counterparties)
  (dolist (cp (append (get-ratings-from-blueprint) extra-counterparties))
    (ins-entity cp)))

;;;; AccountingArea
(defconstant +accounting-areas+ '(:BE :CH :DE :FR :LU :NL :LI))
(define-stored-procedure ins-accountingarea (@AccountingAreaID
                                             &optional
                                             @Description
                                             &key
                                             @Owner
                                             @ApprovalStatus)
  :stored-name "Ins_ACCOUNTINGAREA;1" :catalog :mfiles)
(defun write-accountingarea ()
  (dolist (accarea +accounting-areas+)
    (ins-accountingarea (symbol-name accarea) (get-country-name-from-code accarea))))


;;;; AccountingGroup

(defun get-orgunit-from-blueprint ()
  "Select CODE, DESCRIPTION from BLUEPRINT.ORGUNIT"
  (query "select code, description from blueprint.orgunit" :database (connect-to :blueprint) :flatp t))


(define-stored-procedure ins-accountinggroup (@GroupID
                                              &optional
                                              @Hdesc
                                              &key
                                              @Owner
                                              @Hidate              
                                              @Hcdate              
                                              (@Hexdesc              "")
                                              (@Hexcecode            "")
                                              (@Hexls                0)
                                              @Hexincep            
                                              @Hexmdate            
                                              @Hacctarea           
                                              (@Hexqty               0.0)
                                              (@Hexstrike            0.0)
                                              (@Hexputcall           0)
                                              (@Hextenor             "")
                                              (@Hrisk                0)
                                              @Htemplate           
                                              (@Hexrate              0.0)
                                              @Hexmonth
                                              @ApprovalStatus)
  :stored-name "Ins_ACCOUNTINGGROUP;1" :catalog :mfiles)



(defun write-accountinggroup ()
  "Write the orgunits from BluePrint as AccountingGroups into the MFiles."
  (dolist (accgr (get-orgunit-from-blueprint))
    (ins-accountinggroup (first accgr) (second accgr))))



;;;; MDO

;;;; BondMatrix

(define-stored-procedure WriteBondIssueMatrixPoint (@BondIssueMatrixID
                                                    @IssueID
                                                    &key
                                                    @ActivePrice
                                                    @ActiveYield
                                                    (@ActiveType           "B")
                                                    (@ActiveFreq           "12m")
                                                    (@ActiveTax            "No")
                                                    (@ActiveCleanDirty     "Cln")
                                                    (@ActiveYieldMethod    "ISMA")
                                                    (@ActiveRiskFactor     "Prc")
                                                    (@RecalcMethod         "Spc")
                                                    @YcvSpread
                                                    (@YcvFrequency         "12m")
                                                    @BenchmarkID
                                                    @BmkSpread
                                                    (@BmkFrequency         "12m")
                                                    @BondFutureID
                                                    @Delivery
                                                    (@GrossBasis           0.0)
                                                    (@NetBasis             0.0)
                                                    (@RepoRateMethod       "Par")
                                                    (@RepoRate             0.0)
                                                    @RepoSpread
                                                    (@HedgeType            "BFt")
                                                    @HedgeIssueContract
                                                    (@HedgeMethod          "Bpv")
                                                    @TickerID
                                                    (@TickerType           "Prc")
                                                    (@RTFEnabled           "No")
                                                    @LastUpdate
                                                    @IssueSpecific
                                                    (@RepoYieldCurveMethod "Par")
                                                    @CreditSpread
                                                    (@CBEquityVol          0.0)
                                                    (@OAS                  0.0))
  :stored-name "WriteBondIssueMatrixPoint;1" :catalog :adaptiv :gives-results t)



(define-stored-procedure pub-BondSetPriceCst (@BondMatrixID
                                              @IssueID
                                              @BondPrice)
  :stored-name "pub_BondSetPriceCst;1" :catalog :adaptiv :gives-results t)



;;;; Equity Market Parameters

(define-stored-procedure pub-equitycreatemarketparam (@IssueID @ParamSet @PricingRule)
  :stored-name "pub_EquityCreateMarketParam;1" :catalog :adaptiv :gives-results t)
(define-stored-procedure pub-equitysetprice (@IssueID @Price @ParamSet)
  :stored-name "pub_EquitySetPrice;1" :catalog :adaptiv :gives-results t)


;;;;================================================================================================

(define-stored-procedure sp-deletesecuritydefs (@Object
                                                @ObjectKey)
  :stored-name "sp_DeleteSecurityDefs;1" :catalog :adaptiv)
(define-stored-procedure sp-writesecuritydefs (@Object
                                               @ObjectKey
                                               &key
                                               (@Owner "MGR")
                                               (@Permissions 0)
                                               (@Audit "No"))
  :stored-name "sp_WriteSecurityDefs;1" :catalog :adaptiv)
(define-stored-procedure volcurve-delete (@ID
                                          @Currency)
  :stored-name "VolCurve_Delete;1" :catalog :adaptiv)

;;;;------------------------------------------------------------------------------------------------
;;;; Volas
(define-stored-procedure volcurve-insert (@ID
                                          @Currency
                                          &key
                                          (@Description "")
                                          (@RefDate "2009-06-29 00:00:00.0")
                                          (@LastModDate "2009-06-29 00:01:00.0")
                                          (@LastRTUpdateDate "2009-06-29 00:01:00.0")
                                          @Tenor
                                          (@UsingCaps "Yes")
                                          (@NumFutPoints 0)
                                          (@YieldCurveID "YCURVE")
                                          (@RTEnabled "No")
                                          (@CapIndexID (format nil "~A~A" @Currency (string-upcase @Tenor)))
                                          (@CapOffset 2)
                                          (@CapBasis "Act/360")
                                          (@CapAdjUnadj "A")
                                          (@CapAdjMeth "MFw")
                                          (@CapHol1 "SWX")
                                          (@CapHol2 "")
                                          (@FutOptContractID "")
                                          (@CreatorID "")
                                          (@AccessRights "")
                                          (@IsAudited 0)
                                          (@FreezeFlags 4)
                                          (@ATMCol 1)
                                          (@CapParVolAsRiskFactor "No")
                                          (@VolSurfaceUpdatePremia "No"))
  :stored-name "VolCurve_Insert;1" :catalog :adaptiv)
(define-stored-procedure volcurvepoint-insert (@ID
                                               @Currency
                                               @PointNumber
                                               &key
                                               (@Product "Cap")
                                               @Term
                                               (@FutConMatDate "2071-01-01 00:00:00.0")
                                               (@ParVol 10)
                                               (@Premium 1)
                                               (@ImpliedVol 0)
                                               (@UserInputVol 0)
                                               (@Strike 2)
                                               (@OptnType "Cap")
                                               (@LastTradingOffset 0)
                                               (@PutOrCall "Cal")
                                               (@FuturePrice 0)
                                               (@ParOrDiscount "Par")
                                               (@PointOn "Yes")
                                               (@ParVolRTFSourceID "")
                                               (@PremiumRTFSourceID "")
                                               (@StrikeRTFSourceID "")
                                               (@FuturesPriceRTFSourceID "")
                                               (@RTEnabled "No")
                                               (@StrikeSpread 0)
                                               (@ParVolSpread 0))
  :stored-name "VolCurvePoint_Insert;1" :catalog :adaptiv)
(define-stored-procedure sp-savevolcurvestroptions (@VolCurveID
                                                    @Currency
                                                    &key
                                                    (@IsConfigured 0)
                                                    (@MinimumStrike 0)
                                                    (@MaximumStrike 0)
                                                    (@BasisPointIncrement 0))
  :stored-name "sp_SaveVolCurveStrOptions;1" :catalog :adaptiv)
(define-stored-procedure sp-deletevolcurvestroptions (@VolCurveID
                                                      @Currency)
  :stored-name "sp_DeleteVolCurveStrOptions;1" :catalog :adaptiv)
(defun write-volcurve (&optional currency-list tenor-list)
  (dolist (currency-id (mapcar #'string-upcase (or currency-list
                                                   (mapcar #'car (browse-all-currencies)))))
    (dolist (tenor (or tenor-list '("1m" "3m" "6m" "12m")))
      (let ((id (format nil "CP~A~A" currency-id (string-upcase tenor)))
            (point-nr -1))
        (volcurve-insert id currency-id :@description "Cap Volatility" :@tenor tenor)
        (dolist (term '("1y" "2y" "3y" "4y" "5y" "7y" "10y"))
          (volcurvepoint-insert id currency-id (incf point-nr) :@term term))
        (sp-writesecuritydefs "VolCurve" (format nil "~A|~A" id currency-id))
        (sp-savevolcurvestroptions id currency-id)))))

(defun delete-volcurve (&optional currency-list tenor-list)
  (dolist (currency-id (or currency-list
                           (mapcar #'(lambda (x) (intern (car x) :keyword))
                                   (browse-all-currencies))))
    (dolist (tenor (or tenor-list '("1m" "3m" "6m" "12m")))
      (let ((id (format nil "CP~A~A" currency-id (string-upcase tenor))))
        (sp-deletevolcurvestroptions id currency-id)
        (sp-deletesecuritydefs "VolCurve" (format nil "~A|~A" id currency-id))
        (volcurve-delete id currency-id)))))


;;;;------------------------------------------------------------------------------------------------
;;;; Yieldcurve


(define-stored-procedure sp-deleteyieldcurve (@ID @Currency)
  :stored-name "sp_DeleteYieldCurve;1" :catalog :adaptiv)
(define-stored-procedure yieldcurveisindb (@ID @Currency)
  :stored-name "YieldCurveIsInDB;1" :catalog :adaptiv :gives-results t)
(define-stored-procedure sp-insertintoyieldcurve (@ID
                                                  @Currency
                                                  &optional
                                                  (@Description "")
                                                  &key
                                                  (@RefDate "2009-06-29 00:00:00.0")
                                                  (@CurveType "Int")
                                                  (@LastModDate "2009-06-29 00:00:00.0")
                                                  (@LastRTUpdateDate "2009-06-29 00:00:00.0")
                                                  (@RTEnabled "No")
                                                  (@FutConID "")
                                                  (@CreatorID "")
                                                  (@AccessRights "")
                                                  (@IsSpreadCurve "No")
                                                  (@UnderlyingCurrency "")
                                                  (@UnderlyingID "")
                                                  (@IsAudited 0)
                                                  (@NumSerial 0)
                                                  (@VARUseZeroCoupon 1)
                                                  (@VARZeroCouponGridID "ZERO")
                                                  (@VARZeroCouponBasis "Act/360")
                                                  (@VARZeroCouponFreq "12m")
                                                  (@ImpliedFromFX 0)
                                                  (@FXGridID "")
                                                  (@RTRollEnabled "No")
                                                  (@Flags 0))
  :stored-name "sp_InsertIntoYieldCurve;1" :catalog :adaptiv)
(defun get-swp-pay-freq (cur)
  (case cur
    (:hkd "3m")
    ((:usd :gbp :jpy) "6m")
    (t "12m")))
(defun get-swp-base (cur)
  (case cur
    ((:gbp :hkd :jpy :nzd :aud :cad :huf :pln :inr :ils :myr :sgd :zar) "A365Fix")
    ((:czk :isk :cny :mxn :try) "Act/360")
    (:rub :brl "Act/Act")
    (:usd "30/360B")
    (t "30E/360")))
(defun get-csh-base (cur)
  (case cur
    ((:gbp :hkd :sgd :myr :ils :pln) "A365Fix")
    (:rub "Act/Act")
    (t "Act/360")))
(defun get-swp-float-idx (cur)
  (format nil "~A~A" cur (case cur
                           (:usd "3M")
                           (t "6M"))))
(define-stored-procedure sp-insertintoyieldcurvepoint (@ID
                                                       @Currency
                                                       @PointNumber
                                                       &key
                                                       (@UsePointInHedge "Yes")
                                                       (@PointOn "Yes")
                                                       (@RTEnabled "No")
                                                       (@RTSourceID "")
                                                       (@RateInputFormat "M")
                                                       (@Rate1 1.0)
                                                       (@Rate2 0.0)
                                                       (@RateInc 1.0)
                                                       (@DefaultSpreadAmount 1.0)
                                                       (@Spread1 0.0)
                                                       (@Spread2 0.0)
                                                       (@SpreadFormat "N")
                                                       (@Term "1d")
                                                       (@PaymentFreq (get-swp-pay-freq (setq @Currency (intern (string-upcase @Currency) :keyword))))
                                                       (@Product (if  (member @Term '("1d" "1m" "3m" "6m") :test #'string=)
                                                                      "Csh"
                                                                      "Swp"))
                                                       (@CalcBasis (if (string= @Product "Csh")
                                                                       (get-csh-base @Currency)
                                                                       (get-swp-base @Currency)))
                                                       (@TermToFRAStart "")
                                                       (@FutConMatDate "1971-01-01 00:00:00.0")
                                                       (@SpotOffset 0)
                                                       (@DefaultIntBasis @CalcBasis)
                                                       (@AdjUnadj "A")
                                                       (@DateAdjustmentMethod (if (string= @Term "1d")
                                                                                  "Fwd"
                                                                                  "MFw"))
                                                       (@FincCentre1 "")
                                                       (@FincCentre2 "")
                                                       (@SettlementOffset 0)
                                                       (@IssueDate "2009-06-29 00:00:00.0")
                                                       (@MaturityDate "2009-06-30 00:00:00.0")
                                                       (@CouponRate 0.0)
                                                       (@Sticky "No")
                                                       (@FutConSettlement "3We")
                                                       (@FutConFwdPrdStart "Stl")
                                                       (@IndexID "")
                                                       (@ParDisc "Par")
                                                       (@BondPrice 0.0)
                                                       (@UnderlyingID "")
                                                       (@BidMidRTFSourceID "")
                                                       (@OfferRTFSourceID "")
                                                       (@BidSpreadRTFSourceID "")
                                                       (@OfferSpreadRTFSourceID "")
                                                       (@BondPriceRTFSourceID "")
                                                       (@BondFutPriceRTFSourceID ""))
  :stored-name "sp_InsertIntoYieldCurvePoint;1" :catalog :adaptiv
  :var-symbols (@ID @Currency @PointNumber @UsePointInHedge @PointOn @RTEnabled @RTSourceID @RateInputFormat @Rate1 @Rate2 @RateInc @DefaultSpreadAmount @Spread1 @Spread2 @SpreadFormat @CalcBasis @Product @TermToFRAStart @FutConMatDate @Term @SpotOffset @DefaultIntBasis @PaymentFreq @AdjUnadj @DateAdjustmentMethod @FincCentre1 @FincCentre2 @SettlementOffset @IssueDate @MaturityDate @CouponRate @Sticky @FutConSettlement @FutConFwdPrdStart @IndexID @ParDisc @BondPrice @UnderlyingID @BidMidRTFSourceID @OfferRTFSourceID @BidSpreadRTFSourceID @OfferSpreadRTFSourceID @BondPriceRTFSourceID @BondFutPriceRTFSourceID))
(define-stored-procedure sp-insertintoztablepoint (@ID
                                                   @Currency
                                                   @PointNumber
                                                   &key
                                                   (@Day 0)
                                                   (@Inst (if (< @Day 365) "Csh" "Swp"))
                                                   (@ImpliedDF 1)
                                                   (@SpecifiedDF 0.0)
                                                   (@PointOn "Yes")
                                                   (@SystemPoint "Yes"))
  :stored-name "sp_InsertIntoZTablePoint;1" :catalog :adaptiv
  :var-symbols (@ID @Currency @PointNumber @Inst @Day @ImpliedDF @SpecifiedDF @PointOn @SystemPoint))

(defun parse-duration (string)
  (let ((count (parse-integer string :junk-allowed t))
        (unit (intern (string-upcase (string-trim '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\Space) string)) :keyword)))
    (make-duration (case unit
                     (:d :day)
                     (:m :month)
                     (:y :year))
                   count)))

(defun write-yieldcurve (currency-id term-list &key (id :ycurve))
  (let ((point-nr -1))
    (sp-deleteyieldcurve id currency-id)
    (sp-insertintoyieldcurve id currency-id (format nil "~A swap curve" currency-id))
    (dolist (term term-list)
      (sp-insertintoyieldcurvepoint id currency-id (incf point-nr) :@term term))
    (sp-writesecuritydefs "YieldCurve" (format nil "~A|~A" id currency-id))))

(defun write-yieldcurves (&optional &key (curr-list (mapcar #'car (browse-all-currencies)))
                          (term-list '("1d" "3m" "6m" "1y" "2y" "3y" "4y" "5y" "6y" "7y" "8y" "9y" "10y" "12y" "15y" "20y" "25y" "30y")))
  (dolist (cur curr-list)
    (write-yieldcurve cur  (case (intern (string-upcase cur) :keyword)
                               (:nzd (remove-if (lambda (x) (member x '("8y" "9y" "30y") :test #'string=)) term-list))
                               ((:czk :pln) (remove-if (lambda (x) (member x '("25y" "30y") :test #'string=)) term-list))
                               (:nok (remove-if (lambda (x) (member x '("1d" "25y" "30y") :test #'string=)) term-list))
                               (:huf (remove-if (lambda (x) (member x '("15y" "20y" "25y" "30y") :test #'string=)) term-list))
                               (:isk (remove-if (lambda (x) (member x '("6y" "8y" "9y" "12y" "15y" "20y" "25y" "30y")
                                                                    :test #'string=)) term-list))
                               ((:brl :cny :hkd :inr :ils :myr :mxn :rub :sgd :zar :krw :twd :try) '("1y"))
                               (t term-list)))))


;;;; Swaption volatilities
(defun trim-blanks (string)
  (string-trim '(#\Space #\Tab #\Newline) string))
(let ((grid-cache (make-hash-table)))
  (defun get-grid-terms (grid-id)
    (let ((grid-id (intern (string-upcase grid-id) :keyword)))
      (or (gethash grid-id grid-cache)
          (setf (gethash grid-id grid-cache)
                (apply #'vector (mapcar #'trim-blanks
                                        (query (format nil "select Term from GridTerms where GridID = ~A order by TermNumber" (to-sql grid-id))
                                               :database (connect-to :adaptiv) :flatp t :field-names nil))))))))
(defun get-grid-term (grid-id nbr)
  (let ((terms (get-grid-terms grid-id)))
    (aref terms (mod nbr (length terms)))))

(define-stored-procedure swapvolsurface-delete (@ID @Currency)
  :stored-name "SwapVolSurface_Delete;1" :catalog :adaptiv)
(define-stored-procedure swapvolsurface-write (@Currency
                                               &key
                                               (@ID (progn (setq @Currency (intern (string-upcase @Currency) :keyword))
                                                      (format nil "~ASWPN" @Currency)))
                                               (@Description (format nil "~A swaption volatility" @Currency))
                                               (@ExerciseGridID "SLSWPN")
                                               (@NumberOfPoints (if (member @Currency '(:chf :eur :usd))
                                                                    1
                                                                    1))
                                               (@DisplayStrikeSpread 0)
                                               (@MinStrike 2.0)
                                               (@MaxStrike 5.0)
                                               (@StrikeIncrement (if (= 1 @NumberOfPoints)
                                                                     0.0
                                                                     1.0))
                                               (@AtmTnrGridID "SLTENOR")
                                               (@AtmExGridID (if (string= @Currency "USD") "SLSWPUSD" "SLSWPN"))
                                               (@GridType "Trm")
                                               (@FixedPosition "Pay")
                                               (@FixedFrequency (get-swp-pay-freq @Currency))
                                               (@FixedAdjMethod "MFw")
                                               (@FixedInterestBasis (get-swp-base @Currency))
                                               (@FixedFinc1 "SWX")
                                               (@FixedFinc2 "")
                                               (@FixedFinc3 "")
                                               (@FixedFinc4 "")
                                               (@FloatingIndex (get-swp-float-idx @Currency))
                                               (@FreezeStrikes (if (= 1 @NumberOfPoints)
                                                                   0
                                                                   -1))
                                               (@FreezeVols 0)
                                               (@AtmColumnNumber (if (= 1 @NumberOfPoints)
                                                                     1
                                                                     5))
                                               (@ReferenceMarketID "MARKET")
                                               (@DiscountMarketID "MARKET")
                                               (@PricingRuleID "SL")
                                               (@SwaptionInterfaceName "Panorama")
                                               (@StrikeInterpInterfaceName "Cubic Spline")
                                               (@UseCalcATM -1)
                                               (@VolSurfaceUpdatePremia 0))
  :stored-name "SwapVolSurface_Write;1" :catalog :adaptiv
  :var-symbols (@ID @Currency @Description @ExerciseGridID @NumberOfPoints @DisplayStrikeSpread @MinStrike @MaxStrike @StrikeIncrement @AtmTnrGridID @AtmExGridID @GridType @FixedPosition @FixedFrequency @FixedAdjMethod @FixedInterestBasis @FixedFinc1 @FixedFinc2 @FixedFinc3 @FixedFinc4 @FloatingIndex @FreezeStrikes @FreezeVols @AtmColumnNumber @ReferenceMarketID @DiscountMarketID @PricingRuleID @SwaptionInterfaceName @StrikeInterpInterfaceName @UseCalcATM @VolSurfaceUpdatePremia))

(define-stored-procedure swapvolsurfacepoints-delete (@CurrencyID @SurfaceID)
  :stored-name "SwapVolSurfacePoints_Delete;1" :catalog :adaptiv)
(define-stored-procedure swapvolsurfacepoint-write (@CurrencyID
                                                    @TenorNumber
                                                    @ExerciseTermNumber
                                                    @StrikeNumber
                                                    &key
                                                    (@SurfaceID (progn (setq @CurrencyID (intern (string-upcase @CurrencyID) :keyword))
                                                                       (format nil "~ASWPN" @CurrencyID)))
                                                    (@Term (get-grid-term "SLTENOR" @TenorNumber))
                                                    (@ExerciseTerm (get-grid-term "SLSWPN" @ExerciseTermNumber))
                                                    (@Volatility 10.0)
                                                    (@Strike 2.0)
                                                    (@Premium 0.0)
                                                    (@StrikeSpread (aref #(-2 -1 -0.5 -0.25 0 0.25 0.5 1 2) (+ 4 @StrikeNumber)))
                                                    (@VolatilitySpread 0.0)
                                                    (@RTFOn "No")
                                                    @RTFVolSource
                                                    @RTFPremSource)
  :stored-name "SwapVolSurfacePoint_Write;1" :catalog :adaptiv
  :var-symbols (@CurrencyID @SurfaceID @Term @ExerciseTerm @StrikeNumber @Volatility @Strike @Premium @StrikeSpread @VolatilitySpread @RTFOn @RTFVolSource @RTFPremSource))

(defun write-swaption-volatility-surfaces (&key (curr-list (mapcar #'car (browse-all-currencies))))
  (dolist (currency-id curr-list)
    (let ((id (format nil "~ASWPN" currency-id)))
      (swapvolsurface-delete id currency-id)
      (swapvolsurface-write currency-id)
      (dotimes (tenor-number (length (get-grid-terms "SLTENOR")))
        (dotimes (exerc-number (length  (get-grid-terms (if (string= currency-id "USD") "SLSWPUSD" "SLSWPN"))))
          (swapvolsurfacepoint-write currency-id tenor-number exerc-number 0)))
      (sp-writesecuritydefs "SwaptionVolSurface" (format nil "~A|~A" currency-id id)))))
;;;; Spread curve

(define-stored-procedure spreadcurve-delete (@SpreadCurveName)
  :stored-name "SpreadCurve_Delete;1" :catalog :adaptiv)
(define-stored-procedure spreadcurve-get (@SpreadCurveName)
  :stored-name "SpreadCurve_Get;1" :catalog :adaptiv :gives-results t)

(define-stored-procedure spreadcurve-set (@SpreadCurveName
                                          &key
                                          (@Description @SpreadCurveName)
                                          (@GridID "ZERO")
                                          (@SubtractSpread 0))
  :stored-name "SpreadCurve_Set;1" :catalog :adaptiv)
(define-stored-procedure spreadcurve-update (@SpreadCurveName
                                             &key
                                             (@Description @SpreadCurveName)
                                             (@GridID "ZERO")
                                             (@SubtractSpread 0))
  :stored-name "SpreadCurve_Update;1" :catalog :adaptiv)
(define-stored-procedure spreadcurvepoints-delete (@SpreadCurveID
                                                   @GridID)
  :stored-name "SpreadCurvePoints_Delete;1" :catalog :adaptiv)
(define-stored-procedure spreadcurvepoints-set (@SpreadCurveID
                                                @GridID
                                                @TermNumber
                                                &key
                                                (@Spread 0.0)
                                                (@TickerID "")
                                                (@RTFOn "No"))
  :stored-name "SpreadCurvePoints_Set;1" :catalog :adaptiv)

(defun write-spreadcurves (&key
                           (curr-list (mapcar #'car (browse-all-currencies)))
                           (spread-types (mapcar #'trim-blanks (query "select EntityID from ENTITY where Type = 'COUNTERPARTY'"
                                                                      :database (connect-to :mfiles) :flatp t :field-names nil)))
                           (grid-id "ZERO"))
  (dolist (currency-id curr-list)
    (dolist (spread-type spread-types)
      (let ((curve-id (format nil "~A~A" currency-id spread-type)))
        (spreadcurve-delete curve-id)
        (spreadcurve-set curve-id :@gridid grid-id)
        (dotimes (term-number (length (get-grid-terms grid-id)))
          (spreadcurvepoints-set curve-id grid-id term-number))
        (sp-writesecuritydefs "SpreadCurve" curve-id)))))

;;;; Spread curve config
(define-stored-procedure spreadcurveconfig-main-browse ()
  :stored-name "SpreadCurveConfig_Main_Browse;1" :catalog :adaptiv :gives-results t)
(define-stored-procedure spreadcurveconfig-main-read (@SpreadCurveConfigKey)
  :stored-name "SpreadCurveConfig_Main_Read;1" :catalog :adaptiv :gives-results t)
(define-stored-procedure spreadcurveconfig-main-create (@SpreadCurveConfigKey
                                                        @Description
                                                        @GroupingType1
                                                        @GroupingType2
                                                        @GroupingType3
                                                        @GroupingType4
                                                        @HierarchicalGroupings)
  :stored-name "SpreadCurveConfig_Main_Create;1" :catalog :adaptiv)
(define-stored-procedure spreadcurveconfig-point-read (@SpreadCurveConfigID)
  :stored-name "SpreadCurveConfig_Point_Read;1" :catalog :adaptiv :gives-results t)
(define-stored-procedure spreadcurveconfig-point-create (@SpreadCurveConfigID
                                                         @GroupingID1
                                                         @GroupingID2
                                                         @GroupingID3
                                                         @GroupingID4
                                                         @SpreadCurveID
                                                         @UseFor)
  :stored-name "SpreadCurveConfig_Point_Create;1" :catalog :adaptiv)
(define-stored-procedure spreadcurveconfig-pr-create (@PricingRulesID
                                                      @DateChanged
                                                      @SpreadCurveConfigID)
  :stored-name "SpreadCurveConfig_PR_Create;1" :catalog :adaptiv)

(let ((uid->name (make-hash-table))
      (name->uid (make-hash-table))
      (uid->type-id (make-hash-table))
      (type-id->uids (make-hash-table)))
  (defun update-type-map ()
    (clrhash uid->name)
    (clrhash name->uid)
    (clrhash uid->type-id)
    (map nil #'(lambda (x)
                 (destructuring-bind (uid type-id name) x
                   (let ((name (intern (string-trim " " name) :keyword))
                         (uid (parse-integer uid :junk-allowed t)))
                     (setf (gethash uid uid->name) name
                           (gethash name name->uid) uid
                           (gethash uid uid->type-id) type-id)
                     (push uid (gethash type-id type-id->uids)))))
         (query "select * from TypeTable" :database (connect-to :adaptiv))))
  (defun name<-uid (uid)
    (gethash uid uid->name))
  (defun uid<-name (name)
    (gethash name name->uid))
  (defun type-id<-uid (uid)
    (gethash uid uid->type-id))
  (defun uids<-type-id (type-id)
    (gethash type-id type-id->uids)))
(defun get-type-id (type)
  1)

#+IGNORE
(defmacro write-spreadcurve-config (config-name (grp1 grp2 grp3 grp4 &key (hierarch-p t)) &body configs)
  (update-type-map)
  (let ((config-name config-name)
        (grp1 grp1)
        (grp2 grp2)
        (grp3 grp3)
        (grp4 grp4)
        (hierarch-p (if hierarch-p 1 0))
        (configs (if hierarch-p
                     (reduce #'append #'(lambda (conf)
                                         ())
                             :initial-value nil)
                     (mapcar #'(lambda (conf)
                                 (destructuring-bind ((grp1 grp2 grp3 grp4) spread-curve &optional (usage :both)) conf
                                   (list (uid<-name grp1)
                                         (uid<-name grp2)
                                         (uid<-name grp3)
                                         (uid<-name grp4)
                                         (uid<-name spread-curve)
                                         (string-capitalize usage))))
                             configs))))
    (format t "~A ~A ~A ~A ~A ~A~%~A" config-name grp1 grp2 grp3 grp4 hierarch-p configs)))



(let* ((alphabet #(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9 :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z))
       (size-alph (length alphabet))
       (code->int (let ((i -1)
                        (c->i (make-hash-table)))
                    (map nil #'(lambda (x)
                                 (setf (gethash x c->i) (incf i)))
                         alphabet)
                    c->i)))
  (defun base36<-base10 (real)
    (multiple-value-bind (m r) (floor real size-alph)
      (do* ((mult m)
            (rest (floor r))
            (code (list (elt alphabet rest)) (cons (elt alphabet rest) code)))
           ((zerop mult) code)
        (multiple-value-bind (m r) (floor mult size-alph)
          (setq mult m
                rest r)))))
  (defun base10<-base36 (code)
    (reduce #'(lambda (r c)
                (+ (* r size-alph) (gethash c code->int)))
            code
            :initial-value 0)))

#+IGNORE
(loop for a in '(((a1 sc1)
                  (b1
                   (c1 
                    (d1 sc1)
                    (d2 sc2))
                   ((c2 sc5)
                    (d2 sc4)
                    (d7 sc3)))
                  ((b2 sc8)))
                 ((a2 sc6)))
   append (loop with aux = (car a)
             when (consp aux) append (list (car aux) nil nil nil (cadr aux))))

;;;; Pricing rule

(defun pricerulevol-get (@ID @Curr @Mkt)
  (query (apply #'format nil "select ID,
DateChanged,
Curr,
Mkt,
VolType,
Freq,
Optn,
VolID,
VolBidOff,
CorrSetID,
CorrSetBidOff,
FwdFwdID,
MatrixID,
IRVolBootstrap,
IRVolInterpolation,
FlatVolOverride,
LIBORConvexityCorrection,
CMSConvexityCorrection,
RateModel,
PanTreeModel,
PanTreeNumberNodes,
PanTreeNumberDeviations,
PanTreePeriodSplitter,
IRVolExtrapolation
from dbo.PriceRuleVol
WHERE ~A = ID AND ~A = Curr AND ~A = Mkt
	AND DateChanged = (SELECT MAX(DateChanged)
						FROM dbo.PriceRuleVol WHERE ~3:* ~A = ID
						AND ~A = Curr
						AND ~A = Mkt)
order by Freq, Optn
" (map-values @ID @Curr @Mkt)) :database (connect-to :adaptiv)))
(define-stored-procedure pricerulevol-write (@CURR
                                             @FREQ
                                             @OPTN
                                             &key
                                             (@ID "SL")
                                             (@DATECHANGED (get-time))
                                             (@MKT "MARKET")
                                             (@VOLTYPE (format nil "~[Mtx~;Imp~]" (floor @Optn 2)))
                                             (@VOLID (format nil "~[CP~A~[12~;6~;3~;1~]M~;~]" (case @Optn
                                                                                                (1 1)
                                                                                                (t 0)) @Curr @Freq))
                                             (@VOLBIDOFF "M")
                                             (@CORRSETID "")
                                             (@CORRSETBIDOFF "M")
                                             (@FWDFWDID (format nil "~[LORGB~[A~;S~;Q~;M~]~:;~]" @Optn @Freq))
                                             (@MATRIXID (format nil "~[~ASWPN~:;~]" @Optn @Curr))
                                             (@IRVOLBOOTSTRAP "Slope")
                                             (@IRVOLINTERPOLATION "Spl")
                                             (@FLATVOLOVERRIDE "No")
                                             (@LIBORCONVEXITYCORRECTION 1)
                                             (@CMSCONVEXITYCORRECTION 1)
                                             (@RATEMODEL 1)
                                             (@PANTREEMODEL 1)
                                             (@PANTREENUMBERNODES 36)
                                             (@PANTREENUMBERDEVIATIONS 6)
                                             (@PANTREEPERIODSPLITTER 1)
                                             (@IRVolExtrapolation "Flt"))
  :stored-name "PriceRuleVol_Write;1" :catalog :adaptiv
  :var-symbols (@ID @DateChanged @Curr @Mkt @VolType @Freq @Optn @VolID @VolBidOff @CorrSetID @CorrSetBidOff @FwdFwdID @MatrixID @IRVolBootstrap @IRVolInterpolation @FlatVolOverride @LIBORConvexityCorrection @CMSConvexityCorrection @RateModel @PanTreeModel @PanTreeNumberNodes @PanTreeNumberDeviations @PanTreePeriodSplitter @IRVolExtrapolation))
(define-stored-procedure pricerulevol-delete (@ID)
  :stored-name "PriceRuleVol_Delete;1" :catalog :adaptiv)


(defun write-price-rule-vol (&optional (curr-list (mapcar #'car (browse-all-currencies))))
  (dolist (curr curr-list)
    (dotimes (freq 4)
      (dotimes (optn 4)
        (pricerulevol-write curr freq optn)))))




;;;; Banking instruments


(define-stored-procedure writebankinginstmapping (@BankingInstrument
                                                  @EquivalentProduct
                                                  @BankingInstCode)
  :stored-name "WriteBankingInstMapping;1" :catalog :adaptiv)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((generate-write-function ()
               (let ((mt->bic (let ((h-t (make-hash-table)))
                                (map nil #'(lambda (mt)
                                             (setf (gethash (intern (string-upcase (car mt)) :keyword) h-t) (cadr mt)))
                                     (query "select
    MAPPINGTYPE_CODE,
    SUBSTR(to_char(ID,'FM0000'), 2) as ID
from
    BLUEPRINT.DM_AEXP_MD_CATEGORIES_MAP" :database (connect-to :blueprint) :field-names nil))
                                h-t))
                     (mt->mtd (let ((h-t (make-hash-table)))
                                (map nil #'(lambda (mt)                                             
                                             (setf (gethash (intern (string-upcase (car mt)) :keyword) h-t) (cadr mt)))
                                     (query "select distinct
    MAPPINGTYPE, MAPPINGTYPE_DESC
from
    BLUEPRINT.DM_AEXP_POSITIONS" :database (connect-to :blueprint) :field-names nil))
                                h-t))
                     (eqp (let ((h-t (make-hash-table)))
                            (map nil #'(lambda (ep)
                                         (setf (gethash (intern (substitute #\- #\Space (string-upcase ep)) :keyword) h-t) ep))
                                 '("Benchmark" "Equity Future" "Equity Future Option" "Equity Option" "Equity Repo" "Excel" "Floor" "FRA" "FRN" "Fund" "Future" "Future Option" "FX" "FX Future" "FX Future Option" "FX Option" "Index Linked Bond" "MBS" "Multi Leg" "N-To-Default Swap" "Partial Reval Grouping" "Payment" "Reech" "Structured Note" "Structured Swap" "Swap" "Swap Future" "Bond" "Swaption" "Treasury Bill" "Bond Future" "Bond Future Option" "Bond Lending" "Bond Option" "Bond Repo" "Buy/Sell Back" "Call Money" "Cap" "Cash" "Cash Balance" "CD" "CD Index" "Collar" "Commodity" "Commodity Future" "Commodity Future Option" "Commodity Option" "CP" "Credit Default Swap" "Credit_Default_Swap" "Discount Bill" "Energy" "Energy Option" "Equity"))
                            h-t)))
                 (labels ((bicode<-mappingtype (mappingtype)
                            (gethash mappingtype mt->bic))
                          (mtdesc<-mappingtype (mappingtype)
                            (gethash mappingtype mt->mtd)))
                   (let ((mapping (sort (let (mapping)
                                          (maphash #'(lambda (k v)
                                                       (push (list k
                                                                   (intern (symbol-name k))
                                                                   (or (bicode<-mappingtype k)
                                                                       (error "The expression ~S is not a valid mappingtype." k)))
                                                             mapping))
                                                   mt->bic)
                                          mapping)
                                        #'(lambda (x y) (string< (symbol-name x)
                                                                 (symbol-name y)))
                                        :key #'car)))
                     `(let ((eqp ,eqp))
                        (flet ((mtdesc<-mappingtype (mappingtype)
                                 (gethash mappingtype ,mt->mtd))
                               (bicode<-mappingtype (mappingtype)
                                 (gethash mappingtype ,mt->bic)))
                          (defun write-banking-instrument-mapping (&rest mapping &key ,@(mapcar #'cadr mapping))
                            (loop for key in mapping by #'cddr
                               for map in (cdr mapping) by #'cddr do
                               (funcall #'writebankinginstmapping;(formatter "~@{~28A~#[~%~:; ~]~}") t; ;
                                        key;(mtdesc<-mappingtype key)
                                        (or (gethash map eqp)
                                            (error "The expression ~S is not a valid equivalent product." map))
                                        (bicode<-mappingtype key)))))))))))
    (generate-write-function)))


#+IGNORE
(in-environment :production
  (write-banking-instrument-mapping
   :assetbackedsecurity          :mbs
   :bondcallable                 :bond
   :bondcat                      :bond
   :bondfixed                    :bond
   :bondfixtofloat               :bond
   :bondfloating                 :bond
   :bondinflationlinked          :bond
   :bondratinglinked             :bond
   :cashaccount                  :cash
   :collateraliseddebtobligation :bond
   :creditlinkednote             :bond
   :equity                       :equity
   :equityprivate                :equity
   :forwardfx                    :fx
   :fundbond                     :equity
   :fundequity                   :equity
   :fundmoneymarket              :equity
   :fundofhedgefunds             :equity
   :fundrealestate               :equity
   :futureequityindex            :equity-future
   :futureir                     :bond-future
   :hedgefund                    :equity
   :loanfixed                    :cash
   :loanfloating                 :cash
   :loanpolicyholder             :cash
   :moneymarket                  :cash
   :mortgageannuityfixed         :cash
   :mortgageannuityfloating      :cash
   :mortgagebackedsecurity       :bond
   :mortgagefixed                :cash
   :mortgagefloating             :cash
   :mortgageswissvariable        :cash
   :optionequityplainvanilla     :equity-option
   :optionfxplainvanilla         :fx-option
   :participationconsolidated    :equity
   :participationstrategic       :equity
   :realestatecompany            :equity
   :realestatedirect             :equity
   :swapcreditdefault            :credit-default-swap
   :swapcreditdefaultindex       :credit-default-swap
   :swapcurrency                 :fx
   :swapir                       :swap
   :swaption                     :swaption))

#+IGNORE
(write-banking-instrument-mapping
  :alternativeinvestmentsother  :equity
  :assetbackedsecurity          :mbs
  :bondamortising               :bond
  :bondcallable                 :bond
  :bondcat                      :bond
  :bondconvertible              :bond
  :bondconvertiblereverse       :bond
  :bondfixed                    :bond
  :bondfixtofloat               :multi-leg
  :bondfloating                 :bond
  :bondfloatwithtenor           :bond
  :bondhighyield                :bond
  :bondinflationlinked          :bond
  :bondmakewholecallable        :bond
  :bondmultiplecoupons          :bond
  :bondratinglinked             :bond
  :capir                        :cap
  :cashaccount                  :cash-balance
  :collateraliseddebtobligation :cash
  :collateralisedmortgageobliga :cash
  :commodity                    :commodity
  :creditlinkednote             :bond
  :equity                       :equity
  :equityprivate                :equity
  :floorir                      :floor
  :forwardequity                :equity
  :forwardequityvariance        :equity
  :forwardequityvolatility      :equity
  :forwardfx                    :fx
  :forwardfxvariance            :fx
  :forwardfxvolatility          :fx
  :forwardir                    :cash
  :fundbond                     :equity
  :fundequity                   :equity
  :fundequityprivate            :equity
  :fundmoneymarket              :equity
  :fundofhedgefunds             :equity
  :fundrealestate               :equity
  :futureequityindex            :equity-future
  :futurefx                     :fx-future
  :futureir                     :future
  :hedgefund                    :equity
  :loanfixed                    :cash
  :loanfloating                 :cash
  :loanpolicyholder             :cash
  :moneymarket                  :cash
  :mortgageannuityfixed         :cash
  :mortgageannuityfloating      :cash
  :mortgagebackedsecurity       :bond
  :mortgagefixed                :cash
  :mortgagefloating             :cash
  :mortgageswissvariable        :cash
  :mortgageswissvariablecap     :cash
  :optionequitybarrier          :equity-option
  :optionequityfixedstrikelookb :equity-option
  :optionequityfloatstrikelookb :equity-option
  :optionequityplainvanilla     :equity-option
  :optionfxbarrier              :fx-option
  :optionfxfixedstrikelookback  :fx-option
  :optionfxfloatstrikelookback  :fx-option
  :optionfxplainvanilla         :fx-option
  :participationconsolidated    :equity
  :participationstrategic       :equity
  :realestatecompany            :equity
  :realestatedirect             :equity
  :structureddebt               :cash
  :structuredequity             :equity
  :structuredother              :equity
  :swapconstantmaturity         :swap
  :swapcreditdefault            :credit-default-swap
  :swapcreditdefaultindex       :credit-default-swap
  :swapcurrency                 :fx
  :swapequity                   nil
  :swapequityindexvariance      nil
  :swapequityindexvolatility    nil
  :swapfxvariance               nil
  :swapfxvolatility             nil
  :swapir                       :swap
  :swapovernightindex           :swap
  :swaption                     :swaption)
