
(defun node<-make (name &optional attributes children)
  (cons (cons name attributes) children))
(defun getname<-node (node)
  (caar node))
(defun getattributes<-node (node)
  (cdar node))
(defun getchildren<-node (node)
  (cdr node))
(defun setattributes<-node (node new-attributes)
  (rplacd (car node) new-attributes)
  node)

(defun inherit (node-name &rest children)
  "Return a node with name NODE-NAME and children CHILDREN. The set of the attributes of this node is the intersection of the sets of attributes of the children. The children are changed such that their attributes are the set difference between its original attributes and the attributes of this node."
  (let* ((children-attributes (mapcar #'getattributes<-node children))
         (parent-attributes (funcall (if (oddp (length children))
                                         #'reverse
                                         #'identity)
                                     (reduce #'intersection (rest children-attributes)
                                             :initial-value (first children-attributes)))))
    (node<-make node-name parent-attributes
                (mapcar #'(lambda (node)
                            (setattributes<-node node (set-difference (getattributes<-node node) parent-attributes)))
                        children))))





(defun print-node (node &optional (stream nil))
  (format stream "~I~A~%  ~{~I~A~%~}  ~I~{--------------------~%~A~%~}" (getname<-node node) (getattributes<-node node) 
          (mapcar #'print-node (getchildren<-node node))))





(let* ((cd-issue '(|IssueID| |ExternSystName| |ExternID| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |Market| |PrcYldType| |SettleMethod| |YieldMethod| |AccrualAccuracyMode| |AccrualAccuracyDp| |MaturityDateUnadj| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |UnitAmount| |CapGainAccretionMethod| |Coupon| |CouponDenomination| |CouponPrecision| |CouponRounding| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |DateStubAbsEnd| |DateStubAbsStart| |FirstAccrualDate| |Frequency| |InterestCalcToAdjOrUnadjDates| |JgbRoundingMethod| |IssueAltKey| |Extensions|))
       (deferred-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |AccrualAccuracyMode| |AccrualAccuracyDp| |BasePrice| |BasketID| |CompoundingFrequency| |CompoundingPeriod| |CompoundingStyle| |DefaultSettleMethod| |DualCurrency| |DualFXRate| |DualMarket| |DualType| |ExDivDaysType| |IssuedByBank| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PreSoldDate| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenomAccruals| |RedenominationDate| |RedenominationMethod| |UnitAmount| |AccrualCalcBasis| |AccrualUsingAdjOrUnadj| |AccrualExDivMethod| |CapGainAccretionMethod| |Coupon| |CouponDenomination| |CouponMonthSameDay| |CouponPrecision| |CouponRounding| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |ExDivDays| |FirstAccrualDate| |Frequency| |InterestCalcBasis| |InterestCalcToAdjOrUnadjDates| |JgbRoundingMethod| |IssueAltKey| |BondIssueCashflows| |Extensions|))
       (discount-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |BasePrice| |BasketID| |DefaultSettleMethod| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenominationDate| |RedenominationMethod| |UnitAmount| |CapGainAccretionMethod| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |JgbRoundingMethod| |IssueAltKey| |Extensions|))
       (fixed-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |AccrualAccuracyMode| |AccrualAccuracyDp| |BasePrice| |BasketID| |DefaultSettleMethod| |DualCurrency| |DualFXRate| |DualMarket| |DualType| |ExDivDaysType| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenomAccruals| |RedenominationDate| |RedenominationMethod| |UnitAmount| |AccrualCalcBasis| |AccrualUsingAdjOrUnadj| |AccrualExDivMethod| |ActiveRedemption| |Amortisation| |Annuity| |CapGainAccretionMethod| |Coupon| |CouponDenomination| |CouponMonthSameDay| |CouponPrecision| |CouponRounding| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesCallPayAdjMethod| |DatesCallPayFinc1| |DatesCallPayFinc2| |DatesCallPayFinc3| |DatesCallPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |DateStubAbsEnd| |DateStubAbsStart| |ExDivDays| |FirstAccrualDate| |Frequency| |InterestCalcBasis| |InterestCalcToAdjOrUnadjDates| |JgbRoundingMethod| |SettleCallMethod| |CallPut| |IssueAltKey| |BondIssueCashflows| |Extensions|))
       (floating-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |AccrualAccuracyMode| |AccrualAccuracyDp| |BasePrice| |BasketID| |DefaultSettleMethod| |DualCurrency| |DualFXRate| |DualMarket| |DualType| |ExDivDaysType| |Flavour| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenomAccruals| |RedenominationDate| |RedenominationIndex| |RedenominationMethod| |UnitAmount| |AccrualCalcBasis| |AccrualUsingAdjOrUnadj| |AccrualExDivMethod| |ActiveRedemption| |Amortisation| |Annuity| |CapGainAccretionMethod| |CouponDenomination| |CouponMonthSameDay| |CouponPrecision| |CouponRounding| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesCallPayAdjMethod| |DatesCallPayFinc1| |DatesCallPayFinc2| |DatesCallPayFinc3| |DatesCallPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |DateStubAbsEnd| |DateStubAbsStart| |ExDivDays| |FirstAccrualDate| |FloatingIndexID| |Frequency| |FrnCap| |FrnFirstIndex| |FrnFirstInterpIndex| |FrnFloor| |FrnLastIndex| |FrnLastInterpIndex| |FrnMargin| |FrnSimpleComplex| |FrnUseDefaultInterp| |InterestCalcBasis| |InterestCalcToAdjOrUnadjDates| |IsRateCapped| |IsRateFloored| |JgbRoundingMethod| |OverrideCalendars| |ModifiedFinc1| |ModifiedFinc2| |ModifiedFinc3| |ModifiedFinc4| |OverrideOffset| |ModifiedObsOffset| |RateConst| |RateMult| |SettleCallMethod| |UseRateMult| |CallPut| |IssueAltKey| |BondIssueCashflows| |Extensions|))
       (indexed-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |AccrualAccuracyMode| |AccrualAccuracyDp| |BasePrice| |BasketID| |DefaultSettleMethod| |DualCurrency| |DualFXRate| |DualMarket| |DualType| |ExDivDaysType| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenomAccruals| |RedenominationDate| |RedenominationIndex| |RedenominationMethod| |UnitAmount| |AccrualCalcBasis| |AccrualUsingAdjOrUnadj| |AccrualExDivMethod| |ActiveRedemption| |Amortisation| |Annuity| |CapGainAccretionMethod| |Coupon| |CouponDenomination| |CouponMonthSameDay| |CouponPrecision| |CouponRounding| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesCallPayAdjMethod| |DatesCallPayFinc1| |DatesCallPayFinc2| |DatesCallPayFinc3| |DatesCallPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |DateStubAbsEnd| |DateStubAbsStart| |ExDivDays| |FirstAccrualDate| |FloatingIndexID| |Frequency| |IndexBasicValue| |IndexStyle| |InterestCalcBasis| |InterestCalcToAdjOrUnadjDates| |JgbRoundingMethod| |SettleCallMethod| |CallPut| |IssueAltKey| |BondIssueCashflows| |Extensions|))
       (mbs-bond-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |AccrualAccuracyMode| |AccrualAccuracyDp| |BasePrice| |BasketID| |DefaultSettleMethod| |DualCurrency| |DualFXRate| |DualMarket| |DualType| |ExDivDaysType| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenomAccruals| |RedenominationDate| |RedenominationMethod| |UnitAmount| |AccrualCalcBasis| |AccrualUsingAdjOrUnadj| |AccrualExDivMethod| |ActiveRedemption| |Amortisation| |Annuity| |CapGainAccretionMethod| |Coupon| |CouponDenomination| |CouponMonthSameDay| |CouponPrecision| |CouponRounding| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesCallPayAdjMethod| |DatesCallPayFinc1| |DatesCallPayFinc2| |DatesCallPayFinc3| |DatesCallPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |DateStubAbsEnd| |DateStubAbsStart| |ExDivDays| |FirstAccrualDate| |Frequency| |InterestCalcBasis| |InterestCalcToAdjOrUnadjDates| |JgbRoundingMethod| |SettleCallMethod| |CallPut| |IssueAltKey| |BondIssueCashflows| |Extensions|))
       (zero-coupon-issue '(|IssueID| |ExternSystName| |ExternID| |AmountIssued| |BOContractCode| |CCY| |CleanDirtyFlag| |DayOffset| |Description| |IssueDate| |IssuerCode| |IssuePrice| |LongDescription| |Market| |MinimumTradeQty| |MoodyRating| |PrcYldType| |PriceIncrement| |PriceInputBidOfferSpread| |SettleMethod| |SPRating| |TaxMethod| |YieldMethod| |BasePrice| |BasketID| |DefaultSettleMethod| |LegacyCurrency| |LegacyMkt| |MaturityDateUnadj| |MaturityPrice| |PriceYieldCalcBasis| |QuoteAccuracyMode| |QuoteAccuracyDp| |RedenominationDate| |RedenominationMethod| |UnitAmount| |CapGainAccretionMethod| |DatesAdjMethod| |DatesPayFinc1| |DatesPayFinc2| |DatesPayFinc3| |DatesPayFinc4| |DatesExtraPayAdjMethod| |DatesExtraPayFinc1| |DatesExtraPayFinc2| |DatesExtraPayFinc3| |DatesExtraPayFinc4| |JgbRoundingMethod| |IssueAltKey| |Extensions|)))
  ;#+IGNORE
  (pprint (inherit 'Issue
                   (node<-make 'CDIssue cd-issue)
                   (inherit 'BondTypeIssue
                            (inherit 'CouponPayingIssue
                                     (node<-make 'DeferredIssue deferred-issue)
                                     (inherit 'NonDeferredIssue
                                              (node<-make 'FloatingIssue floating-issue)
                                              (node<-make 'IndexedIssue indexed-issue)
                                              (inherit 'FixCouponIssue
                                                       (node<-make 'FixedIssue fixed-issue)
                                                       (node<-make 'MBSBondIssue mbs-bond-issue))))
                            (inherit 'NonCouponPayingIssue
                                     (node<-make 'DiscountIssue discount-issue)
                                     (node<-make 'ZeroCouponIssue zero-coupon-issue)))) t)
  #+IGNORE
  (format t "XML.containerNode(\"~A\", XML.MANDATORY,~{~%XML.node(\"~A\", XML.MANDATORY,~:* ~A)~#[~:;,~]~});"
          '|FixedIssue| fixed-issue)
  #+IGNORE
  (format t "~{~%~A~#[~:;,~]~}" fixed-issue)
  #+IGNORE
  (inherit 'Issue
           (node<-make 'CDIssue cd-issue)
           (inherit 'BondTypeIssue
                    (inherit 'CouponPayingIssue
                             (node<-make 'DeferredIssue deferred-issue)
                             (inherit 'NonDeferredIssue
                                      (node<-make 'FloatingIssue floating-issue)
                                      (node<-make 'IndexedIssue indexed-issue)
                                      (inherit 'FixCouponIssue
                                               (node<-make 'FixedIssue fixed-issue)
                                               (node<-make 'MBSBondIssue mbs-bond-issue))))
                    (inherit 'NonCouponPayingIssue
                             (node<-make 'DiscountIssue discount-issue)
                             (node<-make 'ZeroCouponIssue zero-coupon-issue)))))

