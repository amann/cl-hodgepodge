

(in-package #:sl.load-prices)


(defvar *bp* (connect-generic :Driver "{Microsoft ODBC for Oracle}"
                              :Server "p014.swisslife.ch"
                              :Uid "ADAPTIV_READ"
                              :Pwd "pada14p"))
(defvar *adaptiv* (connect-generic :driver "{SQL Native Client}"
                                   :server "nx3038"
                                   :uid "mgr"
                                   :pwd "itsystem"))

(defvar *price-query* "with
    sel as (select ID as SIGNATURE, SITUATIONDATE
            from BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE
            where SITUATIONDATE = last_day(to_date('~A','yymm')))
   ,cte as (select 'yyyy-mm-dd\"T\"hh24:mi:ss'                                                           as isodatetimeformat,
                   'yyyy-mm-dd'                                                                        as isodateformat,
                   decode(SYS_CONTEXT('USERENV','DB_NAME'), 'P014', 'PROD', 'Q014MU', 'MU-UAT', 'UAT') as bpenv from dual)
   ,aux as (select
                cte.isodatetimeformat, cte.isodateformat, cte.bpenv,
                decode(cte.bpenv, 'PROD', 'P', 'MU-UAT', 'M', 'K')                                     as issue_id_prefix,
                decode(cte.bpenv, 'PROD', 'Q', 'MU-UAT', 'N', 'L')                                     as undrl_id_prefix
            from dual, cte)
   ,lot as (select p.SIGNATURE, p.POSITION_RN, s.SITUATIONDATE, s.LOADDATE, p.MAPPINGTYPE, p.CONVERSIONDATE,
                   case when s.DELIVERYTYPE = 'RPF' and regexp_like(p.upid, '.*RPF[[:digit:]]{4}.*')
                   then last_day(to_date(regexp_replace(p.upid, '.*RPF([[:digit:]]{4}).*', '\\1'), 'yymm')) else null end     as calibrationDate,
                   case when s.DELIVERYTYPE = 'RPF' and regexp_like(p.upid, '.*RPF[[:digit:]]{4}.*')
                   then decode(mod(to_number(regexp_replace(p.upid, '.*RPF([[:digit:]]{2})[[:digit:]]{2}.*', '\\1')),3) * 12
                                 + to_number(regexp_replace(p.upid, '.*RPF[[:digit:]]{2}([[:digit:]]{2}).*', '\\1')) - 1,
                          0,'0',1,'1',2,'2',3,'3',4,'4',5,'5',6,'6',7,'7',8,'8',9,'9',10,'A',11,'B',12,'C',13,'D',14,'E',15,'F',16,'G',17,'H',
                          18,'I',19,'J',20,'K',21,'L',22,'M',23,'N',24,'O',25,'P',26,'Q',27,'R',28,'S',29,'T',30,'U',31,'V',32,'W',33,'X',34,'Y',35,'Z')
                   else null end                                                                                             as calibrationDateCode,
                   case aux.bpenv when 'PROD' then '' else substr('0' || p.PFLOCALPORTFOLIOID, -2) end                       as portfolioid,
                   decode(mod(to_number(to_char(s.SITUATIONDATE,'YY')),3) * 12 + to_number(to_char(s.SITUATIONDATE,'MM')) - 1,
                          0,'0',1,'1',2,'2',3,'3',4,'4',5,'5',6,'6',7,'7',8,'8',9,'9',10,'A',11,'B',12,'C',13,'D',14,'E',15,'F',16,'G',17,'H',
                          18,'I',19,'J',20,'K',21,'L',22,'M',23,'N',24,'O',25,'P',26,'Q',27,'R',28,'S',29,'T',30,'U',31,'V',32,'W',33,'X',34,'Y',35,'Z')
                                                                                                                             as situationDateCode,
                   decode(s.companycode,'CH','0','DE','1','LU','2','SLAB','3','SLAP','4','SLPS','5','SLGROUP','6','SL France','7','9') as companyCode
            from aux
            join BLUEPRINT.ADAPTIV_DMAEXP_POSITION p on p.ISREXP_POS = 1 and (p.PFPORTFOLIOTYPE = 'OR' or p.BASKETTYPE_LOCAL_CODE = 'SLINDEX')
            join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE s on s.ID = p.SIGNATURE and s.SITUATIONDATE = last_day(s.SITUATIONDATE) and s.CLOSING = 0
            join (select distinct SITUATIONDATE from sel) l on l.SITUATIONDATE = s.SITUATIONDATE)
   ,cpn as (select P_DMASI_SIGNATURE_ID as signature, S_DMAPH_POS_RN as position_rn,
                              S_COUPN_COUPNTYPE_CODE,
                              S_COUPN_STARTDATE,
                              S_COUPN_ENDDATE,
                              S_COUPN_COUPON,
                              S_COUPN_SPREADFLOAT,
                              S_COUPN_FREQ,
                              S_COUPN_ACCRBASIS_CODE,
                              S_COUPN_LAG,
                              S_COUPN_PAIDASSUM
            from BLUEPRINT.ADAPTIV_PLAIN_COUPON c
            join (select first_value(s.ROWID) over (partition by s.P_DMASI_SIGNATURE_ID, s.S_DMAPH_POS_RN
                                                    order by s.S_COUPN_STARTDATE asc rows unbounded preceding) as ID
                  from lot
                  join BLUEPRINT.ADAPTIV_PLAIN_COUPON s on s.P_DMASI_SIGNATURE_ID = lot.SIGNATURE and s.S_DMAPH_POS_RN = lot.POSITION_RN
                                                       and (s.S_COUPN_ENDDATE > lot.SITUATIONDATE or s.S_COUPN_ENDDATE is null)) s on s.ID = c.ROWID)
   ,leg as (select SIGNATURE, POSITION_RN,
                LEGPAYMENTTYPE,
                LEGCOUPONTYPE,
                LEGCOUPONINDEXNAME,
                LEGCOUPONINDEXVAL
            from BLUEPRINT.ADAPTIV_DMAEXP_LEG l
            join lot using (SIGNATURE, POSITION_RN)
            where lot.MAPPINGTYPE = 'SWAPEQUITY' and l.LEGCOUPONTYPE is null)
   ,cap as (select l.SIGNATURE, l.POSITION_RN, c.S_CAPUT_STRIKE, c.S_CAPUT_CAPUTTYPE_CODE, c.S_CAPUT_DATE
            from (select ROWID as ID, S_CAPUT_STRIKE, S_CAPUT_CAPUTTYPE_CODE, S_CAPUT_DATE
                  from BLUEPRINT.ADAPTIV_DMAEXP_CALLPUT) c
            join (select min(c.ROWID) as ID, lot.SIGNATURE, lot.POSITION_RN, min(c.S_CAPUT_DATE) as S_CAPUT_DATE
                  from lot
                  left join BLUEPRINT.ADAPTIV_DMAEXP_CALLPUT c on c.P_DMASI_SIGNATURE_ID = lot.SIGNATURE and c.S_DMAPH_POS_RN = lot.POSITION_RN
                                                               and (c.S_CAPUT_DATE > lot.SITUATIONDATE or c.S_CAPUT_DATE is null)
                  group by lot.SIGNATURE, lot.POSITION_RN) l using (ID))
   ,bbt as (select org.CODE as ORGUNIT, bbt.NAME as S_UNDRL_NAME, bbt.BLOOMBERGTICKER
            from BLUEPRINT.ORGUNIT org
            left join BLUEPRINT.MD_BLOOMBERG_NAMETRANS bbt on bbt.ORGUNIT = org.ID)
   ,pos as (select
                pos.SIGNATURE,
                pos.POSITION_RN,
                sig.SITUATIONDATE,
                'BP' || lot.portfolioid                                                                                      as \"SystemID\",
                to_char(sig.SITUATIONDATE, 'yymm')                                                                           as \"Location\",
                lot.portfolioid                                                                                              as \"PortfolioID\",
                rpad(to_char(sysdate, aux.isodatetimeformat), 30) || lpad(sig.ID, 30) || aux.bpenv                           as \"LoadInfo\",
                decode(sig.DELIVERYTYPE,
                       'RPF', sig.companycode || regexp_replace(pos.upid, 'RPF([[:digit:]]{4})(.*)', '-\\1-\\2'),
                       nvl2(pid.posid, to_char(pid.posid),
                                       to_char(pos.signature) || '-' || to_char(pos.position_rn)))                           as \"TradeID\",
                sig.SITUATIONDATE - 3                                                                                        as \"TradeDate\",
                decode(aux.bpenv, 'PROD', '', 'ALM')                                                                         as \"TradeOwner\",
                nvl(pos.PFPORTFOLIOSEGMENT, 'ALL')                                                                           as \"TraderID\",
                case
                    when pos.REPORTINGTYPE = 'HC' then 'HC'
                    when sig.DELIVERYTYPE = 'RPF' then decode(pos.REXP_ADAPTIVCPARTYID,
                                                              'CPDefaulted',     'D',
                                                              'CPBelow B-',      'CCC',
                                                              'CPAAA',           'LIAB',
                                                              'CPB',             'LIAB',
                                                              'CPNot allocated', 'LIAB',
                                                              'CPREALESTATE',    'NA',
                                                              'CP',              'NR',
                                                              nvl2(pos.REXP_ADAPTIVCPARTYID,
                                                                   'R' || case when sig.COMPANYCODE = 'CH'
                                                                               then decode(pos.PFPORTFOLIOSEGMENT, 'GRP', 'CK', 'CE')
                                                                               else upper(substr(decode(sig.COMPANYCODE, 'SL France', 'FH',
                                                                                                                         'SLGROUP', 'HN',
                                                                                                                         sig.COMPANYCODE), -2)) end
                                                                       || substr(pos.REXP_ADAPTIVCPARTYID, 3), 'LIAB'))
                    when pos.POSITIONCATEGORY = 'Liability' then 'LIAB'
                    when pos.ISSUETYPE in ('GOV', 'SOV') and pos.REXP_ADAPTIVCPARTYID in ('CPAAA', 'CPAA') then 'GOV'
                    when pos.MAPPINGTYPE like 'OPTION%'     or pos.MAPPINGTYPE like 'FUTURE%'    then 'DERIV'
                    when pos.MAPPINGTYPE like 'REALESTAED%'                                      then 'NA'
                    else substr(decode(pos.REXP_ADAPTIVCPARTYID,
                                       'CPDefaulted',     'CPD',
                                       'CPBelow B-',      'CPCCC',
                                       'CPNot allocated', 'CPNA',
                                       'CPREALESTATE',    'CPNA',
                                       'CP',              'CPNR',
                                       nvl(pos.REXP_ADAPTIVCPARTYID, 'CPNA')), 3)
                end                                                                                                          as \"Counterparty\",
                pos.UPID,
                replace(pos.INSTRUMENTNAME || decode(aux.bpenv, 'PROD', '', ' ' || pos.PFLOCALPORTFOLIONAME), ',', '.')      as \"TradeDescription\",
                lot.calibrationDate,
                decode(pos.POSITIONCATEGORY, 'Asset', 'A', 'Liability', 'L', 'A')                                            as \"TradingArea\",
                decode(upper(sig.ORGUNIT), 'SLGROUP', 'SLHN', upper(sig.ORGUNIT))                                            as \"AccountingArea\",
                decode(upper(sig.COMPANYCODE), 'SLGROUP', 'SLHN', upper(sig.COMPANYCODE))                                    as \"AccountingGroup\",
                case
                    when pos.MAPPINGTYPE = 'OPTIONEQUITYPLAINVANILLA' and pos.OPTIONSTYLE = 'FSE'      then 'OPTIONEQUITYFWDSTART'
                    when sig.DELIVERYTYPE = 'RPF' and regexp_like(pos.UPID, 'RPF[[:digit:]]{4}CDS.*')  then 'RPFCDS'
                    else pos.MAPPINGTYPE end                                                                                 as MAPPINGTYPE,
                sig.SITUATIONDATE - 3                                                                                        as \"SettlementDate\",
                case
                    when pos.MAPPINGTYPE not in ('FORWARDIR','FUTUREIR',
                        'SWAPCURRENCY','FORWARDFX','FUTUREFX','OPTIONFXBARRIER','OPTIONFXFIXEDSTRIKELOOKBACK','OPTIONFXFLOATSTRIKELOOKBACK','OPTIONFXPLAINVANILLA',
                        'SWAPFXVARIANCE','FORWARDFXVARIANCE','SWAPFXVOLATILITY','FORWARDFXVOLATILITY',
                        'CASHACCOUNT','MONEYMARKET','LOANFIXED','LOANFLOATING','LOANPOLICYHOLDER',
                        'MORTGAGEANNUITYFIXED','MORTGAGEANNUITYFLOATING','MORTGAGEFIXED','MORTGAGEFLOATING','MORTGAGESWISSVARIABLE','MORTGAGESWISSVARIABLECAP',
                        'SWAPCONSTANTMATURITY','SWAPIR','SWAPOVERNIGHTINDEX','SWAPTION','CAPIR','FLOORIR',
                        'SWAPCREDITDEFAULT','SWAPCREDITDEFAULTINDEX')
                    then
                    case
                        when sig.DELIVERYTYPE = 'RPF' then
                        case
                            when pos.OPTIONSTYLE = 'FSE' then null
                            when pos.OPTIONSTYLE = 'EU' then 'RPF' || dbms_utility.get_hash_value(lot.companycode
                                                                                        || to_char(sig.SITUATIONDATE, 'yymm')
                                                                                        || decode(aux.bpenv, 'PROD', '', lot.PORTFOLIOID)
                                                                                        || cap.S_CAPUT_CAPUTTYPE_CODE
                                                                                        || to_char(pos.INSTRUMENTMATURITYYEAR, 'FM009')
                                                                                        || regexp_replace(pos.S_UNDRL_ISIN, 'RPF([[:alnum:]]+?)0+', '\\1')
                                                                                        || to_char(cap.S_CAPUT_STRIKE)
                                                                                        || to_char(lot.calibrationDate, 'yymm'),
                                                                                        10000000, 67108864)
                                                                   || aux.issue_id_prefix
                            when 0=1 then aux.issue_id_prefix
                                       || lot.companycode
                                       || decode(aux.bpenv, 'PROD', to_char(sig.SITUATIONDATE, 'ymm'), lot.situationDateCode || lot.PORTFOLIOID)
                                       || decode(cap.S_CAPUT_CAPUTTYPE_CODE,'C','C','P')
                                       || to_char(pos.INSTRUMENTMATURITYYEAR, 'FM09')
                                       || regexp_replace(pos.S_UNDRL_ISIN, 'RPF([[:alnum:]])[[:alnum:]]([[:alnum:]])[[:alnum:]]+?0+', '\\1\\2')
                                       || to_char(mod(10 * cap.S_CAPUT_STRIKE, 100), 'FM09')
                                       || lot.calibrationDateCode
                            else substr(pos.ISIN, 1, 11) || lot.calibrationDateCode
                        end
                        when pos.MAPPINGTYPE = 'REALESTATEDIRECT' then 'RE' || pos.REALESTATEUSAGE
                                                                            || decode(nvl(pos.S_RLEST_COUNTRY_CODE, sig.ORGUNIT), 'CH', 'CH', 'EU')
                        when pos.MAPPINGTYPE like '%HEDGE%' then 'HFRXGL-' || pos.CURRENCYORIG
                        when pos.ISIN is null
                          or pos.ISIN in ('CH0000000000', 'DE0000000001', 'FR0000000000')
                          or pos.MAPPINGTYPE = 'FUTUREEQUITYINDEX'
                          or (pos.MAPPINGTYPE like 'FUND%' and pos.POSLINK is not null) then aux.issue_id_prefix || pid.posid
                        else pos.ISIN
                    end
                    else null
                end                                                                                                          as Issue_ID,
                case
                    when pos.S_UNDRL_NAME is not null or pos.MAPPINGTYPE = 'SWAPEQUITY' then
                    case
                        when sig.DELIVERYTYPE = 'RPF' then substr(pos.S_UNDRL_ISIN, 1, 11) || lot.calibrationDateCode
                        when bbt.BLOOMBERGTICKER like 'SX5ED%' then '0INDEX0SX5ED'
                        when bbt.BLOOMBERGTICKER is not null then null
                        when pos.S_UNDRL_ISIN in ('CH0000000000', 'DE0000000001', 'FR0000000000') then aux.undrl_id_prefix || pid.posid
                        else nvl(pos.S_UNDRL_ISIN, aux.undrl_id_prefix || pid.posid)
                    end
                    else null
                end                                                                                                          as \"UnderlyingID\",
                case
                    when pos.MAPPINGTYPE in ('ALTERNATIVEINVESTMENTSOTHER',
                                             'COMMODITY','EQUITY','EQUITYPRIVATE',
                                             'FUNDBALANCED','FUNDBOND','FUNDEQUITYPRIVATE','FUNDEQUITY','FUNDMONEYMARKET','FUNDOFHEDGEFUNDS','FUNDREALESTATE','HEDGEFUND',
                                             'FORWARDEQUITY','FORWARDEQUITYVARIANCE','FORWARDEQUITYVOLATILITY','FUTUREEQUITYINDEX',
                                             'OPTIONEQUITYBARRIER','OPTIONEQUITYFIXEDSTRIKELOOKB','OPTIONEQUITYFLOATSTRIKELOOKB','OPTIONEQUITYPLAINVANILLA',
                                             'PARTICIPATIONCONSOLIDATED','PARTICIPATIONSTRATEGIC',
                                             'REALESTATECOMPANY','REALESTATEDIRECT',
                                             'STRUCTUREDDEBT','STRUCTUREDEQUITY','STRUCTUREDOTHER',
                                             'SWAPEQUITY','SWAPEQUITYINDEXVARIANCE','SWAPEQUITYINDEXVOLATILITY')
                    then regexp_replace(upper(
                        case 
                            when sig.DELIVERYTYPE = 'RPF' then nvl2(pos.S_UNDRL_NAME, regexp_replace(pos.S_UNDRL_ISIN, 'RPF([[:alnum:]]+?)0+', '\\1'),
                                                                                      regexp_replace(pos.ISIN,         'RPF([[:alnum:]]+?)0+', '\\1'))
                            when pos.MAPPINGTYPE in ('EQUITYPRIVATE', 'PARTICIPATIONSTRATEGIC') then 'MXWO'
                            when pos.MAPPINGTYPE in ('HEDGEFUND', 'FUNDOFHEDGEFUNDS') then 'HFRXGL'
                            when pos.MAPPINGTYPE like '%REALESTATE%' then 'RE' || decode(nvl(pos.S_RLEST_COUNTRY_CODE, sig.ORGUNIT), 'CH', 'CH', 'EU')
                            else nvl2(pos.S_UNDRL_NAME, nvl(bbt.BLOOMBERGTICKER, pos.S_UNDRL_BENCHMARK_CODE),
                                                                                 nvl(pos.BENCHMARKINDEX, decode(pos.RISKFACTORID,
                                'Swiss Life N',          'SLHN',
                                'MSCI BELGIUM',          'MXBE',
                                'MSCI EURO',             'MSER',
                                'MSCI EUROPE',           'MXEU',
                                'MSCI FRANCE',           'MXFR',
                                'MSCI GERMANY',          'MXDE',
                                'MSCI JAPAN',            'MXJP',
                                'MSCI NETHERLANDS',      'MXNL',
                                'MSCI PACIFIC EX JAPAN', 'MXPCJ',
                                'MSCI SWITZERLAND',      'MXCH',
                                'MSCI UK',               'MXGB',
                                'MSCI USA',              'MXUS',
                                'MSCI WORLD',            'MXWO', 'MXWO')))
                        end), '([[:alnum:]]{3}[[:alpha:]]*([[:space:]]+[[:alpha:]]+)*?)[[:space:]]*[[:digit:]]?.*(INDEX|EQUITY)?', '\\1')
                    else null
                end                                                                                                            as \"EQBenchmark\",
                pos.CURRENCYORIG,
                pos.ISIN,
                pos.TICKERNAMEBLOOMBERG,
                case when pos.MAPPINGTYPE = 'FUTUREIR' then pos.FUTUREID_CODE else pos.ACCACCOUNTCODEIFRS end                  as ACCACCOUNTCODEIFRS,
                pos.ACCACCOUNTCODEIFRS_DAAR,
                pos.ACCACCOUNTNAMEIFRS,
                pos.ACCACCOUNTNOIFRS,
                pos.ACCACCOUNTNOLST,
                pos.ACCCUSTODYACCOUNTNAME,
                pos.ACCOUNTING_CODE_DALM,
                pos.AMACCRUEDINTEREST_LOCAL,
                pos.AMACCRUEDINTEREST_ORIG,
                pos.AMAMORTISEDCOSTVALUE_LOCAL,
                pos.AMAMORTISEDCOSTVALUE_ORIG,
                pos.AMBOOKVALUEIFRS_LOCAL,
                pos.AMBOOKVALUEIFRS_ORIG,
                pos.AMBOOKVALUELST_LOCAL,
                pos.AMBOOKVALUELST_ORIG,
                pos.AMCOSTVALUE_LOCAL,
                pos.AMCOSTVALUE_ORIG,
                pos.AMFUTURESCOSTVALUE_LOCAL,
                pos.AMFUTURESCOSTVALUE_ORIG,
                pos.AMMARKETVALUECLEAN_LOCAL,
                pos.AMMARKETVALUECLEAN_ORIG,
                pos.AMMARKETVALUEINDEX_LOCAL,
                case pos.MAPPINGTYPE when 'REALESTATEDIRECT' then pos.AMMARKETVALUE_LOCAL / 10 else pos.AMMARKETVALUE_LOCAL end as AMMARKETVALUE_LOCAL,
                case pos.MAPPINGTYPE when 'REALESTATEDIRECT' then pos.AMMARKETVALUE_ORIG / 10 else pos.AMMARKETVALUE_ORIG end   as AMMARKETVALUE_ORIG,
                pos.AMNOMINALPERYEAR_LOCAL,
                pos.AMNOMINALVALUE_ORIG,
                case when pos.MAPPINGTYPE like '%HEDGE%' then pos.AMMARKETVALUE_ORIG else pos.AMNUMBEROFUNITS end               as AMNUMBEROFUNITS,
                pos.AMSHAREPERCENTAGE,
                pos.BARRIERLEVEL,
                pos.BARRIERREBATE,
                pos.BARRIERTRIGGERED,
                pos.BARRIERTYPE,
                pos.BASKETNAME_LOCAL,
                pos.BASKETTYPE_LOCAL_CODE,
                pos.BENCHMARKINDEX,
                pos.BOOK_VALUE_IFRS_DALM,
                pos.BOOK_VALUE_LST_DALM,
                pos.CICOMPANYCODE,
                nvl(pos.CISUBORDINATEDCOMPANY,
                    case
                      when sig.COMPANYCODE not like 'SLAP%' and regexp_like(pos.INSTRUMENTNAME, 'SL[ ]*(AP| ASSU.*ET PATRIM)')                then 'SLAP'
                      when sig.COMPANYCODE not like 'SLAB%' and regexp_like(pos.INSTRUMENTNAME, 'SL[ ]*AB')                                   then 'SLAB'
                      when sig.COMPANYCODE not like 'SLPS%' and regexp_like(pos.INSTRUMENTNAME, 'SL[ ]*(PS| PREVOY.*SANTE)')                  then 'SLPS'
                      when sig.COMPANYCODE not like 'SL France%' and regexp_like(pos.INSTRUMENTNAME, 'SL[ ]+FRANCE')                          then 'FR'
                      when sig.COMPANYCODE not like 'LU%'   and regexp_like(pos.INSTRUMENTNAME, 'SL[ ]+LUXEMBO?URG')                          then 'LU'
                      when sig.COMPANYCODE not like 'DE%'   and regexp_like(pos.INSTRUMENTNAME, 'SL[ ]*(DE(UTSCHLAND)?| ALLEMAGNE| GERMANY)') then 'DE'
                      when sig.COMPANYCODE like 'SLGROUP'   and regexp_like(pos.INSTRUMENTNAME, '(CH|DE)[0-9]* *\(SLAG(|, .*)\).*')           then 'SLAG'
                      else '' end) as CISUBORDINATEDCOMPANY,
                pos.COLTYPE_CODE,
                pos.CONTRACTSIZE,
                case when nvl(pos.CONVERSIONDATE, to_date('1901', 'yyyy')) > sig.SITUATIONDATE then pos.CONVERSIONDATE else null end as CONVERSIONDATE,
                pos.CONVERSIONFACTOR,
                pos.COUNTERPARTY_RATING_DALM,
                pos.COUPON_PER_YEAR_DALM,
                pos.CPRATINGLTERM_OVERALL,
                pos.CPRATINGSTERM_OVERALL,
                pos.CURRENCYRISK,
                pos.CURRENCY_DALM,
                pos.C_CPART_RTNG_OVALL_CLMAP_CODE,
                pos.C_CPART_RTNG_OVALL_CLMAP_RNK,
                pos.C_DMAPA_EXPOSURE_LOCAL,
                pos.C_DMAPA_MV_DAAR_LOCAL,
                pos.C_INSTR_BMRKINDEX_CLMAP_CODE,
                pos.C_INSTR_CCYORIG_CLMAP_CODE,
                pos.C_INSTR_MAP_CATEGORY_CODE,
                pos.C_INSTR_MAP_CATEGORY_CODE_REP,
                pos.C_INSTR_MAP_INSTRGR_CODE,
                pos.C_INSTR_MAP_INSTRGR_CODE_REP,
                pos.C_INSTR_MAP_SUBCAT_CODE,
                pos.C_INSTR_MAP_SUBCAT_CODE_REP,
                pos.C_INSTR_RTNG_OVALL_CLMAP_CODE,
                pos.C_INSTR_RTNG_OVALL_CLMAP_RNK,
                decode(sig.DELIVERYTYPE, 'RPF', 'RPF:' || to_char(lot.calibrationDate, 'yyyy-mm-dd'), sig.DELIVERYTYPE)      as DELIVERYTYPE,
                pos.DIVIDENDYIELD,
                pos.DURATION,
                pos.FINTYPE,
                pos.FORWARDPRICE,
                pos.FUTUREID_CODE,
                pos.HIDDENRISK,
                pos.IDLOCALBASKETID,
                pos.IDLOCALPOSITIONID,
                pos.INDEXFULLNAME,
                pos.INDEXNAME,
                pos.INDEXTICKERNAME,
                pos.INDEXVALUE,
                pos.INDEX_WEIGHT,
                pos.INSTALMENTORIG,
                greatest(to_date('1933', 'yyyy'), nvl(pos.INSTRUMENTEFFECTIVEDATE, sig.SITUATIONDATE -3))                    as INSTRUMENTEFFECTIVEDATE,
                pos.INSTRUMENTMATURITYDATE,
                pos.INSTRUMENTMATURITYYEAR,
                pos.INSTRUMENTNAME,
                pos.INSTRURATE_OVERALL,
                pos.ISSUERANKING,
                pos.ISSUETYPE,
                sig.IS_PRECUTOFF,
                pos.LEVERAGELEVEL_CODE,
                sig.LOADDATE,
                pos.LOCALSECURITYTYPE,
                pos.MARKET_VALUE_DALM,
                sig.MART_VERSION,
                pos.MATURITYBUCKET,
                pos.MATURITY_DAYS,
                pos.NOMINAL_VALUE_DALM,
                pos.OPTIONDELTA,
                pos.OPTIONFREQUENCYDAYS,
                pos.OPTIONLASTRATE,
                pos.OPTIONPROCESSCODE,
                pos.OPTIONSTYLE,
                pos.PFBLOCKEDPOSITION,
                lot.portfolioid                                                                                              as PFLOCALPORTFOLIOID,
                pos.PFLOCALPORTFOLIONAME,
                pos.PFPORTFOLIOSEGMENT,
                pos.PFPORTFOLIOTYPE,
                pos.PORTFOLIO_TYPE_DALM,
                pos.POSLINK,
                pos.PRICEORIGMAX,
                pos.PRICEORIGMIN,
                pos.PRICEQUOTEDORIGDATE,
                pos.REALESTATEMATURITYTIME,
                pos.REALESTATEREGIONCLASSCODE,
                pos.REALESTATERESIDENTIALRATIO,
                pos.REALESTATESALEDATE,
                pos.REALESTATEUSAGE,
                pos.REALESTATEVACANCY,
                pos.REALESTATEVALUATIONDATE,
                pos.REDEMPTION,
                pos.REDEMPTION_YEAR_DALM,
                pos.REPORTINGTYPE,
                pos.REXP_BALSHEETTYPE,
                pos.REXP_BALSHEETTYPE_REP,
                pos.REXP_POSTYPE,
                pos.REXP_POSTYPE_REP,
                pos.RISKFACTORID,
                pos.RISK_FACTOR_DALM,
                pos.SECURITY_TYPE_DALM,
                pos.SHARESPERNOMINAL,
                pos.SPECIALGUARANTEES,
                pos.SPREADDISK,
                pos.SWAPTIONEXPIRYDATE,
                pos.SWAPTIONVOLATILITY,
                pos.S_CPART_COUNTRY_CODE,
                pos.S_CPART_GOVTYPE_CODE,
                pos.S_CPART_INDSECT_CODE,
                pos.S_CPART_PRTYTYPE_CODE,
                pos.S_INSTR_RTNGEX_CODE,
                pos.S_INSTR_RTNGFI_CODE,
                pos.S_INSTR_RTNGIN_CODE,
                pos.S_INSTR_RTNGMO_CODE,
                pos.S_INSTR_RTNGSP_CODE,
                pos.S_RLEST_COUNTRY_CODE,
                pos.S_RLEST_REGION,
                pos.S_UNDRL_BENCHMARK_CODE,
                pos.S_UNDRL_CLEANPRICEPCT,
                pos.S_UNDRL_CTDCOUPON,
                pos.S_UNDRL_CTDCOUPONFREQ,
                pos.S_UNDRL_CTDMATDATE,
                pos.S_UNDRL_CTDSECTYPE_CODE,
                pos.S_UNDRL_CURRENCY_CODE,
                pos.S_UNDRL_IDLOCAL,
                pos.S_UNDRL_ISIN,
                pos.S_UNDRL_NAME,
                pos.S_UNDRL_PRICE,
                pos.S_UNDRL_PRICEDATE,
                pos.S_UNDRL_PRICEMAX,
                pos.S_UNDRL_PRICEMIN,
                pos.S_UNDRL_VOLA,
                pos.TRADETYPE,
                pos.UNITSOUTSTANDING,
                sig.VERSION_ASSET,
                pos.YIELDBETA
            from aux
            join lot on 1=1
            join BLUEPRINT.ADAPTIV_DMAEXP_POSITION  pos      on pos.SIGNATURE = lot.SIGNATURE and pos.POSITION_RN = lot.POSITION_RN
            join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig      on sig.ID = pos.SIGNATURE
            left join BLUEPRINT.ADAPTIV_DMAEXP_UPIDTRANS pid on pid.COMPANYCODE = sig.COMPANYCODE and pid.UPID = pos.UPID
            left join bbt on bbt.ORGUNIT = sig.ORGUNIT and bbt.S_UNDRL_NAME = pos.S_UNDRL_NAME
            left join cap on cap.SIGNATURE = lot.SIGNATURE and cap.POSITION_RN = lot.POSITION_RN)
   ,iss as (select
                SIGNATURE,
                POSITION_RN,
                SITUATIONDATE,
                pos.issue_id,
                case pos.MAPPINGTYPE
                    when 'BONDFIXTOFLOAT' then nvl2(pos.CONVERSIONDATE, pos.Issue_ID, substr(pos.Issue_ID, 1, 11) || 'C')
                    else pos.Issue_ID
                end                                                                                                          as \"IssueID\",
                case pos.MAPPINGTYPE
                    when 'BONDFIXTOFLOAT' then nvl2(pos.CONVERSIONDATE, 'false', 'true')
                    else decode(cpn.S_COUPN_COUPNTYPE_CODE, null, null, 'FLOAT', 'true', 'false')
                end                                                                                                          as \"isFloat\",
                greatest(0.0001,
                        abs(case
                            when pos.MAPPINGTYPE like 'BOND%' or pos.MAPPINGTYPE like '%BACKEDSECURITY' or pos.MAPPINGTYPE like 'COLLATERALISED%OBLIGA%' 
                                 then 100 * pos.AMMARKETVALUECLEAN_ORIG
                            when pos.MAPPINGTYPE = 'REALESTATEDIRECT' then pos.AMMARKETVALUE_ORIG / 10
                            else pos.AMMARKETVALUE_ORIG end 
                            /
                            greatest(1, abs(nvl(nvl(case when pos.MAPPINGTYPE like '%HEDGE%' then pos.AMMARKETVALUE_ORIG else pos.AMNUMBEROFUNITS end,
                                                    pos.AMNOMINALVALUE_ORIG), 1.0)))))                                       as PRICEQUOTEDORIG,
                pos.\"UnderlyingID\",
                nvl(case when pos.MAPPINGTYPE = 'SWAPEQUITY' then leg.LEGCOUPONINDEXVAL else pos.S_UNDRL_PRICE end, 0.0001)  as S_UNDRL_PRICE,
                case when pos.\"EQBenchmark\" is not null then
                    case
                    when pos.MAPPINGTYPE in ('EQUITYPRIVATE', 'PARTICIPATIONCONSOLIDATED', 'HEDGEFUND', 'FUNDOFHEDGEFUNDS') then 2
                    when pos.\"EQBenchmark\" = 'HFRXGL' then 2
                    when pos.\"EQBenchmark\" = 'SLHN'   then 1
                    else 1
                    end
                else null end                                                                                                as BETA,
                pos.calibrationDate,
                pos.\"EQBenchmark\",
                decode(pos.\"EQBenchmark\", null, 'false', 'true') isEquityType,
                case
                when pos.\"EQBenchmark\" is not null then
                    case
                    when regexp_like(pos.\"EQBenchmark\", 'SLHN')        then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'HSBC')        then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'CHDOM.*')     then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'LBEA.*')      then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'LBED.*')      then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'LBSA.*')      then 'GBP'
                    when regexp_like(pos.\"EQBenchmark\", 'MXCH.*')      then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'MXGB.*')      then 'GBP'
                    when regexp_like(pos.\"EQBenchmark\", 'MXJP.*')      then 'JPY'
                    when regexp_like(pos.\"EQBenchmark\", 'MXUS.*')      then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'MXPCJ.*')     then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'MXWO.*')      then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'M[XS]E.*')    then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'MX.*')        then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'CH.*')        then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'EU.*')        then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'US.*')        then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'RECH.*')      then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'REEU.*')      then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'ARTIUSD.*')   then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'CAPHTRUG.*')  then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'SLI.*')       then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'SMI.*')       then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'SWIIT.*')     then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'GGENEUEU.*')  then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'SX5E.*')      then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'GDDLEURO.*')  then 'EUR'
                    when regexp_like(pos.\"EQBenchmark\", 'GDDLSZ.*')    then 'CHF'
                    when regexp_like(pos.\"EQBenchmark\", 'GDDLUK.*')    then 'GBP'
                    when regexp_like(pos.\"EQBenchmark\", 'GDDLUS.*')    then 'USD'
                    when regexp_like(pos.\"EQBenchmark\", 'GDDUPXJ.*')   then 'USD'
                    else 'USD' end
                else null
                end                                                                                                          as BenchCCY
            from (select distinct
                    first_value(pos.SIGNATURE)   over (partition by pos.SITUATIONDATE, pos.Issue_ID
                                                       order by mu.code, pos.UPID asc rows unbounded preceding) as SIGNATURE,
                    first_value(pos.POSITION_RN) over (partition by pos.SITUATIONDATE, pos.Issue_ID
                                                       order by mu.code, pos.UPID asc rows unbounded preceding) as POSITION_RN
                  from pos
                  join (select decode(o.code, 'SLGROUP', 'SLHN', o.code)                           as \"AccountingArea\",
                               decode(o.code, 'CH', 0, 'DE', 1, 'LU', 2, 'FR', 3, 'SLGROUP', 4, 5) as code
                        from BLUEPRINT.ORGUNIT o where o.ismarketunit = 1) mu using (\"AccountingArea\")
                  where pos.Issue_ID is not null)
            join pos using (SIGNATURE, POSITION_RN)
            left join cpn using (SIGNATURE, POSITION_RN)
            left join leg using (SIGNATURE, POSITION_RN))
   ,trd as (select *
            from pos
            join sel using (SIGNATURE, SITUATIONDATE)
            left join (select SITUATIONDATE, Issue_ID, \"IssueID\", \"isFloat\", BenchCCY from iss) using (SITUATIONDATE, Issue_ID))
select p.*, iss.calibrationDate
from (select issueid, price, beta, \"EQBenchmark\", SITUATIONDATE
      from (select distinct \"UnderlyingID\" as issueid from iss
            minus
            select \"IssueID\" as issueid from iss)
      join (select \"UnderlyingID\" as issueid, sum(S_UNDRL_PRICE)/count(S_UNDRL_PRICE) as price, max(beta) as beta,
                   max(\"EQBenchmark\") as \"EQBenchmark\", SITUATIONDATE
            from iss group by \"UnderlyingID\", SITUATIONDATE) using (issueid)
      union
      select iss.\"IssueID\" as issueid, iss.PRICEQUOTEDORIG as price, iss.beta, \"EQBenchmark\", SITUATIONDATE
      from iss where \"UnderlyingID\" is null) p
left join iss on iss.\"IssueID\" = p.issueid and iss.SITUATIONDATE = p.SITUATIONDATE
left join trd on 0=1
order by SITUATIONDATE, \"EQBenchmark\", issueid
")


(defun query-prices (situationdate)
  (exec-query *bp* (format nil *price-query* situationdate)))

