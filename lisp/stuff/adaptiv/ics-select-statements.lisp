(in-package #:ics)


"select
    indexID as \"IndexID\",
    case
    when regexp_like(indexID, 'CHDOM.*') then 'CHF'
    when regexp_like(indexID, 'LBEA.*') then 'EUR'
    when regexp_like(indexID, 'LBED.*') then 'USD'
    when regexp_like(indexID, 'LBSA.*') then 'GBP'
    when regexp_like(indexID, 'MXCH.*') then 'CHF'
    when regexp_like(indexID, 'MXGB.*') then 'GBP'
    when regexp_like(indexID, 'MXJP.*') then 'JPY'
    when regexp_like(indexID, 'MXUS.*') then 'USD'
    when regexp_like(indexID, 'MXPCJ.*') then 'USD'
    when regexp_like(indexID, 'MXWO.*') then 'USD'
    when regexp_like(indexID, 'M[XS]E.*') then 'EUR'
    when regexp_like(indexID, 'CH.*') then 'CHF'
    when regexp_like(indexID, 'EU.*') then 'EUR'
    when regexp_like(indexID, 'US.*') then 'USD'
    when regexp_like(indexID, 'RECH.*') then 'CHF'
    when regexp_like(indexID, 'REEU.*') then 'EUR'
    when regexp_like(indexID, 'ARTIUSD.*') then 'USD'            
    when regexp_like(indexID, 'CAPHTRUG.*') then 'USD'            
    when regexp_like(indexID, 'SLI.*') then 'CHF'            
    when regexp_like(indexID, 'SMI.*') then 'CHF'            
    when regexp_like(indexID, 'SWIIT.*') then 'CHF'            
    when regexp_like(indexID, 'GGENEUEU.*') then 'EUR'            
    when regexp_like(indexID, 'SX5E.*') then 'EUR'            
    when regexp_like(indexID, 'GDDLEURO.*') then 'EUR'            
    when regexp_like(indexID, 'GDDLSZ.*') then 'CHF'            
    when regexp_like(indexID, 'GDDLUK.*') then 'GBP'            
    when regexp_like(indexID, 'GDDLUS.*') then 'USD'
    when regexp_like(indexID, 'GDDUPXJ.*') then 'USD'
    else 'USD' end as ccy
from (select distinct
    case when pos.MAPPINGTYPE in ('ALTERNATIVEINVESTMENTSOTHER',
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
        when sig.DELIVERYTYPE = 'RPF' then nvl2(pos.S_UNDRL_NAME, substr(pos.S_UNDRL_ISIN, 4, 4), nvl(pos.BENCHMARKINDEX, substr(pos.ISIN, 4, 4)))
        when pos.MAPPINGTYPE in ('EQUITYPRIVATE', 'PARTICIPATIONSTRATEGIC') then 'MXWO'
        when pos.MAPPINGTYPE in ('HEDGEFUND', 'FUNDOFHEDGEFUNDS') then 'HFRXGL'
        when pos.MAPPINGTYPE like '%REALESTATE%' then 'RE' || decode(nvl(pos.S_RLEST_COUNTRY_CODE, sig.ORGUNIT), 'CH', 'CH', 'EU')
        else nvl2(pos.S_UNDRL_NAME, nvl2(bbt.BLOOMBERGTICKER, null, pos.S_UNDRL_BENCHMARK_CODE), nvl(pos.BENCHMARKINDEX, decode(pos.RISKFACTORID,
            'Swiss Life N', 'SLHN',
            'MSCI BELGIUM', 'MXBE',
            'MSCI EURO', 'MSER',
            'MSCI EUROPE', 'MXEU',
            'MSCI FRANCE', 'MXFR',
            'MSCI GERMANY', 'MXDE',
            'MSCI JAPAN', 'MXJP',
            'MSCI NETHERLANDS', 'MXNL',
            'MSCI PACIFIC EX JAPAN', 'MXPCJ',
            'MSCI SWITZERLAND', 'MXCH',
            'MSCI UK', 'MXGB',
            'MSCI USA', 'MXUS',
            'MSCI WORLD', 'MXWO', 'MXWO'))) end), '([[:alnum:]]{3}[[:alpha:]]*([[:space:]]+[[:alpha:]]+)*?)[[:space:]]*[[:digit:]]?.*(INDEX|EQUITY)?', '\1')
    else null end                                                                        as indexID
    from (select last_day(to_date('09-06', 'yy-mm')) as sitDate from dual) aux
    join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig on sig.CLOSING = 0 and sig.SITUATIONDATE = aux.sitDate
    join BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos on pos.SIGNATURE = sig.ID and pos.ISREXP_POS = 1 and (pos.PFPORTFOLIOTYPE = 'OR' or pos.BASKETTYPE_LOCAL_CODE = 'SLINDEX')
    left join BLUEPRINT.ADAPTIV_DMAEXP_UPIDTRANS pid on pid.COMPANYCODE = sig.COMPANYCODE and pid.UPID = pos.UPID
    join BLUEPRINT.ORGUNIT org on org.CODE = sig.ORGUNIT
    left join BLUEPRINT.MD_BLOOMBERG_NAMETRANS bbt on bbt.NAME = pos.S_UNDRL_NAME and bbt.ORGUNIT = org.ID)
where indexID is not null
"


(defvar *equity-type-instruments* '("ALTERNATIVEINVESTMENTSOTHER" 
                                    "COMMODITY" "EQUITY" "EQUITYPRIVATE" 
                                    "FUNDBALANCED" "FUNDBOND" "FUNDEQUITYPRIVATE" "FUNDEQUITY" "FUNDMONEYMARKET" "FUNDOFHEDGEFUNDS" "FUNDREALESTATE" "HEDGEFUND" 
                                    "FORWARDEQUITY" "FORWARDEQUITYVARIANCE" "FORWARDEQUITYVOLATILITY" "FUTUREEQUITYINDEX" 
                                    "OPTIONEQUITYBARRIER" "OPTIONEQUITYFIXEDSTRIKELOOKB" "OPTIONEQUITYFLOATSTRIKELOOKB" "OPTIONEQUITYPLAINVANILLA" 
                                    "PARTICIPATIONCONSOLIDATED" "PARTICIPATIONSTRATEGIC" 
                                    "REALESTATECOMPANY" "REALESTATEDIRECT" 
                                    "STRUCTUREDDEBT" "STRUCTUREDEQUITY" "STRUCTUREDOTHER" 
                                    "SWAPEQUITY" "SWAPEQUITYINDEXVARIANCE" "SWAPEQUITYINDEXVOLATILITY"))



(defun from-aux (&optional situation-date)
  (format nil "from (select last_day(~:[add_months(sysdate, -1)~;~:*to_date('~A', 'yy-mm')~]) as sitDate from dual) aux"
          situation-date))
(defun join-sig-pos ()
  "    join BLUEPRINT.ADAPTIV_DMAEXP_SIGNATURE sig on sig.CLOSING = 0 and sig.SITUATIONDATE = aux.sitDate
    join BLUEPRINT.ADAPTIV_DMAEXP_POSITION pos on pos.SIGNATURE = sig.ID and pos.ISREXP_POS = 1 and (pos.PFPORTFOLIOTYPE = 'OR' or pos.BASKETTYPE_LOCAL_CODE = 'SLINDEX')")

(defun join-pid ()
  "    left join BLUEPRINT.ADAPTIV_DMAEXP_UPIDTRANS pid on pid.COMPANYCODE = sig.COMPANYCODE and pid.UPID = pos.UPID")

(defun join-bbt ()
  "    join BLUEPRINT.ORGUNIT org on org.CODE = sig.ORGUNIT
    left join BLUEPRINT.MD_BLOOMBERG_NAMETRANS bbt on bbt.NAME = pos.S_UNDRL_NAME and bbt.ORGUNIT = org.ID")



(defun column-index-id ()
  (format nil "~
    case when pos.MAPPINGTYPE in ~A
    then regexp_replace(upper(
        case 
        when sig.DELIVERYTYPE = 'RPF' then nvl2(pos.S_UNDRL_NAME, substr(pos.S_UNDRL_ISIN, 4, 4), nvl(pos.BENCHMARKINDEX, substr(pos.ISIN, 4, 4)))
        when pos.MAPPINGTYPE in ('EQUITYPRIVATE', 'PARTICIPATIONSTRATEGIC') then 'MXWO'
        when pos.MAPPINGTYPE in ('HEDGEFUND', 'FUNDOFHEDGEFUNDS') then 'HFRXGL'
        when pos.MAPPINGTYPE like '%REALESTATE%' then 'RE' || decode(nvl(pos.S_RLEST_COUNTRY_CODE, sig.ORGUNIT), 'CH', 'CH', 'EU')
        else nvl2(pos.S_UNDRL_NAME, nvl2(bbt.BLOOMBERGTICKER, null, pos.S_UNDRL_BENCHMARK_CODE), nvl(pos.BENCHMARKINDEX, decode(pos.RISKFACTORID,
            'Swiss Life N', 'SLHN',
            'MSCI BELGIUM', 'MXBE',
            'MSCI EURO', 'MSER',
            'MSCI EUROPE', 'MXEU',
            'MSCI FRANCE', 'MXFR',
            'MSCI GERMANY', 'MXDE',
            'MSCI JAPAN', 'MXJP',
            'MSCI NETHERLANDS', 'MXNL',
            'MSCI PACIFIC EX JAPAN', 'MXPCJ',
            'MSCI SWITZERLAND', 'MXCH',
            'MSCI UK', 'MXGB',
            'MSCI USA', 'MXUS',
            'MSCI WORLD', 'MXWO', 'MXWO'))) end), '([[:alnum:]]{3}[[:alpha:]]*([[:space:]]+[[:alpha:]]+)*?)[[:space:]]*[[:digit:]]?.*(INDEX|EQUITY)?', '\1')
    else null end"
          *equity-type-instruments*))
