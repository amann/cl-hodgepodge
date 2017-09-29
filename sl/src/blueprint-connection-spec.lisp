(setf *connection-specs*
  '((:pav    . (:Driver "{Oracle in OraHome920}"
                :hostname "orapro14"
                :dbname "p014"
                :port 1530
                :dbq "p014.swisslife.ch"
                :Uid "ADAPTIV_READ"
                :Pwd "pada14p"))
    (:uat    . (:Driver "{Oracle in OraHome920}"
                :hostname "oraact14"
                :dbname "q014"
                :port 1530
                :dbq "q014.swisslife.ch"
                :Uid "ADAPTIV_READ"
                :Pwd "adaq14"))
    (:uat-mu . (:Driver "{Oracle in OraHome920}"
                :hostname "oraact13"
                :dbname "q014mu"
                :port 1530
                :dbq "q014mu.swisslife.ch"
                :Uid "ADAPTIV_READ"
                :Pwd "adaq14mu"))))
