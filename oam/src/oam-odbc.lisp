

(setq plain-odbc::*universal-time-to-date-dataype*
      #'date::date-time<-universal-time
      plain-odbc::*date-datatype-to-universal-time*
      #'date::universal-time<-date-time
      plain-odbc:*date-type-predicate*
      #'(lambda (obj) (typep obj 'date:date-time)))



