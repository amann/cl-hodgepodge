(with-open-file (*standard-output* #p"C:/temp/test.aas" :direction :output :if-exists :supersede)
  (to-aas (mapcar #'(lambda (x)
                      (list "EquityPrice" (string-trim " "(first x)) "" "Log" "" 1 (string-trim " " (concatenate 'string "EquityPrice." (second x)))))
                  (get-equity-issueid-benchmark))))




(let ((line-format (format nil "~~:{~~A.~~@{~~A~~^~A~~}~~%~~}" #\Tab)))
 (defun to-aas (item-list)
   (format t line-format (cons '(TYPE ID POINT RETURN DRIFT BETA INDEX) item-list))))

(let* ((args '(:columns (("EquityPrice" type)
                         (|i.IssueID| id)
                         (|mp.SpotPrice| point)
                         ("Log" return)
                         ("" drift)
                         (1 beta)
                         (|i.Underlying| index))
               :table (ISSUE |i|)
               :join (((|[ADAPTIV].[dbo].[RioMarketParameters]| mp) (= |i.IssueID| |mp.Issue|)))
               :where ((= |i.IssueType| "Div"))))
       (sql (formatter "select~%~:{~4T~A~@[ as ~A_~]~^,~%~}~%from ~{~A~^ ~A_~}~%~@[join ~:{~4T~A~@[ ~A_~]~^,~%~}~]~%where~%~A~%and~%~A"))
       (sql-statement "select IssueID, Underlying from ISSUE where IssueType = 'Div'")
       (sql-statement2 "select
    'EquityPrice' as TYPE_,
    i.IssueID as ID_,
    mp.SpotPrice as POINT_,
    'Log' as RETURN_,
    '' as DRIFT_,
    mp.Beta as BETA_,
    i.Underlying as INDEX_
from ISSUE i
join [ADAPTIV].[dbo].[RioMarketParameters] mp on i.IssueID = mp.Issue
where IssueType = 'Div'"))
  (defun get-equity-issueid-benchmark ()
    (query sql-statement :database (connect-to :mfiles))))

