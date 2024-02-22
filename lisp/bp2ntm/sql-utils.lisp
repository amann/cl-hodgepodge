(in-package #:adaptiv)



(let* ((args '(:columns (("EquityPrice" type)
                         (|i.IssueID| id)
                         (|mp.SpotPrice| point)
                         ("Log" return)
                         (1 drift)
                         (1 beta)
                         (|i.Underlying| index))
               :tables ((ISSUE |i|)
                        (|[ADAPTIV].[dbo].[RioMarketParameters]| mp))
               :join ((= |i.IssueID| |mp.Issue|))
               :where ((= |i.IssueType| "Div")))))
  (labels (
           (sql (formatter "select~%~:{~4T~A~@[ as ~A_~]~^,~%~}~%from~%~:{~4T~A~@[ as ~A_~]~^,~%~}~%where~%~A~%and~%~A"))))
  (defun get-equity-issueid-benchmark ()
    (query sql :database (connect-to :mfiles))))

(let ((format-string "select~%~:{~4T~A~@[ as ~A_~]~^,~%~}~%from~%~:{~4T~A~@[ as ~A_~]~^,~%~}~%where~%~A~%and~%~A"))
  (defun sql-select (&key columns tables join where database)
    (let ((columns (mapcar #'(lambda (x)
                               (mapcar #'to-sql
                                       (etypecase x
                                         (atom (list x nil))
                                         (cons x))))
                           columns))
          (tables (mapcar #'(lambda (x)
                               (mapcar #'to-sql
                                       (etypecase x
                                         (atom (list x nil))
                                         (cons x))))
                          tables))
          (join (to-sql join))
          (where (to-sql where))
          (database (etypecase database
                      (keyword (connect-to database))
                      (database database))))
      (format t format-string columns tables join where))))


(sql-select :columns '(("EquityPrice" type)
                       (|i.IssueID| id)
                       (|mp.SpotPrice| point)
                       ("Log" return)
                       (1 drift)
                       beta
                       (|i.Underlying| index))
               :tables '((ISSUE |i|)
                        (|[ADAPTIV].[dbo].[RioMarketParameters]| mp))
               :join '((= |i.IssueID| |mp.Issue|))
               :where '((= |i.IssueType| "Div"))
               :database :adaptiv)
