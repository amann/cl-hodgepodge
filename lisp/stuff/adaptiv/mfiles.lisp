(in-package #:mfiles)
(defconstant +mfiles-connection-specs+ '(("Mfiles" "mgr" "system") :database-type :odbc))


(defun connect-to-mfiles ()
  (apply 'clsql:connect +mfiles-connection-specs+))

(define-stored-procedure (name arglist &key stored-name gives-results)
    (let ((stored-name (or stored-name (substitute #\- #\_ (string-downcase name)))))
      `(defun name arglist
         (,(if gives-results 'query 'execute-command)
           ,@(com.informatimago.common-lisp.source-form:make-argument-list
              (com.informatimago.common-lisp.source-form:parse-lambda-list arglist))))))



(define-stored-procedure sp-blah (a b) :gives-result t)

(dolist (row (sp-blah 1 2))
(print row))

