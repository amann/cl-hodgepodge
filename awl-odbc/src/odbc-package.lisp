;;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage "CH.AMANN-WOLOWYK.ODBC-SYSTEM"
  (:use "COMMON-LISP"))

(defpackage "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL-CONSTANTS"
  (:use)
  (:nicknames "$"))
(defpackage "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL"
  (:use "COMMON-LISP" "CFFI")
  (:nicknames "SQL")
  (:documentation "Package containing the low-level ODBC functions and constants."))

(defpackage "CH.AMANN-WOLOWYK.ODBC"
  (:use)
  (:nicknames "ODBC")
  #+ (or)
  (:export
   "EXEC-SQL"
   "EXEC-QUERY" 
   "EXEC-UPDATE" 
   "EXEC-COMMAND"

   "EXEC-SQL*"
   "EXEC-QUERY*" 
   "EXEC-UPDATE*" 
   "EXEC-COMMAND*"

   "PREPARE-STATEMENT" 
   "EXEC-PREPARED-QUERY" 
   "EXEC-PREPARED-UPDATE"
   "EXEC-PREPARED-COMMAND"
   "FREE-STATEMENT" 
   "CONNECT"   

   ;Metadata
   "GET-PRIMARY-KEYS"
   "GET-FOREIGN-KEYS"
   "GET-TABLES"
   "GET-COLUMNS"
   ;"DRIVER-CONNECT"
   "CONNECT-GENERIC"
   "CLOSE-CONNECTION"
   "COMMIT"
   "ROLLBACK"
   
   "WITH-PREPARED-STATEMENT"
   
   "*UNIVERSAL-TIME-TO-DATE-DATAYPE*"
   "*DATE-DATATYPE-TO-UNIVERSAL-TIME*"
   "*DATE-TYPE-PREDICATE*"

   ;; utilities
   "*DEFAULT-ACCESS-DSN*"
   "*DEFAULT-ORACLE-DSN*"
   "*DEFAULT-SQL-SERVER-DSN*"
   "*DEFAULT-MYSQL-DSN*"
   "*DEFAULT-SQLITE-DSN*"

   
   "USE-BIND-COLUMN"

   "CONNECT-ACCESS"
   "CONNECT-SQL-SERVER"
   "CONNECT-ORACLE" 
   "CONNECT-MYSQL"
   "CONNECT-SQLITE" 

   "TRACE-CONNECTION"
   "UNTRACE-CONNECTION"))

