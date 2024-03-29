;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")


(define-foreign-library :odbc 
    (:windows (:default "odbc32"))
  (:unix (:default "libodbc")))

(load-foreign-library :odbc)


;;;;* Structure and Enumeration Types
;;;;** Transfer Types for DATE, TIME, TIMESTAMP

(cffi:defcstruct $::DATE-STRUCT
  (year   #.$::SQLSMALLINT)
  (month  #.$::SQLUSMALLINT)
  (day    #.$::SQLUSMALLINT))
#-odbc2
(defconstant $::SQL-DATE-STRUCT '$::DATE-STRUCT)

(cffi:defcstruct $::TIME-STRUCT
  (hour   #.$::SQLUSMALLINT)
  (minute #.$::SQLUSMALLINT)
  (second #.$::SQLUSMALLINT))
#-odbc2
(defconstant $::SQL-TIME-STRUCT '$::TIME-STRUCT)

(cffi:defcstruct $::TIMESTAMP-STRUCT
  (year     #.$::SQLSMALLINT)
  (month    #.$::SQLUSMALLINT)
  (day      #.$::SQLUSMALLINT)
  (hour     #.$::SQLUSMALLINT)
  (minute   #.$::SQLUSMALLINT)
  (second   #.$::SQLUSMALLINT)
  (fraction #.$::SQLUINTEGER))
#-odbc2
(defconstant $::SQL-TIMESTAMP-STRUCT '$::TIMESTAMP-STRUCT)

;;;;** Enumerations for DATETIME_INTERVAL_SUBCODE values for Interval Data Types
;;;; These values are from SQL-92
#-odbc2
(progn
  (cffi:defcenum $::SQLINTERVAL
    :SQL-IS-YEAR
    :SQL-IS-MONTH
    :SQL-IS-DAY
    :SQL-IS-HOUR
    :SQL-IS-MINUTE
    :SQL-IS-SECOND
    :SQL-IS-YEAR-TO-MONTH
    :SQL-IS-DAY-TO-HOUR
    :SQL-IS-DAY-TO-MINUTE
    :SQL-IS-DAY-TO-SECOND
    :SQL-IS-HOUR-TO-MINUTE
    :SQL-IS-HOUR-TO-SECOND
    :SQL-IS-MINUTE-TO-SECOND)
  (cffi:defcstruct $::SQL-YEAR-MONTH-STRUCT
    (year  #.$::SQLUINTEGER)
    (month #.$::SQLUINTEGER))
  (cffi:defcstruct $::SQL-DAY-SECOND-STRUCT
    (day      #.$::SQLUINTEGER)
    (hour     #.$::SQLUINTEGER)
    (minute   #.$::SQLUINTEGER)
    (second   #.$::SQLUINTEGER)
    (fraction #.$::SQLUINTEGER))
  (cffi:defcunion $::interval-value-struct
    (year-month $::SQL-YEAR-MONTH-STRUCT)
    (day-second $::SQL-DAY-SECOND-STRUCT))
  (cffi:defcstruct $::SQL-INTERVAL-STRUCT
    (interval-type   $::SQLINTERVAL)
    (interval-sign  #.$::SQLSMALLINT)
    (interval-value  $::interval-value-struct)))

;;;;** ODBCINT64

#-odbc2
(progn
  (defconstant $::ODBCINT64  :int64)
  (defconstant $::SQLBIGINT  $::ODBCINT64)
  (defconstant $::SQLUBIGINT :uint64))

;;;;** Internal Representation of Numeric Data Type
#-odbc2
(progn

  (cffi:defcstruct $::SQL-NUMERIC-STRUCT
    (precision #.$::SQLCHAR)
    (scale     #.$::SQLSCHAR)
    (sign      #.$::SQLCHAR)
    (val       #.$::SQLCHAR :count #.$::SQL_MAX_NUMERIC_LEN))
  (cffi:defcstruct SQLGUID
    (data1 #.$::SDWORD)
    (data2 #.$::UWORD)
    (data3 #.$::UWORD)
    (data4 #.$::UCHAR :count 8)))

(defconstant $::BOOKMARK $::SQLULEN)
(defconstant $::SQLWCHAR :ushort)
#+sb-unicode
(defconstant $::SQLTCHAR $::SQLWCHAR)
#-sb-unicode
(defconstant $::SQLTCHAR SQLCHAR)




;;;;* SQL API Functions

(defmacro defsqlfun (sql-name &body doc&args)
  `(cffi:defcfun (,(string sql-name) ,(intern (string-upcase sql-name)))
       #.$::SQLRETURN
     ,@doc&args))

;;;;* Handles

#-odbc2
(progn
 (defsqlfun "SQLAllocHandle"
   (HandleType    #.$::SQLSMALLINT)
   (InputHandle   #.$::SQLHANDLE)
   (*OutputHandle #.$::*SQLHANDLE))

 (defsqlfun "SQLCancelHandle"
   (HandleType  #.$::SQLSMALLINT)
   (InputHandle #.$::SQLHANDLE))

 (defsqlfun "SQLFreeHandle"
   (HandleType #.$::SQLSMALLINT)
   (Handle     #.$::SQLHANDLE)))

(defsqlfun "SQLFreeStmt"
  (StatementHandle #.$::SQLHSTMT)
  (Option          #.$::SQLUSMALLINT))

;;;;** Deprecated in ODBC 3
#+odbc2
(progn
 (defsqlfun "SQLAllocEnv"
   (*EnvironmentHandle #.$::*SQLHENV))
 (defsqlfun "SQLAllocConnect"
   (EnvironmentHandle #.$::SQLHENV)
   (*ConnectionHandle #.$::*SQLHDBC))
 (defsqlfun "SQLAllocStmt"
   (ConnectionHandle #.$::SQLHDBC)
   (*StatementHandle #.$::*SQLHSTMT))

 (defsqlfun "SQLFreeConnect"
   (ConnectionHandle #.$::SQLHDBC))
 (defsqlfun "SQLFreeEnv"
   (EnvironmentHandle #.$::SQLHENV)))


;;;;* Diagnostics


#-odbc2
(progn
  (defsqlfun "SQLGetDiagField"
    (HandleType     #.$::SQLSMALLINT)
    (Handle         #.$::SQLHANDLE)
    (RecNumber      #.$::SQLSMALLINT)
    (DiagIdentifier #.$::SQLSMALLINT)
    (DiagInfo       #.$::SQLPOINTER)
    (BufferLength   #.$::SQLSMALLINT)
    (*StringLength  #.$::*SQLSMALLINT))
  (defsqlfun "SQLGetDiagRec"
    (HandleType   #.$::SQLSMALLINT)
    (Handle       #.$::SQLHANDLE)
    (RecNumber    #.$::SQLSMALLINT)
    (*Sqlstate    #.$::*SQLCHAR)
    (*NativeError #.$::*SQLINTEGER)
    (*MessageText #.$::*SQLCHAR)
    (BufferLength #.$::SQLSMALLINT)
    (*TextLength  #.$::*SQLSMALLINT)))


#+odbc2
(defsqlfun "SQLError"
  (EnvironmentHandle #.$::SQLHENV)
  (ConnectionHandle  #.$::SQLHDBC)
  (StatementHandle   #.$::SQLHSTMT)
  (*Sqlstate         #.$::*SQLCHAR)
  (*NativeError      #.$::*SQLINTEGER)
  (*MessageText      #.$::*SQLCHAR)
  (BufferLength      #.$::SQLSMALLINT)
  (*TextLength       #.$::*SQLSMALLINT))


;;;;* Using a Environment Handle
;;;;** Attributes
#-odbc2
(progn
  (defsqlfun "SQLGetEnvAttr"
    (EnvironmentHandle #.$::SQLHENV)
    (Attribute         #.$::SQLINTEGER)
    (Value             #.$::SQLPOINTER)
    (BufferLength      #.$::SQLINTEGER)
    (*StringLength     #.$::*SQLINTEGER))
  (defsqlfun "SQLSetEnvAttr"
    (EnvironmentHandle #.$::SQLHENV)
    (Attribute         #.$::SQLINTEGER)
    (Value             #.$::SQLPOINTER)
    (StringLength      #.$::SQLINTEGER)))

;;;;** Meta Information

(defsqlfun "SQLDataSources"
  (EnvironmentHandle #.$::SQLHENV)
  (Direction         #.$::SQLUSMALLINT)
  (*ServerName       #.$::*SQLCHAR)
  (BufferLength1     #.$::SQLSMALLINT)
  (*NameLength1Ptr   #.$::*SQLSMALLINT)
  (*Description      #.$::*SQLCHAR)
  (BufferLength2     #.$::SQLSMALLINT)
  (*NameLength2Ptr   #.$::*SQLSMALLINT))

(defsqlfun "SQLDrivers"
  (henv                #.$::SQLHENV)
  (fDirection          #.$::SQLUSMALLINT)
  (*szDriverDesc       #.$::*SQLCHAR)
  (cchDriverDescMax    #.$::SQLSMALLINT)
  (*pcchDriverDesc     #.$::*SQLSMALLINT)
  (*szDriverAttributes #.$::*SQLCHAR)
  (cchDrvrAttrMax      #.$::SQLSMALLINT)
  (*pcchDrvrAttr       #.$::*SQLSMALLINT))

;;;;* Using a Descriptor Handle
;;;; Descriptor handles exist only since ODBC v3.0

#-odbc2
(progn
  (defsqlfun "SQLCopyDesc"
    (SourceDescHandle #.$::SQLHDESC)
    (TargetDescHandle #.$::SQLHDESC))

  (defsqlfun "SQLGetDescField"
    (DescriptorHandle #.$::SQLHDESC)
    (RecNumber        #.$::SQLSMALLINT)
    (FieldIdentifier  #.$::SQLSMALLINT)
    (Value            #.$::SQLPOINTER)
    (BufferLength     #.$::SQLINTEGER)
    (*StringLength    #.$::*SQLINTEGER))
  (defsqlfun "SQLSetDescField"
    (DescriptorHandle #.$::SQLHDESC)
    (RecNumber        #.$::SQLSMALLINT)
    (FieldIdentifier  #.$::SQLSMALLINT)
    (Value            #.$::SQLPOINTER)
    (BufferLength     #.$::SQLINTEGER))

  (defsqlfun "SQLGetDescRec"
    (DescriptorHandle #.$::SQLHDESC)
    (RecNumber        #.$::SQLSMALLINT)
    (*Name            #.$::*SQLCHAR)
    (BufferLength     #.$::SQLSMALLINT)
    (*StringLengthPtr #.$::*SQLSMALLINT)
    (*TypePtr         #.$::*SQLSMALLINT)
    (*SubTypePtr      #.$::*SQLSMALLINT)
    (*LengthPtr       #.$::*SQLLEN)
    (*PrecisionPtr    #.$::*SQLSMALLINT)
    (*ScalePtr        #.$::*SQLSMALLINT)
    (*NullablePtr     #.$::*SQLSMALLINT))
  (defsqlfun "SQLSetDescRec"
    (DescriptorHandle #.$::SQLHDESC)
    (RecNumber        #.$::SQLSMALLINT)
    (Type             #.$::SQLSMALLINT)
    (SubType          #.$::SQLSMALLINT)
    (Length           #.$::SQLLEN)
    (Precision        #.$::SQLSMALLINT)
    (Scale            #.$::SQLSMALLINT)
    (Data             #.$::SQLPOINTER)
    (*StringLength    #.$::*SQLLEN)
    (*Indicator       #.$::*SQLLEN)))

;;;;* Using a Connection Handle
;;;;** Attributes
#-odbc2
(progn
  (defsqlfun "SQLGetConnectAttr"
    (ConnectionHandle #.$::SQLHDBC)
    (Attribute        #.$::SQLINTEGER)
    (Value            #.$::SQLPOINTER)
    (BufferLength     #.$::SQLINTEGER)
    (*StringLengthPtr #.$::*SQLINTEGER))
  (defsqlfun "SQLSetConnectAttr"
    (ConnectionHandle #.$::SQLHDBC)
    (Attribute        #.$::SQLINTEGER)
    (Value            #.$::SQLPOINTER)
    (StringLength     #.$::SQLINTEGER)))

#+odbc2
(progn
  (defsqlfun "SQLGetConnectOption"
    (ConnectionHandle #.$::SQLHDBC)
    (Option           #.$::SQLUSMALLINT)
    (Value            #.$::SQLPOINTER))
  (defsqlfun "SQLSetConnectOption"
    (ConnectionHandle #.$::SQLHDBC)
    (Option           #.$::SQLUSMALLINT)
    (Value            #.$::SQLULEN)))

;;;;** Connect Functions
(defsqlfun "SQLConnect"
  (ConnectionHandle #.$::SQLHDBC)
  (*ServerName      #.$::*SQLCHAR)
  (NameLength1      #.$::SQLSMALLINT)
  (*UserName        #.$::*SQLCHAR)
  (NameLength2      #.$::SQLSMALLINT)
  (*Authentication  #.$::*SQLCHAR)
  (NameLength3      #.$::SQLSMALLINT))

(defsqlfun "SQLDriverConnect"
  (hdbc              #.$::SQLHDBC)
  (hwnd              #.$::SQLHWND)
  (*szConnStrIn      #.$::*SQLCHAR)
  (cchConnStrIn      #.$::SQLSMALLINT)
  (*szConnStrOut     #.$::*SQLCHAR)
  (cchConnStrOutMax  #.$::SQLSMALLINT)
  (*pcchConnStrOut   #.$::*SQLSMALLINT)
  (fDriverCompletion #.$::SQLUSMALLINT))

(defsqlfun "SQLBrowseConnect"
  (hdbc             #.$::SQLHDBC)
  (*szConnStrIn     #.$::*SQLCHAR)
  (cchConnStrIn     #.$::SQLSMALLINT)
  (*szConnStrOut    #.$::*SQLCHAR)
  (cchConnStrOutMax #.$::SQLSMALLINT)
  (*pcchConnStrOut  #.$::*SQLSMALLINT))

(defsqlfun "SQLDisconnect"
    (ConnectionHandle #.$::SQLHDBC))

#-odbc2
(defsqlfun "SQLEndTran"
  (HandleType     #.$::SQLSMALLINT)
  (Handle         #.$::SQLHANDLE)
  (CompletionType #.$::SQLSMALLINT))
#+odbc2
(defsqlfun "SQLTransact"
  (EnvironmentHandle #.$::SQLHENV)
  (ConnectionHandle  #.$::SQLHDBC)
  (CompletionType    #.$::SQLUSMALLINT))

(defsqlfun "SQLGetFunctions"
  (ConnectionHandle #.$::SQLHDBC)
  (FunctionId       #.$::SQLUSMALLINT)
  (*Supported       #.$::*SQLUSMALLINT))

(defsqlfun "SQLGetInfo"
  (ConnectionHandle #.$::SQLHDBC)
  (InfoType         #.$::SQLUSMALLINT)
  (InfoValue        #.$::SQLPOINTER)
  (BufferLength     #.$::SQLSMALLINT)
  (*StringLengthPtr #.$::*SQLSMALLINT))

;;;;* Using a Statement Handle
;;;;** Attributes
#-odbc2
(progn
  (defsqlfun "SQLGetStmtAttr"
    (StatementHandle #.$::SQLHSTMT)
    (Attribute       #.$::SQLINTEGER)
    (Value           #.$::SQLPOINTER)
    (BufferLength    #.$::SQLINTEGER)
    (*StringLength   #.$::*SQLINTEGER))
  (defsqlfun "SQLSetStmtAttr"
    (StatementHandle #.$::SQLHSTMT)
    (Attribute       #.$::SQLINTEGER)
    (Value           #.$::SQLPOINTER)
    (StringLength    #.$::SQLINTEGER)))

#+odbc2
(progn
  (defsqlfun "SQLGetStmtOption"
    (StatementHandle #.$::SQLHSTMT)
    (Option          #.$::SQLUSMALLINT)
    (Value           #.$::SQLPOINTER))
  (defsqlfun "SQLSetStmtOption"
    (StatementHandle #.$::SQLHSTMT)
    (Option          #.$::SQLUSMALLINT)
    (Value           #.$::SQLULEN)))

;;;;** Execute/Prepare

(defsqlfun "SQLPrepare"
  (StatementHandle #.$::SQLHSTMT)
  (*StatementText  #.$::*SQLCHAR)
  (TextLength      #.$::SQLINTEGER))

(defsqlfun "SQLExecDirect"
  (StatementHandle #.$::SQLHSTMT)
  (*StatementText  #.$::*SQLCHAR)
  (TextLength      #.$::SQLINTEGER))

(defsqlfun "SQLExecute"
  (StatementHandle #.$::SQLHSTMT))

(defsqlfun "SQLCancel"
  (StatementHandle #.$::SQLHSTMT))

;;;;** Cursors

(defsqlfun "SQLGetCursorName"
  (StatementHandle #.$::SQLHSTMT)
  (*CursorName     #.$::*SQLCHAR)
  (BufferLength    #.$::SQLSMALLINT)
  (*NameLengthPtr  #.$::*SQLSMALLINT))

(defsqlfun "SQLSetCursorName "
  (StatementHandle #.$::SQLHSTMT)
  (*CursorName     #.$::*SQLCHAR)
  (NameLength      #.$::SQLSMALLINT))

#-odbc2
(defsqlfun "SQLCloseCursor"
  (StatementHandle #.$::SQLHSTMT))

;;;;** Columns

(defsqlfun "SQLBindCol"
  (StatementHandle #.$::SQLHSTMT)
  (ColumnNumber    #.$::SQLUSMALLINT)
  (TargetType      #.$::SQLSMALLINT)
  (TargetValue     #.$::SQLPOINTER)
  (BufferLength    #.$::SQLLEN)
  (*StrLen_or_Ind  #.$::*SQLLEN))

(defsqlfun "SQLDescribeCol"
  (StatementHandle #.$::SQLHSTMT)
  (ColumnNumber    #.$::SQLUSMALLINT)
  (*ColumnName     #.$::*SQLCHAR)
  (BufferLength    #.$::SQLSMALLINT)
  (*NameLength     #.$::*SQLSMALLINT)
  (*DataType       #.$::*SQLSMALLINT)
  (*ColumnSize     #.$::*SQLULEN)
  (*DecimalDigits  #.$::*SQLSMALLINT)
  (*Nullable       #.$::*SQLSMALLINT))

#-odbc2
(progn
 #+x86-64
 (defsqlfun "SQLColAttribute"
   (StatementHandle    #.$::SQLHSTMT)
   (ColumnNumber       #.$::SQLUSMALLINT)
   (FieldIdentifier    #.$::SQLUSMALLINT)
   (CharacterAttribute #.$::SQLPOINTER)
   (BufferLength       #.$::SQLSMALLINT)
   (*StringLength      #.$::*SQLSMALLINT)
   (*NumericAttribute  #.$::*SQLLEN))
 #-x86-64
 (defsqlfun "SQLColAttribute"
   (StatementHandle    #.$::SQLHSTMT)
   (ColumnNumber       #.$::SQLUSMALLINT)
   (FieldIdentifier    #.$::SQLUSMALLINT)
   (CharacterAttribute #.$::SQLPOINTER)
   (BufferLength       #.$::SQLSMALLINT)
   (*StringLength      #.$::*SQLSMALLINT)
   (NumericAttribute   #.$::SQLPOINTER)))

;;;;** Parameters

#-odbc2
(defsqlfun "SQLBindParam"
  (StatementHandle #.$::SQLHSTMT)
  (ParameterNumber #.$::SQLUSMALLINT)
  (ValueType       #.$::SQLSMALLINT)
  (ParameterType   #.$::SQLSMALLINT)
  (LengthPrecision #.$::SQLULEN)
  (ParameterScale  #.$::SQLSMALLINT)
  (ParameterValue  #.$::SQLPOINTER)
  (*StrLen_or_Ind  #.$::*SQLLEN))

#+odbc2
(defsqlfun "SQLSetParam"
  (StatementHandle #.$::SQLHSTMT)
  (ParameterNumber #.$::SQLUSMALLINT)
  (ValueType       #.$::SQLSMALLINT)
  (ParameterType   #.$::SQLSMALLINT)
  (LengthPrecision #.$::SQLULEN)
  (ParameterScale  #.$::SQLSMALLINT)
  (ParameterValue  #.$::SQLPOINTER)
  (*StrLen_or_Ind  #.$::*SQLLEN))

(defsqlfun "SQLBindParameter"
  (hstmt      #.$::SQLHSTMT)
  (ipar       #.$::SQLUSMALLINT)
  (fParamType #.$::SQLSMALLINT)
  (fCType     #.$::SQLSMALLINT)
  (fSqlType   #.$::SQLSMALLINT)
  (cbColDef   #.$::SQLULEN)
  (ibScale    #.$::SQLSMALLINT)
  (rgbValue   #.$::SQLPOINTER)
  (cbValueMax #.$::SQLLEN)
  (*pcbValue  #.$::*SQLLEN))

(defsqlfun "SQLNumResultCols"
  (StatementHandle #.$::SQLHSTMT)
  (*ColumnCount    #.$::*SQLSMALLINT))

(defsqlfun "SQLRowCount"
  (StatementHandle #.$::SQLHSTMT)
  (*RowCount       #.$::*SQLLEN))

(defsqlfun "SQLFetch"
  (StatementHandle #.$::SQLHSTMT))

#-odbc2
(defsqlfun "SQLFetchScroll"
  (StatementHandle  #.$::SQLHSTMT)
  (FetchOrientation #.$::SQLSMALLINT)
  (FetchOffset      #.$::SQLLEN))

(defsqlfun "SQLParamData"
  (StatementHandle #.$::SQLHSTMT)
  (*Value          #.$::*SQLPOINTER))

(defsqlfun "SQLGetData"
  (StatementHandle   #.$::SQLHSTMT)
  (ColumnNumber      #.$::SQLUSMALLINT)
  (TargetType        #.$::SQLSMALLINT)
  (TargetValue       #.$::SQLPOINTER)
  (BufferLength      #.$::SQLLEN)
  (*StrLen_or_IndPtr #.$::*SQLLEN))

(defsqlfun "SQLPutData"
  (StatementHandle #.$::SQLHSTMT)
  (Data            #.$::SQLPOINTER)
  (StrLen_or_Ind   #.$::SQLLEN))

;;;;** Meta Information

(defsqlfun "SQLGetTypeInfo"
  (StatementHandle #.$::SQLHSTMT)
  (DataType        #.$::SQLSMALLINT))

(defsqlfun "SQLStatistics"
  (StatementHandle #.$::SQLHSTMT)
  (*CatalogName    #.$::*SQLCHAR)
  (NameLength1     #.$::SQLSMALLINT)
  (*SchemaName     #.$::*SQLCHAR)
  (NameLength2     #.$::SQLSMALLINT)
  (*TableName      #.$::*SQLCHAR)
  (NameLength3     #.$::SQLSMALLINT)
  (Unique          #.$::SQLUSMALLINT)
  (Reserved        #.$::SQLUSMALLINT))

(defsqlfun "SQLTables"
  (StatementHandle #.$::SQLHSTMT)
  (*CatalogName    #.$::*SQLCHAR)
  (NameLength1     #.$::SQLSMALLINT)
  (*SchemaName     #.$::*SQLCHAR)
  (NameLength2     #.$::SQLSMALLINT)
  (*TableName      #.$::*SQLCHAR)
  (NameLength3     #.$::SQLSMALLINT)
  (*TableType      #.$::*SQLCHAR)
  (NameLength4     #.$::SQLSMALLINT))

(defsqlfun "SQLColumns"
  (StatementHandle #.$::SQLHSTMT)
  (*CatalogName    #.$::*SQLCHAR)
  (NameLength1     #.$::SQLSMALLINT)
  (*SchemaName     #.$::*SQLCHAR)
  (NameLength2     #.$::SQLSMALLINT)
  (*TableName      #.$::*SQLCHAR)
  (NameLength3     #.$::SQLSMALLINT)
  (*ColumnName     #.$::*SQLCHAR)
  (NameLength4     #.$::SQLSMALLINT))

(defsqlfun "SQLSpecialColumns"
  (StatementHandle #.$::SQLHSTMT)
  (IdentifierType  #.$::SQLUSMALLINT)
  (*CatalogName    #.$::*SQLCHAR)
  (NameLength1     #.$::SQLSMALLINT)
  (*SchemaName     #.$::*SQLCHAR)
  (NameLength2     #.$::SQLSMALLINT)
  (*TableName      #.$::*SQLCHAR)
  (NameLength3     #.$::SQLSMALLINT)
  (Scope           #.$::SQLUSMALLINT)
  (Nullable        #.$::SQLUSMALLINT))

;;;;**

#-odbc2
(defsqlfun "SQLBulkOperations"
  (StatementHandle #.$::SQLHSTMT)
  (Operation       #.$::SQLSMALLINT))

(defsqlfun "SQLColAttributes"
  (hstmt     #.$::SQLHSTMT)
  (icol      #.$::SQLUSMALLINT)
  (fDescType #.$::SQLUSMALLINT)
  (rgbDesc   #.$::SQLPOINTER)
  (cbDescMax #.$::SQLSMALLINT)
  (*pcbDesc  #.$::*SQLSMALLINT)
  (*pfDesc   #.$::*SQLLEN))

(defsqlfun "SQLColumnPrivileges"
  (hstmt #.$::SQLHSTMT)
  (*szCatalogName #.$::*SQLCHAR)
  (cchCatalogName #.$::SQLSMALLINT)
  (*szSchemaName  #.$::*SQLCHAR)
  (cchSchemaName  #.$::SQLSMALLINT)
  (*szTableName   #.$::*SQLCHAR)
  (cchTableName   #.$::SQLSMALLINT)
  (*szColumnName  #.$::*SQLCHAR)
  (cchColumnName  #.$::SQLSMALLINT))

(defsqlfun "SQLDescribeParam"
  (hstmt        #.$::SQLHSTMT)
  (ipar         #.$::SQLUSMALLINT)
  (*pfSqlType   #.$::*SQLSMALLINT)
  (*pcbParamDef #.$::*SQLULEN)
  (*pibScale    #.$::*SQLSMALLINT)
  (*pfNullable  #.$::*SQLSMALLINT))

(defsqlfun "SQLExtendedFetch"
  (hstmt         #.$::SQLHSTMT)
  (fFetchType    #.$::SQLUSMALLINT)
  (irow          #.$::SQLLEN)
  (*pcrow        #.$::*SQLULEN)
  (*rgfRowStatus #.$::*SQLUSMALLINT))

(defsqlfun "SQLForeignKeys"
  (hstmt            #.$::SQLHSTMT)
  (*szPkCatalogName #.$::*SQLCHAR)
  (cchPkCatalogName #.$::SQLSMALLINT)
  (*szPkSchemaName  #.$::*SQLCHAR)
  (cchPkSchemaName  #.$::SQLSMALLINT)
  (*szPkTableName   #.$::*SQLCHAR)
  (cchPkTableName   #.$::SQLSMALLINT)
  (*szFkCatalogName #.$::*SQLCHAR)
  (cchFkCatalogName #.$::SQLSMALLINT)
  (*szFkSchemaName  #.$::*SQLCHAR)
  (cchFkSchemaName  #.$::SQLSMALLINT)
  (*szFkTableName   #.$::*SQLCHAR)
  (cchFkTableName   #.$::SQLSMALLINT))

(defsqlfun "SQLMoreResults"
  (hstmt #.$::SQLHSTMT))

(defsqlfun "SQLNativeSql"
  (hdbc         #.$::SQLHDBC)
  (cchSqlStrIn  #.$::SQLINTEGER)
  (cchSqlStrMax #.$::SQLINTEGER)
  (*pcbSqlStr   #.$::*SQLINTEGER))

(defsqlfun "SQLNumParams"
  (hstmt  #.$::SQLHSTMT)
  (*pcpar #.$::*SQLSMALLINT))

(defsqlfun "SQLParamOptions"
  (hstmt  #.$::SQLHSTMT)
  (crow   #.$::SQLULEN)
  (*pirow #.$::*SQLULEN))

(defsqlfun "SQLPrimaryKeys"
  (hstmt          #.$::SQLHSTMT)
  (*szCatalogName #.$::*SQLCHAR)
  (cchCatalogName #.$::SQLSMALLINT)
  (*szSchemaName  #.$::*SQLCHAR)
  (cchSchemaName  #.$::SQLSMALLINT)
  (*szTableName   #.$::*SQLCHAR)
  (cchTableName   #.$::SQLSMALLINT))

(defsqlfun "SQLProcedureColumns"
  (hstmt          #.$::SQLHSTMT)
  (*szCatalogName #.$::*SQLCHAR)
  (cchCatalogName #.$::SQLSMALLINT)
  (*szSchemaName  #.$::*SQLCHAR)
  (cchSchemaName  #.$::SQLSMALLINT)
  (*szProcName    #.$::*SQLCHAR)
  (cchProcName    #.$::SQLSMALLINT)
  (*szColumnName  #.$::*SQLCHAR)
  (cchColumnName  #.$::SQLSMALLINT))

(defsqlfun "SQLProcedures"
  (hstmt          #.$::SQLHSTMT)
  (*szCatalogName #.$::*SQLCHAR)
  (cchCatalogName #.$::SQLSMALLINT)
  (*szSchemaName  #.$::*SQLCHAR)
  (cchSchemaName  #.$::SQLSMALLINT)
  (*szProcName    #.$::*SQLCHAR)
  (cchProcName    #.$::SQLSMALLINT))

(defsqlfun "SQLSetPos"
  (hstmt   #.$::SQLHSTMT)
  (irow    #.$::SQLSETPOSIROW)
  (fOption #.$::SQLUSMALLINT)
  (fLock   #.$::SQLUSMALLINT))

;;;;** Macros for SQLSetPos
(defmacro SQL-POSITION-TO (hstmt irow)
  `(SQLSetPos ,hstmt ,irow #.$::SQL_POSITION #.$::SQL_LOCK_NO_CHANGE))
(defmacro SQL-LOCK-RECORD (hstmt irow fLock)
  `(SQLSetPos ,hstmt ,irow #.$::SQL_POSITION ,fLock))
(defmacro SQL-REFRESH-RECORD (hstmt irow fLock)
  `(SQLSetPos ,hstmt ,irow #.$::SQL_REFRESH ,fLock))
(defmacro SQL-UPDATE-RECORD (hstmt irow)
  `(SQLSetPos ,hstmt ,irow #.$::SQL_UPDATE #.$::SQL_LOCK_NO_CHANGE))
(defmacro SQL-DELETE-RECORD (hstmt irow)
  `(SQLSetPos ,hstmt ,irow #.$::SQL_DELETE #.$::SQL_LOCK_NO_CHANGE))
(defmacro SQL-ADD-RECORD (hstmt irow)
  `(SQLSetPos ,hstmt ,irow #.$::SQL_ADD #.$::SQL_LOCK_NO_CHANGE))


(defsqlfun "SQLTablePrivileges"
  (hstmt #.$::SQLHSTMT)
  (*szCatalogName #.$::*SQLCHAR)
  (cchCatalogName #.$::SQLSMALLINT)
  (*szSchemaName  #.$::*SQLCHAR)
  (cchSchemaName  #.$::SQLSMALLINT)
  (*szTableName   #.$::*SQLCHAR)
  (cchTableName   #.$::SQLSMALLINT))




(do-symbols (sym "$") (export sym "$"))

