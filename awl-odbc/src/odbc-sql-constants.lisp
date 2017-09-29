;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")



;;;;** Special length/indicator values */
(defconstant $::SQL_NULL_DATA -1)
(defconstant $::SQL_DATA_AT_EXEC -2)


;;;;** Return values from functions
(defconstant $::SQL_SUCCESS 0)
(defconstant $::SQL_SUCCESS_WITH_INFO 1)
#-odbc2
(progn
  (defconstant $::SQL_NO_DATA 100)
  (defconstant $::SQL_PARAM_DATA_AVAILABLE 101))

(defconstant $::SQL_ERROR -1)
(defconstant $::SQL_INVALID_HANDLE -2)

(defconstant $::SQL_STILL_EXECUTING 2)
(defconstant $::SQL_NEED_DATA 99)

;;;;** Test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO
;;;; TODO:
#+ (or) (defmacro $::SQL_SUCCEEDED (rc)
  ((rc)&(~1))==0)

;;;;** Flags for null-terminated string
(defconstant $::SQL_NTS -3)
(defconstant $::SQL_NTSL -3)

;;;;** Maximum message length
(defconstant $::SQL_MAX_MESSAGE_LENGTH 512)

;;;;** Date/time, Numeric length constants
#-odbc2
(progn
  (defconstant $::SQL_NEGATIVE_VALUE #+<odbc3.5 0 #-<odbc3.5 2) ; Stupid Microsoft is unable to write a consistent specification...
  (defconstant $::SQL_MAX_NUMERIC_LEN 16)
  (defconstant $::SQL_DATE_LEN 10)
  (defconstant $::SQL_TIME_LEN 8)       ;; add P+1 if precision is nonzero
  (defconstant $::SQL_TIMESTAMP_LEN 19)) ;; add P+1 if precision is nonzero


;;;;** Handle type identifiers
#-odbc2
(progn
  (defconstant $::SQL_HANDLE_ENV 1)
  (defconstant $::SQL_HANDLE_DBC 2)
  (defconstant $::SQL_HANDLE_STMT 3)
  (defconstant $::SQL_HANDLE_DESC 4))
;;;;#endif

;;;;** Environment attribute
#-odbc2
(progn
  (defconstant $::SQL_ATTR_OUTPUT_NTS 10001)

;;;;** Connection attributes
  (defconstant $::SQL_ATTR_AUTO_IPD 10001)
  (defconstant $::SQL_ATTR_METADATA_ID 10014)

;;;;** statement attributes
  (defconstant $::SQL_ATTR_APP_ROW_DESC 10010)
  (defconstant $::SQL_ATTR_APP_PARAM_DESC 10011)
  (defconstant $::SQL_ATTR_IMP_ROW_DESC 10012)
  (defconstant $::SQL_ATTR_IMP_PARAM_DESC 10013)
  (defconstant $::SQL_ATTR_CURSOR_SCROLLABLE -1)
  (defconstant $::SQL_ATTR_CURSOR_SENSITIVITY -2)

;;;;** $::SQL_ATTR_CURSOR_SCROLLABLE values
  (defconstant $::SQL_NONSCROLLABLE 0)
  (defconstant $::SQL_SCROLLABLE 1)

;;;;** identifiers of fields in the SQL descriptor

  (defconstant $::SQL_DESC_COUNT 1001)
  (defconstant $::SQL_DESC_TYPE 1002)
  (defconstant $::SQL_DESC_LENGTH 1003)
  (defconstant $::SQL_DESC_OCTET_LENGTH_PTR 1004)
  (defconstant $::SQL_DESC_PRECISION 1005)
  (defconstant $::SQL_DESC_SCALE 1006)
  (defconstant $::SQL_DESC_DATETIME_INTERVAL_CODE 1007)
  (defconstant $::SQL_DESC_NULLABLE 1008)
  (defconstant $::SQL_DESC_INDICATOR_PTR 1009)
  (defconstant $::SQL_DESC_DATA_PTR 1010)
  (defconstant $::SQL_DESC_NAME 1011)
  (defconstant $::SQL_DESC_UNNAMED 1012)
  (defconstant $::SQL_DESC_OCTET_LENGTH 1013)
  (defconstant $::SQL_DESC_ALLOC_TYPE 1099)

;;;;** identifiers of fields in the diagnostics area
  (defconstant $::SQL_DIAG_RETURNCODE 1)
  (defconstant $::SQL_DIAG_NUMBER 2)
  (defconstant $::SQL_DIAG_ROW_COUNT 3)
  (defconstant $::SQL_DIAG_SQLSTATE 4)
  (defconstant $::SQL_DIAG_NATIVE 5)
  (defconstant $::SQL_DIAG_MESSAGE_TEXT 6)
  (defconstant $::SQL_DIAG_DYNAMIC_FUNCTION 7)
  (defconstant $::SQL_DIAG_CLASS_ORIGIN 8)
  (defconstant $::SQL_DIAG_SUBCLASS_ORIGIN 9)
  (defconstant $::SQL_DIAG_CONNECTION_NAME 10)
  (defconstant $::SQL_DIAG_SERVER_NAME 11)
  (defconstant $::SQL_DIAG_DYNAMIC_FUNCTION_CODE 12)

;;;;** defines for diagnostics fields
  (defconstant $::SQL_DIAG_CURSOR_ROW_COUNT -1249)
  (defconstant $::SQL_DIAG_ROW_NUMBER -1248)
  (defconstant $::SQL_DIAG_COLUMN_NUMBER -1247)

;;;;** dynamic function codes
  (defconstant $::SQL_DIAG_DYNAMIC_FUNCTION_MAX_LEN 25)
  
  (defconstant $::SQL_DIAG_ALTER_DOMAIN 3)
  (defconstant $::SQL_DIAG_ALTER_TABLE 4)
  (defconstant $::SQL_DIAG_CALL 7)
  (defconstant $::SQL_DIAG_CREATE_ASSERTION 6)
  (defconstant $::SQL_DIAG_CREATE_CHARACTER_SET 8)
  (defconstant $::SQL_DIAG_CREATE_COLLATION 10)
  (defconstant $::SQL_DIAG_CREATE_DOMAIN 23)
  (defconstant $::SQL_DIAG_CREATE_INDEX -1)
  (defconstant $::SQL_DIAG_CREATE_SCHEMA 64)
  (defconstant $::SQL_DIAG_CREATE_TABLE 77)
  (defconstant $::SQL_DIAG_CREATE_TRANSLATION 79)
  (defconstant $::SQL_DIAG_CREATE_VIEW 84)
  (defconstant $::SQL_DIAG_DELETE_WHERE 19)
  (defconstant $::SQL_DIAG_DROP_ASSERTION 24)
  (defconstant $::SQL_DIAG_DROP_CHARACTER_SET 25)
  (defconstant $::SQL_DIAG_DROP_COLLATION 26)
  (defconstant $::SQL_DIAG_DROP_DOMAIN 27)
  (defconstant $::SQL_DIAG_DROP_INDEX -2)
  (defconstant $::SQL_DIAG_DROP_SCHEMA 31)
  (defconstant $::SQL_DIAG_DROP_TABLE 32)
  (defconstant $::SQL_DIAG_DROP_TRANSLATION 33)
  (defconstant $::SQL_DIAG_DROP_VIEW 36)
  (defconstant $::SQL_DIAG_DYNAMIC_DELETE_CURSOR 38)
  (defconstant $::SQL_DIAG_DYNAMIC_UPDATE_CURSOR 81)
  (defconstant $::SQL_DIAG_GRANT 48)
  (defconstant $::SQL_DIAG_INSERT 50)
  (defconstant $::SQL_DIAG_REVOKE 59)
  (defconstant $::SQL_DIAG_SELECT_CURSOR 85)
  (defconstant $::SQL_DIAG_UNKNOWN_STATEMENT 0)
  (defconstant $::SQL_DIAG_UPDATE_WHERE 82))

;;;;** SQL datatype codes
(defconstant $::SQL_UNKNOWN_TYPE 0)
(defconstant $::SQL_CHAR 1)
(defconstant $::SQL_NUMERIC 2)
(defconstant $::SQL_DECIMAL 3)
(defconstant $::SQL_INTEGER 4)
(defconstant $::SQL_SMALLINT 5)
(defconstant $::SQL_FLOAT 6)
(defconstant $::SQL_REAL 7)
(defconstant $::SQL_DOUBLE 8)
#-odbc2
(defconstant $::SQL_DATETIME 9)
(defconstant $::SQL_VARCHAR 12)

;;;;** One-parameter shortcuts for date/time data types
#-odbc2
(progn
  (defconstant $::SQL_TYPE_DATE 91)
  (defconstant $::SQL_TYPE_TIME 92)
  (defconstant $::SQL_TYPE_TIMESTAMP 93)

;;;;** Statement attribute values for cursor sensitivity
  (defconstant $::SQL_UNSPECIFIED 0)
  (defconstant $::SQL_INSENSITIVE 1)
  (defconstant $::SQL_SENSITIVE 2))

;;;;** GetTypeInfo() request for all data types
(defconstant $::SQL_ALL_TYPES 0)

;;;;** Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
#-odbc2
(progn
  (defconstant $::SQL_DEFAULT 99)

;;;; SQLSQLLEN GetData() code indicating that the application row descriptor
;;;; specifies the data type
  (defconstant $::SQL_ARD_TYPE -99)
  ;; #if (ODBCVER >= 0x0380)
  (defconstant $::SQL_APD_TYPE -100)
  ;; #endif

;;;;** SQL date/time type subcodes
  (defconstant $::SQL_CODE_DATE 1)
  (defconstant $::SQL_CODE_TIME 2)
  (defconstant $::SQL_CODE_TIMESTAMP 3)

;;;;** CLI option values
  (defconstant $::SQL_FALSE 0)
  (defconstant $::SQL_TRUE 1))

;;;;** values of NULLABLE field in descriptor
(defconstant $::SQL_NO_NULLS 0)
(defconstant $::SQL_NULLABLE 1)

;;;; Value returned by SQLGetTypeInfo() to denote that it is
;;;; not known whether or not a data type supports null values.
(defconstant $::SQL_NULLABLE_UNKNOWN 2)

;;;; Values returned by SQLGetTypeInfo() to show WHERE clause
;;;; supported
#-odbc2
(progn
  (defconstant $::SQL_PRED_NONE 0)
  (defconstant $::SQL_PRED_CHAR 1)
  (defconstant $::SQL_PRED_BASIC 2)

;;;;** values of UNNAMED field in descriptor
  (defconstant $::SQL_NAMED 0)
  (defconstant $::SQL_UNNAMED 1)

;;;;** values of ALLOC_TYPE field in descriptor
  (defconstant $::SQL_DESC_ALLOC_AUTO 1)
  (defconstant $::SQL_DESC_ALLOC_USER 2))

;;;;** FreeStmt() options
(defconstant $::SQL_CLOSE 0)
(defconstant $::SQL_DROP 1)
(defconstant $::SQL_UNBIND 2)
(defconstant $::SQL_RESET_PARAMS 3)

;;;; Codes used for FetchOrientation in SQLFetchScroll(),
;;;; and in SQLDataSources()
(defconstant $::SQL_FETCH_NEXT 1)
(defconstant $::SQL_FETCH_FIRST 2)

;;;;** Other codes used for FetchOrientation in SQLFetchScroll()
(defconstant $::SQL_FETCH_LAST 3)
(defconstant $::SQL_FETCH_PRIOR 4)
(defconstant $::SQL_FETCH_ABSOLUTE 5)
(defconstant $::SQL_FETCH_RELATIVE 6)

;;;;** SQLEndTran() options
(defconstant $::SQL_COMMIT 0)
(defconstant $::SQL_ROLLBACK 1)

;;;;** null handles returned by SQLAllocHandle()
(defconstant $::SQL_NULL_HENV 0)
(defconstant $::SQL_NULL_HDBC 0)
(defconstant $::SQL_NULL_HSTMT 0)
#-odbc2
(progn
  (defconstant $::SQL_NULL_HDESC 0)

;;;;*** null handle used in place of parent handle when allocating HENV
  (defconstant $::SQL_NULL_HANDLE 0))

;;;;** Values that may appear in the result set of SQLSpecialColumns()
(defconstant $::SQL_SCOPE_CURROW 0)
(defconstant $::SQL_SCOPE_TRANSACTION 1)
(defconstant $::SQL_SCOPE_SESSION 2)

(defconstant $::SQL_PC_UNKNOWN 0)
#-odbc2
(defconstant $::SQL_PC_NON_PSEUDO 1)
(defconstant $::SQL_PC_PSEUDO 2)

;;;;** Reserved value for the IdentifierType argument of SQLSpecialColumns()
#-odbc2
(defconstant $::SQL_ROW_IDENTIFIER 1)

;;;;** Reserved values for UNIQUE argument of SQLStatistics()
(defconstant $::SQL_INDEX_UNIQUE 0)
(defconstant $::SQL_INDEX_ALL 1)

;;;;** Values that may appear in the result set of SQLStatistics()
(defconstant $::SQL_INDEX_CLUSTERED 1)
(defconstant $::SQL_INDEX_HASHED 2)
(defconstant $::SQL_INDEX_OTHER 3)

;;;;** SQLGetFunctions() values to identify ODBC APIs
(defconstant $::SQL_API_SQLALLOCCONNECT 1)
(defconstant $::SQL_API_SQLALLOCENV 2)
#-odbc2
(defconstant $::SQL_API_SQLALLOCHANDLE 1001)
(defconstant $::SQL_API_SQLALLOCSTMT 3)
(defconstant $::SQL_API_SQLBINDCOL 4)
#-odbc2
(defconstant $::SQL_API_SQLBINDPARAM 1002)
(defconstant $::SQL_API_SQLCANCEL 5)
#-odbc2
(progn
  (defconstant $::SQL_API_SQLCLOSECURSOR 1003)
  (defconstant $::SQL_API_SQLCOLATTRIBUTE 6))
(defconstant $::SQL_API_SQLCOLUMNS 40)
(defconstant $::SQL_API_SQLCONNECT 7)
#-odbc2
(defconstant $::SQL_API_SQLCOPYDESC 1004)
(defconstant $::SQL_API_SQLDATASOURCES 57)
(defconstant $::SQL_API_SQLDESCRIBECOL 8)
(defconstant $::SQL_API_SQLDISCONNECT 9)
#-odbc2
(defconstant $::SQL_API_SQLENDTRAN 1005)
(defconstant $::SQL_API_SQLERROR 10)
(defconstant $::SQL_API_SQLEXECDIRECT 11)
(defconstant $::SQL_API_SQLEXECUTE 12)
(defconstant $::SQL_API_SQLFETCH 13)
#-odbc2
(defconstant $::SQL_API_SQLFETCHSCROLL 1021)
(defconstant $::SQL_API_SQLFREECONNECT 14)
(defconstant $::SQL_API_SQLFREEENV 15)
#-odbc2
(defconstant $::SQL_API_SQLFREEHANDLE 1006)
(defconstant $::SQL_API_SQLFREESTMT 16)
#-odbc2
(defconstant $::SQL_API_SQLGETCONNECTATTR 1007)
(defconstant $::SQL_API_SQLGETCONNECTOPTION 42)
(defconstant $::SQL_API_SQLGETCURSORNAME 17)
(defconstant $::SQL_API_SQLGETDATA 43)
#-odbc2
(progn
  (defconstant $::SQL_API_SQLGETDESCFIELD 1008)
  (defconstant $::SQL_API_SQLGETDESCREC 1009)
  (defconstant $::SQL_API_SQLGETDIAGFIELD 1010)
  (defconstant $::SQL_API_SQLGETDIAGREC 1011)
  (defconstant $::SQL_API_SQLGETENVATTR 1012))
(defconstant $::SQL_API_SQLGETFUNCTIONS 44)
(defconstant $::SQL_API_SQLGETINFO 45)
#-odbc2
(defconstant $::SQL_API_SQLGETSTMTATTR 1014)
(defconstant $::SQL_API_SQLGETSTMTOPTION 46)
(defconstant $::SQL_API_SQLGETTYPEINFO 47)
(defconstant $::SQL_API_SQLNUMRESULTCOLS 18)
(defconstant $::SQL_API_SQLPARAMDATA 48)
(defconstant $::SQL_API_SQLPREPARE 19)
(defconstant $::SQL_API_SQLPUTDATA 49)
(defconstant $::SQL_API_SQLROWCOUNT 20)
#-odbc2
(defconstant $::SQL_API_SQLSETCONNECTATTR 1016)
(defconstant $::SQL_API_SQLSETCONNECTOPTION 50)
(defconstant $::SQL_API_SQLSETCURSORNAME 21)
#-odbc2
(progn
  (defconstant $::SQL_API_SQLSETDESCFIELD 1017)
  (defconstant $::SQL_API_SQLSETDESCREC 1018)
  (defconstant $::SQL_API_SQLSETENVATTR 1019))
(defconstant $::SQL_API_SQLSETPARAM 22)
#-odbc2
(defconstant $::SQL_API_SQLSETSTMTATTR 1020)
(defconstant $::SQL_API_SQLSETSTMTOPTION 51)
(defconstant $::SQL_API_SQLSPECIALCOLUMNS 52)
(defconstant $::SQL_API_SQLSTATISTICS 53)
(defconstant $::SQL_API_SQLTABLES 54)
(defconstant $::SQL_API_SQLTRANSACT 23)

#-odbc2
(defconstant $::SQL_API_SQLCANCELHANDLE 1022)

;;;;** Information requested by SQLGetInfo()
#-odbc2
(progn
  (defconstant $::SQL_MAX_DRIVER_CONNECTIONS 0)
  (defconstant $::SQL_MAXIMUM_DRIVER_CONNECTIONS $::SQL_MAX_DRIVER_CONNECTIONS)
  (defconstant $::SQL_MAX_CONCURRENT_ACTIVITIES 1)
  (defconstant $::SQL_MAXIMUM_CONCURRENT_ACTIVITIES $::SQL_MAX_CONCURRENT_ACTIVITIES))
(defconstant $::SQL_DATA_SOURCE_NAME 2)
(defconstant $::SQL_FETCH_DIRECTION 8)
(defconstant $::SQL_SERVER_NAME 13)
(defconstant $::SQL_SEARCH_PATTERN_ESCAPE 14)
(defconstant $::SQL_DBMS_NAME 17)
(defconstant $::SQL_DBMS_VER 18)
(defconstant $::SQL_ACCESSIBLE_TABLES 19)
(defconstant $::SQL_ACCESSIBLE_PROCEDURES 20)
(defconstant $::SQL_CURSOR_COMMIT_BEHAVIOR 23)
(defconstant $::SQL_DATA_SOURCE_READ_ONLY 25)
(defconstant $::SQL_DEFAULT_TXN_ISOLATION 26)
(defconstant $::SQL_IDENTIFIER_CASE 28)
(defconstant $::SQL_IDENTIFIER_QUOTE_CHAR 29)
(defconstant $::SQL_MAX_COLUMN_NAME_LEN 30)
(defconstant $::SQL_MAXIMUM_COLUMN_NAME_LENGTH $::SQL_MAX_COLUMN_NAME_LEN)
(defconstant $::SQL_MAX_CURSOR_NAME_LEN 31)
(defconstant $::SQL_MAXIMUM_CURSOR_NAME_LENGTH $::SQL_MAX_CURSOR_NAME_LEN)
(defconstant $::SQL_MAX_SCHEMA_NAME_LEN 32)
(defconstant $::SQL_MAXIMUM_SCHEMA_NAME_LENGTH $::SQL_MAX_SCHEMA_NAME_LEN)
(defconstant $::SQL_MAX_CATALOG_NAME_LEN 34)
(defconstant $::SQL_MAXIMUM_CATALOG_NAME_LENGTH $::SQL_MAX_CATALOG_NAME_LEN)
(defconstant $::SQL_MAX_TABLE_NAME_LEN 35)
(defconstant $::SQL_SCROLL_CONCURRENCY 43)
(defconstant $::SQL_TXN_CAPABLE 46)
(defconstant $::SQL_TRANSACTION_CAPABLE $::SQL_TXN_CAPABLE)
(defconstant $::SQL_USER_NAME 47)
(defconstant $::SQL_TXN_ISOLATION_OPTION 72)
(defconstant $::SQL_TRANSACTION_ISOLATION_OPTION $::SQL_TXN_ISOLATION_OPTION)
(defconstant $::SQL_INTEGRITY 73)
(defconstant $::SQL_GETDATA_EXTENSIONS 81)
(defconstant $::SQL_NULL_COLLATION 85)
(defconstant $::SQL_ALTER_TABLE 86)
(defconstant $::SQL_ORDER_BY_COLUMNS_IN_SELECT 90)
(defconstant $::SQL_SPECIAL_CHARACTERS 94)
(defconstant $::SQL_MAX_COLUMNS_IN_GROUP_BY 97)
(defconstant $::SQL_MAXIMUM_COLUMNS_IN_GROUP_BY $::SQL_MAX_COLUMNS_IN_GROUP_BY)
(defconstant $::SQL_MAX_COLUMNS_IN_INDEX 98)
(defconstant $::SQL_MAXIMUM_COLUMNS_IN_INDEX $::SQL_MAX_COLUMNS_IN_INDEX)
(defconstant $::SQL_MAX_COLUMNS_IN_ORDER_BY 99)
(defconstant $::SQL_MAXIMUM_COLUMNS_IN_ORDER_BY $::SQL_MAX_COLUMNS_IN_ORDER_BY)
(defconstant $::SQL_MAX_COLUMNS_IN_SELECT 100)
(defconstant $::SQL_MAXIMUM_COLUMNS_IN_SELECT $::SQL_MAX_COLUMNS_IN_SELECT)
(defconstant $::SQL_MAX_COLUMNS_IN_TABLE 101)
(defconstant $::SQL_MAX_INDEX_SIZE 102)
(defconstant $::SQL_MAXIMUM_INDEX_SIZE $::SQL_MAX_INDEX_SIZE)
(defconstant $::SQL_MAX_ROW_SIZE 104)
(defconstant $::SQL_MAXIMUM_ROW_SIZE $::SQL_MAX_ROW_SIZE)
(defconstant $::SQL_MAX_STATEMENT_LEN 105)
(defconstant $::SQL_MAXIMUM_STATEMENT_LENGTH $::SQL_MAX_STATEMENT_LEN)
(defconstant $::SQL_MAX_TABLES_IN_SELECT 106)
(defconstant $::SQL_MAXIMUM_TABLES_IN_SELECT $::SQL_MAX_TABLES_IN_SELECT)
(defconstant $::SQL_MAX_USER_NAME_LEN 107)
(defconstant $::SQL_MAXIMUM_USER_NAME_LENGTH $::SQL_MAX_USER_NAME_LEN)
#-odbc2
(progn
  (defconstant $::SQL_OJ_CAPABILITIES 115)
  (defconstant $::SQL_OUTER_JOIN_CAPABILITIES $::SQL_OJ_CAPABILITIES)

  (defconstant $::SQL_XOPEN_CLI_YEAR 10000)
  (defconstant $::SQL_CURSOR_SENSITIVITY 10001)
  (defconstant $::SQL_DESCRIBE_PARAMETER 10002)
  (defconstant $::SQL_CATALOG_NAME 10003)
  (defconstant $::SQL_COLLATION_SEQ 10004)
  (defconstant $::SQL_MAX_IDENTIFIER_LEN 10005)
  (defconstant $::SQL_MAXIMUM_IDENTIFIER_LENGTH $::SQL_MAX_IDENTIFIER_LEN))

;;;;** $::SQL_ALTER_TABLE bitmasks
;;#if (ODBCVER >= 0x0200)
(defconstant $::SQL_AT_ADD_COLUMN #x00000001)
(defconstant $::SQL_AT_DROP_COLUMN #x00000002)
;;#endif /* ODBCVER >= 0x0200 */

#-odbc2
(defconstant $::SQL_AT_ADD_CONSTRAINT #x00000008)

;;;;/* The following bitmasks are ODBC extensions and defined in sqlext.h
;;;;*#define    $::SQL_AT_COLUMN_SINGLE                    #x00000020
;;;;*#define    $::SQL_AT_ADD_COLUMN_DEFAULT               #x00000040
;;;;*#define    $::SQL_AT_ADD_COLUMN_COLLATION             #x00000080
;;;;*#define    $::SQL_AT_SET_COLUMN_DEFAULT               #x00000100
;;;;*#define    $::SQL_AT_DROP_COLUMN_DEFAULT              #x00000200
;;;;*#define    $::SQL_AT_DROP_COLUMN_CASCADE              #x00000400
;;;;*#define    $::SQL_AT_DROP_COLUMN_RESTRICT             #x00000800
;;;;*#define $::SQL_AT_ADD_TABLE_CONSTRAINT                #x00001000
;;;;*#define $::SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE       #x00002000
;;;;*#define $::SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT      #x00004000
;;;;*#define $::SQL_AT_CONSTRAINT_NAME_DEFINITION          #x00008000
;;;;*#define $::SQL_AT_CONSTRAINT_INITIALLY_DEFERRED       #x00010000
;;;;*#define $::SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE      #x00020000
;;;;*#define $::SQL_AT_CONSTRAINT_DEFERRABLE               #x00040000
;;;;*#define $::SQL_AT_CONSTRAINT_NON_DEFERRABLE           #x00080000
;;;;*/
;;;;#endif  /* ODBCVER >= 0x0300 */


;;;;** $::SQL_ASYNC_MODE values
#-odbc2
(progn
  (defconstant $::SQL_AM_NONE 0)
  (defconstant $::SQL_AM_CONNECTION 1)
  (defconstant $::SQL_AM_STATEMENT 2))

;;;;** $::SQL_CURSOR_COMMIT_BEHAVIOR values
(defconstant $::SQL_CB_DELETE 0)
(defconstant $::SQL_CB_CLOSE 1)
(defconstant $::SQL_CB_PRESERVE 2)

;;;;** $::SQL_FETCH_DIRECTION bitmasks
(defconstant $::SQL_FD_FETCH_NEXT #x00000001)
(defconstant $::SQL_FD_FETCH_FIRST #x00000002)
(defconstant $::SQL_FD_FETCH_LAST #x00000004)
(defconstant $::SQL_FD_FETCH_PRIOR #x00000008)
(defconstant $::SQL_FD_FETCH_ABSOLUTE #x00000010)
(defconstant $::SQL_FD_FETCH_RELATIVE #x00000020)

;;;;** $::SQL_GETDATA_EXTENSIONS bitmasks
(defconstant $::SQL_GD_ANY_COLUMN #x00000001)
(defconstant $::SQL_GD_ANY_ORDER #x00000002)

;;;;** $::SQL_IDENTIFIER_CASE values
(defconstant $::SQL_IC_UPPER 1)
(defconstant $::SQL_IC_LOWER 2)
(defconstant $::SQL_IC_SENSITIVE 3)
(defconstant $::SQL_IC_MIXED 4)

;;;;** $::SQL_OJ_CAPABILITIES bitmasks
;;;;** NB: this means 'outer join', not what you may be thinking
;;#if (ODBCVER >= 0x0201)
(defconstant $::SQL_OJ_LEFT #x00000001)
(defconstant $::SQL_OJ_RIGHT #x00000002)
(defconstant $::SQL_OJ_FULL #x00000004)
(defconstant $::SQL_OJ_NESTED #x00000008)
(defconstant $::SQL_OJ_NOT_ORDERED #x00000010)
(defconstant $::SQL_OJ_INNER #x00000020)
(defconstant $::SQL_OJ_ALL_COMPARISON_OPS #x00000040)
;;#endif

;;;;** $::SQL_SCROLL_CONCURRENCY bitmasks
(defconstant $::SQL_SCCO_READ_ONLY #x00000001)
(defconstant $::SQL_SCCO_LOCK #x00000002)
(defconstant $::SQL_SCCO_OPT_ROWVER #x00000004)
(defconstant $::SQL_SCCO_OPT_VALUES #x00000008)

;;;;** $::SQL_TXN_CAPABLE values
(defconstant $::SQL_TC_NONE 0)
(defconstant $::SQL_TC_DML 1)
(defconstant $::SQL_TC_ALL 2)
(defconstant $::SQL_TC_DDL_COMMIT 3)
(defconstant $::SQL_TC_DDL_IGNORE 4)

;;;;** $::SQL_TXN_ISOLATION_OPTION bitmasks
(defconstant $::SQL_TXN_READ_UNCOMMITTED #x00000001)
(defconstant $::SQL_TRANSACTION_READ_UNCOMMITTED $::SQL_TXN_READ_UNCOMMITTED)
(defconstant $::SQL_TXN_READ_COMMITTED #x00000002)
(defconstant $::SQL_TRANSACTION_READ_COMMITTED $::SQL_TXN_READ_COMMITTED)
(defconstant $::SQL_TXN_REPEATABLE_READ #x00000004)
(defconstant $::SQL_TRANSACTION_REPEATABLE_READ $::SQL_TXN_REPEATABLE_READ)
(defconstant $::SQL_TXN_SERIALIZABLE #x00000008)
(defconstant $::SQL_TRANSACTION_SERIALIZABLE $::SQL_TXN_SERIALIZABLE)

;;;;** $::SQL_NULL_COLLATION values
(defconstant $::SQL_NC_HIGH 0)
(defconstant $::SQL_NC_LOW 1)

;;;;* From sqlext.h

;;;;** generally useful constants
(defconstant $::SQL_SPEC_MAJOR 3);; Major version of specification 
(defconstant $::SQL_SPEC_MINOR 80);; Minor version of specification 
(defconstant $::SQL_SPEC_STRING :03.80);; String constant for version

(defconstant $::SQL_SQLSTATE_SIZE 5);; size of SQLSTATE

;;;;(cffi:defctype SQLSTATE[$::SQL_SQLSTATE_SIZE+1] SQLTCHAR) ;

(defconstant $::SQL_MAX_DSN_LENGTH 32);; maximum data source name size

(defconstant $::SQL_MAX_OPTION_STRING_LENGTH 256)

;;;;** return code $::SQL_NO_DATA_FOUND is the same as $::SQL_NO_DATA
#+odbc2
(defconstant $::SQL_NO_DATA_FOUND 100)
#-odbc2
(defconstant $::SQL_NO_DATA_FOUND $::SQL_NO_DATA)

;;;;** an end handle type
#-odbc2
(defconstant $::SQL_HANDLE_SENV 5)

;;;;** env attribute
#-odbc2
(progn
  (defconstant $::SQL_ATTR_ODBC_VERSION 200)
  (defconstant $::SQL_ATTR_CONNECTION_POOLING 201)
  (defconstant $::SQL_ATTR_CP_MATCH 202)

;;;;  #if (ODBCVER >= 0x0300)
;;;;** values for $::SQL_ATTR_CONNECTION_POOLING
  (defconstant $::SQL_CP_OFF 0)
  (defconstant $::SQL_CP_ONE_PER_DRIVER 1)
  (defconstant $::SQL_CP_ONE_PER_HENV 2)
  (defconstant $::SQL_CP_DEFAULT $::SQL_CP_OFF)

;;;;** values for $::SQL_ATTR_CP_MATCH
  (defconstant $::SQL_CP_STRICT_MATCH 0)
  (defconstant $::SQL_CP_RELAXED_MATCH 1)
  (defconstant $::SQL_CP_MATCH_DEFAULT $::SQL_CP_STRICT_MATCH)

;;;;** values for $::SQL_ATTR_ODBC_VERSION
  (defconstant $::SQL_OV_ODBC2 2)
  (defconstant $::SQL_OV_ODBC3 3)
;;;; new values for $::SQL_ATTR_ODBC_VERSION 
;;;; From ODBC 3.8 onwards, we should use <major version> * 100 + <minor version>
  (defconstant $::SQL_OV_ODBC3_80 380))

;;;;** connection attributes
(defconstant $::SQL_ACCESS_MODE 101)
(defconstant $::SQL_AUTOCOMMIT 102)
(defconstant $::SQL_LOGIN_TIMEOUT 103)
(defconstant $::SQL_OPT_TRACE 104)
(defconstant $::SQL_OPT_TRACEFILE 105)
(defconstant $::SQL_TRANSLATE_DLL 106)
(defconstant $::SQL_TRANSLATE_OPTION 107)
(defconstant $::SQL_TXN_ISOLATION 108)
(defconstant $::SQL_CURRENT_QUALIFIER 109)
(defconstant $::SQL_ODBC_CURSORS 110)
(defconstant $::SQL_QUIET_MODE 111)
(defconstant $::SQL_PACKET_SIZE 112)

;;;;** connection attributes with new names
#-odbc2
(progn
  (defconstant $::SQL_ATTR_ACCESS_MODE $::SQL_ACCESS_MODE)
  (defconstant $::SQL_ATTR_AUTOCOMMIT $::SQL_AUTOCOMMIT)
  (defconstant $::SQL_ATTR_CONNECTION_TIMEOUT 113)
  (defconstant $::SQL_ATTR_CURRENT_CATALOG $::SQL_CURRENT_QUALIFIER)
  (defconstant $::SQL_ATTR_DISCONNECT_BEHAVIOR 114)
  (defconstant $::SQL_ATTR_ENLIST_IN_DTC 1207)
  (defconstant $::SQL_ATTR_ENLIST_IN_XA 1208)
  (defconstant $::SQL_ATTR_LOGIN_TIMEOUT $::SQL_LOGIN_TIMEOUT)
  (defconstant $::SQL_ATTR_ODBC_CURSORS $::SQL_ODBC_CURSORS)
  (defconstant $::SQL_ATTR_PACKET_SIZE $::SQL_PACKET_SIZE)
  (defconstant $::SQL_ATTR_QUIET_MODE $::SQL_QUIET_MODE)
  (defconstant $::SQL_ATTR_TRACE $::SQL_OPT_TRACE)
  (defconstant $::SQL_ATTR_TRACEFILE $::SQL_OPT_TRACEFILE)
  (defconstant $::SQL_ATTR_TRANSLATE_LIB $::SQL_TRANSLATE_DLL)
  (defconstant $::SQL_ATTR_TRANSLATE_OPTION $::SQL_TRANSLATE_OPTION)
  (defconstant $::SQL_ATTR_TXN_ISOLATION $::SQL_TXN_ISOLATION))

(defconstant $::SQL_ATTR_CONNECTION_DEAD 1209);; GetConnectAttr only

#-odbc2
;;;;  ODBC Driver Manager sets this connection attribute to a unicode driver
;;;;  (which supports SQLConnectW) when the application is an ANSI application
;;;;  (which calls SQLConnect, SQLDriverConnect, or SQLBrowseConnect).
;;;;  This is SetConnectAttr only and application does not set this attribute
;;;;  This attribute was introduced because some unicode driver's some APIs may
;;;;  need to behave differently on ANSI or Unicode applications. A unicode
;;;;  driver, which  has same behavior for both ANSI or Unicode applications,
;;;;  should return $::SQL_ERROR when the driver manager sets this connection
;;;;  attribute. When a unicode driver returns $::SQL_SUCCESS on this attribute,
;;;;  the driver manager treates ANSI and Unicode connections differently in
;;;;  connection pooling.
;;;;
(progn
  (defconstant $::SQL_ATTR_ANSI_APP 115)
  (defconstant $::SQL_ATTR_RESET_CONNECTION 116)
  (defconstant $::SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE 117))

;;;;** $::SQL_CONNECT_OPT_DRVR_START is not meaningful for 3.0 driver
#+odbc2
(progn
  (defconstant $::SQL_CONNECT_OPT_DRVR_START 1000)
  (defconstant $::SQL_CONN_OPT_MAX $::SQL_PACKET_SIZE)
  (defconstant $::SQL_CONN_OPT_MIN $::SQL_ACCESS_MODE))

;;;;** $::SQL_ACCESS_MODE options
(defconstant $::SQL_MODE_READ_WRITE 0)
(defconstant $::SQL_MODE_READ_ONLY 1)
(defconstant $::SQL_MODE_DEFAULT $::SQL_MODE_READ_WRITE)

;;;;** $::SQL_AUTOCOMMIT options
(defconstant $::SQL_AUTOCOMMIT_OFF 0)
(defconstant $::SQL_AUTOCOMMIT_ON 1)
(defconstant $::SQL_AUTOCOMMIT_DEFAULT $::SQL_AUTOCOMMIT_ON)

;;;;** $::SQL_LOGIN_TIMEOUT options
(defconstant $::SQL_LOGIN_TIMEOUT_DEFAULT 15)

;;;;** $::SQL_OPT_TRACE options
(defconstant $::SQL_OPT_TRACE_OFF 0)
(defconstant $::SQL_OPT_TRACE_ON 1)
(defconstant $::SQL_OPT_TRACE_DEFAULT $::SQL_OPT_TRACE_OFF)
(defparameter $::SQL_OPT_TRACE_FILE_DEFAULT "\\SQL.LOG")

;;;;** $::SQL_ODBC_CURSORS options
(defconstant $::SQL_CUR_USE_IF_NEEDED 0)
(defconstant $::SQL_CUR_USE_ODBC 1)
(defconstant $::SQL_CUR_USE_DRIVER 2)
(defconstant $::SQL_CUR_DEFAULT $::SQL_CUR_USE_DRIVER)

#-odbc2
(progn
;;;;** values for $::SQL_ATTR_DISCONNECT_BEHAVIOR
  (defconstant $::SQL_DB_RETURN_TO_POOL 0)
  (defconstant $::SQL_DB_DISCONNECT 1)
  (defconstant $::SQL_DB_DEFAULT $::SQL_DB_RETURN_TO_POOL)

;;;;** values for $::SQL_ATTR_ENLIST_IN_DTC
  (defconstant $::SQL_DTC_DONE 0))

;;;;** values for $::SQL_ATTR_CONNECTION_DEAD
(defconstant $::SQL_CD_TRUE 1);; Connection is closed/dead
(defconstant $::SQL_CD_FALSE 0);; Connection is open/available

;;;;** values for $::SQL_ATTR_ANSI_APP
#-odbc2
(progn
  (defconstant $::SQL_AA_TRUE 1);; the application is an ANSI app
  (defconstant $::SQL_AA_FALSE 0);; the application is a Unicode app

;;;;** values for $::SQL_ATTR_RESET_CONNECTION
  ;;#if (ODBCVER >= 0x0380)
  (defconstant $::SQL_RESET_CONNECTION_YES 1)
  ;;#endif

;;;;** values for $::SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE
  ;;#if (ODBCVER >= 0x0380)
  (defconstant $::SQL_ASYNC_DBC_ENABLE_ON 1)
  (defconstant $::SQL_ASYNC_DBC_ENABLE_OFF 0)
  (defconstant $::SQL_ASYNC_DBC_ENABLE_DEFAULT $::SQL_ASYNC_DBC_ENABLE_OFF))
;;#endif // ODBCVER >= 0x0380


;;;;** SQLColAttributes defines
(defconstant $::SQL_COLUMN_COUNT 0)
(defconstant $::SQL_COLUMN_NAME 1)
(defconstant $::SQL_COLUMN_TYPE 2)
(defconstant $::SQL_COLUMN_LENGTH 3)
(defconstant $::SQL_COLUMN_PRECISION 4)
(defconstant $::SQL_COLUMN_SCALE 5)
(defconstant $::SQL_COLUMN_DISPLAY_SIZE 6)
(defconstant $::SQL_COLUMN_NULLABLE 7)
(defconstant $::SQL_COLUMN_UNSIGNED 8)
(defconstant $::SQL_COLUMN_MONEY 9)
(defconstant $::SQL_COLUMN_UPDATABLE 10)
(defconstant $::SQL_COLUMN_AUTO_INCREMENT 11)
(defconstant $::SQL_COLUMN_CASE_SENSITIVE 12)
(defconstant $::SQL_COLUMN_SEARCHABLE 13)
(defconstant $::SQL_COLUMN_TYPE_NAME 14)
(defconstant $::SQL_COLUMN_TABLE_NAME 15)
(defconstant $::SQL_COLUMN_OWNER_NAME 16)
(defconstant $::SQL_COLUMN_QUALIFIER_NAME 17)
(defconstant $::SQL_COLUMN_LABEL 18)
(defconstant $::SQL_COLATT_OPT_MAX $::SQL_COLUMN_LABEL)
#+odbc2
(defconstant $::SQL_COLUMN_DRIVER_START 1000)

(defconstant $::SQL_COLATT_OPT_MIN $::SQL_COLUMN_COUNT)

;;;;** SQLColAttributes subdefines for $::SQL_COLUMN_UPDATABLE
(defconstant $::SQL_ATTR_READONLY 0)
(defconstant $::SQL_ATTR_WRITE 1)
(defconstant $::SQL_ATTR_READWRITE_UNKNOWN 2)

;;;;** SQLColAttributes subdefines for $::SQL_COLUMN_SEARCHABLE
;;;;** These are also used by SQLGetInfo                    
(defconstant $::SQL_UNSEARCHABLE 0)
(defconstant $::SQL_LIKE_ONLY 1)
(defconstant $::SQL_ALL_EXCEPT_LIKE 2)
(defconstant $::SQL_SEARCHABLE 3)
(defconstant $::SQL_PRED_SEARCHABLE $::SQL_SEARCHABLE)


;;;;** Special return values for SQLGetData
(defconstant $::SQL_NO_TOTAL -4)


;;;;** Statement Attributes
(defconstant $::SQL_QUERY_TIMEOUT 0)
(defconstant $::SQL_MAX_ROWS 1)
(defconstant $::SQL_NOSCAN 2)
(defconstant $::SQL_MAX_LENGTH 3)
(defconstant $::SQL_ASYNC_ENABLE 4);; same as $::SQL_ATTR_ASYNC_ENABLE
(defconstant $::SQL_BIND_TYPE 5)
(defconstant $::SQL_CURSOR_TYPE 6)
(defconstant $::SQL_CONCURRENCY 7)
(defconstant $::SQL_KEYSET_SIZE 8)
(defconstant $::SQL_ROWSET_SIZE 9)
(defconstant $::SQL_SIMULATE_CURSOR 10)
(defconstant $::SQL_RETRIEVE_DATA 11)
(defconstant $::SQL_USE_BOOKMARKS 12)
(defconstant $::SQL_GET_BOOKMARK 13);;      GetStmtOption Only
(defconstant $::SQL_ROW_NUMBER 14);;      GetStmtOption Only

;;;;*** Statement Attributes for ODBC 3.0
#-odbc2
(progn
  (defconstant $::SQL_ATTR_ASYNC_ENABLE 4)
  (defconstant $::SQL_ATTR_CONCURRENCY $::SQL_CONCURRENCY)
  (defconstant $::SQL_ATTR_CURSOR_TYPE $::SQL_CURSOR_TYPE)
  (defconstant $::SQL_ATTR_ENABLE_AUTO_IPD 15)
  (defconstant $::SQL_ATTR_FETCH_BOOKMARK_PTR 16)
  (defconstant $::SQL_ATTR_KEYSET_SIZE $::SQL_KEYSET_SIZE)
  (defconstant $::SQL_ATTR_MAX_LENGTH $::SQL_MAX_LENGTH)
  (defconstant $::SQL_ATTR_MAX_ROWS $::SQL_MAX_ROWS)
  (defconstant $::SQL_ATTR_NOSCAN $::SQL_NOSCAN)
  (defconstant $::SQL_ATTR_PARAM_BIND_OFFSET_PTR 17)
  (defconstant $::SQL_ATTR_PARAM_BIND_TYPE 18)
  (defconstant $::SQL_ATTR_PARAM_OPERATION_PTR 19)
  (defconstant $::SQL_ATTR_PARAM_STATUS_PTR 20)
  (defconstant $::SQL_ATTR_PARAMS_PROCESSED_PTR 21)
  (defconstant $::SQL_ATTR_PARAMSET_SIZE 22)
  (defconstant $::SQL_ATTR_QUERY_TIMEOUT $::SQL_QUERY_TIMEOUT)
  (defconstant $::SQL_ATTR_RETRIEVE_DATA $::SQL_RETRIEVE_DATA)
  (defconstant $::SQL_ATTR_ROW_BIND_OFFSET_PTR 23)
  (defconstant $::SQL_ATTR_ROW_BIND_TYPE $::SQL_BIND_TYPE)
  (defconstant $::SQL_ATTR_ROW_NUMBER $::SQL_ROW_NUMBER);; GetStmtAttr
  (defconstant $::SQL_ATTR_ROW_OPERATION_PTR 24)
  (defconstant $::SQL_ATTR_ROW_STATUS_PTR 25)
  (defconstant $::SQL_ATTR_ROWS_FETCHED_PTR 26)
  (defconstant $::SQL_ATTR_ROW_ARRAY_SIZE 27)
  (defconstant $::SQL_ATTR_SIMULATE_CURSOR $::SQL_SIMULATE_CURSOR)
  (defconstant $::SQL_ATTR_USE_BOOKMARKS $::SQL_USE_BOOKMARKS))

#+odbc2
(progn
  (defconstant $::SQL_STMT_OPT_MAX $::SQL_ROW_NUMBER)
  (defconstant $::SQL_STMT_OPT_MIN $::SQL_QUERY_TIMEOUT))

;;;;** New Defines for SEARCHABLE Column in SQLGetTypeInfo
#-odbc2
(progn
  (defconstant $::SQL_COL_PRED_CHAR $::SQL_LIKE_ONLY)
  (defconstant $::SQL_COL_PRED_BASIC $::SQL_ALL_EXCEPT_LIKE)

;;;;** whether an attribute is a pointer or not
  (defconstant $::SQL_IS_POINTER -4)
  (defconstant $::SQL_IS_UINTEGER -5)
  (defconstant $::SQL_IS_INTEGER -6)
  (defconstant $::SQL_IS_USMALLINT -7)
  (defconstant $::SQL_IS_SMALLINT -8)

;;;;** the value of $::SQL_ATTR_PARAM_BIND_TYPE
  (defconstant $::SQL_PARAM_BIND_BY_COLUMN 0)
  (defconstant $::SQL_PARAM_BIND_TYPE_DEFAULT $::SQL_PARAM_BIND_BY_COLUMN))


;;;;** $::SQL_QUERY_TIMEOUT options
(defconstant $::SQL_QUERY_TIMEOUT_DEFAULT 0)

;;;;** $::SQL_MAX_ROWS options
(defconstant $::SQL_MAX_ROWS_DEFAULT 0)

;;;;** $::SQL_NOSCAN options
(defconstant $::SQL_NOSCAN_OFF 0);;      1.0 FALSE
(defconstant $::SQL_NOSCAN_ON 1);;      1.0 TRUE
(defconstant $::SQL_NOSCAN_DEFAULT $::SQL_NOSCAN_OFF)

;;;;** $::SQL_MAX_LENGTH options
(defconstant $::SQL_MAX_LENGTH_DEFAULT 0)

;;;;** values for $::SQL_ATTR_ASYNC_ENABLE
(defconstant $::SQL_ASYNC_ENABLE_OFF 0)
(defconstant $::SQL_ASYNC_ENABLE_ON 1)
(defconstant $::SQL_ASYNC_ENABLE_DEFAULT $::SQL_ASYNC_ENABLE_OFF)

;;;;** $::SQL_BIND_TYPE options
(defconstant $::SQL_BIND_BY_COLUMN 0)
(defconstant $::SQL_BIND_TYPE_DEFAULT $::SQL_BIND_BY_COLUMN);; Default value

;;;;** $::SQL_CONCURRENCY options
(defconstant $::SQL_CONCUR_READ_ONLY 1)
(defconstant $::SQL_CONCUR_LOCK 2)
(defconstant $::SQL_CONCUR_ROWVER 3)
(defconstant $::SQL_CONCUR_VALUES 4)
(defconstant $::SQL_CONCUR_DEFAULT $::SQL_CONCUR_READ_ONLY);; Default value

;;;;** $::SQL_CURSOR_TYPE options
(defconstant $::SQL_CURSOR_FORWARD_ONLY 0)
(defconstant $::SQL_CURSOR_KEYSET_DRIVEN 1)
(defconstant $::SQL_CURSOR_DYNAMIC 2)
(defconstant $::SQL_CURSOR_STATIC 3)
(defconstant $::SQL_CURSOR_TYPE_DEFAULT $::SQL_CURSOR_FORWARD_ONLY);; Default value

;;;;** $::SQL_ROWSET_SIZE options
(defconstant $::SQL_ROWSET_SIZE_DEFAULT 1)

;;;;** $::SQL_KEYSET_SIZE options
(defconstant $::SQL_KEYSET_SIZE_DEFAULT 0)

;;;;** $::SQL_SIMULATE_CURSOR options
(defconstant $::SQL_SC_NON_UNIQUE 0)
(defconstant $::SQL_SC_TRY_UNIQUE 1)
(defconstant $::SQL_SC_UNIQUE 2)

;;;;** $::SQL_RETRIEVE_DATA options
(defconstant $::SQL_RD_OFF 0)
(defconstant $::SQL_RD_ON 1)
(defconstant $::SQL_RD_DEFAULT $::SQL_RD_ON)

;;;;** $::SQL_USE_BOOKMARKS options
(defconstant $::SQL_UB_OFF 0)
(defconstant $::SQL_UB_ON 01)
(defconstant $::SQL_UB_DEFAULT $::SQL_UB_OFF)

;;;;** New values for $::SQL_USE_BOOKMARKS attribute
#-odbc2
(progn
  (defconstant $::SQL_UB_FIXED $::SQL_UB_ON)
  (defconstant $::SQL_UB_VARIABLE 2)

;;;;** extended descriptor field
  (defconstant $::SQL_DESC_ARRAY_SIZE 20)
  (defconstant $::SQL_DESC_ARRAY_STATUS_PTR 21)
  (defconstant $::SQL_DESC_AUTO_UNIQUE_VALUE $::SQL_COLUMN_AUTO_INCREMENT)
  (defconstant $::SQL_DESC_BASE_COLUMN_NAME 22)
  (defconstant $::SQL_DESC_BASE_TABLE_NAME 23)
  (defconstant $::SQL_DESC_BIND_OFFSET_PTR 24)
  (defconstant $::SQL_DESC_BIND_TYPE 25)
  (defconstant $::SQL_DESC_CASE_SENSITIVE $::SQL_COLUMN_CASE_SENSITIVE)
  (defconstant $::SQL_DESC_CATALOG_NAME $::SQL_COLUMN_QUALIFIER_NAME)
  (defconstant $::SQL_DESC_CONCISE_TYPE $::SQL_COLUMN_TYPE)
  (defconstant $::SQL_DESC_DATETIME_INTERVAL_PRECISION 26)
  (defconstant $::SQL_DESC_DISPLAY_SIZE $::SQL_COLUMN_DISPLAY_SIZE)
  (defconstant $::SQL_DESC_FIXED_PREC_SCALE $::SQL_COLUMN_MONEY)
  (defconstant $::SQL_DESC_LABEL $::SQL_COLUMN_LABEL)
  (defconstant $::SQL_DESC_LITERAL_PREFIX 27)
  (defconstant $::SQL_DESC_LITERAL_SUFFIX 28)
  (defconstant $::SQL_DESC_LOCAL_TYPE_NAME 29)
  (defconstant $::SQL_DESC_MAXIMUM_SCALE 30)
  (defconstant $::SQL_DESC_MINIMUM_SCALE 31)
  (defconstant $::SQL_DESC_NUM_PREC_RADIX 32)
  (defconstant $::SQL_DESC_PARAMETER_TYPE 33)
  (defconstant $::SQL_DESC_ROWS_PROCESSED_PTR 34)
  ;;#if (ODBCVER >= 0x0350)
  (defconstant $::SQL_DESC_ROWVER 35)
  ;;#endif /* ODBCVER >= 0x0350 */
  (defconstant $::SQL_DESC_SCHEMA_NAME $::SQL_COLUMN_OWNER_NAME)
  (defconstant $::SQL_DESC_SEARCHABLE $::SQL_COLUMN_SEARCHABLE)
  (defconstant $::SQL_DESC_TYPE_NAME $::SQL_COLUMN_TYPE_NAME)
  (defconstant $::SQL_DESC_TABLE_NAME $::SQL_COLUMN_TABLE_NAME)
  (defconstant $::SQL_DESC_UNSIGNED $::SQL_COLUMN_UNSIGNED)
  (defconstant $::SQL_DESC_UPDATABLE $::SQL_COLUMN_UPDATABLE))

;;;;** SQL extended datatypes
(defconstant $::SQL_DATE 9)
#-odbc2
(defconstant $::SQL_INTERVAL 10)

(defconstant $::SQL_TIME 10)
(defconstant $::SQL_TIMESTAMP 11)
(defconstant $::SQL_LONGVARCHAR -1)
(defconstant $::SQL_BINARY -2)
(defconstant $::SQL_VARBINARY -3)
(defconstant $::SQL_LONGVARBINARY -4)
(defconstant $::SQL_BIGINT -5)
(defconstant $::SQL_TINYINT -6)
(defconstant $::SQL_BIT -7)
#-odbc2
(progn
  ;;#if (ODBCVER >= 0x0350)
  (defconstant $::SQL_GUID -11)
  ;;#endif  /* ODBCVER >= 0x0350 */

;;;;** interval code
  (defconstant $::SQL_CODE_YEAR 1)
  (defconstant $::SQL_CODE_MONTH 2)
  (defconstant $::SQL_CODE_DAY 3)
  (defconstant $::SQL_CODE_HOUR 4)
  (defconstant $::SQL_CODE_MINUTE 5)
  (defconstant $::SQL_CODE_SECOND 6)
  (defconstant $::SQL_CODE_YEAR_TO_MONTH 7)
  (defconstant $::SQL_CODE_DAY_TO_HOUR 8)
  (defconstant $::SQL_CODE_DAY_TO_MINUTE 9)
  (defconstant $::SQL_CODE_DAY_TO_SECOND 10)
  (defconstant $::SQL_CODE_HOUR_TO_MINUTE 11)
  (defconstant $::SQL_CODE_HOUR_TO_SECOND 12)
  (defconstant $::SQL_CODE_MINUTE_TO_SECOND 13)

  (defconstant $::SQL_INTERVAL_YEAR (+ 100 $::SQL_CODE_YEAR))
  (defconstant $::SQL_INTERVAL_MONTH (+ 100 $::SQL_CODE_MONTH))
  (defconstant $::SQL_INTERVAL_DAY (+ 100 $::SQL_CODE_DAY))
  (defconstant $::SQL_INTERVAL_HOUR (+ 100 $::SQL_CODE_HOUR))
  (defconstant $::SQL_INTERVAL_MINUTE (+ 100 $::SQL_CODE_MINUTE))
  (defconstant $::SQL_INTERVAL_SECOND (+ 100 $::SQL_CODE_SECOND))
  (defconstant $::SQL_INTERVAL_YEAR_TO_MONTH (+ 100 $::SQL_CODE_YEAR_TO_MONTH))
  (defconstant $::SQL_INTERVAL_DAY_TO_HOUR (+ 100 $::SQL_CODE_DAY_TO_HOUR))
  (defconstant $::SQL_INTERVAL_DAY_TO_MINUTE (+ 100 $::SQL_CODE_DAY_TO_MINUTE))
  (defconstant $::SQL_INTERVAL_DAY_TO_SECOND (+ 100 $::SQL_CODE_DAY_TO_SECOND))
  (defconstant $::SQL_INTERVAL_HOUR_TO_MINUTE (+ 100 $::SQL_CODE_HOUR_TO_MINUTE))
  (defconstant $::SQL_INTERVAL_HOUR_TO_SECOND (+ 100 $::SQL_CODE_HOUR_TO_SECOND))
  (defconstant $::SQL_INTERVAL_MINUTE_TO_SECOND (+ 100 $::SQL_CODE_MINUTE_TO_SECOND)))

#+odbc2
(progn
  (defconstant $::SQL_INTERVAL_YEAR -80)
  (defconstant $::SQL_INTERVAL_MONTH -81)
  (defconstant $::SQL_INTERVAL_YEAR_TO_MONTH -82)
  (defconstant $::SQL_INTERVAL_DAY -83)
  (defconstant $::SQL_INTERVAL_HOUR -84)
  (defconstant $::SQL_INTERVAL_MINUTE -85)
  (defconstant $::SQL_INTERVAL_SECOND -86)
  (defconstant $::SQL_INTERVAL_DAY_TO_HOUR -87)
  (defconstant $::SQL_INTERVAL_DAY_TO_MINUTE -88)
  (defconstant $::SQL_INTERVAL_DAY_TO_SECOND -89)
  (defconstant $::SQL_INTERVAL_HOUR_TO_MINUTE -90)
  (defconstant $::SQL_INTERVAL_HOUR_TO_SECOND -91)
  (defconstant $::SQL_INTERVAL_MINUTE_TO_SECOND -92)

;;;;** Obsolete definitions for $::SQL_UNICODE_
  (defconstant $::SQL_UNICODE -95)
  (defconstant $::SQL_UNICODE_VARCHAR -96)
  (defconstant $::SQL_UNICODE_LONGVARCHAR -97)
  (defconstant $::SQL_UNICODE_CHAR $::SQL_UNICODE))

;;;;** The previous definitions for $::SQL_UNICODE_ are historical and obsolete
(defconstant $::SQL_WCHAR -8)
(defconstant $::SQL_WVARCHAR -9)
(defconstant $::SQL_WLONGVARCHAR -10)
(defconstant $::SQL_C_WCHAR $::SQL_WCHAR)

#+sb-unicode
(defconstant $::SQL_C_TCHAR $::SQL_C_WCHAR)
#-sb-unicode
(defconstant $::SQL_C_TCHAR $::SQL_C_CHAR)

(defconstant $::SQL_SQLSTATE_SIZEW 10); size of SQLSTATE for unicode
#-odbc2
(progn
  (defconstant $::SQL_UNICODE $::SQL_WCHAR)

  (defconstant $::SQL_UNICODE_VARCHAR $::SQL_WVARCHAR)
  (defconstant $::SQL_UNICODE_LONGVARCHAR $::SQL_WLONGVARCHAR)
  (defconstant $::SQL_UNICODE_CHAR $::SQL_WCHAR))

#+odbc2
(progn
  (defconstant $::SQL_TYPE_DRIVER_START $::SQL_INTERVAL_YEAR)
  (defconstant $::SQL_TYPE_DRIVER_END $::SQL_UNICODE_LONGVARCHAR))


;;;;** C Datatype to SQL Datatype Mapping
;;;; SQL types
(defconstant $::SQL_C_CHAR $::SQL_CHAR);; CHAR, VARCHAR, DECIMAL, NUMERIC
(defconstant $::SQL_C_LONG $::SQL_INTEGER);; INTEGER                     
(defconstant $::SQL_C_SHORT $::SQL_SMALLINT);; SMALLINT                    
(defconstant $::SQL_C_FLOAT $::SQL_REAL);; REAL                        
(defconstant $::SQL_C_DOUBLE $::SQL_DOUBLE);; FLOAT, DOUBLE               
#-odbc2
(defconstant $::SQL_C_NUMERIC $::SQL_NUMERIC)

(defconstant $::SQL_C_DEFAULT 99)

(defconstant $::SQL_SIGNED_OFFSET -20)
(defconstant $::SQL_UNSIGNED_OFFSET -22)

(defconstant $::SQL_C_DATE $::SQL_DATE)
(defconstant $::SQL_C_TIME $::SQL_TIME)
(defconstant $::SQL_C_TIMESTAMP $::SQL_TIMESTAMP)
#-odbc2
(progn
  (defconstant $::SQL_C_TYPE_DATE $::SQL_TYPE_DATE)
  (defconstant $::SQL_C_TYPE_TIME $::SQL_TYPE_TIME)
  (defconstant $::SQL_C_TYPE_TIMESTAMP $::SQL_TYPE_TIMESTAMP)
  (defconstant $::SQL_C_INTERVAL_YEAR $::SQL_INTERVAL_YEAR)
  (defconstant $::SQL_C_INTERVAL_MONTH $::SQL_INTERVAL_MONTH)
  (defconstant $::SQL_C_INTERVAL_DAY $::SQL_INTERVAL_DAY)
  (defconstant $::SQL_C_INTERVAL_HOUR $::SQL_INTERVAL_HOUR)
  (defconstant $::SQL_C_INTERVAL_MINUTE $::SQL_INTERVAL_MINUTE)
  (defconstant $::SQL_C_INTERVAL_SECOND $::SQL_INTERVAL_SECOND)
  (defconstant $::SQL_C_INTERVAL_YEAR_TO_MONTH $::SQL_INTERVAL_YEAR_TO_MONTH)
  (defconstant $::SQL_C_INTERVAL_DAY_TO_HOUR $::SQL_INTERVAL_DAY_TO_HOUR)
  (defconstant $::SQL_C_INTERVAL_DAY_TO_MINUTE $::SQL_INTERVAL_DAY_TO_MINUTE)
  (defconstant $::SQL_C_INTERVAL_DAY_TO_SECOND $::SQL_INTERVAL_DAY_TO_SECOND)
  (defconstant $::SQL_C_INTERVAL_HOUR_TO_MINUTE $::SQL_INTERVAL_HOUR_TO_MINUTE)
  (defconstant $::SQL_C_INTERVAL_HOUR_TO_SECOND $::SQL_INTERVAL_HOUR_TO_SECOND)
  (defconstant $::SQL_C_INTERVAL_MINUTE_TO_SECOND $::SQL_INTERVAL_MINUTE_TO_SECOND))

(defconstant $::SQL_C_BINARY $::SQL_BINARY)
(defconstant $::SQL_C_BIT $::SQL_BIT)
#-odbc2
(progn
  (defconstant $::SQL_C_SBIGINT (+ $::SQL_BIGINT $::SQL_SIGNED_OFFSET));; SIGNED BIGINT
  (defconstant $::SQL_C_UBIGINT (+ $::SQL_BIGINT $::SQL_UNSIGNED_OFFSET)));; UNSIGNED BIGINT
(defconstant $::SQL_C_TINYINT $::SQL_TINYINT)
(defconstant $::SQL_C_SLONG (+ $::SQL_C_LONG $::SQL_SIGNED_OFFSET));; SIGNED INTEGER 
(defconstant $::SQL_C_SSHORT (+ $::SQL_C_SHORT $::SQL_SIGNED_OFFSET));; SIGNED SMALLINT
(defconstant $::SQL_C_STINYINT (+ $::SQL_TINYINT $::SQL_SIGNED_OFFSET));; SIGNED TINYINT 
(defconstant $::SQL_C_ULONG (+ $::SQL_C_LONG $::SQL_UNSIGNED_OFFSET));; UNSIGNED INTEGER
(defconstant $::SQL_C_USHORT (+ $::SQL_C_SHORT $::SQL_UNSIGNED_OFFSET));; UNSIGNED SMALLINT
(defconstant $::SQL_C_UTINYINT (+ $::SQL_TINYINT $::SQL_UNSIGNED_OFFSET));; UNSIGNED TINYINT

#+x86-64
(defconstant $::SQL_C_BOOKMARK $::SQL_C_UBIGINT);; BOOKMARK       
#-x86-64
(defconstant $::SQL_C_BOOKMARK $::SQL_C_ULONG);; BOOKMARK       
#-odbc2
;;#if (ODBCVER >= 0x0350)
(defconstant $::SQL_C_GUID $::SQL_GUID)
;;#endif  /* ODBCVER >= 0x0350 */
(defconstant $::SQL_TYPE_NULL 0)
#+odbc2
(progn
  (defconstant $::SQL_TYPE_MIN $::SQL_BIT)
  (defconstant $::SQL_TYPE_MAX $::SQL_VARCHAR))

#-odbc2
(progn
;;;; base value of driver-specific C-Type (max is 0x7fff)
;;;; define driver-specific C-Type, named as $::SQL_DRIVER_C_TYPE_BASE, 
;;;; $::SQL_DRIVER_C_TYPE_BASE+1, $::SQL_DRIVER_C_TYPE_BASE+2, etc.
  ;;#if (ODBCVER >= 0x380)
  (defconstant $::SQL_DRIVER_C_TYPE_BASE #x4000)
  ;;#endif

;;;; base value of driver-specific fields/attributes (max are 0x7fff [16-bit] or 0x00007fff [32-bit])
;;;; define driver-specific SQL-Type, named as $::SQL_DRIVER_SQL_TYPE_BASE, 
;;;; $::SQL_DRIVER_SQL_TYPE_BASE+1, $::SQL_DRIVER_SQL_TYPE_BASE+2, etc.
;;;; 
;;;; Please note that there is no runtime change in this version of DM. 
;;;; However, we suggest that driver manufacturers adhere to this range
;;;; as future versions of the DM may enforce these constraints
  ;;#if (ODBCVER >= 0x380)
  (defconstant $::SQL_DRIVER_SQL_TYPE_BASE #x4000)
  (defconstant $::SQL_DRIVER_DESC_FIELD_BASE #x4000)
  (defconstant $::SQL_DRIVER_DIAG_FIELD_BASE #x4000)
  (defconstant $::SQL_DRIVER_INFO_TYPE_BASE #x4000)
  (defconstant $::SQL_DRIVER_CONN_ATTR_BASE #x00004000); 32-bit
  (defconstant $::SQL_DRIVER_STMT_ATTR_BASE #x00004000); 32-bit
  ;;#endif

  (defconstant $::SQL_C_VARBOOKMARK $::SQL_C_BINARY)

;;;;** define for $::SQL_DIAG_ROW_NUMBER and $::SQL_DIAG_COLUMN_NUMBER
  (defconstant $::SQL_NO_ROW_NUMBER -1)
  (defconstant $::SQL_NO_COLUMN_NUMBER -1)
  (defconstant $::SQL_ROW_NUMBER_UNKNOWN -2)
  (defconstant $::SQL_COLUMN_NUMBER_UNKNOWN -2))

;;;;** SQLBindParameter extensions
(defconstant $::SQL_DEFAULT_PARAM -5)
(defconstant $::SQL_IGNORE -6)
#-odbc2
(defconstant $::SQL_COLUMN_IGNORE $::SQL_IGNORE)

(defconstant $::SQL_LEN_DATA_AT_EXEC_OFFSET -100)
(defmacro $::SQL_LEN_DATA_AT_EXEC (length)
  `(- ,$::SQL_LEN_DATA_AT_EXEC_OFFSET ,length))

;;;;** binary length for driver specific attributes
(defconstant $::SQL_LEN_BINARY_ATTR_OFFSET -100)
(defmacro $::SQL_LEN_BINARY_ATTR (length)
  `(- $::SQL_LEN_BINARY_ATTR_OFFSET ,length))

;;;;********************************************/
;;;;** SQLGetFunctions: additional values for  
;;;;** fFunction to represent functions that   
;;;;** are not in the X/Open spec.             
;;;;********************************************/

#-odbc2
(progn
  (defconstant $::SQL_API_SQLALLOCHANDLESTD 73)
  (defconstant $::SQL_API_SQLBULKOPERATIONS 24))

(defconstant $::SQL_API_SQLBINDPARAMETER 72)
(defconstant $::SQL_API_SQLBROWSECONNECT 55)
(defconstant $::SQL_API_SQLCOLATTRIBUTES 6)
(defconstant $::SQL_API_SQLCOLUMNPRIVILEGES 56)
(defconstant $::SQL_API_SQLDESCRIBEPARAM 58)
(defconstant $::SQL_API_SQLDRIVERCONNECT 41)
(defconstant $::SQL_API_SQLDRIVERS 71)
(defconstant $::SQL_API_SQLEXTENDEDFETCH 59)
(defconstant $::SQL_API_SQLFOREIGNKEYS 60)
(defconstant $::SQL_API_SQLMORERESULTS 61)
(defconstant $::SQL_API_SQLNATIVESQL 62)
(defconstant $::SQL_API_SQLNUMPARAMS 63)
(defconstant $::SQL_API_SQLPARAMOPTIONS 64)
(defconstant $::SQL_API_SQLPRIMARYKEYS 65)
(defconstant $::SQL_API_SQLPROCEDURECOLUMNS 66)
(defconstant $::SQL_API_SQLPROCEDURES 67)
(defconstant $::SQL_API_SQLSETPOS 68)
(defconstant $::SQL_API_SQLSETSCROLLOPTIONS 69)
(defconstant $::SQL_API_SQLTABLEPRIVILEGES 70)

;;;;*-------------------------------------------*/
;;;;** $::SQL_EXT_API_LAST is not useful with ODBC 
;;;;** version 3.0 because some of the values   
;;;;** from X/Open are in the 10000 range.      
;;;;*-------------------------------------------*/

#+odbc2
(progn
  (defconstant $::SQL_EXT_API_LAST $::SQL_API_SQLBINDPARAMETER)
  (defconstant $::SQL_NUM_FUNCTIONS 23)
  (defconstant $::SQL_EXT_API_START 40)
  (defconstant $::SQL_NUM_EXTENSIONS ($::SQL_EXT_API_LAST-$::SQL_EXT_API_START+1)))

;;;;**--------------------------------------------*/
;;;;** $::SQL_API_ALL_FUNCTIONS returns an array    
;;;;** of 'booleans' representing whether a      
;;;;** function is implemented by the driver.    
;;;;**                                           
;;;;** CAUTION: Only functions defined in ODBC   
;;;;** version 2.0 and earlier are returned, the 
;;;;** new high-range function numbers defined by
;;;;** X/Open break this scheme.   See the new   
;;;;** method -- $::SQL_API_ODBC3_ALL_FUNCTIONS     
;;;;**--------------------------------------------*/

(defconstant $::SQL_API_ALL_FUNCTIONS 0);; See CAUTION above

;;;;**----------------------------------------------*/
;;;;** 2.X drivers export a dummy function with    
;;;;** ordinal number $::SQL_API_LOADBYORDINAL to speed
;;;;** loading under the windows operating system. 
;;;;**                     
;;;;** CAUTION: Loading by ordinal is not supported
;;;;** for 3.0 and above drivers.          
;;;;**----------------------------------------------*/

(defconstant $::SQL_API_LOADBYORDINAL 199);; See CAUTION above

;;;;**----------------------------------------------*/
;;;;** $::SQL_API_ODBC3_ALL_FUNCTIONS                 
;;;;** This returns a bitmap, which allows us to   
;;;;** handle the higher-valued function numbers.  
;;;;** Use  $::SQL_FUNC_EXISTS(bitmap,function_number)
;;;;** to determine if the function exists.        
;;;;**----------------------------------------------*/


#-odbc2
(progn
  (defconstant $::SQL_API_ODBC3_ALL_FUNCTIONS 999)
  (defconstant $::SQL_API_ODBC3_ALL_FUNCTIONS_SIZE 250);; array of 250 words
  #+ (or)
  (defconstant $::SQL_FUNC_EXISTS(pfExists, uwAPI)
    ((*(((UWORD*) (pfExists)) + ((uwAPI) >> 4)) \
       & (1 << ((uwAPI) & 0x000F)) \
       ) ? $::SQL_TRUE : $::SQL_FALSE \
     )))



;;;;************************************************/
;;;;** Extended definitions for SQLGetInfo         
;;;;************************************************/

;;;;**---------------------------------*/
;;;;** Values in ODBC 2.0 that are not
;;;;** in the X/Open spec             
;;;;**---------------------------------*/

(defconstant $::SQL_INFO_FIRST 0)
(defconstant $::SQL_ACTIVE_CONNECTIONS 0);; MAX_DRIVER_CONNECTIONS
(defconstant $::SQL_ACTIVE_STATEMENTS 1);; MAX_CONCURRENT_ACTIVITIES
(defconstant $::SQL_DRIVER_HDBC 3)
(defconstant $::SQL_DRIVER_HENV 4)
(defconstant $::SQL_DRIVER_HSTMT 5)
(defconstant $::SQL_DRIVER_NAME 6)
(defconstant $::SQL_DRIVER_VER 7)
(defconstant $::SQL_ODBC_API_CONFORMANCE 9)
(defconstant $::SQL_ODBC_VER 10)
(defconstant $::SQL_ROW_UPDATES 11)
(defconstant $::SQL_ODBC_SAG_CLI_CONFORMANCE 12)
(defconstant $::SQL_ODBC_SQL_CONFORMANCE 15)
(defconstant $::SQL_PROCEDURES 21)
(defconstant $::SQL_CONCAT_NULL_BEHAVIOR 22)
(defconstant $::SQL_CURSOR_ROLLBACK_BEHAVIOR 24)
(defconstant $::SQL_EXPRESSIONS_IN_ORDERBY 27)
(defconstant $::SQL_MAX_OWNER_NAME_LEN 32);; MAX_SCHEMA_NAME_LEN
(defconstant $::SQL_MAX_PROCEDURE_NAME_LEN 33)
(defconstant $::SQL_MAX_QUALIFIER_NAME_LEN 34);; MAX_CATALOG_NAME_LEN
(defconstant $::SQL_MULT_RESULT_SETS 36)
(defconstant $::SQL_MULTIPLE_ACTIVE_TXN 37)
(defconstant $::SQL_OUTER_JOINS 38)
(defconstant $::SQL_OWNER_TERM 39)
(defconstant $::SQL_PROCEDURE_TERM 40)
(defconstant $::SQL_QUALIFIER_NAME_SEPARATOR 41)
(defconstant $::SQL_QUALIFIER_TERM 42)
(defconstant $::SQL_SCROLL_OPTIONS 44)
(defconstant $::SQL_TABLE_TERM 45)
(defconstant $::SQL_CONVERT_FUNCTIONS 48)
(defconstant $::SQL_NUMERIC_FUNCTIONS 49)
(defconstant $::SQL_STRING_FUNCTIONS 50)
(defconstant $::SQL_SYSTEM_FUNCTIONS 51)
(defconstant $::SQL_TIMEDATE_FUNCTIONS 52)
(defconstant $::SQL_CONVERT_BIGINT 53)
(defconstant $::SQL_CONVERT_BINARY 54)
(defconstant $::SQL_CONVERT_BIT 55)
(defconstant $::SQL_CONVERT_CHAR 56)
(defconstant $::SQL_CONVERT_DATE 57)
(defconstant $::SQL_CONVERT_DECIMAL 58)
(defconstant $::SQL_CONVERT_DOUBLE 59)
(defconstant $::SQL_CONVERT_FLOAT 60)
(defconstant $::SQL_CONVERT_INTEGER 61)
(defconstant $::SQL_CONVERT_LONGVARCHAR 62)
(defconstant $::SQL_CONVERT_NUMERIC 63)
(defconstant $::SQL_CONVERT_REAL 64)
(defconstant $::SQL_CONVERT_SMALLINT 65)
(defconstant $::SQL_CONVERT_TIME 66)
(defconstant $::SQL_CONVERT_TIMESTAMP 67)
(defconstant $::SQL_CONVERT_TINYINT 68)
(defconstant $::SQL_CONVERT_VARBINARY 69)
(defconstant $::SQL_CONVERT_VARCHAR 70)
(defconstant $::SQL_CONVERT_LONGVARBINARY 71)
(defconstant $::SQL_ODBC_SQL_OPT_IEF 73);; $::SQL_INTEGRITY
(defconstant $::SQL_CORRELATION_NAME 74)
(defconstant $::SQL_NON_NULLABLE_COLUMNS 75)
(defconstant $::SQL_DRIVER_HLIB 76)
(defconstant $::SQL_DRIVER_ODBC_VER 77)
(defconstant $::SQL_LOCK_TYPES 78)
(defconstant $::SQL_POS_OPERATIONS 79)
(defconstant $::SQL_POSITIONED_STATEMENTS 80)
(defconstant $::SQL_BOOKMARK_PERSISTENCE 82)
(defconstant $::SQL_STATIC_SENSITIVITY 83)
(defconstant $::SQL_FILE_USAGE 84)
(defconstant $::SQL_COLUMN_ALIAS 87)
(defconstant $::SQL_GROUP_BY 88)
(defconstant $::SQL_KEYWORDS 89)
(defconstant $::SQL_OWNER_USAGE 91)
(defconstant $::SQL_QUALIFIER_USAGE 92)
(defconstant $::SQL_QUOTED_IDENTIFIER_CASE 93)
(defconstant $::SQL_SUBQUERIES 95)
(defconstant $::SQL_UNION 96)
(defconstant $::SQL_MAX_ROW_SIZE_INCLUDES_LONG 103)
(defconstant $::SQL_MAX_CHAR_LITERAL_LEN 108)
(defconstant $::SQL_TIMEDATE_ADD_INTERVALS 109)
(defconstant $::SQL_TIMEDATE_DIFF_INTERVALS 110)
(defconstant $::SQL_NEED_LONG_DATA_LEN 111)
(defconstant $::SQL_MAX_BINARY_LITERAL_LEN 112)
(defconstant $::SQL_LIKE_ESCAPE_CLAUSE 113)
(defconstant $::SQL_QUALIFIER_LOCATION 114)

#+odbc2
;;#if (ODBCVER >= 0x0201 && ODBCVER < 0x0300)
(defconstant $::SQL_OJ_CAPABILITIES 65003);; Temp value until ODBC 3.0
;;#endif  /* ODBCVER >= 0x0201 && ODBCVER < 0x0300 */

;;;;**----------------------------------------------*/
;;;;** $::SQL_INFO_LAST and $::SQL_INFO_DRIVER_START are 
;;;;** not useful anymore, because  X/Open has     
;;;;** values in the 10000 range.   You            
;;;;** must contact X/Open directly to get a range 
;;;;** of numbers for driver-specific values.      
;;;;**----------------------------------------------*/

#+odbc2
(progn
  (defconstant $::SQL_INFO_LAST $::SQL_QUALIFIER_LOCATION)
  (defconstant $::SQL_INFO_DRIVER_START 1000))

;;;;**-----------------------------------------------*/
;;;;** ODBC 3.0 SQLGetInfo values that are not part 
;;;;** of the X/Open standard at this time.   X/Open
;;;;** standard values are in sql.h.                
;;;;**-----------------------------------------------*/

#-odbc2
(progn
  (defconstant $::SQL_ACTIVE_ENVIRONMENTS 116)
  (defconstant $::SQL_ALTER_DOMAIN 117)

  (defconstant $::SQL_SQL_CONFORMANCE 118)
  (defconstant $::SQL_DATETIME_LITERALS 119)

  (defconstant $::SQL_ASYNC_MODE 10021);; new X/Open spec
  (defconstant $::SQL_BATCH_ROW_COUNT 120)
  (defconstant $::SQL_BATCH_SUPPORT 121)
  (defconstant $::SQL_CATALOG_LOCATION $::SQL_QUALIFIER_LOCATION)
  (defconstant $::SQL_CATALOG_NAME_SEPARATOR $::SQL_QUALIFIER_NAME_SEPARATOR)
  (defconstant $::SQL_CATALOG_TERM $::SQL_QUALIFIER_TERM)
  (defconstant $::SQL_CATALOG_USAGE $::SQL_QUALIFIER_USAGE)
  (defconstant $::SQL_CONVERT_WCHAR 122)
  (defconstant $::SQL_CONVERT_INTERVAL_DAY_TIME 123)
  (defconstant $::SQL_CONVERT_INTERVAL_YEAR_MONTH 124)
  (defconstant $::SQL_CONVERT_WLONGVARCHAR 125)
  (defconstant $::SQL_CONVERT_WVARCHAR 126)
  (defconstant $::SQL_CREATE_ASSERTION 127)
  (defconstant $::SQL_CREATE_CHARACTER_SET 128)
  (defconstant $::SQL_CREATE_COLLATION 129)
  (defconstant $::SQL_CREATE_DOMAIN 130)
  (defconstant $::SQL_CREATE_SCHEMA 131)
  (defconstant $::SQL_CREATE_TABLE 132)
  (defconstant $::SQL_CREATE_TRANSLATION 133)
  (defconstant $::SQL_CREATE_VIEW 134)
  (defconstant $::SQL_DRIVER_HDESC 135)
  (defconstant $::SQL_DROP_ASSERTION 136)
  (defconstant $::SQL_DROP_CHARACTER_SET 137)
  (defconstant $::SQL_DROP_COLLATION 138)
  (defconstant $::SQL_DROP_DOMAIN 139)
  (defconstant $::SQL_DROP_SCHEMA 140)
  (defconstant $::SQL_DROP_TABLE 141)
  (defconstant $::SQL_DROP_TRANSLATION 142)
  (defconstant $::SQL_DROP_VIEW 143)
  (defconstant $::SQL_DYNAMIC_CURSOR_ATTRIBUTES1 144)
  (defconstant $::SQL_DYNAMIC_CURSOR_ATTRIBUTES2 145)
  (defconstant $::SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 146)
  (defconstant $::SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 147)
  (defconstant $::SQL_INDEX_KEYWORDS 148)
  (defconstant $::SQL_INFO_SCHEMA_VIEWS 149)
  (defconstant $::SQL_KEYSET_CURSOR_ATTRIBUTES1 150)
  (defconstant $::SQL_KEYSET_CURSOR_ATTRIBUTES2 151)
  (defconstant $::SQL_MAX_ASYNC_CONCURRENT_STATEMENTS 10022);; new X/Open spec
  (defconstant $::SQL_ODBC_INTERFACE_CONFORMANCE 152)
  (defconstant $::SQL_PARAM_ARRAY_ROW_COUNTS 153)
  (defconstant $::SQL_PARAM_ARRAY_SELECTS 154)
  (defconstant $::SQL_SCHEMA_TERM $::SQL_OWNER_TERM)
  (defconstant $::SQL_SCHEMA_USAGE $::SQL_OWNER_USAGE)
  (defconstant $::SQL_SQL92_DATETIME_FUNCTIONS 155)
  (defconstant $::SQL_SQL92_FOREIGN_KEY_DELETE_RULE 156)
  (defconstant $::SQL_SQL92_FOREIGN_KEY_UPDATE_RULE 157)
  (defconstant $::SQL_SQL92_GRANT 158)
  (defconstant $::SQL_SQL92_NUMERIC_VALUE_FUNCTIONS 159)
  (defconstant $::SQL_SQL92_PREDICATES 160)
  (defconstant $::SQL_SQL92_RELATIONAL_JOIN_OPERATORS 161)
  (defconstant $::SQL_SQL92_REVOKE 162)
  (defconstant $::SQL_SQL92_ROW_VALUE_CONSTRUCTOR 163)
  (defconstant $::SQL_SQL92_STRING_FUNCTIONS 164)
  (defconstant $::SQL_SQL92_VALUE_EXPRESSIONS 165)
  (defconstant $::SQL_STANDARD_CLI_CONFORMANCE 166)
  (defconstant $::SQL_STATIC_CURSOR_ATTRIBUTES1 167)
  (defconstant $::SQL_STATIC_CURSOR_ATTRIBUTES2 168)

  (defconstant $::SQL_AGGREGATE_FUNCTIONS 169)
  (defconstant $::SQL_DDL_INDEX 170)
  (defconstant $::SQL_DM_VER 171)
  (defconstant $::SQL_INSERT_STATEMENT 172)
  (defconstant $::SQL_CONVERT_GUID 173)
  (defconstant $::SQL_UNION_STATEMENT $::SQL_UNION)

  ;;#if (ODBCVER >= 0x0380)
;;;;** Info Types
  (defconstant $::SQL_ASYNC_DBC_FUNCTIONS 10023))
;;#endif // ODBCVER >= 0x0380

(defconstant $::SQL_DTC_TRANSITION_COST 1750)

;;;;** $::SQL_ALTER_TABLE bitmasks
#-odbc2
(progn
;;;;/* the following 5 bitmasks are defined in sql.h
;;;;*#define $::SQL_AT_ADD_COLUMN                      #x00000001
;;;;*#define $::SQL_AT_DROP_COLUMN                     #x00000002
;;;;*#define $::SQL_AT_ADD_CONSTRAINT                  #x00000008
;;;;*/
  (defconstant $::SQL_AT_ADD_COLUMN_SINGLE #x00000020)
  (defconstant $::SQL_AT_ADD_COLUMN_DEFAULT #x00000040)
  (defconstant $::SQL_AT_ADD_COLUMN_COLLATION #x00000080)
  (defconstant $::SQL_AT_SET_COLUMN_DEFAULT #x00000100)
  (defconstant $::SQL_AT_DROP_COLUMN_DEFAULT #x00000200)
  (defconstant $::SQL_AT_DROP_COLUMN_CASCADE #x00000400)
  (defconstant $::SQL_AT_DROP_COLUMN_RESTRICT #x00000800)
  (defconstant $::SQL_AT_ADD_TABLE_CONSTRAINT #x00001000)
  (defconstant $::SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE #x00002000)
  (defconstant $::SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT #x00004000)
  (defconstant $::SQL_AT_CONSTRAINT_NAME_DEFINITION #x00008000)
  (defconstant $::SQL_AT_CONSTRAINT_INITIALLY_DEFERRED #x00010000)
  (defconstant $::SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE #x00020000)
  (defconstant $::SQL_AT_CONSTRAINT_DEFERRABLE #x00040000)
  (defconstant $::SQL_AT_CONSTRAINT_NON_DEFERRABLE #x00080000))

;;;;** $::SQL_CONVERT_*  return value bitmasks
(defconstant $::SQL_CVT_CHAR #x00000001)
(defconstant $::SQL_CVT_NUMERIC #x00000002)
(defconstant $::SQL_CVT_DECIMAL #x00000004)
(defconstant $::SQL_CVT_INTEGER #x00000008)
(defconstant $::SQL_CVT_SMALLINT #x00000010)
(defconstant $::SQL_CVT_FLOAT #x00000020)
(defconstant $::SQL_CVT_REAL #x00000040)
(defconstant $::SQL_CVT_DOUBLE #x00000080)
(defconstant $::SQL_CVT_VARCHAR #x00000100)
(defconstant $::SQL_CVT_LONGVARCHAR #x00000200)
(defconstant $::SQL_CVT_BINARY #x00000400)
(defconstant $::SQL_CVT_VARBINARY #x00000800)
(defconstant $::SQL_CVT_BIT #x00001000)
(defconstant $::SQL_CVT_TINYINT #x00002000)
(defconstant $::SQL_CVT_BIGINT #x00004000)
(defconstant $::SQL_CVT_DATE #x00008000)
(defconstant $::SQL_CVT_TIME #x00010000)
(defconstant $::SQL_CVT_TIMESTAMP #x00020000)
(defconstant $::SQL_CVT_LONGVARBINARY #x00040000)
#-odbc2
(progn
  (defconstant $::SQL_CVT_INTERVAL_YEAR_MONTH #x00080000)
  (defconstant $::SQL_CVT_INTERVAL_DAY_TIME #x00100000)
  (defconstant $::SQL_CVT_WCHAR #x00200000)
  (defconstant $::SQL_CVT_WLONGVARCHAR #x00400000)
  (defconstant $::SQL_CVT_WVARCHAR #x00800000)
  (defconstant $::SQL_CVT_GUID #x01000000))

;;;;** $::SQL_CONVERT_FUNCTIONS functions
(defconstant $::SQL_FN_CVT_CONVERT #x00000001)
#-odbc2
(defconstant $::SQL_FN_CVT_CAST #x00000002)

;;;;** $::SQL_STRING_FUNCTIONS functions
(defconstant $::SQL_FN_STR_CONCAT #x00000001)
(defconstant $::SQL_FN_STR_INSERT #x00000002)
(defconstant $::SQL_FN_STR_LEFT #x00000004)
(defconstant $::SQL_FN_STR_LTRIM #x00000008)
(defconstant $::SQL_FN_STR_LENGTH #x00000010)
(defconstant $::SQL_FN_STR_LOCATE #x00000020)
(defconstant $::SQL_FN_STR_LCASE #x00000040)
(defconstant $::SQL_FN_STR_REPEAT #x00000080)
(defconstant $::SQL_FN_STR_REPLACE #x00000100)
(defconstant $::SQL_FN_STR_RIGHT #x00000200)
(defconstant $::SQL_FN_STR_RTRIM #x00000400)
(defconstant $::SQL_FN_STR_SUBSTRING #x00000800)
(defconstant $::SQL_FN_STR_UCASE #x00001000)
(defconstant $::SQL_FN_STR_ASCII #x00002000)
(defconstant $::SQL_FN_STR_CHAR #x00004000)
(defconstant $::SQL_FN_STR_DIFFERENCE #x00008000)
(defconstant $::SQL_FN_STR_LOCATE_2 #x00010000)
(defconstant $::SQL_FN_STR_SOUNDEX #x00020000)
(defconstant $::SQL_FN_STR_SPACE #x00040000)
#-odbc2
(progn
  (defconstant $::SQL_FN_STR_BIT_LENGTH #x00080000)
  (defconstant $::SQL_FN_STR_CHAR_LENGTH #x00100000)
  (defconstant $::SQL_FN_STR_CHARACTER_LENGTH #x00200000)
  (defconstant $::SQL_FN_STR_OCTET_LENGTH #x00400000)
  (defconstant $::SQL_FN_STR_POSITION #x00800000)

;;;;** $::SQL_SQL92_STRING_FUNCTIONS
  (defconstant $::SQL_SSF_CONVERT #x00000001)
  (defconstant $::SQL_SSF_LOWER #x00000002)
  (defconstant $::SQL_SSF_UPPER #x00000004)
  (defconstant $::SQL_SSF_SUBSTRING #x00000008)
  (defconstant $::SQL_SSF_TRANSLATE #x00000010)
  (defconstant $::SQL_SSF_TRIM_BOTH #x00000020)
  (defconstant $::SQL_SSF_TRIM_LEADING #x00000040)
  (defconstant $::SQL_SSF_TRIM_TRAILING #x00000080))

;;;;** $::SQL_NUMERIC_FUNCTIONS functions
(defconstant $::SQL_FN_NUM_ABS #x00000001)
(defconstant $::SQL_FN_NUM_ACOS #x00000002)
(defconstant $::SQL_FN_NUM_ASIN #x00000004)
(defconstant $::SQL_FN_NUM_ATAN #x00000008)
(defconstant $::SQL_FN_NUM_ATAN2 #x00000010)
(defconstant $::SQL_FN_NUM_CEILING #x00000020)
(defconstant $::SQL_FN_NUM_COS #x00000040)
(defconstant $::SQL_FN_NUM_COT #x00000080)
(defconstant $::SQL_FN_NUM_EXP #x00000100)
(defconstant $::SQL_FN_NUM_FLOOR #x00000200)
(defconstant $::SQL_FN_NUM_LOG #x00000400)
(defconstant $::SQL_FN_NUM_MOD #x00000800)
(defconstant $::SQL_FN_NUM_SIGN #x00001000)
(defconstant $::SQL_FN_NUM_SIN #x00002000)
(defconstant $::SQL_FN_NUM_SQRT #x00004000)
(defconstant $::SQL_FN_NUM_TAN #x00008000)
(defconstant $::SQL_FN_NUM_PI #x00010000)
(defconstant $::SQL_FN_NUM_RAND #x00020000)
(defconstant $::SQL_FN_NUM_DEGREES #x00040000)
(defconstant $::SQL_FN_NUM_LOG10 #x00080000)
(defconstant $::SQL_FN_NUM_POWER #x00100000)
(defconstant $::SQL_FN_NUM_RADIANS #x00200000)
(defconstant $::SQL_FN_NUM_ROUND #x00400000)
(defconstant $::SQL_FN_NUM_TRUNCATE #x00800000)

;;;;** $::SQL_SQL92_NUMERIC_VALUE_FUNCTIONS
#-odbc2
(progn
  (defconstant $::SQL_SNVF_BIT_LENGTH #x00000001)
  (defconstant $::SQL_SNVF_CHAR_LENGTH #x00000002)
  (defconstant $::SQL_SNVF_CHARACTER_LENGTH #x00000004)
  (defconstant $::SQL_SNVF_EXTRACT #x00000008)
  (defconstant $::SQL_SNVF_OCTET_LENGTH #x00000010)
  (defconstant $::SQL_SNVF_POSITION #x00000020))

;;;;** $::SQL_TIMEDATE_FUNCTIONS functions
(defconstant $::SQL_FN_TD_NOW #x00000001)
(defconstant $::SQL_FN_TD_CURDATE #x00000002)
(defconstant $::SQL_FN_TD_DAYOFMONTH #x00000004)
(defconstant $::SQL_FN_TD_DAYOFWEEK #x00000008)
(defconstant $::SQL_FN_TD_DAYOFYEAR #x00000010)
(defconstant $::SQL_FN_TD_MONTH #x00000020)
(defconstant $::SQL_FN_TD_QUARTER #x00000040)
(defconstant $::SQL_FN_TD_WEEK #x00000080)
(defconstant $::SQL_FN_TD_YEAR #x00000100)
(defconstant $::SQL_FN_TD_CURTIME #x00000200)
(defconstant $::SQL_FN_TD_HOUR #x00000400)
(defconstant $::SQL_FN_TD_MINUTE #x00000800)
(defconstant $::SQL_FN_TD_SECOND #x00001000)
(defconstant $::SQL_FN_TD_TIMESTAMPADD #x00002000)
(defconstant $::SQL_FN_TD_TIMESTAMPDIFF #x00004000)
(defconstant $::SQL_FN_TD_DAYNAME #x00008000)
(defconstant $::SQL_FN_TD_MONTHNAME #x00010000)
#-odbc2
(progn
  (defconstant $::SQL_FN_TD_CURRENT_DATE #x00020000)
  (defconstant $::SQL_FN_TD_CURRENT_TIME #x00040000)
  (defconstant $::SQL_FN_TD_CURRENT_TIMESTAMP #x00080000)
  (defconstant $::SQL_FN_TD_EXTRACT #x00100000)

;;;;** $::SQL_SQL92_DATETIME_FUNCTIONS
  (defconstant $::SQL_SDF_CURRENT_DATE #x00000001)
  (defconstant $::SQL_SDF_CURRENT_TIME #x00000002)
  (defconstant $::SQL_SDF_CURRENT_TIMESTAMP #x00000004))

;;;;** $::SQL_SYSTEM_FUNCTIONS functions
(defconstant $::SQL_FN_SYS_USERNAME #x00000001)
(defconstant $::SQL_FN_SYS_DBNAME #x00000002)
(defconstant $::SQL_FN_SYS_IFNULL #x00000004)

;;;;** $::SQL_TIMEDATE_ADD_INTERVALS and $::SQL_TIMEDATE_DIFF_INTERVALS functions
(defconstant $::SQL_FN_TSI_FRAC_SECOND #x00000001)
(defconstant $::SQL_FN_TSI_SECOND #x00000002)
(defconstant $::SQL_FN_TSI_MINUTE #x00000004)
(defconstant $::SQL_FN_TSI_HOUR #x00000008)
(defconstant $::SQL_FN_TSI_DAY #x00000010)
(defconstant $::SQL_FN_TSI_WEEK #x00000020)
(defconstant $::SQL_FN_TSI_MONTH #x00000040)
(defconstant $::SQL_FN_TSI_QUARTER #x00000080)
(defconstant $::SQL_FN_TSI_YEAR #x00000100)

;;;; bitmasks for $::SQL_DYNAMIC_CURSOR_ATTRIBUTES1,
;;;; $::SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1,
;;;; $::SQL_KEYSET_CURSOR_ATTRIBUTES1, and $::SQL_STATIC_CURSOR_ATTRIBUTES1
;;;;
#-odbc2
(progn
;;;;** supported SQLFetchScroll FetchOrientation's
  (defconstant $::SQL_CA1_NEXT #x00000001)
  (defconstant $::SQL_CA1_ABSOLUTE #x00000002)
  (defconstant $::SQL_CA1_RELATIVE #x00000004)
  (defconstant $::SQL_CA1_BOOKMARK #x00000008)

;;;;** supported SQLSetPos LockType's
  (defconstant $::SQL_CA1_LOCK_NO_CHANGE #x00000040)
  (defconstant $::SQL_CA1_LOCK_EXCLUSIVE #x00000080)
  (defconstant $::SQL_CA1_LOCK_UNLOCK #x00000100)

;;;;** supported SQLSetPos Operations
  (defconstant $::SQL_CA1_POS_POSITION #x00000200)
  (defconstant $::SQL_CA1_POS_UPDATE #x00000400)
  (defconstant $::SQL_CA1_POS_DELETE #x00000800)
  (defconstant $::SQL_CA1_POS_REFRESH #x00001000)

;;;;** positioned updates and deletes
  (defconstant $::SQL_CA1_POSITIONED_UPDATE #x00002000)
  (defconstant $::SQL_CA1_POSITIONED_DELETE #x00004000)
  (defconstant $::SQL_CA1_SELECT_FOR_UPDATE #x00008000)

;;;;** supported SQLBulkOperations operations
  (defconstant $::SQL_CA1_BULK_ADD #x00010000)
  (defconstant $::SQL_CA1_BULK_UPDATE_BY_BOOKMARK #x00020000)
  (defconstant $::SQL_CA1_BULK_DELETE_BY_BOOKMARK #x00040000)
  (defconstant $::SQL_CA1_BULK_FETCH_BY_BOOKMARK #x00080000)

;;;; bitmasks for $::SQL_DYNAMIC_CURSOR_ATTRIBUTES2,
;;;; $::SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,
;;;; $::SQL_KEYSET_CURSOR_ATTRIBUTES2, and $::SQL_STATIC_CURSOR_ATTRIBUTES2
;;;;/
;;;;** supported values for $::SQL_ATTR_SCROLL_CONCURRENCY
  (defconstant $::SQL_CA2_READ_ONLY_CONCURRENCY #x00000001)
  (defconstant $::SQL_CA2_LOCK_CONCURRENCY #x00000002)
  (defconstant $::SQL_CA2_OPT_ROWVER_CONCURRENCY #x00000004)
  (defconstant $::SQL_CA2_OPT_VALUES_CONCURRENCY #x00000008)

;;;;** sensitivity of the cursor to its own inserts, deletes, and updates
  (defconstant $::SQL_CA2_SENSITIVITY_ADDITIONS #x00000010)
  (defconstant $::SQL_CA2_SENSITIVITY_DELETIONS #x00000020)
  (defconstant $::SQL_CA2_SENSITIVITY_UPDATES #x00000040)

;;;;** semantics of $::SQL_ATTR_MAX_ROWS
  (defconstant $::SQL_CA2_MAX_ROWS_SELECT #x00000080)
  (defconstant $::SQL_CA2_MAX_ROWS_INSERT #x00000100)
  (defconstant $::SQL_CA2_MAX_ROWS_DELETE #x00000200)
  (defconstant $::SQL_CA2_MAX_ROWS_UPDATE #x00000400)
  (defconstant $::SQL_CA2_MAX_ROWS_CATALOG #x00000800)
  #+ (or)
  (defconstant $::SQL_CA2_MAX_ROWS_AFFECTS_ALL ($::SQL_CA2_MAX_ROWS_SELECT | 
                    $::SQL_CA2_MAX_ROWS_INSERT | $::SQL_CA2_MAX_ROWS_DELETE |
                    $::SQL_CA2_MAX_ROWS_UPDATE | $::SQL_CA2_MAX_ROWS_CATALOG))

;;;;** semantics of $::SQL_DIAG_CURSOR_ROW_COUNT
  (defconstant $::SQL_CA2_CRC_EXACT #x00001000)
  (defconstant $::SQL_CA2_CRC_APPROXIMATE #x00002000)

;;;;** the kinds of positioned statements that can be simulated
  (defconstant $::SQL_CA2_SIMULATE_NON_UNIQUE #x00004000)
  (defconstant $::SQL_CA2_SIMULATE_TRY_UNIQUE #x00008000)
  (defconstant $::SQL_CA2_SIMULATE_UNIQUE #x00010000))

;;;;** $::SQL_ODBC_API_CONFORMANCE values
(defconstant $::SQL_OAC_NONE #x0000)
(defconstant $::SQL_OAC_LEVEL1 #x0001)
(defconstant $::SQL_OAC_LEVEL2 #x0002)

;;;;** $::SQL_ODBC_SAG_CLI_CONFORMANCE values
(defconstant $::SQL_OSCC_NOT_COMPLIANT #x0000)
(defconstant $::SQL_OSCC_COMPLIANT #x0001)

;;;;** $::SQL_ODBC_SQL_CONFORMANCE values
(defconstant $::SQL_OSC_MINIMUM #x0000)
(defconstant $::SQL_OSC_CORE #x0001)
(defconstant $::SQL_OSC_EXTENDED #x0002)

;;;;** $::SQL_CONCAT_NULL_BEHAVIOR values
(defconstant $::SQL_CB_NULL #x0000)
(defconstant $::SQL_CB_NON_NULL #x0001)

;;;;** $::SQL_SCROLL_OPTIONS masks
(defconstant $::SQL_SO_FORWARD_ONLY #x00000001)
(defconstant $::SQL_SO_KEYSET_DRIVEN #x00000002)
(defconstant $::SQL_SO_DYNAMIC #x00000004)
(defconstant $::SQL_SO_MIXED #x00000008)
(defconstant $::SQL_SO_STATIC #x00000010)

;;;;** $::SQL_FETCH_DIRECTION masks

;;;;/* $::SQL_FETCH_RESUME is no longer supported
;;;;(defconstant $::SQL_FD_FETCH_RESUME #x00000040)
;;;;*/
(defconstant $::SQL_FD_FETCH_BOOKMARK #x00000080)

;;;;** $::SQL_TXN_ISOLATION_OPTION masks
;;;;/* $::SQL_TXN_VERSIONING is no longer supported
;;;;(defconstant $::SQL_TXN_VERSIONING #x00000010)
;;;;*/

;;;;** $::SQL_CORRELATION_NAME values
(defconstant $::SQL_CN_NONE #x0000)
(defconstant $::SQL_CN_DIFFERENT #x0001)
(defconstant $::SQL_CN_ANY #x0002)

;;;;** $::SQL_NON_NULLABLE_COLUMNS values
(defconstant $::SQL_NNC_NULL #x0000)
(defconstant $::SQL_NNC_NON_NULL #x0001)

;;;;** $::SQL_NULL_COLLATION values
(defconstant $::SQL_NC_START #x0002)
(defconstant $::SQL_NC_END #x0004)

;;;;** $::SQL_FILE_USAGE values
(defconstant $::SQL_FILE_NOT_SUPPORTED #x0000)
(defconstant $::SQL_FILE_TABLE #x0001)
(defconstant $::SQL_FILE_QUALIFIER #x0002)
(defconstant $::SQL_FILE_CATALOG $::SQL_FILE_QUALIFIER); ODBC 3.0


;;;;** $::SQL_GETDATA_EXTENSIONS values
(defconstant $::SQL_GD_BLOCK #x00000004)
(defconstant $::SQL_GD_BOUND #x00000008)
#-odbc2
;;#if (ODBCVER >= #x0380)
(defconstant $::SQL_GD_OUTPUT_PARAMS #x00000010)
;;#endif

;;;;** $::SQL_POSITIONED_STATEMENTS masks
(defconstant $::SQL_PS_POSITIONED_DELETE #x00000001)
(defconstant $::SQL_PS_POSITIONED_UPDATE #x00000002)
(defconstant $::SQL_PS_SELECT_FOR_UPDATE #x00000004)

;;;;** $::SQL_GROUP_BY values
(defconstant $::SQL_GB_NOT_SUPPORTED #x0000)
(defconstant $::SQL_GB_GROUP_BY_EQUALS_SELECT #x0001)
(defconstant $::SQL_GB_GROUP_BY_CONTAINS_SELECT #x0002)
(defconstant $::SQL_GB_NO_RELATION #x0003)
#-odbc2
(defconstant $::SQL_GB_COLLATE #x0004)

;;;;** $::SQL_OWNER_USAGE masks
(defconstant $::SQL_OU_DML_STATEMENTS #x00000001)
(defconstant $::SQL_OU_PROCEDURE_INVOCATION #x00000002)
(defconstant $::SQL_OU_TABLE_DEFINITION #x00000004)
(defconstant $::SQL_OU_INDEX_DEFINITION #x00000008)
(defconstant $::SQL_OU_PRIVILEGE_DEFINITION #x00000010)

;;;;** $::SQL_SCHEMA_USAGE masks
#-odbc2
(progn
  (defconstant $::SQL_SU_DML_STATEMENTS $::SQL_OU_DML_STATEMENTS)
  (defconstant $::SQL_SU_PROCEDURE_INVOCATION $::SQL_OU_PROCEDURE_INVOCATION)
  (defconstant $::SQL_SU_TABLE_DEFINITION $::SQL_OU_TABLE_DEFINITION)
  (defconstant $::SQL_SU_INDEX_DEFINITION $::SQL_OU_INDEX_DEFINITION)
  (defconstant $::SQL_SU_PRIVILEGE_DEFINITION $::SQL_OU_PRIVILEGE_DEFINITION))

;;;;** $::SQL_QUALIFIER_USAGE masks
(defconstant $::SQL_QU_DML_STATEMENTS #x00000001)
(defconstant $::SQL_QU_PROCEDURE_INVOCATION #x00000002)
(defconstant $::SQL_QU_TABLE_DEFINITION #x00000004)
(defconstant $::SQL_QU_INDEX_DEFINITION #x00000008)
(defconstant $::SQL_QU_PRIVILEGE_DEFINITION #x00000010)

#-odbc2
(progn
;;;;** $::SQL_CATALOG_USAGE masks
  (defconstant $::SQL_CU_DML_STATEMENTS $::SQL_QU_DML_STATEMENTS)
  (defconstant $::SQL_CU_PROCEDURE_INVOCATION $::SQL_QU_PROCEDURE_INVOCATION)
  (defconstant $::SQL_CU_TABLE_DEFINITION $::SQL_QU_TABLE_DEFINITION)
  (defconstant $::SQL_CU_INDEX_DEFINITION $::SQL_QU_INDEX_DEFINITION)
  (defconstant $::SQL_CU_PRIVILEGE_DEFINITION $::SQL_QU_PRIVILEGE_DEFINITION))

;;;;** $::SQL_SUBQUERIES masks
(defconstant $::SQL_SQ_COMPARISON #x00000001)
(defconstant $::SQL_SQ_EXISTS #x00000002)
(defconstant $::SQL_SQ_IN #x00000004)
(defconstant $::SQL_SQ_QUANTIFIED #x00000008)
(defconstant $::SQL_SQ_CORRELATED_SUBQUERIES #x00000010)

;;;;** $::SQL_UNION masks
(defconstant $::SQL_U_UNION #x00000001)
(defconstant $::SQL_U_UNION_ALL #x00000002)

;;;;** $::SQL_BOOKMARK_PERSISTENCE values
(defconstant $::SQL_BP_CLOSE #x00000001)
(defconstant $::SQL_BP_DELETE #x00000002)
(defconstant $::SQL_BP_DROP #x00000004)
(defconstant $::SQL_BP_TRANSACTION #x00000008)
(defconstant $::SQL_BP_UPDATE #x00000010)
(defconstant $::SQL_BP_OTHER_HSTMT #x00000020)
(defconstant $::SQL_BP_SCROLL #x00000040)

;;;;** $::SQL_STATIC_SENSITIVITY values
(defconstant $::SQL_SS_ADDITIONS #x00000001)
(defconstant $::SQL_SS_DELETIONS #x00000002)
(defconstant $::SQL_SS_UPDATES #x00000004)

;;;;** $::SQL_VIEW values
(defconstant $::SQL_CV_CREATE_VIEW #x00000001)
(defconstant $::SQL_CV_CHECK_OPTION #x00000002)
(defconstant $::SQL_CV_CASCADED #x00000004)
(defconstant $::SQL_CV_LOCAL #x00000008)

;;;;** $::SQL_LOCK_TYPES masks
(defconstant $::SQL_LCK_NO_CHANGE #x00000001)
(defconstant $::SQL_LCK_EXCLUSIVE #x00000002)
(defconstant $::SQL_LCK_UNLOCK #x00000004)

;;;;** $::SQL_POS_OPERATIONS masks
(defconstant $::SQL_POS_POSITION #x00000001)
(defconstant $::SQL_POS_REFRESH #x00000002)
(defconstant $::SQL_POS_UPDATE #x00000004)
(defconstant $::SQL_POS_DELETE #x00000008)
(defconstant $::SQL_POS_ADD #x00000010)

;;;;** $::SQL_QUALIFIER_LOCATION values
(defconstant $::SQL_QL_START #x0001)
(defconstant $::SQL_QL_END #x0002)

;;;;** Here start return values for ODBC 3.0 SQLGetInfo
#-odbc2
(progn
;;;;** $::SQL_AGGREGATE_FUNCTIONS bitmasks
  (defconstant $::SQL_AF_AVG #x00000001)
  (defconstant $::SQL_AF_COUNT #x00000002)
  (defconstant $::SQL_AF_MAX #x00000004)
  (defconstant $::SQL_AF_MIN #x00000008)
  (defconstant $::SQL_AF_SUM #x00000010)
  (defconstant $::SQL_AF_DISTINCT #x00000020)
  (defconstant $::SQL_AF_ALL #x00000040)

;;;;** $::SQL_SQL_CONFORMANCE bit masks
  (defconstant $::SQL_SC_SQL92_ENTRY #x00000001)
  (defconstant $::SQL_SC_FIPS127_2_TRANSITIONAL #x00000002)
  (defconstant $::SQL_SC_SQL92_INTERMEDIATE #x00000004)
  (defconstant $::SQL_SC_SQL92_FULL #x00000008)

;;;;** $::SQL_DATETIME_LITERALS masks
  (defconstant $::SQL_DL_SQL92_DATE #x00000001)
  (defconstant $::SQL_DL_SQL92_TIME #x00000002)
  (defconstant $::SQL_DL_SQL92_TIMESTAMP #x00000004)
  (defconstant $::SQL_DL_SQL92_INTERVAL_YEAR #x00000008)
  (defconstant $::SQL_DL_SQL92_INTERVAL_MONTH #x00000010)
  (defconstant $::SQL_DL_SQL92_INTERVAL_DAY #x00000020)
  (defconstant $::SQL_DL_SQL92_INTERVAL_HOUR #x00000040)
  (defconstant $::SQL_DL_SQL92_INTERVAL_MINUTE #x00000080)
  (defconstant $::SQL_DL_SQL92_INTERVAL_SECOND #x00000100)
  (defconstant $::SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH #x00000200)
  (defconstant $::SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR #x00000400)
  (defconstant $::SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE #x00000800)
  (defconstant $::SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND #x00001000)
  (defconstant $::SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE #x00002000)
  (defconstant $::SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND #x00004000)
  (defconstant $::SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND #x00008000)

;;;;** $::SQL_CATALOG_LOCATION values
  (defconstant $::SQL_CL_START $::SQL_QL_START)
  (defconstant $::SQL_CL_END $::SQL_QL_END)

;;;;** values for $::SQL_BATCH_ROW_COUNT
  (defconstant $::SQL_BRC_PROCEDURES #x0000001)
  (defconstant $::SQL_BRC_EXPLICIT #x0000002)
  (defconstant $::SQL_BRC_ROLLED_UP #x0000004)

;;;;** bitmasks for $::SQL_BATCH_SUPPORT
  (defconstant $::SQL_BS_SELECT_EXPLICIT #x00000001)
  (defconstant $::SQL_BS_ROW_COUNT_EXPLICIT #x00000002)
  (defconstant $::SQL_BS_SELECT_PROC #x00000004)
  (defconstant $::SQL_BS_ROW_COUNT_PROC #x00000008)

;;;;** Values for $::SQL_PARAM_ARRAY_ROW_COUNTS getinfo
  (defconstant $::SQL_PARC_BATCH 1)
  (defconstant $::SQL_PARC_NO_BATCH 2)

;;;;** values for $::SQL_PARAM_ARRAY_SELECTS
  (defconstant $::SQL_PAS_BATCH 1)
  (defconstant $::SQL_PAS_NO_BATCH 2)
  (defconstant $::SQL_PAS_NO_SELECT 3)

;;;;** Bitmasks for $::SQL_INDEX_KEYWORDS
  (defconstant $::SQL_IK_NONE #x00000000)
  (defconstant $::SQL_IK_ASC #x00000001)
  (defconstant $::SQL_IK_DESC #x00000002)
  (defconstant $::SQL_IK_ALL (+ $::SQL_IK_ASC $::SQL_IK_DESC))

;;;;** Bitmasks for $::SQL_INFO_SCHEMA_VIEWS

  (defconstant $::SQL_ISV_ASSERTIONS #x00000001)
  (defconstant $::SQL_ISV_CHARACTER_SETS #x00000002)
  (defconstant $::SQL_ISV_CHECK_CONSTRAINTS #x00000004)
  (defconstant $::SQL_ISV_COLLATIONS #x00000008)
  (defconstant $::SQL_ISV_COLUMN_DOMAIN_USAGE #x00000010)
  (defconstant $::SQL_ISV_COLUMN_PRIVILEGES #x00000020)
  (defconstant $::SQL_ISV_COLUMNS #x00000040)
  (defconstant $::SQL_ISV_CONSTRAINT_COLUMN_USAGE #x00000080)
  (defconstant $::SQL_ISV_CONSTRAINT_TABLE_USAGE #x00000100)
  (defconstant $::SQL_ISV_DOMAIN_CONSTRAINTS #x00000200)
  (defconstant $::SQL_ISV_DOMAINS #x00000400)
  (defconstant $::SQL_ISV_KEY_COLUMN_USAGE #x00000800)
  (defconstant $::SQL_ISV_REFERENTIAL_CONSTRAINTS #x00001000)
  (defconstant $::SQL_ISV_SCHEMATA #x00002000)
  (defconstant $::SQL_ISV_SQL_LANGUAGES #x00004000)
  (defconstant $::SQL_ISV_TABLE_CONSTRAINTS #x00008000)
  (defconstant $::SQL_ISV_TABLE_PRIVILEGES #x00010000)
  (defconstant $::SQL_ISV_TABLES #x00020000)
  (defconstant $::SQL_ISV_TRANSLATIONS #x00040000)
  (defconstant $::SQL_ISV_USAGE_PRIVILEGES #x00080000)
  (defconstant $::SQL_ISV_VIEW_COLUMN_USAGE #x00100000)
  (defconstant $::SQL_ISV_VIEW_TABLE_USAGE #x00200000)
  (defconstant $::SQL_ISV_VIEWS #x00400000)

;;;;** Bitmasks for $::SQL_ASYNC_MODE
  (defconstant $::SQL_AM_NONE 0)
  (defconstant $::SQL_AM_CONNECTION 1)
  (defconstant $::SQL_AM_STATEMENT 2)

;;;;** Bitmasks for $::SQL_ALTER_DOMAIN
  (defconstant $::SQL_AD_CONSTRAINT_NAME_DEFINITION #x00000001)
  (defconstant $::SQL_AD_ADD_DOMAIN_CONSTRAINT #x00000002)
  (defconstant $::SQL_AD_DROP_DOMAIN_CONSTRAINT #x00000004)
  (defconstant $::SQL_AD_ADD_DOMAIN_DEFAULT #x00000008)
  (defconstant $::SQL_AD_DROP_DOMAIN_DEFAULT #x00000010)
  (defconstant $::SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED #x00000020)
  (defconstant $::SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE #x00000040)
  (defconstant $::SQL_AD_ADD_CONSTRAINT_DEFERRABLE #x00000080)
  (defconstant $::SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE #x00000100)

;;;;** $::SQL_CREATE_SCHEMA bitmasks
  (defconstant $::SQL_CS_CREATE_SCHEMA #x00000001)
  (defconstant $::SQL_CS_AUTHORIZATION #x00000002)
  (defconstant $::SQL_CS_DEFAULT_CHARACTER_SET #x00000004)

;;;;** $::SQL_CREATE_TRANSLATION bitmasks
  (defconstant $::SQL_CTR_CREATE_TRANSLATION #x00000001)

;;;;** $::SQL_CREATE_ASSERTION bitmasks
  (defconstant $::SQL_CA_CREATE_ASSERTION #x00000001)
  (defconstant $::SQL_CA_CONSTRAINT_INITIALLY_DEFERRED #x00000010)
  (defconstant $::SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE #x00000020)
  (defconstant $::SQL_CA_CONSTRAINT_DEFERRABLE #x00000040)
  (defconstant $::SQL_CA_CONSTRAINT_NON_DEFERRABLE #x00000080)

;;;;** $::SQL_CREATE_CHARACTER_SET bitmasks
  (defconstant $::SQL_CCS_CREATE_CHARACTER_SET #x00000001)
  (defconstant $::SQL_CCS_COLLATE_CLAUSE #x00000002)
  (defconstant $::SQL_CCS_LIMITED_COLLATION #x00000004)

;;;;** $::SQL_CREATE_COLLATION bitmasks
  (defconstant $::SQL_CCOL_CREATE_COLLATION #x00000001)

;;;;** $::SQL_CREATE_DOMAIN bitmasks
  (defconstant $::SQL_CDO_CREATE_DOMAIN #x00000001)
  (defconstant $::SQL_CDO_DEFAULT #x00000002)
  (defconstant $::SQL_CDO_CONSTRAINT #x00000004)
  (defconstant $::SQL_CDO_COLLATION #x00000008)
  (defconstant $::SQL_CDO_CONSTRAINT_NAME_DEFINITION #x00000010)
  (defconstant $::SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED #x00000020)
  (defconstant $::SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE #x00000040)
  (defconstant $::SQL_CDO_CONSTRAINT_DEFERRABLE #x00000080)
  (defconstant $::SQL_CDO_CONSTRAINT_NON_DEFERRABLE #x00000100)

;;;;** $::SQL_CREATE_TABLE bitmasks
  (defconstant $::SQL_CT_CREATE_TABLE #x00000001)
  (defconstant $::SQL_CT_COMMIT_PRESERVE #x00000002)
  (defconstant $::SQL_CT_COMMIT_DELETE #x00000004)
  (defconstant $::SQL_CT_GLOBAL_TEMPORARY #x00000008)
  (defconstant $::SQL_CT_LOCAL_TEMPORARY #x00000010)
  (defconstant $::SQL_CT_CONSTRAINT_INITIALLY_DEFERRED #x00000020)
  (defconstant $::SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE #x00000040)
  (defconstant $::SQL_CT_CONSTRAINT_DEFERRABLE #x00000080)
  (defconstant $::SQL_CT_CONSTRAINT_NON_DEFERRABLE #x00000100)
  (defconstant $::SQL_CT_COLUMN_CONSTRAINT #x00000200)
  (defconstant $::SQL_CT_COLUMN_DEFAULT #x00000400)
  (defconstant $::SQL_CT_COLUMN_COLLATION #x00000800)
  (defconstant $::SQL_CT_TABLE_CONSTRAINT #x00001000)
  (defconstant $::SQL_CT_CONSTRAINT_NAME_DEFINITION #x00002000)

;;;;** $::SQL_DDL_INDEX bitmasks
  (defconstant $::SQL_DI_CREATE_INDEX #x00000001)
  (defconstant $::SQL_DI_DROP_INDEX #x00000002)

;;;;** $::SQL_DROP_COLLATION bitmasks
  (defconstant $::SQL_DC_DROP_COLLATION #x00000001)

;;;;** $::SQL_DROP_DOMAIN bitmasks
  (defconstant $::SQL_DD_DROP_DOMAIN #x00000001)
  (defconstant $::SQL_DD_RESTRICT #x00000002)
  (defconstant $::SQL_DD_CASCADE #x00000004)

;;;;** $::SQL_DROP_SCHEMA bitmasks
  (defconstant $::SQL_DS_DROP_SCHEMA #x00000001)
  (defconstant $::SQL_DS_RESTRICT #x00000002)
  (defconstant $::SQL_DS_CASCADE #x00000004)

;;;;** $::SQL_DROP_CHARACTER_SET bitmasks
  (defconstant $::SQL_DCS_DROP_CHARACTER_SET #x00000001)

;;;;** $::SQL_DROP_ASSERTION bitmasks
  (defconstant $::SQL_DA_DROP_ASSERTION #x00000001)

;;;;** $::SQL_DROP_TABLE bitmasks
  (defconstant $::SQL_DT_DROP_TABLE #x00000001)
  (defconstant $::SQL_DT_RESTRICT #x00000002)
  (defconstant $::SQL_DT_CASCADE #x00000004)

;;;;** $::SQL_DROP_TRANSLATION bitmasks
  (defconstant $::SQL_DTR_DROP_TRANSLATION #x00000001)

;;;;** $::SQL_DROP_VIEW bitmasks
  (defconstant $::SQL_DV_DROP_VIEW #x00000001)
  (defconstant $::SQL_DV_RESTRICT #x00000002)
  (defconstant $::SQL_DV_CASCADE #x00000004)

;;;;** $::SQL_INSERT_STATEMENT bitmasks
  (defconstant $::SQL_IS_INSERT_LITERALS #x00000001)
  (defconstant $::SQL_IS_INSERT_SEARCHED #x00000002)
  (defconstant $::SQL_IS_SELECT_INTO #x00000004)

;;;;** $::SQL_ODBC_INTERFACE_CONFORMANCE values
  (defconstant $::SQL_OIC_CORE 1)
  (defconstant $::SQL_OIC_LEVEL1 2)
  (defconstant $::SQL_OIC_LEVEL2 3)

;;;;** $::SQL_SQL92_FOREIGN_KEY_DELETE_RULE bitmasks
  (defconstant $::SQL_SFKD_CASCADE #x00000001)
  (defconstant $::SQL_SFKD_NO_ACTION #x00000002)
  (defconstant $::SQL_SFKD_SET_DEFAULT #x00000004)
  (defconstant $::SQL_SFKD_SET_NULL #x00000008)

;;;;** $::SQL_SQL92_FOREIGN_KEY_UPDATE_RULE bitmasks
  (defconstant $::SQL_SFKU_CASCADE #x00000001)
  (defconstant $::SQL_SFKU_NO_ACTION #x00000002)
  (defconstant $::SQL_SFKU_SET_DEFAULT #x00000004)
  (defconstant $::SQL_SFKU_SET_NULL #x00000008)

;;;;** $::SQL_SQL92_GRANT  bitmasks
  (defconstant $::SQL_SG_USAGE_ON_DOMAIN #x00000001)
  (defconstant $::SQL_SG_USAGE_ON_CHARACTER_SET #x00000002)
  (defconstant $::SQL_SG_USAGE_ON_COLLATION #x00000004)
  (defconstant $::SQL_SG_USAGE_ON_TRANSLATION #x00000008)
  (defconstant $::SQL_SG_WITH_GRANT_OPTION #x00000010)
  (defconstant $::SQL_SG_DELETE_TABLE #x00000020)
  (defconstant $::SQL_SG_INSERT_TABLE #x00000040)
  (defconstant $::SQL_SG_INSERT_COLUMN #x00000080)
  (defconstant $::SQL_SG_REFERENCES_TABLE #x00000100)
  (defconstant $::SQL_SG_REFERENCES_COLUMN #x00000200)
  (defconstant $::SQL_SG_SELECT_TABLE #x00000400)
  (defconstant $::SQL_SG_UPDATE_TABLE #x00000800)
  (defconstant $::SQL_SG_UPDATE_COLUMN #x00001000)

;;;;** $::SQL_SQL92_PREDICATES bitmasks
  (defconstant $::SQL_SP_EXISTS #x00000001)
  (defconstant $::SQL_SP_ISNOTNULL #x00000002)
  (defconstant $::SQL_SP_ISNULL #x00000004)
  (defconstant $::SQL_SP_MATCH_FULL #x00000008)
  (defconstant $::SQL_SP_MATCH_PARTIAL #x00000010)
  (defconstant $::SQL_SP_MATCH_UNIQUE_FULL #x00000020)
  (defconstant $::SQL_SP_MATCH_UNIQUE_PARTIAL #x00000040)
  (defconstant $::SQL_SP_OVERLAPS #x00000080)
  (defconstant $::SQL_SP_UNIQUE #x00000100)
  (defconstant $::SQL_SP_LIKE #x00000200)
  (defconstant $::SQL_SP_IN #x00000400)
  (defconstant $::SQL_SP_BETWEEN #x00000800)
  (defconstant $::SQL_SP_COMPARISON #x00001000)
  (defconstant $::SQL_SP_QUANTIFIED_COMPARISON #x00002000)

;;;;** $::SQL_SQL92_RELATIONAL_JOIN_OPERATORS bitmasks
  (defconstant $::SQL_SRJO_CORRESPONDING_CLAUSE #x00000001)
  (defconstant $::SQL_SRJO_CROSS_JOIN #x00000002)
  (defconstant $::SQL_SRJO_EXCEPT_JOIN #x00000004)
  (defconstant $::SQL_SRJO_FULL_OUTER_JOIN #x00000008)
  (defconstant $::SQL_SRJO_INNER_JOIN #x00000010)
  (defconstant $::SQL_SRJO_INTERSECT_JOIN #x00000020)
  (defconstant $::SQL_SRJO_LEFT_OUTER_JOIN #x00000040)
  (defconstant $::SQL_SRJO_NATURAL_JOIN #x00000080)
  (defconstant $::SQL_SRJO_RIGHT_OUTER_JOIN #x00000100)
  (defconstant $::SQL_SRJO_UNION_JOIN #x00000200)

;;;;** $::SQL_SQL92_REVOKE bitmasks
  (defconstant $::SQL_SR_USAGE_ON_DOMAIN #x00000001)
  (defconstant $::SQL_SR_USAGE_ON_CHARACTER_SET #x00000002)
  (defconstant $::SQL_SR_USAGE_ON_COLLATION #x00000004)
  (defconstant $::SQL_SR_USAGE_ON_TRANSLATION #x00000008)
  (defconstant $::SQL_SR_GRANT_OPTION_FOR #x00000010)
  (defconstant $::SQL_SR_CASCADE #x00000020)
  (defconstant $::SQL_SR_RESTRICT #x00000040)
  (defconstant $::SQL_SR_DELETE_TABLE #x00000080)
  (defconstant $::SQL_SR_INSERT_TABLE #x00000100)
  (defconstant $::SQL_SR_INSERT_COLUMN #x00000200)
  (defconstant $::SQL_SR_REFERENCES_TABLE #x00000400)
  (defconstant $::SQL_SR_REFERENCES_COLUMN #x00000800)
  (defconstant $::SQL_SR_SELECT_TABLE #x00001000)
  (defconstant $::SQL_SR_UPDATE_TABLE #x00002000)
  (defconstant $::SQL_SR_UPDATE_COLUMN #x00004000)

;;;;** $::SQL_SQL92_ROW_VALUE_CONSTRUCTOR bitmasks
  (defconstant $::SQL_SRVC_VALUE_EXPRESSION #x00000001)
  (defconstant $::SQL_SRVC_NULL #x00000002)
  (defconstant $::SQL_SRVC_DEFAULT #x00000004)
  (defconstant $::SQL_SRVC_ROW_SUBQUERY #x00000008)

;;;;** $::SQL_SQL92_VALUE_EXPRESSIONS bitmasks
  (defconstant $::SQL_SVE_CASE #x00000001)
  (defconstant $::SQL_SVE_CAST #x00000002)
  (defconstant $::SQL_SVE_COALESCE #x00000004)
  (defconstant $::SQL_SVE_NULLIF #x00000008)

;;;;** $::SQL_STANDARD_CLI_CONFORMANCE bitmasks
  (defconstant $::SQL_SCC_XOPEN_CLI_VERSION1 #x00000001)
  (defconstant $::SQL_SCC_ISO92_CLI #x00000002)

;;;;** $::SQL_UNION_STATEMENT bitmasks
  (defconstant $::SQL_US_UNION $::SQL_U_UNION)
  (defconstant $::SQL_US_UNION_ALL $::SQL_U_UNION_ALL))

;;;;** $::SQL_DTC_TRANSITION_COST bitmasks
(defconstant $::SQL_DTC_ENLIST_EXPENSIVE #x00000001)
(defconstant $::SQL_DTC_UNENLIST_EXPENSIVE #x00000002)

#-odbc2
(progn
;;;;** Possible values for $::SQL_ASYNC_DBC_FUNCTIONS 
  (defconstant $::SQL_ASYNC_DBC_NOT_CAPABLE #x00000000)
  (defconstant $::SQL_ASYNC_DBC_CAPABLE #x00000001)

;;;;** additional SQLDataSources fetch directions
  (defconstant $::SQL_FETCH_FIRST_USER 31)
  (defconstant $::SQL_FETCH_FIRST_SYSTEM 32))

;;;;** Defines for SQLSetPos
(defconstant $::SQL_ENTIRE_ROWSET 0)

;;;;** Operations in SQLSetPos
(defconstant $::SQL_POSITION 0);;      1.0 FALSE
(defconstant $::SQL_REFRESH 1);;      1.0 TRUE
(defconstant $::SQL_UPDATE 2)
(defconstant $::SQL_DELETE 3)

;;;;** Operations in SQLBulkOperations
(defconstant $::SQL_ADD 4)
(defconstant $::SQL_SETPOS_MAX_OPTION_VALUE $::SQL_ADD)
#-odbc2
(progn
  (defconstant $::SQL_UPDATE_BY_BOOKMARK 5)
  (defconstant $::SQL_DELETE_BY_BOOKMARK 6)
  (defconstant $::SQL_FETCH_BY_BOOKMARK 7))

;;;;** Lock options in SQLSetPos
(defconstant $::SQL_LOCK_NO_CHANGE 0);;      1.0 FALSE
(defconstant $::SQL_LOCK_EXCLUSIVE 1);;      1.0 TRUE
(defconstant $::SQL_LOCK_UNLOCK 2)

(defconstant $::SQL_SETPOS_MAX_LOCK_VALUE $::SQL_LOCK_UNLOCK)

;;;;** Column types and scopes in SQLSpecialColumns. 
(defconstant $::SQL_BEST_ROWID 1)
(defconstant $::SQL_ROWVER 2)

;;;;** Defines for SQLSpecialColumns (returned in the result set)
;;;; $::SQL_PC_UNKNOWN and $::SQL_PC_PSEUDO are defined in sql.h
(defconstant $::SQL_PC_NOT_PSEUDO 1)

;;;;** Defines for SQLStatistics
(defconstant $::SQL_QUICK 0)
(defconstant $::SQL_ENSURE 1)

;;;; Defines for SQLStatistics (returned in the result set)
;;;; $::SQL_INDEX_CLUSTERED, $::SQL_INDEX_HASHED, and $::SQL_INDEX_OTHER are
;;;; defined in sql.h 
(defconstant $::SQL_TABLE_STAT 0)

;;;;** Defines for SQLTables
#-odbc2
(progn
  (defparameter $::SQL_ALL_CATALOGS "%")
  (defparameter $::SQL_ALL_SCHEMAS "%")
  (defparameter $::SQL_ALL_TABLE_TYPES "%"))

;;;;** Options for SQLDriverConnect
(defconstant $::SQL_DRIVER_NOPROMPT 0)
(defconstant $::SQL_DRIVER_COMPLETE 1)
(defconstant $::SQL_DRIVER_PROMPT 2)
(defconstant $::SQL_DRIVER_COMPLETE_REQUIRED 3)

;;;;** Level 2 Functions                            

;;;;** SQLExtendedFetch "fFetchType" values
(defconstant $::SQL_FETCH_BOOKMARK 8)

;;;;** SQLExtendedFetch "rgfRowStatus" element values
(defconstant $::SQL_ROW_SUCCESS 0)
(defconstant $::SQL_ROW_DELETED 1)
(defconstant $::SQL_ROW_UPDATED 2)
(defconstant $::SQL_ROW_NOROW 3)
(defconstant $::SQL_ROW_ADDED 4)
(defconstant $::SQL_ROW_ERROR 5)
#-odbc2
(progn
  (defconstant $::SQL_ROW_SUCCESS_WITH_INFO 6)
  (defconstant $::SQL_ROW_PROCEED 0)
  (defconstant $::SQL_ROW_IGNORE 1)

;;;;** value for $::SQL_DESC_ARRAY_STATUS_PTR
  (defconstant $::SQL_PARAM_SUCCESS 0)
  (defconstant $::SQL_PARAM_SUCCESS_WITH_INFO 6)
  (defconstant $::SQL_PARAM_ERROR 5)
  (defconstant $::SQL_PARAM_UNUSED 7)
  (defconstant $::SQL_PARAM_DIAG_UNAVAILABLE 1)

  (defconstant $::SQL_PARAM_PROCEED 0)
  (defconstant $::SQL_PARAM_IGNORE 1))

;;;;** Defines for SQLForeignKeys (UPDATE_RULE and DELETE_RULE)
(defconstant $::SQL_CASCADE 0)
(defconstant $::SQL_RESTRICT 1)
(defconstant $::SQL_SET_NULL 2)
#-odbc2
(progn
  (defconstant $::SQL_NO_ACTION 3)
  (defconstant $::SQL_SET_DEFAULT 4)

;;;;** Note that the following are in a different column of SQLForeignKeys than
;;;;** the previous #defines.   These are for DEFERRABILITY.                   
  (defconstant $::SQL_INITIALLY_DEFERRED 5)
  (defconstant $::SQL_INITIALLY_IMMEDIATE 6)
  (defconstant $::SQL_NOT_DEFERRABLE 7))

;;;; Defines for SQLBindParameter and
;;;; SQLProcedureColumns (returned in the result set)
(defconstant $::SQL_PARAM_TYPE_UNKNOWN 0)
(defconstant $::SQL_PARAM_INPUT 1)
(defconstant $::SQL_PARAM_INPUT_OUTPUT 2)
(defconstant $::SQL_RESULT_COL 3)
(defconstant $::SQL_PARAM_OUTPUT 4)
(defconstant $::SQL_RETURN_VALUE 5)
#-odbc2
(progn
  (defconstant $::SQL_PARAM_INPUT_OUTPUT_STREAM   8)
  (defconstant $::SQL_PARAM_OUTPUT_STREAM         16))


;;;;/* Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter
;;;;*/
(defconstant $::SQL_PARAM_TYPE_DEFAULT $::SQL_PARAM_INPUT_OUTPUT)
(defconstant $::SQL_SETPARAM_VALUE_MAX -1)

;;;;** Defines for SQLProcedures (returned in the result set)
(defconstant $::SQL_PT_UNKNOWN 0)
(defconstant $::SQL_PT_PROCEDURE 1)
(defconstant $::SQL_PT_FUNCTION 2)

;;;;**      This define is too large for RC
(defparameter $::SQL_ODBC_KEYWORDS
  '(:ABSOLUTE :ACTION :ADA :ADD :ALL :ALLOCATE :ALTER :AND :ANY :ARE :AS 
    :ASC :ASSERTION :AT :AUTHORIZATION :AVG 
    :BEGIN :BETWEEN :BIT :BIT_LENGTH :BOTH :BY
    :CASCADE :CASCADED :CASE :CAST :CATALOG :CHAR :CHAR_LENGTH :CHARACTER
    :CHARACTER_LENGTH :CHECK :CLOSE :COALESCE :COLLATE :COLLATION :COLUMN
    :COMMIT :CONNECT :CONNECTION :CONSTRAINT :CONSTRAINTS :CONTINUE :CONVERT
    :CORRESPONDING :COUNT :CREATE :CROSS :CURRENT :CURRENT_DATE :CURRENT_TIME
    :CURRENT_TIMESTAMP :CURRENT_USER :CURSOR 
    :DATE :DAY :DEALLOCATE :DEC :DECIMAL :DECLARE :DEFAULT :DEFERRABLE 
    :DEFERRED :DELETE :DESC :DESCRIBE :DESCRIPTOR :DIAGNOSTICS :DISCONNECT 
    :DISTINCT :DOMAIN :DOUBLE :DROP 
    :ELSE :END :END_EXEC :ESCAPE :EXCEPT :EXCEPTION :EXEC :EXECUTE 
    :EXISTS :EXTERNAL :EXTRACT 
    :FALSE :FETCH :FIRST :FLOAT :FOR :FOREIGN :FORTRAN :FOUND :FROM :FULL 
    :GET :GLOBAL :GO :GOTO :GRANT :GROUP :HAVING :HOUR 
    :IDENTITY :IMMEDIATE :IN :INCLUDE :INDEX :INDICATOR :INITIALLY :INNER 
    :INPUT :INSENSITIVE :INSERT :INT :INTEGER :INTERSECT :INTERVAL :INTO :IS
    :ISOLATION 
    :JOIN :KEY :LANGUAGE :LAST :LEADING :LEFT :LEVEL :LIKE :LOCAL :LOWER 
    :MATCH :MAX :MIN :MINUTE :MODULE :MONTH 
    :NAMES :NATIONAL :NATURAL :NCHAR :NEXT :NO :NONE :NOT :NULL :NULLIF
    :NUMERIC 
    :OCTET_LENGTH :OF :ON :ONLY :OPEN :OPTION :OR :ORDER :OUTER :OUTPUT
    :OVERLAPS 
    :PAD :PARTIAL :PASCAL :PLI :POSITION :PRECISION :PREPARE :PRESERVE 
    :PRIMARY :PRIOR :PRIVILEGES :PROCEDURE :PUBLIC 
    :READ :REAL :REFERENCES :RELATIVE :RESTRICT :REVOKE :RIGHT :ROLLBACK :ROWS
    :SCHEMA :SCROLL :SECOND :SECTION :SELECT :SESSION :SESSION_USER :SET :SIZE 
    :SMALLINT :SOME :SPACE :SQL :SQLCA :SQLCODE :SQLERROR :SQLSTATE :SQLWARNING 
    :SUBSTRING :SUM :SYSTEM_USER 
    :TABLE :TEMPORARY :THEN :TIME :TIMESTAMP :TIMEZONE_HOUR :TIMEZONE_MINUTE 
    :TO :TRAILING :TRANSACTION :TRANSLATE :TRANSLATION :TRIM :TRUE 
    :UNION :UNIQUE :UNKNOWN :UPDATE :UPPER :USAGE :USER :USING 
    :VALUE :VALUES :VARCHAR :VARYING :VIEW :WHEN :WHENEVER :WHERE :WITH :WORK
    :WRITE 
    :YEAR :ZONE))


;;;;** Internal type subcodes
(defconstant $::SQL_YEAR $::SQL_CODE_YEAR)
(defconstant $::SQL_MONTH $::SQL_CODE_MONTH)
(defconstant $::SQL_DAY $::SQL_CODE_DAY)
(defconstant $::SQL_HOUR $::SQL_CODE_HOUR)
(defconstant $::SQL_MINUTE $::SQL_CODE_MINUTE)
(defconstant $::SQL_SECOND $::SQL_CODE_SECOND)
(defconstant $::SQL_YEAR_TO_MONTH $::SQL_CODE_YEAR_TO_MONTH)
(defconstant $::SQL_DAY_TO_HOUR $::SQL_CODE_DAY_TO_HOUR)
(defconstant $::SQL_DAY_TO_MINUTE $::SQL_CODE_DAY_TO_MINUTE)
(defconstant $::SQL_DAY_TO_SECOND $::SQL_CODE_DAY_TO_SECOND)
(defconstant $::SQL_HOUR_TO_MINUTE $::SQL_CODE_HOUR_TO_MINUTE)
(defconstant $::SQL_HOUR_TO_SECOND $::SQL_CODE_HOUR_TO_SECOND)
(defconstant $::SQL_MINUTE_TO_SECOND $::SQL_CODE_MINUTE_TO_SECOND)
;;#endif /* ODBC_STD */


(do-symbols (sym "$") (export sym "$"))

