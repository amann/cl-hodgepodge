(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")

#+ODBCVER-2
(define "ODBCVER" #x0200)

(include "sql.h")

;;/*
;;* ODBCVER  Default to ODBC version number (0x0380).   To exclude
;;*          definitions introduced in version 3.8 (or above)
;;*          #define ODBCVER 0x0351 before #including <sql.h>
;;*/
(constant (+odbcver+ "ODBCVER")) ;; 0x0380


;;/* special length/indicator values */
(constant (+sql-null-data+ "SQL_NULL_DATA")) ;; (-1)
(constant (+sql-data-at-exec+ "SQL_DATA_AT_EXEC")) ;; (-2)


;;/* return values from functions */
(constant (+sql-success+ "SQL_SUCCESS")) ;; 0
(constant (+sql-success-with-info+ "SQL_SUCCESS_WITH_INFO")) ;; 1
;;#if (ODBCVER >= 0x0300)
(constant (+sql-no-data+ "SQL_NO_DATA")
          :optional t) ;; 100
;;#endif

;;#if (ODBCVER >= 0x0380)
(constant (+sql-param-data-available+ "SQL_PARAM_DATA_AVAILABLE")) ;; 101  
;;#endif

(constant (+sql-error+ "SQL_ERROR")) ;; (-1)
(constant (+sql-invalid-handle+ "SQL_INVALID_HANDLE")) ;; (-2)

(constant (+sql-still-executing+ "SQL_STILL_EXECUTING")) ;; 2
(constant (+sql-need-data+ "SQL_NEED_DATA")) ;; 99

;;/* test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO */
;;#define SQL_SUCCEEDED(rc) (((rc)&(~1))==0)

;;/* flags for null-terminated string */
(constant (+sql-nts+ "SQL_NTS")) ;; (-3)
(constant (+sql-ntsl+ "SQL_NTSL")) ;; (-3L)

;;/* maximum message length */
(constant (+sql-max-message-length+ "SQL_MAX_MESSAGE_LENGTH")) ;; 512

;;/* date/time length constants */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-date-len+ "SQL_DATE_LEN")) ;; 10
(constant (+sql-time-len+ "SQL_TIME_LEN")) ;; 8  /* add P+1 if precision is nonzero */
(constant (+sql-timestamp-len+ "SQL_TIMESTAMP_LEN")) ;; 19  /* add P+1 if precision is nonzero */
;;#endif

;;/* handle type identifiers */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-handle-env+ "SQL_HANDLE_ENV")) ;; 1
(constant (+sql-handle-dbc+ "SQL_HANDLE_DBC")) ;; 2
(constant (+sql-handle-stmt+ "SQL_HANDLE_STMT")) ;; 3
(constant (+sql-handle-desc+ "SQL_HANDLE_DESC")) ;; 4
;;#endif

;;/* environment attribute */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-attr-output-nts+ "SQL_ATTR_OUTPUT_NTS")) ;; 10001
;;#endif

;;/* connection attributes */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-attr-auto-ipd+ "SQL_ATTR_AUTO_IPD")) ;; 10001
(constant (+sql-attr-metadata-id+ "SQL_ATTR_METADATA_ID")) ;; 10014
;;#endif  /* ODBCVER >= 0x0300 */

;;/* statement attributes */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-attr-app-row-desc+ "SQL_ATTR_APP_ROW_DESC")) ;; 10010
(constant (+sql-attr-app-param-desc+ "SQL_ATTR_APP_PARAM_DESC")) ;; 10011
(constant (+sql-attr-imp-row-desc+ "SQL_ATTR_IMP_ROW_DESC")) ;; 10012
(constant (+sql-attr-imp-param-desc+ "SQL_ATTR_IMP_PARAM_DESC")) ;; 10013
(constant (+sql-attr-cursor-scrollable+ "SQL_ATTR_CURSOR_SCROLLABLE")) ;; (-1)
(constant (+sql-attr-cursor-sensitivity+ "SQL_ATTR_CURSOR_SENSITIVITY")) ;; (-2)
;;;;#endif

;;/* SQL_ATTR_CURSOR_SCROLLABLE values */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-nonscrollable+ "SQL_NONSCROLLABLE")) ;; 0
(constant (+sql-scrollable+ "SQL_SCROLLABLE")) ;; 1
;;#endif  /* ODBCVER >= 0x0300 */

;;/* identifiers of fields in the SQL descriptor */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-desc-count+ "SQL_DESC_COUNT")) ;; 1001
(constant (+sql-desc-type+ "SQL_DESC_TYPE")) ;; 1002
(constant (+sql-desc-length+ "SQL_DESC_LENGTH")) ;; 1003
(constant (+sql-desc-octet-length-ptr+ "SQL_DESC_OCTET_LENGTH_PTR")) ;; 1004
(constant (+sql-desc-precision+ "SQL_DESC_PRECISION")) ;; 1005
(constant (+sql-desc-scale+ "SQL_DESC_SCALE")) ;; 1006
(constant (+sql-desc-datetime-interval-code+ "SQL_DESC_DATETIME_INTERVAL_CODE")) ;; 1007
(constant (+sql-desc-nullable+ "SQL_DESC_NULLABLE")) ;; 1008
(constant (+sql-desc-indicator-ptr+ "SQL_DESC_INDICATOR_PTR")) ;; 1009
(constant (+sql-desc-data-ptr+ "SQL_DESC_DATA_PTR")) ;; 1010
(constant (+sql-desc-name+ "SQL_DESC_NAME")) ;; 1011
(constant (+sql-desc-unnamed+ "SQL_DESC_UNNAMED")) ;; 1012
(constant (+sql-desc-octet-length+ "SQL_DESC_OCTET_LENGTH")) ;; 1013
(constant (+sql-desc-alloc-type+ "SQL_DESC_ALLOC_TYPE")) ;; 1099
;;#endif

;;/* identifiers of fields in the diagnostics area */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-diag-returncode+ "SQL_DIAG_RETURNCODE")) ;; 1
(constant (+sql-diag-number+ "SQL_DIAG_NUMBER")) ;; 2
(constant (+sql-diag-row-count+ "SQL_DIAG_ROW_COUNT")) ;; 3
(constant (+sql-diag-sqlstate+ "SQL_DIAG_SQLSTATE")) ;; 4
(constant (+sql-diag-native+ "SQL_DIAG_NATIVE")) ;; 5
(constant (+sql-diag-message-text+ "SQL_DIAG_MESSAGE_TEXT")) ;; 6
(constant (+sql-diag-dynamic-function+ "SQL_DIAG_DYNAMIC_FUNCTION")) ;; 7
(constant (+sql-diag-class-origin+ "SQL_DIAG_CLASS_ORIGIN")) ;; 8
(constant (+sql-diag-subclass-origin+ "SQL_DIAG_SUBCLASS_ORIGIN")) ;; 9
(constant (+sql-diag-connection-name+ "SQL_DIAG_CONNECTION_NAME")) ;; 10
(constant (+sql-diag-server-name+ "SQL_DIAG_SERVER_NAME")) ;; 11
(constant (+sql-diag-dynamic-function-code+ "SQL_DIAG_DYNAMIC_FUNCTION_CODE")) ;; 12
;;#endif

;;/* dynamic function codes */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-diag-alter-domain+ "SQL_DIAG_ALTER_DOMAIN")) ;; 3
(constant (+sql-diag-alter-table+ "SQL_DIAG_ALTER_TABLE")) ;; 4
(constant (+sql-diag-call+ "SQL_DIAG_CALL")) ;; 7
(constant (+sql-diag-create-assertion+ "SQL_DIAG_CREATE_ASSERTION")) ;; 6
(constant (+sql-diag-create-character-set+ "SQL_DIAG_CREATE_CHARACTER_SET")) ;; 8
(constant (+sql-diag-create-collation+ "SQL_DIAG_CREATE_COLLATION")) ;; 10
(constant (+sql-diag-create-domain+ "SQL_DIAG_CREATE_DOMAIN")) ;; 23
(constant (+sql-diag-create-index+ "SQL_DIAG_CREATE_INDEX")) ;; (-1)
(constant (+sql-diag-create-schema+ "SQL_DIAG_CREATE_SCHEMA")) ;; 64
(constant (+sql-diag-create-table+ "SQL_DIAG_CREATE_TABLE")) ;; 77
(constant (+sql-diag-create-translation+ "SQL_DIAG_CREATE_TRANSLATION")) ;; 79
(constant (+sql-diag-create-view+ "SQL_DIAG_CREATE_VIEW")) ;; 84
(constant (+sql-diag-delete-where+ "SQL_DIAG_DELETE_WHERE")) ;; 19
(constant (+sql-diag-drop-assertion+ "SQL_DIAG_DROP_ASSERTION")) ;; 24
(constant (+sql-diag-drop-character-set+ "SQL_DIAG_DROP_CHARACTER_SET")) ;; 25
(constant (+sql-diag-drop-collation+ "SQL_DIAG_DROP_COLLATION")) ;; 26
(constant (+sql-diag-drop-domain+ "SQL_DIAG_DROP_DOMAIN")) ;; 27
(constant (+sql-diag-drop-index+ "SQL_DIAG_DROP_INDEX")) ;; (-2)
(constant (+sql-diag-drop-schema+ "SQL_DIAG_DROP_SCHEMA")) ;; 31
(constant (+sql-diag-drop-table+ "SQL_DIAG_DROP_TABLE")) ;; 32
(constant (+sql-diag-drop-translation+ "SQL_DIAG_DROP_TRANSLATION")) ;; 33
(constant (+sql-diag-drop-view+ "SQL_DIAG_DROP_VIEW")) ;; 36
(constant (+sql-diag-dynamic-delete-cursor+ "SQL_DIAG_DYNAMIC_DELETE_CURSOR")) ;; 38
(constant (+sql-diag-dynamic-update-cursor+ "SQL_DIAG_DYNAMIC_UPDATE_CURSOR")) ;; 81
(constant (+sql-diag-grant+ "SQL_DIAG_GRANT")) ;; 48
(constant (+sql-diag-insert+ "SQL_DIAG_INSERT")) ;; 50
(constant (+sql-diag-revoke+ "SQL_DIAG_REVOKE")) ;; 59
(constant (+sql-diag-select-cursor+ "SQL_DIAG_SELECT_CURSOR")) ;; 85
(constant (+sql-diag-unknown-statement+ "SQL_DIAG_UNKNOWN_STATEMENT")) ;; 0
(constant (+sql-diag-update-where+ "SQL_DIAG_UPDATE_WHERE")) ;; 82
;;#endif  /* ODBCVER >= 0x0300 */

;;/* SQL data type codes */
(constant (+sql-unknown-type+ "SQL_UNKNOWN_TYPE")) ;; 0
(constant (+sql-char+ "SQL_CHAR")) ;; 1
(constant (+sql-numeric+ "SQL_NUMERIC")) ;; 2
(constant (+sql-decimal+ "SQL_DECIMAL")) ;; 3
(constant (+sql-integer+ "SQL_INTEGER")) ;; 4
(constant (+sql-smallint+ "SQL_SMALLINT")) ;; 5
(constant (+sql-float+ "SQL_FLOAT")) ;; 6
(constant (+sql-real+ "SQL_REAL")) ;; 7
(constant (+sql-double+ "SQL_DOUBLE")) ;; 8
;;#if (ODBCVER >= 0x0300)
(constant (+sql-datetime+ "SQL_DATETIME")) ;; 9
;;#endif
(constant (+sql-varchar+ "SQL_VARCHAR")) ;; 12

;;/* One-parameter shortcuts for date/time data types */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-type-date+ "SQL_TYPE_DATE")) ;; 91
(constant (+sql-type-time+ "SQL_TYPE_TIME")) ;; 92
(constant (+sql-type-timestamp+ "SQL_TYPE_TIMESTAMP")) ;; 93
;;#endif

;;/* Statement attribute values for cursor sensitivity */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-unspecified+ "SQL_UNSPECIFIED")) ;; 0
(constant (+sql-insensitive+ "SQL_INSENSITIVE")) ;; 1
(constant (+sql-sensitive+ "SQL_SENSITIVE")) ;; 2
;;#endif

;;/* GetTypeInfo() request for all data types */
(constant (+sql-all-types+ "SQL_ALL_TYPES")) ;; 0

;;/* Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData() */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-default+ "SQL_DEFAULT")) ;; 99
;;#endif

;;/* SQLSQLLEN GetData() code indicating that the application row descriptor
;; * specifies the data type
;; */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-ard-type+ "SQL_ARD_TYPE")) ;; (-99)
;;#endif

;;#if (ODBCVER >= 0x0380)
(constant (+sql-apd-type+ "SQL_APD_TYPE")) ;; (-100)
;;#endif

;;/* SQL date/time type subcodes */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-code-date+ "SQL_CODE_DATE")) ;; 1
(constant (+sql-code-time+ "SQL_CODE_TIME")) ;; 2
(constant (+sql-code-timestamp+ "SQL_CODE_TIMESTAMP")) ;; 3
;;#endif

;;/* CLI option values */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-false+ "SQL_FALSE")) ;; 0
(constant (+sql-true+ "SQL_TRUE")) ;; 1
;;#endif

;;/* values of NULLABLE field in descriptor */
(constant (+sql-no-nulls+ "SQL_NO_NULLS")) ;; 0
(constant (+sql-nullable+ "SQL_NULLABLE")) ;; 1

;;/* Value returned by SQLGetTypeInfo() to denote that it is
;; * not known whether or not a data type supports null values.
;; */
(constant (+sql-nullable-unknown+ "SQL_NULLABLE_UNKNOWN")) ;; 2

;;/* Values returned by SQLGetTypeInfo() to show WHERE clause
;; * supported
;; */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-pred-none+ "SQL_PRED_NONE")) ;; 0
(constant (+sql-pred-char+ "SQL_PRED_CHAR")) ;; 1
(constant (+sql-pred-basic+ "SQL_PRED_BASIC")) ;; 2
;;#endif

;;/* values of UNNAMED field in descriptor */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-named+ "SQL_NAMED")) ;; 0
(constant (+sql-unnamed+ "SQL_UNNAMED")) ;; 1
;;#endif

;;/* values of ALLOC_TYPE field in descriptor */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-desc-alloc-auto+ "SQL_DESC_ALLOC_AUTO")) ;; 1
(constant (+sql-desc-alloc-user+ "SQL_DESC_ALLOC_USER")) ;; 2
;;#endif

;;/* FreeStmt() options */
(constant (+sql-close+ "SQL_CLOSE")) ;; 0
(constant (+sql-drop+ "SQL_DROP")) ;; 1
(constant (+sql-unbind+ "SQL_UNBIND")) ;; 2
(constant (+sql-reset-params+ "SQL_RESET_PARAMS")) ;; 3

;;/* Codes used for FetchOrientation in SQLFetchScroll(),
   and in SQLDataSources()
;;*/
(constant (+sql-fetch-next+ "SQL_FETCH_NEXT")) ;; 1
(constant (+sql-fetch-first+ "SQL_FETCH_FIRST")) ;; 2

;;/* Other codes used for FetchOrientation in SQLFetchScroll() */
(constant (+sql-fetch-last+ "SQL_FETCH_LAST")) ;; 3
(constant (+sql-fetch-prior+ "SQL_FETCH_PRIOR")) ;; 4
(constant (+sql-fetch-absolute+ "SQL_FETCH_ABSOLUTE")) ;; 5
(constant (+sql-fetch-relative+ "SQL_FETCH_RELATIVE")) ;; 6

;;/* SQLEndTran() options */
(constant (+sql-commit+ "SQL_COMMIT")) ;; 0
(constant (+sql-rollback+ "SQL_ROLLBACK")) ;; 1

;;/* null handles returned by SQLAllocHandle() */
(constant (+sql-null-henv+ "SQL_NULL_HENV")) ;; 0
(constant (+sql-null-hdbc+ "SQL_NULL_HDBC")) ;; 0
(constant (+sql-null-hstmt+ "SQL_NULL_HSTMT")) ;; 0
;;#if (ODBCVER >= 0x0300)
(constant (+sql-null-hdesc+ "SQL_NULL_HDESC")) ;; 0
;;#endif

;;/* null handle used in place of parent handle when allocating HENV */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-null-handle+ "SQL_NULL_HANDLE")) ;; 0L
;;#endif

;;/* Values that may appear in the result set of SQLSpecialColumns() */
(constant (+sql-scope-currow+ "SQL_SCOPE_CURROW")) ;; 0
(constant (+sql-scope-transaction+ "SQL_SCOPE_TRANSACTION")) ;; 1
(constant (+sql-scope-session+ "SQL_SCOPE_SESSION")) ;; 2

(constant (+sql-pc-unknown+ "SQL_PC_UNKNOWN")) ;; 0
;;#if (ODBCVER >= 0x0300)
(constant (+sql-pc-non-pseudo+ "SQL_PC_NON_PSEUDO")) ;; 1
;;#endif
(constant (+sql-pc-pseudo+ "SQL_PC_PSEUDO")) ;; 2

;;/* Reserved value for the IdentifierType argument of SQLSpecialColumns() */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-row-identifier+ "SQL_ROW_IDENTIFIER")) ;; 1
;;#endif

;;/* Reserved values for UNIQUE argument of SQLStatistics() */
(constant (+sql-index-unique+ "SQL_INDEX_UNIQUE")) ;; 0
(constant (+sql-index-all+ "SQL_INDEX_ALL")) ;; 1

;;/* Values that may appear in the result set of SQLStatistics() */
(constant (+sql-index-clustered+ "SQL_INDEX_CLUSTERED")) ;; 1
(constant (+sql-index-hashed+ "SQL_INDEX_HASHED")) ;; 2
(constant (+sql-index-other+ "SQL_INDEX_OTHER")) ;; 3

;;/* SQLGetFunctions() values to identify ODBC APIs */
(constant (+sql-api-sqlallocconnect+ "SQL_API_SQLALLOCCONNECT")) ;; 1
(constant (+sql-api-sqlallocenv+ "SQL_API_SQLALLOCENV")) ;; 2
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlallochandle+ "SQL_API_SQLALLOCHANDLE")) ;; 1001
;;#endif
(constant (+sql-api-sqlallocstmt+ "SQL_API_SQLALLOCSTMT")) ;; 3
(constant (+sql-api-sqlbindcol+ "SQL_API_SQLBINDCOL")) ;; 4
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlbindparam+ "SQL_API_SQLBINDPARAM")) ;; 1002
;;#endif
(constant (+sql-api-sqlcancel+ "SQL_API_SQLCANCEL")) ;; 5
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlclosecursor+ "SQL_API_SQLCLOSECURSOR")) ;; 1003
(constant (+sql-api-sqlcolattribute+ "SQL_API_SQLCOLATTRIBUTE")) ;; 6
;;#endif
(constant (+sql-api-sqlcolumns+ "SQL_API_SQLCOLUMNS")) ;; 40
(constant (+sql-api-sqlconnect+ "SQL_API_SQLCONNECT")) ;; 7
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlcopydesc+ "SQL_API_SQLCOPYDESC")) ;; 1004
;;#endif
(constant (+sql-api-sqldatasources+ "SQL_API_SQLDATASOURCES")) ;; 57
(constant (+sql-api-sqldescribecol+ "SQL_API_SQLDESCRIBECOL")) ;; 8
(constant (+sql-api-sqldisconnect+ "SQL_API_SQLDISCONNECT")) ;; 9
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlendtran+ "SQL_API_SQLENDTRAN")) ;; 1005
;;#endif
(constant (+sql-api-sqlerror+ "SQL_API_SQLERROR")) ;; 10
(constant (+sql-api-sqlexecdirect+ "SQL_API_SQLEXECDIRECT")) ;; 11
(constant (+sql-api-sqlexecute+ "SQL_API_SQLEXECUTE")) ;; 12
(constant (+sql-api-sqlfetch+ "SQL_API_SQLFETCH")) ;; 13
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlfetchscroll+ "SQL_API_SQLFETCHSCROLL")) ;; 1021
;;#endif
(constant (+sql-api-sqlfreeconnect+ "SQL_API_SQLFREECONNECT")) ;; 14
(constant (+sql-api-sqlfreeenv+ "SQL_API_SQLFREEENV")) ;; 15
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlfreehandle+ "SQL_API_SQLFREEHANDLE")) ;; 1006
;;#endif
(constant (+sql-api-sqlfreestmt+ "SQL_API_SQLFREESTMT")) ;; 16
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlgetconnectattr+ "SQL_API_SQLGETCONNECTATTR")) ;; 1007
;;#endif
(constant (+sql-api-sqlgetconnectoption+ "SQL_API_SQLGETCONNECTOPTION")) ;; 42
(constant (+sql-api-sqlgetcursorname+ "SQL_API_SQLGETCURSORNAME")) ;; 17
(constant (+sql-api-sqlgetdata+ "SQL_API_SQLGETDATA")) ;; 43
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlgetdescfield+ "SQL_API_SQLGETDESCFIELD")) ;; 1008
(constant (+sql-api-sqlgetdescrec+ "SQL_API_SQLGETDESCREC")) ;; 1009
(constant (+sql-api-sqlgetdiagfield+ "SQL_API_SQLGETDIAGFIELD")) ;; 1010
(constant (+sql-api-sqlgetdiagrec+ "SQL_API_SQLGETDIAGREC")) ;; 1011
(constant (+sql-api-sqlgetenvattr+ "SQL_API_SQLGETENVATTR")) ;; 1012
;;#endif
(constant (+sql-api-sqlgetfunctions+ "SQL_API_SQLGETFUNCTIONS")) ;; 44
(constant (+sql-api-sqlgetinfo+ "SQL_API_SQLGETINFO")) ;; 45
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlgetstmtattr+ "SQL_API_SQLGETSTMTATTR")) ;; 1014
;;#endif
(constant (+sql-api-sqlgetstmtoption+ "SQL_API_SQLGETSTMTOPTION")) ;; 46
(constant (+sql-api-sqlgettypeinfo+ "SQL_API_SQLGETTYPEINFO")) ;; 47
(constant (+sql-api-sqlnumresultcols+ "SQL_API_SQLNUMRESULTCOLS")) ;; 18
(constant (+sql-api-sqlparamdata+ "SQL_API_SQLPARAMDATA")) ;; 48
(constant (+sql-api-sqlprepare+ "SQL_API_SQLPREPARE")) ;; 19
(constant (+sql-api-sqlputdata+ "SQL_API_SQLPUTDATA")) ;; 49
(constant (+sql-api-sqlrowcount+ "SQL_API_SQLROWCOUNT")) ;; 20
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlsetconnectattr+ "SQL_API_SQLSETCONNECTATTR")) ;; 1016
;;#endif
(constant (+sql-api-sqlsetconnectoption+ "SQL_API_SQLSETCONNECTOPTION")) ;; 50
(constant (+sql-api-sqlsetcursorname+ "SQL_API_SQLSETCURSORNAME")) ;; 21
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlsetdescfield+ "SQL_API_SQLSETDESCFIELD")) ;; 1017
(constant (+sql-api-sqlsetdescrec+ "SQL_API_SQLSETDESCREC")) ;; 1018
(constant (+sql-api-sqlsetenvattr+ "SQL_API_SQLSETENVATTR")) ;; 1019
;;#endif
(constant (+sql-api-sqlsetparam+ "SQL_API_SQLSETPARAM")) ;; 22
;;#if (ODBCVER >= 0x0300)
(constant (+sql-api-sqlsetstmtattr+ "SQL_API_SQLSETSTMTATTR")) ;; 1020
;;#endif
(constant (+sql-api-sqlsetstmtoption+ "SQL_API_SQLSETSTMTOPTION")) ;; 51
(constant (+sql-api-sqlspecialcolumns+ "SQL_API_SQLSPECIALCOLUMNS")) ;; 52
(constant (+sql-api-sqlstatistics+ "SQL_API_SQLSTATISTICS")) ;; 53
(constant (+sql-api-sqltables+ "SQL_API_SQLTABLES")) ;; 54
(constant (+sql-api-sqltransact+ "SQL_API_SQLTRANSACT")) ;; 23

;;#if (ODBCVER >= 0x0380)
(constant (+sql-api-sqlcancelhandle+ "SQL_API_SQLCANCELHANDLE")) ;; 1022
;;#endif

;;/* Information requested by SQLGetInfo() */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-max-driver-connections+ "SQL_MAX_DRIVER_CONNECTIONS")) ;; 0
(constant (+sql-maximum-driver-connections+ "SQL_MAXIMUM_DRIVER_CONNECTIONS")) ;; SQL_MAX_DRIVER_CONNECTIONS
(constant (+sql-max-concurrent-activities+ "SQL_MAX_CONCURRENT_ACTIVITIES")) ;; 1
(constant (+sql-maximum-concurrent-activities+ "SQL_MAXIMUM_CONCURRENT_ACTIVITIES")) ;; SQL_MAX_CONCURRENT_ACTIVITIES
;;#endif
(constant (+sql-data-source-name+ "SQL_DATA_SOURCE_NAME")) ;; 2
(constant (+sql-fetch-direction+ "SQL_FETCH_DIRECTION")) ;; 8
(constant (+sql-server-name+ "SQL_SERVER_NAME")) ;; 13
(constant (+sql-search-pattern-escape+ "SQL_SEARCH_PATTERN_ESCAPE")) ;; 14
(constant (+sql-dbms-name+ "SQL_DBMS_NAME")) ;; 17
(constant (+sql-dbms-ver+ "SQL_DBMS_VER")) ;; 18
(constant (+sql-accessible-tables+ "SQL_ACCESSIBLE_TABLES")) ;; 19
(constant (+sql-accessible-procedures+ "SQL_ACCESSIBLE_PROCEDURES")) ;; 20
(constant (+sql-cursor-commit-behavior+ "SQL_CURSOR_COMMIT_BEHAVIOR")) ;; 23
(constant (+sql-data-source-read-only+ "SQL_DATA_SOURCE_READ_ONLY")) ;; 25
(constant (+sql-default-txn-isolation+ "SQL_DEFAULT_TXN_ISOLATION")) ;; 26
(constant (+sql-identifier-case+ "SQL_IDENTIFIER_CASE")) ;; 28
(constant (+sql-identifier-quote-char+ "SQL_IDENTIFIER_QUOTE_CHAR")) ;; 29
(constant (+sql-max-column-name-len+ "SQL_MAX_COLUMN_NAME_LEN")) ;; 30
(constant (+sql-maximum-column-name-length+ "SQL_MAXIMUM_COLUMN_NAME_LENGTH")) ;; SQL_MAX_COLUMN_NAME_LEN
(constant (+sql-max-cursor-name-len+ "SQL_MAX_CURSOR_NAME_LEN")) ;; 31
(constant (+sql-maximum-cursor-name-length+ "SQL_MAXIMUM_CURSOR_NAME_LENGTH")) ;; SQL_MAX_CURSOR_NAME_LEN
(constant (+sql-max-schema-name-len+ "SQL_MAX_SCHEMA_NAME_LEN")) ;; 32
(constant (+sql-maximum-schema-name-length+ "SQL_MAXIMUM_SCHEMA_NAME_LENGTH")) ;; SQL_MAX_SCHEMA_NAME_LEN
(constant (+sql-max-catalog-name-len+ "SQL_MAX_CATALOG_NAME_LEN")) ;; 34
(constant (+sql-maximum-catalog-name-length+ "SQL_MAXIMUM_CATALOG_NAME_LENGTH")) ;; SQL_MAX_CATALOG_NAME_LEN
(constant (+sql-max-table-name-len+ "SQL_MAX_TABLE_NAME_LEN")) ;; 35
(constant (+sql-scroll-concurrency+ "SQL_SCROLL_CONCURRENCY")) ;; 43
(constant (+sql-txn-capable+ "SQL_TXN_CAPABLE")) ;; 46
(constant (+sql-transaction-capable+ "SQL_TRANSACTION_CAPABLE")) ;; SQL_TXN_CAPABLE
(constant (+sql-user-name+ "SQL_USER_NAME")) ;; 47
(constant (+sql-txn-isolation-option+ "SQL_TXN_ISOLATION_OPTION")) ;; 72
(constant (+sql-transaction-isolation-option+ "SQL_TRANSACTION_ISOLATION_OPTION")) ;; SQL_TXN_ISOLATION_OPTION
(constant (+sql-integrity+ "SQL_INTEGRITY")) ;; 73
(constant (+sql-getdata-extensions+ "SQL_GETDATA_EXTENSIONS")) ;; 81
(constant (+sql-null-collation+ "SQL_NULL_COLLATION")) ;; 85
(constant (+sql-alter-table+ "SQL_ALTER_TABLE")) ;; 86
(constant (+sql-order-by-columns-in-select+ "SQL_ORDER_BY_COLUMNS_IN_SELECT")) ;; 90
(constant (+sql-special-characters+ "SQL_SPECIAL_CHARACTERS")) ;; 94
(constant (+sql-max-columns-in-group-by+ "SQL_MAX_COLUMNS_IN_GROUP_BY")) ;; 97
(constant (+sql-maximum-columns-in-group-by+ "SQL_MAXIMUM_COLUMNS_IN_GROUP_BY")) ;; SQL_MAX_COLUMNS_IN_GROUP_BY
(constant (+sql-max-columns-in-index+ "SQL_MAX_COLUMNS_IN_INDEX")) ;; 98
(constant (+sql-maximum-columns-in-index+ "SQL_MAXIMUM_COLUMNS_IN_INDEX")) ;; SQL_MAX_COLUMNS_IN_INDEX
(constant (+sql-max-columns-in-order-by+ "SQL_MAX_COLUMNS_IN_ORDER_BY")) ;; 99
(constant (+sql-maximum-columns-in-order-by+ "SQL_MAXIMUM_COLUMNS_IN_ORDER_BY")) ;; SQL_MAX_COLUMNS_IN_ORDER_BY
(constant (+sql-max-columns-in-select+ "SQL_MAX_COLUMNS_IN_SELECT")) ;; 100
(constant (+sql-maximum-columns-in-select+ "SQL_MAXIMUM_COLUMNS_IN_SELECT")) ;; SQL_MAX_COLUMNS_IN_SELECT
(constant (+sql-max-columns-in-table+ "SQL_MAX_COLUMNS_IN_TABLE")) ;; 101
(constant (+sql-max-index-size+ "SQL_MAX_INDEX_SIZE")) ;; 102
(constant (+sql-maximum-index-size+ "SQL_MAXIMUM_INDEX_SIZE")) ;; SQL_MAX_INDEX_SIZE
(constant (+sql-max-row-size+ "SQL_MAX_ROW_SIZE")) ;; 104
(constant (+sql-maximum-row-size+ "SQL_MAXIMUM_ROW_SIZE")) ;; SQL_MAX_ROW_SIZE
(constant (+sql-max-statement-len+ "SQL_MAX_STATEMENT_LEN")) ;; 105
(constant (+sql-maximum-statement-length+ "SQL_MAXIMUM_STATEMENT_LENGTH")) ;; SQL_MAX_STATEMENT_LEN
(constant (+sql-max-tables-in-select+ "SQL_MAX_TABLES_IN_SELECT")) ;; 106
(constant (+sql-maximum-tables-in-select+ "SQL_MAXIMUM_TABLES_IN_SELECT")) ;; SQL_MAX_TABLES_IN_SELECT
(constant (+sql-max-user-name-len+ "SQL_MAX_USER_NAME_LEN")) ;; 107
(constant (+sql-maximum-user-name-length+ "SQL_MAXIMUM_USER_NAME_LENGTH")) ;; SQL_MAX_USER_NAME_LEN
;;#if (ODBCVER >= 0x0300)
(constant (+sql-oj-capabilities+ "SQL_OJ_CAPABILITIES")) ;; 115
(constant (+sql-outer-join-capabilities+ "SQL_OUTER_JOIN_CAPABILITIES")) ;; SQL_OJ_CAPABILITIES
;;#endif /* ODBCVER >= 0x0300 */

;;#if (ODBCVER >= 0x0300)
(constant (+sql-xopen-cli-year+ "SQL_XOPEN_CLI_YEAR")) ;; 10000
(constant (+sql-cursor-sensitivity+ "SQL_CURSOR_SENSITIVITY")) ;; 10001
(constant (+sql-describe-parameter+ "SQL_DESCRIBE_PARAMETER")) ;; 10002
(constant (+sql-catalog-name+ "SQL_CATALOG_NAME")) ;; 10003
(constant (+sql-collation-seq+ "SQL_COLLATION_SEQ")) ;; 10004
(constant (+sql-max-identifier-len+ "SQL_MAX_IDENTIFIER_LEN")) ;; 10005
(constant (+sql-maximum-identifier-length+ "SQL_MAXIMUM_IDENTIFIER_LENGTH")) ;; SQL_MAX_IDENTIFIER_LEN
;;#endif /* ODBCVER >= 0x0300 */

;;/* SQL_ALTER_TABLE bitmasks */
;;#if (ODBCVER >= 0x0200)
(constant (+sql-at-add-column+ "SQL_AT_ADD_COLUMN")) ;; 0x00000001L
(constant (+sql-at-drop-column+ "SQL_AT_DROP_COLUMN")) ;; 0x00000002L
;;#endif /* ODBCVER >= 0x0200 */

;;#if (ODBCVER >= 0x0300)
(constant (+sql-at-add-constraint+ "SQL_AT_ADD_CONSTRAINT")) ;; 0x00000008L

;;/* The following bitmasks are ODBC extensions and defined in sqlext.h
;;*(constant (+sql-at-column-single+ "SQL_AT_COLUMN_SINGLE")) ;; 0x00000020L
;;*(constant (+sql-at-add-column-default+ "SQL_AT_ADD_COLUMN_DEFAULT")) ;; 0x00000040L
;;*(constant (+sql-at-add-column-collation+ "SQL_AT_ADD_COLUMN_COLLATION")) ;; 0x00000080L
;;*(constant (+sql-at-set-column-default+ "SQL_AT_SET_COLUMN_DEFAULT")) ;; 0x00000100L
;;*(constant (+sql-at-drop-column-default+ "SQL_AT_DROP_COLUMN_DEFAULT")) ;; 0x00000200L
;;*(constant (+sql-at-drop-column-cascade+ "SQL_AT_DROP_COLUMN_CASCADE")) ;; 0x00000400L
;;*(constant (+sql-at-drop-column-restrict+ "SQL_AT_DROP_COLUMN_RESTRICT")) ;; 0x00000800L
;;*(constant (+sql-at-add-table-constraint+ "SQL_AT_ADD_TABLE_CONSTRAINT")) ;; 0x00001000L
;;*(constant (+sql-at-drop-table-constraint-cascade+ "SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE")) ;; 0x00002000L
;;*(constant (+sql-at-drop-table-constraint-restrict+ "SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT")) ;; 0x00004000L
;;*(constant (+sql-at-constraint-name-definition+ "SQL_AT_CONSTRAINT_NAME_DEFINITION")) ;; 0x00008000L
;;*(constant (+sql-at-constraint-initially-deferred+ "SQL_AT_CONSTRAINT_INITIALLY_DEFERRED")) ;; 0x00010000L
;;*(constant (+sql-at-constraint-initially-immediate+ "SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE")) ;; 0x00020000L
;;*(constant (+sql-at-constraint-deferrable+ "SQL_AT_CONSTRAINT_DEFERRABLE")) ;; 0x00040000L
;;*(constant (+sql-at-constraint-non-deferrable+ "SQL_AT_CONSTRAINT_NON_DEFERRABLE")) ;; 0x00080000L
;;*/
;;#endif  /* ODBCVER >= 0x0300 */


;;/* SQL_ASYNC_MODE values */
;;#if (ODBCVER >= 0x0300)
(constant (+sql-am-none+ "SQL_AM_NONE")) ;; 0
(constant (+sql-am-connection+ "SQL_AM_CONNECTION")) ;; 1
(constant (+sql-am-statement+ "SQL_AM_STATEMENT")) ;; 2
;;#endif

;;/* SQL_CURSOR_COMMIT_BEHAVIOR values */
(constant (+sql-cb-delete+ "SQL_CB_DELETE")) ;; 0
(constant (+sql-cb-close+ "SQL_CB_CLOSE")) ;; 1
(constant (+sql-cb-preserve+ "SQL_CB_PRESERVE")) ;; 2

;;/* SQL_FETCH_DIRECTION bitmasks */
(constant (+sql-fd-fetch-next+ "SQL_FD_FETCH_NEXT")) ;; 0x00000001L
(constant (+sql-fd-fetch-first+ "SQL_FD_FETCH_FIRST")) ;; 0x00000002L
(constant (+sql-fd-fetch-last+ "SQL_FD_FETCH_LAST")) ;; 0x00000004L
(constant (+sql-fd-fetch-prior+ "SQL_FD_FETCH_PRIOR")) ;; 0x00000008L
(constant (+sql-fd-fetch-absolute+ "SQL_FD_FETCH_ABSOLUTE")) ;; 0x00000010L
(constant (+sql-fd-fetch-relative+ "SQL_FD_FETCH_RELATIVE")) ;; 0x00000020L

;;/* SQL_GETDATA_EXTENSIONS bitmasks */
(constant (+sql-gd-any-column+ "SQL_GD_ANY_COLUMN")) ;; 0x00000001L
(constant (+sql-gd-any-order+ "SQL_GD_ANY_ORDER")) ;; 0x00000002L

;;/* SQL_IDENTIFIER_CASE values */
(constant (+sql-ic-upper+ "SQL_IC_UPPER")) ;; 1
(constant (+sql-ic-lower+ "SQL_IC_LOWER")) ;; 2
(constant (+sql-ic-sensitive+ "SQL_IC_SENSITIVE")) ;; 3
(constant (+sql-ic-mixed+ "SQL_IC_MIXED")) ;; 4

;;/* SQL_OJ_CAPABILITIES bitmasks */
;;/* NB: this means 'outer join', not what  you may be thinking */


;;#if (ODBCVER >= 0x0201)
(constant (+sql-oj-left+ "SQL_OJ_LEFT")) ;; 0x00000001L
(constant (+sql-oj-right+ "SQL_OJ_RIGHT")) ;; 0x00000002L
(constant (+sql-oj-full+ "SQL_OJ_FULL")) ;; 0x00000004L
(constant (+sql-oj-nested+ "SQL_OJ_NESTED")) ;; 0x00000008L
(constant (+sql-oj-not-ordered+ "SQL_OJ_NOT_ORDERED")) ;; 0x00000010L
(constant (+sql-oj-inner+ "SQL_OJ_INNER")) ;; 0x00000020L
(constant (+sql-oj-all-comparison-ops+ "SQL_OJ_ALL_COMPARISON_OPS")) ;; 0x00000040L
;;#endif

;;/* SQL_SCROLL_CONCURRENCY bitmasks */
(constant (+sql-scco-read-only+ "SQL_SCCO_READ_ONLY")) ;; 0x00000001L
(constant (+sql-scco-lock+ "SQL_SCCO_LOCK")) ;; 0x00000002L
(constant (+sql-scco-opt-rowver+ "SQL_SCCO_OPT_ROWVER")) ;; 0x00000004L
(constant (+sql-scco-opt-values+ "SQL_SCCO_OPT_VALUES")) ;; 0x00000008L

;;/* SQL_TXN_CAPABLE values */
(constant (+sql-tc-none+ "SQL_TC_NONE")) ;; 0
(constant (+sql-tc-dml+ "SQL_TC_DML")) ;; 1
(constant (+sql-tc-all+ "SQL_TC_ALL")) ;; 2
(constant (+sql-tc-ddl-commit+ "SQL_TC_DDL_COMMIT")) ;; 3
(constant (+sql-tc-ddl-ignore+ "SQL_TC_DDL_IGNORE")) ;; 4

;;/* SQL_TXN_ISOLATION_OPTION bitmasks */
(constant (+sql-txn-read-uncommitted+ "SQL_TXN_READ_UNCOMMITTED")) ;; 0x00000001L
(constant (+sql-transaction-read-uncommitted+ "SQL_TRANSACTION_READ_UNCOMMITTED")) ;; SQL_TXN_READ_UNCOMMITTED
(constant (+sql-txn-read-committed+ "SQL_TXN_READ_COMMITTED")) ;; 0x00000002L
(constant (+sql-transaction-read-committed+ "SQL_TRANSACTION_READ_COMMITTED")) ;; SQL_TXN_READ_COMMITTED
(constant (+sql-txn-repeatable-read+ "SQL_TXN_REPEATABLE_READ")) ;; 0x00000004L
(constant (+sql-transaction-repeatable-read+ "SQL_TRANSACTION_REPEATABLE_READ")) ;; SQL_TXN_REPEATABLE_READ
(constant (+sql-txn-serializable+ "SQL_TXN_SERIALIZABLE")) ;; 0x00000008L
(constant (+sql-transaction-serializable+ "SQL_TRANSACTION_SERIALIZABLE")) ;; SQL_TXN_SERIALIZABLE

;;/* SQL_NULL_COLLATION values */
(constant (+sql-nc-high+ "SQL_NC_HIGH")) ;; 0
(constant (+sql-nc-low+ "SQL_NC_LOW")) ;; 1

#ifndef RC_INVOKED

SQLRETURN  SQL_API SQLAllocConnect(SQLHENV EnvironmentHandle,
           __out SQLHDBC *ConnectionHandle);

SQLRETURN  SQL_API SQLAllocEnv(__out SQLHENV *EnvironmentHandle);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLAllocHandle(SQLSMALLINT HandleType,
           SQLHANDLE InputHandle, __out SQLHANDLE *OutputHandle);
#endif

SQLRETURN  SQL_API SQLAllocStmt(SQLHDBC ConnectionHandle,
           __out SQLHSTMT *StatementHandle);

SQLRETURN  SQL_API SQLBindCol(SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType,
           __inout_xcount_opt(BufferLength) SQLPOINTER TargetValue, 
           SQLLEN BufferLength, __inout_opt SQLLEN *StrLen_or_Ind);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLBindParam(SQLHSTMT StatementHandle,
           SQLUSMALLINT ParameterNumber, SQLSMALLINT ValueType,
           SQLSMALLINT ParameterType, SQLULEN LengthPrecision,
           SQLSMALLINT ParameterScale, SQLPOINTER ParameterValue,
           SQLLEN *StrLen_or_Ind);
#endif

SQLRETURN  SQL_API SQLCancel(SQLHSTMT StatementHandle);

#if (ODBCVER >= 0x0380)
SQLRETURN  SQL_API SQLCancelHandle(SQLSMALLINT HandleType, SQLHANDLE InputHandle);
#endif // ODBCVER >= 0x0380

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLCloseCursor(SQLHSTMT StatementHandle);

#ifdef _WIN64
SQLRETURN  SQL_API SQLColAttribute (SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, SQLUSMALLINT FieldIdentifier,
           __out_bcount_opt(BufferLength) SQLPOINTER CharacterAttribute, SQLSMALLINT BufferLength,
           __out_opt SQLSMALLINT *StringLength, __out_opt SQLLEN *NumericAttribute);
#else
SQLRETURN  SQL_API SQLColAttribute (SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, SQLUSMALLINT FieldIdentifier,
           __out_bcount_opt(BufferLength) SQLPOINTER CharacterAttribute, SQLSMALLINT BufferLength,
           __out_opt SQLSMALLINT *StringLength, __out_opt SQLPOINTER NumericAttribute);
#endif
#endif


SQLRETURN  SQL_API SQLColumns(SQLHSTMT StatementHandle,
           __in_ecount_opt(NameLength1) SQLCHAR *CatalogName, SQLSMALLINT NameLength1,
           __in_ecount_opt(NameLength2) SQLCHAR *SchemaName, SQLSMALLINT NameLength2,
           __in_ecount_opt(NameLength3) SQLCHAR *TableName, SQLSMALLINT NameLength3,
           __in_ecount_opt(NameLength4) SQLCHAR *ColumnName, SQLSMALLINT NameLength4);


SQLRETURN  SQL_API SQLConnect(SQLHDBC ConnectionHandle,
           __in_ecount(NameLength1) SQLCHAR *ServerName, SQLSMALLINT NameLength1,
           __in_ecount(NameLength2) SQLCHAR *UserName, SQLSMALLINT NameLength2,
           __in_ecount(NameLength3) SQLCHAR *Authentication, SQLSMALLINT NameLength3);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLCopyDesc(SQLHDESC SourceDescHandle,
           SQLHDESC TargetDescHandle);
#endif

SQLRETURN  SQL_API SQLDataSources(SQLHENV EnvironmentHandle,
           SQLUSMALLINT Direction, __out_ecount_opt(BufferLength1) SQLCHAR *ServerName,
           SQLSMALLINT BufferLength1, __out_opt SQLSMALLINT *NameLength1Ptr,
           __out_ecount_opt(BufferLength2) SQLCHAR *Description, SQLSMALLINT BufferLength2,
           __out_opt SQLSMALLINT *NameLength2Ptr);

SQLRETURN  SQL_API SQLDescribeCol(SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, __out_ecount_opt(BufferLength) SQLCHAR *ColumnName,
           SQLSMALLINT BufferLength, __out_opt SQLSMALLINT *NameLength,
           __out_opt SQLSMALLINT *DataType, __out_opt SQLULEN *ColumnSize,
           __out_opt SQLSMALLINT *DecimalDigits, __out_opt SQLSMALLINT *Nullable);

SQLRETURN  SQL_API SQLDisconnect(SQLHDBC ConnectionHandle);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLEndTran(SQLSMALLINT HandleType, SQLHANDLE Handle,
           SQLSMALLINT CompletionType);
#endif

SQLRETURN  SQL_API SQLError(SQLHENV EnvironmentHandle,
           SQLHDBC ConnectionHandle, SQLHSTMT StatementHandle,
           __out_ecount(6) SQLCHAR *Sqlstate, __out_opt SQLINTEGER *NativeError,
           __out_ecount_opt(BufferLength) SQLCHAR *MessageText, SQLSMALLINT BufferLength,
           __out_opt SQLSMALLINT *TextLength);

SQLRETURN  SQL_API SQLExecDirect
(
    SQLHSTMT StatementHandle,
    __in_ecount_opt(TextLength) SQLCHAR* StatementText,
    SQLINTEGER TextLength
);

SQLRETURN  SQL_API SQLExecute(SQLHSTMT StatementHandle);

SQLRETURN  SQL_API SQLFetch(SQLHSTMT StatementHandle);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLFetchScroll(SQLHSTMT StatementHandle,
           SQLSMALLINT FetchOrientation, SQLLEN FetchOffset);
#endif

SQLRETURN  SQL_API SQLFreeConnect(SQLHDBC ConnectionHandle);

SQLRETURN  SQL_API SQLFreeEnv(SQLHENV EnvironmentHandle);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLFreeHandle(SQLSMALLINT HandleType, SQLHANDLE Handle);
#endif

SQLRETURN  SQL_API SQLFreeStmt(SQLHSTMT StatementHandle,
           SQLUSMALLINT Option);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLGetConnectAttr(SQLHDBC ConnectionHandle,
           SQLINTEGER Attribute, __out_xcount_opt(BufferLength) SQLPOINTER Value,
           SQLINTEGER BufferLength, __out_opt SQLINTEGER *StringLengthPtr);
#endif

SQLRETURN  SQL_API SQLGetConnectOption(SQLHDBC ConnectionHandle,
           SQLUSMALLINT Option, SQLPOINTER Value);

SQLRETURN  SQL_API SQLGetCursorName
(
    SQLHSTMT StatementHandle,
    __out_ecount_opt(BufferLength) SQLCHAR *CursorName,
    SQLSMALLINT BufferLength,
    __out_opt
    SQLSMALLINT *NameLengthPtr
);

SQLRETURN  SQL_API SQLGetData(SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType,
           __out_xcount_opt(BufferLength) SQLPOINTER TargetValue, SQLLEN BufferLength,
           __out_opt SQLLEN *StrLen_or_IndPtr);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLGetDescField(SQLHDESC DescriptorHandle,
           SQLSMALLINT RecNumber, SQLSMALLINT FieldIdentifier,
           __out_xcount_opt(BufferLength) SQLPOINTER Value, SQLINTEGER BufferLength,
           __out_opt SQLINTEGER *StringLength);

SQLRETURN  SQL_API SQLGetDescRec(SQLHDESC DescriptorHandle,
           SQLSMALLINT RecNumber, __out_ecount_opt(BufferLength) SQLCHAR *Name,
           SQLSMALLINT BufferLength, __out_opt SQLSMALLINT *StringLengthPtr,
           __out_opt SQLSMALLINT *TypePtr, __out_opt SQLSMALLINT *SubTypePtr,
           __out_opt SQLLEN     *LengthPtr, __out_opt SQLSMALLINT *PrecisionPtr,
           __out_opt SQLSMALLINT *ScalePtr, __out_opt SQLSMALLINT *NullablePtr);

SQLRETURN  SQL_API SQLGetDiagField(SQLSMALLINT HandleType, SQLHANDLE Handle,
           SQLSMALLINT RecNumber, SQLSMALLINT DiagIdentifier,
           __out_xcount_opt(BufferLength) SQLPOINTER DiagInfo, SQLSMALLINT BufferLength,
           __out_opt SQLSMALLINT *StringLength);

SQLRETURN  SQL_API SQLGetDiagRec
(
    SQLSMALLINT HandleType,
    SQLHANDLE Handle,
    SQLSMALLINT RecNumber,
    __out_ecount_opt(6) SQLCHAR *Sqlstate,
    SQLINTEGER *NativeError,
    __out_ecount_opt(BufferLength) SQLCHAR* MessageText,
    SQLSMALLINT BufferLength,
    __out_opt
    SQLSMALLINT *TextLength
);

SQLRETURN  SQL_API SQLGetEnvAttr(SQLHENV EnvironmentHandle,
           SQLINTEGER Attribute, __out_xcount(BufferLength) SQLPOINTER Value,
           SQLINTEGER BufferLength, __out_opt SQLINTEGER *StringLength);
#endif  /* ODBCVER >= 0x0300 */

SQLRETURN  SQL_API SQLGetFunctions(SQLHDBC ConnectionHandle,
           SQLUSMALLINT FunctionId, 
           __out_xcount_opt("Buffer length pfExists points to depends on fFunction value.") 
           SQLUSMALLINT *Supported);

__success(return == SQL_SUCCESS)
SQLRETURN  SQL_API SQLGetInfo(SQLHDBC ConnectionHandle,
           SQLUSMALLINT InfoType, __out_bcount_opt(BufferLength) SQLPOINTER InfoValue,
           SQLSMALLINT BufferLength, __out_opt SQLSMALLINT *StringLengthPtr);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLGetStmtAttr(SQLHSTMT StatementHandle,
           SQLINTEGER Attribute, __out_xcount_opt(BufferLength) SQLPOINTER Value,
           SQLINTEGER BufferLength, __out_opt SQLINTEGER *StringLength);
#endif  /* ODBCVER >= 0x0300 */

SQLRETURN  SQL_API SQLGetStmtOption(SQLHSTMT StatementHandle,
           SQLUSMALLINT Option, SQLPOINTER Value);

SQLRETURN  SQL_API SQLGetTypeInfo(SQLHSTMT StatementHandle,
           SQLSMALLINT DataType);

SQLRETURN  SQL_API SQLNumResultCols(SQLHSTMT StatementHandle,
           __out SQLSMALLINT *ColumnCount);

SQLRETURN  SQL_API SQLParamData(SQLHSTMT StatementHandle,
           __out_opt SQLPOINTER *Value);

SQLRETURN  SQL_API SQLPrepare
(
    SQLHSTMT StatementHandle,
    __in_ecount(TextLength) SQLCHAR* StatementText,
    SQLINTEGER TextLength
);

SQLRETURN  SQL_API SQLPutData(SQLHSTMT StatementHandle,
           __in_xcount(StrLen_or_Ind) SQLPOINTER Data, SQLLEN StrLen_or_Ind);

SQLRETURN  SQL_API SQLRowCount(__in SQLHSTMT StatementHandle,
                               __out SQLLEN* RowCount);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLSetConnectAttr(SQLHDBC ConnectionHandle,
           SQLINTEGER Attribute, __in_bcount_opt(StringLength) SQLPOINTER Value,
           SQLINTEGER StringLength);
#endif /* ODBCVER >= 0x0300 */

SQLRETURN  SQL_API SQLSetConnectOption(SQLHDBC ConnectionHandle,
           SQLUSMALLINT Option, SQLULEN Value);

SQLRETURN  SQL_API SQLSetCursorName
(
    SQLHSTMT StatementHandle,
    __in_ecount(NameLength) SQLCHAR* CursorName,
    SQLSMALLINT NameLength
);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLSetDescField(SQLHDESC DescriptorHandle,
           SQLSMALLINT RecNumber, SQLSMALLINT FieldIdentifier,
           __in_xcount(BufferLength) SQLPOINTER Value, SQLINTEGER BufferLength);

SQLRETURN  SQL_API SQLSetDescRec(SQLHDESC DescriptorHandle,
           SQLSMALLINT RecNumber, SQLSMALLINT Type,
           SQLSMALLINT SubType, SQLLEN Length,
           SQLSMALLINT Precision, SQLSMALLINT Scale,
           __inout_bcount_opt(Length) SQLPOINTER Data, __inout_opt SQLLEN *StringLength,
           __inout_opt SQLLEN *Indicator);

SQLRETURN  SQL_API SQLSetEnvAttr(SQLHENV EnvironmentHandle,
           SQLINTEGER Attribute, __in_bcount(StringLength) SQLPOINTER Value,
           SQLINTEGER StringLength);
#endif /* ODBCVER >= 0x0300 */

SQLRETURN  SQL_API SQLSetParam(SQLHSTMT StatementHandle,
           SQLUSMALLINT ParameterNumber, SQLSMALLINT ValueType,
           SQLSMALLINT ParameterType, SQLULEN LengthPrecision,
           SQLSMALLINT ParameterScale, __in_xcount(*StrLen_or_Ind) SQLPOINTER ParameterValue,
           __inout SQLLEN *StrLen_or_Ind);

#if (ODBCVER >= 0x0300)
SQLRETURN  SQL_API SQLSetStmtAttr(SQLHSTMT StatementHandle,
           SQLINTEGER Attribute, __in_xcount(StringLength) SQLPOINTER Value,
           SQLINTEGER StringLength);
#endif

SQLRETURN  SQL_API SQLSetStmtOption(SQLHSTMT StatementHandle,
           SQLUSMALLINT Option, SQLULEN Value);

SQLRETURN  SQL_API SQLSpecialColumns(SQLHSTMT StatementHandle,
           SQLUSMALLINT IdentifierType, 
           __in_ecount_opt(NameLength1) SQLCHAR *CatalogName, SQLSMALLINT NameLength1,
           __in_ecount_opt(NameLength2) SQLCHAR *SchemaName, SQLSMALLINT NameLength2, 
           __in_ecount_opt(NameLength3) SQLCHAR *TableName, SQLSMALLINT NameLength3, 
           SQLUSMALLINT Scope, SQLUSMALLINT Nullable);

SQLRETURN  SQL_API SQLStatistics(SQLHSTMT StatementHandle,
           __in_ecount_opt(NameLength1) SQLCHAR *CatalogName, SQLSMALLINT NameLength1,
           __in_ecount_opt(NameLength2) SQLCHAR *SchemaName, SQLSMALLINT NameLength2,
           __in_ecount_opt(NameLength3) SQLCHAR *TableName, SQLSMALLINT NameLength3,
           SQLUSMALLINT Unique, SQLUSMALLINT Reserved);

SQLRETURN  SQL_API SQLTables(SQLHSTMT StatementHandle,
           __in_ecount_opt(NameLength1) SQLCHAR *CatalogName, SQLSMALLINT NameLength1,
           __in_ecount_opt(NameLength2) SQLCHAR *SchemaName, SQLSMALLINT NameLength2,
           __in_ecount_opt(NameLength3) SQLCHAR *TableName, SQLSMALLINT NameLength3,
           __in_ecount_opt(NameLength4) SQLCHAR *TableType, SQLSMALLINT NameLength4);

SQLRETURN  SQL_API SQLTransact(SQLHENV EnvironmentHandle,
           SQLHDBC ConnectionHandle, SQLUSMALLINT CompletionType);

#endif  /* RC_INVOKED */

#ifdef __cplusplus
}                                    /* End of extern "C" { */
#endif  /* __cplusplus */
#endif  /* #ifndef __SQL */






(defconstant $ODBCVER	#x0210)

;; generally useful constants
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_SPEC_MAJOR 2)		;; Major version of specification 
(defconstant $SQL_SPEC_MINOR 10) 	;; Minor version of specification 
(defconstant $SQL_SPEC_STRING 	"02.10") ;; String constant for version	  
;;;; #endif	;; ODBCVER >= #x0200
(defconstant $SQL_SQLSTATE_SIZE 5)		;; size of SQLSTATE 			  
(defconstant $SQL_MAX_MESSAGE_LENGTH 512)	;; message buffer size			  
(defconstant $SQL_MAX_DSN_LENGTH 32)		;; maximum data source name size  

;; RETCODEs
(defconstant $SQL_INVALID_HANDLE -2)
(defconstant $SQL_ERROR -1)
(defconstant $SQL_SUCCESS 0)
(defconstant $SQL_SUCCESS_WITH_INFO 1)
(defconstant $SQL_NO_DATA_FOUND 100)

;; Standard SQL datatypes, using ANSI type numbering
(defconstant $SQL_CHAR 1)
(defconstant $SQL_NUMERIC 2)
(defconstant $SQL_DECIMAL 3)
(defconstant $SQL_INTEGER 4)
(defconstant $SQL_SMALLINT 5)
(defconstant $SQL_FLOAT 6)
(defconstant $SQL_REAL 7)
(defconstant $SQL_DOUBLE 8)
(defconstant $SQL_VARCHAR 12)

(defconstant $SQL_TYPE_MIN $SQL_CHAR)
(defconstant $SQL_TYPE_NULL 0)
(defconstant $SQL_TYPE_MAX $SQL_VARCHAR)

;; C datatype to SQL datatype mapping	SQL types

(defconstant $SQL_C_CHAR $SQL_CHAR)		;; CHAR, VARCHAR, DECIMAL, NUMERIC
(defconstant $SQL_C_LONG $SQL_INTEGER)		;; INTEGER 
(defconstant $SQL_C_SHORT $SQL_SMALLINT)	;; SMALLINT 
(defconstant $SQL_C_FLOAT $SQL_REAL)		;; REAL 
(defconstant $SQL_C_DOUBLE $SQL_DOUBLE)		;; FLOAT, DOUBLE
(defconstant $SQL_C_DEFAULT 99)

;; NULL status constants.  These are used in SQLColumns, SQLColAttributes,
;;SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
;;nullablity of a column in a table.

(defconstant $SQL_NO_NULLS 0)
(defconstant $SQL_NULLABLE 1)
(defconstant $SQL_NULLABLE_UNKNOWN 2)

;; Special length values
(defconstant $SQL_NULL_DATA -1)
(defconstant $SQL_DATA_AT_EXEC -2)
(defconstant $SQL_NTS -3)

;; SQLFreeStmt defines
(defconstant $SQL_CLOSE 0)
(defconstant $SQL_DROP 1)
(defconstant $SQL_UNBIND 2)
(defconstant $SQL_RESET_PARAMS 3)

;; SQLTransact defines
(defconstant $SQL_COMMIT 0)
(defconstant $SQL_ROLLBACK 1)

;; SQLColAttributes defines
(defconstant $SQL_COLUMN_COUNT 0)
(defconstant $SQL_COLUMN_NAME 1)
(defconstant $SQL_COLUMN_TYPE 2)
(defconstant $SQL_COLUMN_LENGTH 3)
(defconstant $SQL_COLUMN_PRECISION 4)
(defconstant $SQL_COLUMN_SCALE 5)
(defconstant $SQL_COLUMN_DISPLAY_SIZE 6)
(defconstant $SQL_COLUMN_NULLABLE 7)
(defconstant $SQL_COLUMN_UNSIGNED 8)
(defconstant $SQL_COLUMN_MONEY 9)
(defconstant $SQL_COLUMN_UPDATABLE 10)
(defconstant $SQL_COLUMN_AUTO_INCREMENT	11)
(defconstant $SQL_COLUMN_CASE_SENSITIVE	12)
(defconstant $SQL_COLUMN_SEARCHABLE 13)
(defconstant $SQL_COLUMN_TYPE_NAME 14)
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_COLUMN_TABLE_NAME 15)
(defconstant $SQL_COLUMN_OWNER_NAME 16)
(defconstant $SQL_COLUMN_QUALIFIER_NAME	17)
(defconstant $SQL_COLUMN_LABEL 18)
(defconstant $SQL_COLATT_OPT_MAX $SQL_COLUMN_LABEL)
#|;; #else
(defconstant $SQL_COLATT_OPT_MAX $SQL_COLUMN_TYPE_NAME) |#

(defconstant $SQL_COLUMN_DRIVER_START 1000)

(defconstant $SQL_COLATT_OPT_MIN $SQL_COLUMN_COUNT)

;; SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
(defconstant $SQL_ATTR_READONLY 0)
(defconstant $SQL_ATTR_WRITE 1)
(defconstant $SQL_ATTR_READWRITE_UNKNOWN 2)

;; SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
;; These are also used by SQLGetInfo                    
(defconstant $SQL_UNSEARCHABLE 0)
(defconstant $SQL_LIKE_ONLY 1)
(defconstant $SQL_ALL_EXCEPT_LIKE 2)
(defconstant $SQL_SEARCHABLE 3)

;; SQLError defines
(defconstant $SQL_NULL_HENV 0)
(defconstant $SQL_NULL_HDBC 0)
(defconstant $SQL_NULL_HSTMT 0)

;; Defines for SQLGetFunctions
;; Core Functions
;;
(defconstant $SQL_API_SQLALLOCCONNECT      1)
(defconstant $SQL_API_SQLALLOCENV          2)
(defconstant $SQL_API_SQLALLOCSTMT         3)
(defconstant $SQL_API_SQLBINDCOL           4)
(defconstant $SQL_API_SQLCANCEL            5)
(defconstant $SQL_API_SQLCOLATTRIBUTES     6)
(defconstant $SQL_API_SQLCONNECT           7)
(defconstant $SQL_API_SQLDESCRIBECOL       8)
(defconstant $SQL_API_SQLDISCONNECT        9)
(defconstant $SQL_API_SQLERROR            10)
(defconstant $SQL_API_SQLEXECDIRECT       11)
(defconstant $SQL_API_SQLEXECUTE          12)
(defconstant $SQL_API_SQLFETCH            13)
(defconstant $SQL_API_SQLFREECONNECT      14)
(defconstant $SQL_API_SQLFREEENV          15)
(defconstant $SQL_API_SQLFREESTMT         16)
(defconstant $SQL_API_SQLGETCURSORNAME    17)
(defconstant $SQL_API_SQLNUMRESULTCOLS    18)
(defconstant $SQL_API_SQLPREPARE          19)
(defconstant $SQL_API_SQLROWCOUNT         20)
(defconstant $SQL_API_SQLSETCURSORNAME    21)
(defconstant $SQL_API_SQLSETPARAM         22)
(defconstant $SQL_API_SQLTRANSACT         23)
(defconstant $SQL_NUM_FUNCTIONS           23)
(defconstant $SQL_EXT_API_START           40)

;; Level 1 Functions

(defconstant $SQL_API_SQLCOLUMNS          40)
(defconstant $SQL_API_SQLDRIVERCONNECT    41)
(defconstant $SQL_API_SQLGETCONNECTOPTION 42)
(defconstant $SQL_API_SQLGETDATA          43)
(defconstant $SQL_API_SQLGETFUNCTIONS     44)
(defconstant $SQL_API_SQLGETINFO          45)
(defconstant $SQL_API_SQLGETSTMTOPTION    46)
(defconstant $SQL_API_SQLGETTYPEINFO      47)
(defconstant $SQL_API_SQLPARAMDATA        48)
(defconstant $SQL_API_SQLPUTDATA          49)
(defconstant $SQL_API_SQLSETCONNECTOPTION 50)
(defconstant $SQL_API_SQLSETSTMTOPTION    51)
(defconstant $SQL_API_SQLSPECIALCOLUMNS   52)
(defconstant $SQL_API_SQLSTATISTICS       53)
(defconstant $SQL_API_SQLTABLES           54)

;; Level 2 Functions

(defconstant $SQL_API_SQLBROWSECONNECT    55)
(defconstant $SQL_API_SQLCOLUMNPRIVILEGES 56)
(defconstant $SQL_API_SQLDATASOURCES      57)
(defconstant $SQL_API_SQLDESCRIBEPARAM    58)
(defconstant $SQL_API_SQLEXTENDEDFETCH    59)
(defconstant $SQL_API_SQLFOREIGNKEYS      60)
(defconstant $SQL_API_SQLMORERESULTS      61)
(defconstant $SQL_API_SQLNATIVESQL        62)
(defconstant $SQL_API_SQLNUMPARAMS        63)
(defconstant $SQL_API_SQLPARAMOPTIONS     64)
(defconstant $SQL_API_SQLPRIMARYKEYS      65)
(defconstant $SQL_API_SQLPROCEDURECOLUMNS 66)
(defconstant $SQL_API_SQLPROCEDURES       67)
(defconstant $SQL_API_SQLSETPOS           68)
(defconstant $SQL_API_SQLSETSCROLLOPTIONS 69)
(defconstant $SQL_API_SQLTABLEPRIVILEGES  70)

;/*		SDK 2.0 Additions		*/
;#if (ODBCVER >= #x0200))
(defconstant $SQL_API_SQLDRIVERS 71)
(defconstant $SQL_API_SQLBINDPARAMETER	72)
(defconstant $SQL_EXT_API_LAST $SQL_API_SQLBINDPARAMETER)
;;; #else
;(defconstant $SQL_EXT_API_LAST $SQL_API_SQLTABLEPRIVILEGES)
;;; #endif	;; ODBCVER >= #x0200

(defconstant $SQL_API_ALL_FUNCTIONS 0)

(defconstant $SQL_NUM_EXTENSIONS (- $SQL_EXT_API_LAST $SQL_EXT_API_START -1))
;#if (ODBCVER >= #x0200))
(defconstant $SQL_API_LOADBYORDINAL 199)
;;;; #endif	;; ODBCVER >= #x0200

;;; Defines for SQLGetInfo
(defconstant $SQL_INFO_FIRST                       0)
(defconstant $SQL_ACTIVE_CONNECTIONS               0)
(defconstant $SQL_ACTIVE_STATEMENTS                1)
(defconstant $SQL_DATA_SOURCE_NAME                 2)
(defconstant $SQL_DRIVER_HDBC                      3)
(defconstant $SQL_DRIVER_HENV                      4)
(defconstant $SQL_DRIVER_HSTMT                     5)
(defconstant $SQL_DRIVER_NAME                      6)
(defconstant $SQL_DRIVER_VER                       7)
(defconstant $SQL_FETCH_DIRECTION                  8)
(defconstant $SQL_ODBC_API_CONFORMANCE             9)
(defconstant $SQL_ODBC_VER                        10)
(defconstant $SQL_ROW_UPDATES                     11)
(defconstant $SQL_ODBC_SAG_CLI_CONFORMANCE        12)
(defconstant $SQL_SERVER_NAME                     13)
(defconstant $SQL_SEARCH_PATTERN_ESCAPE           14)
(defconstant $SQL_ODBC_SQL_CONFORMANCE            15)

(defconstant $SQL_DBMS_NAME                       17)
(defconstant $SQL_DBMS_VER                        18)

(defconstant $SQL_ACCESSIBLE_TABLES               19)
(defconstant $SQL_ACCESSIBLE_PROCEDURES           20)
(defconstant $SQL_PROCEDURES                      21)
(defconstant $SQL_CONCAT_NULL_BEHAVIOR            22)
(defconstant $SQL_CURSOR_COMMIT_BEHAVIOR          23)
(defconstant $SQL_CURSOR_ROLLBACK_BEHAVIOR        24)
(defconstant $SQL_DATA_SOURCE_READ_ONLY           25)
(defconstant $SQL_DEFAULT_TXN_ISOLATION           26)
(defconstant $SQL_EXPRESSIONS_IN_ORDERBY          27)
(defconstant $SQL_IDENTIFIER_CASE                 28)
(defconstant $SQL_IDENTIFIER_QUOTE_CHAR           29)
(defconstant $SQL_MAX_COLUMN_NAME_LEN             30)
(defconstant $SQL_MAX_CURSOR_NAME_LEN             31)
(defconstant $SQL_MAX_OWNER_NAME_LEN              32)
(defconstant $SQL_MAX_PROCEDURE_NAME_LEN          33)
(defconstant $SQL_MAX_QUALIFIER_NAME_LEN          34)
(defconstant $SQL_MAX_TABLE_NAME_LEN              35)
(defconstant $SQL_MULT_RESULT_SETS                36)
(defconstant $SQL_MULTIPLE_ACTIVE_TXN             37)
(defconstant $SQL_OUTER_JOINS                     38)
(defconstant $SQL_OWNER_TERM                      39)
(defconstant $SQL_PROCEDURE_TERM                  40)
(defconstant $SQL_QUALIFIER_NAME_SEPARATOR        41)
(defconstant $SQL_QUALIFIER_TERM                  42)
(defconstant $SQL_SCROLL_CONCURRENCY              43)
(defconstant $SQL_SCROLL_OPTIONS                  44)
(defconstant $SQL_TABLE_TERM                      45)
(defconstant $SQL_TXN_CAPABLE                     46)
(defconstant $SQL_USER_NAME                       47)

(defconstant $SQL_CONVERT_FUNCTIONS               48)
(defconstant $SQL_NUMERIC_FUNCTIONS               49)
(defconstant $SQL_STRING_FUNCTIONS                50)
(defconstant $SQL_SYSTEM_FUNCTIONS                51)
(defconstant $SQL_TIMEDATE_FUNCTIONS              52)

(defconstant $SQL_CONVERT_BIGINT                  53)
(defconstant $SQL_CONVERT_BINARY                  54)
(defconstant $SQL_CONVERT_BIT                     55)
(defconstant $SQL_CONVERT_CHAR                    56)
(defconstant $SQL_CONVERT_DATE                    57)
(defconstant $SQL_CONVERT_DECIMAL                 58)
(defconstant $SQL_CONVERT_DOUBLE                  59)
(defconstant $SQL_CONVERT_FLOAT                   60)
(defconstant $SQL_CONVERT_INTEGER                 61)
(defconstant $SQL_CONVERT_LONGVARCHAR             62)
(defconstant $SQL_CONVERT_NUMERIC                 63)
(defconstant $SQL_CONVERT_REAL                    64)
(defconstant $SQL_CONVERT_SMALLINT                65)
(defconstant $SQL_CONVERT_TIME                    66)
(defconstant $SQL_CONVERT_TIMESTAMP               67)
(defconstant $SQL_CONVERT_TINYINT                 68)
(defconstant $SQL_CONVERT_VARBINARY               69)
(defconstant $SQL_CONVERT_VARCHAR                 70)
(defconstant $SQL_CONVERT_LONGVARBINARY           71)

(defconstant $SQL_TXN_ISOLATION_OPTION            72)
(defconstant $SQL_ODBC_SQL_OPT_IEF                73)

;;; ODBC SDK 1.0 Additions
(defconstant $SQL_CORRELATION_NAME 74)
(defconstant $SQL_NON_NULLABLE_COLUMNS 75)

;;; ODBC SDK 2.0 Additions
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_DRIVER_HLIB 			76)
(defconstant $SQL_DRIVER_ODBC_VER		77)
(defconstant $SQL_LOCK_TYPES			78)
(defconstant $SQL_POS_OPERATIONS		79)
(defconstant $SQL_POSITIONED_STATEMENTS		80)
(defconstant $SQL_GETDATA_EXTENSIONS		81)
(defconstant $SQL_BOOKMARK_PERSISTENCE		82)
(defconstant $SQL_STATIC_SENSITIVITY		83)
(defconstant $SQL_FILE_USAGE			84)
(defconstant $SQL_NULL_COLLATION		85)
(defconstant $SQL_ALTER_TABLE 			86)
(defconstant $SQL_COLUMN_ALIAS			87)
(defconstant $SQL_GROUP_BY			88)
(defconstant $SQL_KEYWORDS			89)
(defconstant $SQL_ORDER_BY_COLUMNS_IN_SELECT	90)
(defconstant $SQL_OWNER_USAGE 			91)
(defconstant $SQL_QUALIFIER_USAGE		92)
(defconstant $SQL_QUOTED_IDENTIFIER_CASE	93)
(defconstant $SQL_SPECIAL_CHARACTERS		94)
(defconstant $SQL_SUBQUERIES			95)
(defconstant $SQL_UNION				96)
(defconstant $SQL_MAX_COLUMNS_IN_GROUP_BY	97)
(defconstant $SQL_MAX_COLUMNS_IN_INDEX		98)
(defconstant $SQL_MAX_COLUMNS_IN_ORDER_BY	99)
(defconstant $SQL_MAX_COLUMNS_IN_SELECT	       100)
(defconstant $SQL_MAX_COLUMNS_IN_TABLE		   101)
(defconstant $SQL_MAX_INDEX_SIZE				   102)
(defconstant $SQL_MAX_ROW_SIZE_INCLUDES_LONG	   103)
(defconstant $SQL_MAX_ROW_SIZE				   104)
(defconstant $SQL_MAX_STATEMENT_LEN			   105)
(defconstant $SQL_MAX_TABLES_IN_SELECT 106)
(defconstant $SQL_MAX_USER_NAME_LEN 107)
(defconstant $SQL_MAX_CHAR_LITERAL_LEN 108)
(defconstant $SQL_TIMEDATE_ADD_INTERVALS 109)
(defconstant $SQL_TIMEDATE_DIFF_INTERVALS 	   110)
(defconstant $SQL_NEED_LONG_DATA_LEN 111)
(defconstant $SQL_MAX_BINARY_LITERAL_LEN		   112)
(defconstant $SQL_LIKE_ESCAPE_CLAUSE			   113)
(defconstant $SQL_QUALIFIER_LOCATION			   114)
(defconstant $SQL_ACTIVE_ENVIRONMENTS 116)

#|

;#if (ODBCVER >= #x0201)
;;/*** ODBC SDK 2.01 Additions ***/)
(defconstant $SQL_OJ_CAPABILITIES 			 65003	;; Temp value until ODBC 3.0
;;;; #endif	;; ODBCVER >= #x0201

(defconstant $SQL_INFO_LAST						SQL_QUALIFIER_LOCATION
;;;; #else
(defconstant $SQL_INFO_LAST						SQL_NON_NULLABLE_COLUMNS
;;;; #endif	;; ODBCVER >= #x0200
)
(defconstant $SQL_INFO_DRIVER_START             1000

;; SQL_CONVERT_*  return value bitmasks
)
(defconstant $SQL_CVT_CHAR				#x00000001L)
(defconstant $SQL_CVT_NUMERIC 			#x00000002L)
(defconstant $SQL_CVT_DECIMAL 			#x00000004L)
(defconstant $SQL_CVT_INTEGER 			#x00000008L)
(defconstant $SQL_CVT_SMALLINT			#x00000010L)
(defconstant $SQL_CVT_FLOAT				#x00000020L)
(defconstant $SQL_CVT_REAL				#x00000040L)
(defconstant $SQL_CVT_DOUBLE				#x00000080L)
(defconstant $SQL_CVT_VARCHAR 			#x00000100L)
(defconstant $SQL_CVT_LONGVARCHAR 		#x00000200L)
(defconstant $SQL_CVT_BINARY				#x00000400L)
(defconstant $SQL_CVT_VARBINARY			#x00000800L)
(defconstant $SQL_CVT_BIT 				#x00001000L)
(defconstant $SQL_CVT_TINYINT 			#x00002000L)
(defconstant $SQL_CVT_BIGINT				#x00004000L)
(defconstant $SQL_CVT_DATE				#x00008000L)
(defconstant $SQL_CVT_TIME				#x00010000L)
(defconstant $SQL_CVT_TIMESTAMP			#x00020000L)
(defconstant $SQL_CVT_LONGVARBINARY		#x00040000L)

;; SQL_CONVERT_FUNCTIONS functions)
(defconstant $SQL_FN_CVT_CONVERT			#x00000001L)

;; SQL_STRING_FUNCTIONS functions

(defconstant $SQL_FN_STR_CONCAT			#x00000001L)
(defconstant $SQL_FN_STR_INSERT			#x00000002L)
(defconstant $SQL_FN_STR_LEFT 			#x00000004L)
(defconstant $SQL_FN_STR_LTRIM			#x00000008L)
(defconstant $SQL_FN_STR_LENGTH			#x00000010L)
(defconstant $SQL_FN_STR_LOCATE			#x00000020L)
(defconstant $SQL_FN_STR_LCASE			#x00000040L)
(defconstant $SQL_FN_STR_REPEAT			#x00000080L)
(defconstant $SQL_FN_STR_REPLACE			#x00000100L)
(defconstant $SQL_FN_STR_RIGHT			#x00000200L)
(defconstant $SQL_FN_STR_RTRIM			#x00000400L)
(defconstant $SQL_FN_STR_SUBSTRING		#x00000800L)
(defconstant $SQL_FN_STR_UCASE			#x00001000L)
(defconstant $SQL_FN_STR_ASCII			#x00002000L)
(defconstant $SQL_FN_STR_CHAR 			#x00004000L
;#if (ODBCVER >= #x0200))
(defconstant $SQL_FN_STR_DIFFERENCE		#x00008000L)
(defconstant $SQL_FN_STR_LOCATE_2 		#x00010000L)
(defconstant $SQL_FN_STR_SOUNDEX			#x00020000L)
(defconstant $SQL_FN_STR_SPACE			#x00040000L
;; #endif	;; ODBCVER >= #x0200

;; SQL_NUMERIC_FUNCTIONS functions
)
(defconstant $SQL_FN_NUM_ABS				#x00000001L)
(defconstant $SQL_FN_NUM_ACOS 			#x00000002L)
(defconstant $SQL_FN_NUM_ASIN 			#x00000004L)
(defconstant $SQL_FN_NUM_ATAN 			#x00000008L)
(defconstant $SQL_FN_NUM_ATAN2			#x00000010L)
(defconstant $SQL_FN_NUM_CEILING			#x00000020L)
(defconstant $SQL_FN_NUM_COS				#x00000040L)
(defconstant $SQL_FN_NUM_COT				#x00000080L)
(defconstant $SQL_FN_NUM_EXP				#x00000100L)
(defconstant $SQL_FN_NUM_FLOOR			#x00000200L)
(defconstant $SQL_FN_NUM_LOG				#x00000400L)
(defconstant $SQL_FN_NUM_MOD				#x00000800L)
(defconstant $SQL_FN_NUM_SIGN 			#x00001000L)
(defconstant $SQL_FN_NUM_SIN				#x00002000L)
(defconstant $SQL_FN_NUM_SQRT 			#x00004000L)
(defconstant $SQL_FN_NUM_TAN				#x00008000L)
(defconstant $SQL_FN_NUM_PI				#x00010000L)
(defconstant $SQL_FN_NUM_RAND 			#x00020000L
;#if (ODBCVER >= #x0200))
(defconstant $SQL_FN_NUM_DEGREES			#x00040000L)
(defconstant $SQL_FN_NUM_LOG10			#x00080000L)
(defconstant $SQL_FN_NUM_POWER			#x00100000L)
(defconstant $SQL_FN_NUM_RADIANS			#x00200000L)
(defconstant $SQL_FN_NUM_ROUND			#x00400000L)
(defconstant $SQL_FN_NUM_TRUNCATE 		#x00800000L
;; #endif	;; ODBCVER >= #x0200

;; SQL_TIMEDATE_FUNCTIONS functions
)
(defconstant $SQL_FN_TD_NOW				#x00000001L)
(defconstant $SQL_FN_TD_CURDATE			#x00000002L)
(defconstant $SQL_FN_TD_DAYOFMONTH		#x00000004L)
(defconstant $SQL_FN_TD_DAYOFWEEK 		#x00000008L)
(defconstant $SQL_FN_TD_DAYOFYEAR 		#x00000010L)
(defconstant $SQL_FN_TD_MONTH 			#x00000020L)
(defconstant $SQL_FN_TD_QUARTER			#x00000040L)
(defconstant $SQL_FN_TD_WEEK				#x00000080L)
(defconstant $SQL_FN_TD_YEAR				#x00000100L)
(defconstant $SQL_FN_TD_CURTIME			#x00000200L)
(defconstant $SQL_FN_TD_HOUR				#x00000400L)
(defconstant $SQL_FN_TD_MINUTE			#x00000800L)
(defconstant $SQL_FN_TD_SECOND			#x00001000L
; #if (ODBCVER >= #x0200))
(defconstant $SQL_FN_TD_TIMESTAMPADD		#x00002000L)
(defconstant $SQL_FN_TD_TIMESTAMPDIFF 	#x00004000L)
(defconstant $SQL_FN_TD_DAYNAME			#x00008000L)
(defconstant $SQL_FN_TD_MONTHNAME 		#x00010000L
;; #endif	;; ODBCVER >= #x0200

;; SQL_SYSTEM_FUNCTIONS functions
)
(defconstant $SQL_FN_SYS_USERNAME 		#x00000001L)
(defconstant $SQL_FN_SYS_DBNAME			#x00000002L)
(defconstant $SQL_FN_SYS_IFNULL			#x00000004L

;; SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions

; #if (ODBCVER >= #x0200))
(defconstant $SQL_FN_TSI_FRAC_SECOND		#x00000001L)
(defconstant $SQL_FN_TSI_SECOND			#x00000002L)
(defconstant $SQL_FN_TSI_MINUTE			#x00000004L)
(defconstant $SQL_FN_TSI_HOUR 			#x00000008L)
(defconstant $SQL_FN_TSI_DAY				#x00000010L)
(defconstant $SQL_FN_TSI_WEEK 			#x00000020L)
(defconstant $SQL_FN_TSI_MONTH			#x00000040L)
(defconstant $SQL_FN_TSI_QUARTER			#x00000080L)
(defconstant $SQL_FN_TSI_YEAR 			#x00000100L
;; #endif	;; ODBCVER >= #x0200

;; SQL_ODBC_API_CONFORMANCE values
)
(defconstant $SQL_OAC_NONE				#x0000)
(defconstant $SQL_OAC_LEVEL1				#x0001)
(defconstant $SQL_OAC_LEVEL2				#x0002

;; SQL_ODBC_SAG_CLI_CONFORMANCE values
)
(defconstant $SQL_OSCC_NOT_COMPLIANT		#x0000)
(defconstant $SQL_OSCC_COMPLIANT			#x0001

;; SQL_ODBC_SQL_CONFORMANCE values
)
(defconstant $SQL_OSC_MINIMUM 			#x0000)
(defconstant $SQL_OSC_CORE				#x0001)
(defconstant $SQL_OSC_EXTENDED			#x0002

;; SQL_CONCAT_NULL_BEHAVIOR values
)
(defconstant $SQL_CB_NULL 				#x0000)
(defconstant $SQL_CB_NON_NULL 			#x0001

;; SQL_CURSOR_COMMIT_BEHAVIOR and SQL_CURSOR_ROLLBACK_BEHAVIOR values
)
(defconstant $SQL_CB_DELETE				#x0000)
(defconstant $SQL_CB_CLOSE				#x0001)
(defconstant $SQL_CB_PRESERVE				#x0002

;; SQL_IDENTIFIER_CASE values
)
(defconstant $SQL_IC_UPPER				#x0001)
(defconstant $SQL_IC_LOWER				#x0002)
(defconstant $SQL_IC_SENSITIVE			#x0003)
(defconstant $SQL_IC_MIXED				#x0004

;; SQL_TXN_CAPABLE values
|#

(defconstant $SQL_TC_NONE 0)
(defconstant $SQL_TC_DML 1)
(defconstant $SQL_TC_ALL 2)

; #if (ODBCVER >= #x0200)
(defconstant $SQL_TC_DDL_COMMIT 3)
(defconstant $SQL_TC_DDL_IGNORE 4)
;; #endif	;; ODBCVER >= #x0200

;; SQL_SCROLL_OPTIONS masks


(defconstant $SQL_SO_FORWARD_ONLY #x00000001)
(defconstant $SQL_SO_KEYSET_DRIVEN #x00000002)
(defconstant $SQL_SO_DYNAMIC #x00000004)
(defconstant $SQL_SO_MIXED #x00000008)
; #if (ODBCVER >= #x0200))
(defconstant $SQL_SO_STATIC #x00000010)
;; #endif	;; ODBCVER >= #x0200

;; SQL_SCROLL_CONCURRENCY masks

(defconstant $SQL_SCCO_READ_ONLY #x00000001)
(defconstant $SQL_SCCO_LOCK #x00000002)
(defconstant $SQL_SCCO_OPT_ROWVER #x00000004)
(defconstant $SQL_SCCO_OPT_VALUES #x00000008)

;; SQL_FETCH_DIRECTION masks

(defconstant $SQL_FD_FETCH_NEXT #x00000001)
(defconstant $SQL_FD_FETCH_FIRST #x00000002)
(defconstant $SQL_FD_FETCH_LAST #x00000004)
(defconstant $SQL_FD_FETCH_PRIOR #x00000008)
(defconstant $SQL_FD_FETCH_ABSOLUTE #x00000010)
(defconstant $SQL_FD_FETCH_RELATIVE #x00000020)
(defconstant $SQL_FD_FETCH_RESUME #x00000040)
; #if (ODBCVER >= #x0200)
(defconstant $SQL_FD_FETCH_BOOKMARK #x00000080)
;; #endif	;; ODBCVER >= #x0200

#|
;; SQL_TXN_ISOLATION_OPTION masks
)
(defconstant $SQL_TXN_READ_UNCOMMITTED	#x00000001L)
(defconstant $SQL_TXN_READ_COMMITTED		#x00000002L)
(defconstant $SQL_TXN_REPEATABLE_READ 	#x00000004L)
(defconstant $SQL_TXN_SERIALIZABLE		#x00000008L)
(defconstant $SQL_TXN_VERSIONING			#x00000010L

;; SQL_CORRELATION_NAME values
)
(defconstant $SQL_CN_NONE 				#x0000)
(defconstant $SQL_CN_DIFFERENT			#x0001)
(defconstant $SQL_CN_ANY					#x0002

;; SQL_NON_NULLABLE_COLUMNS values
)
(defconstant $SQL_NNC_NULL			   	#x0000)
(defconstant $SQL_NNC_NON_NULL			#x0001

; #if (ODBCVER >= #x0200)
;; SQL_NULL_COLLATION values
									  )
(defconstant $SQL_NC_HIGH 				#x0000)
(defconstant $SQL_NC_LOW					#x0001)
(defconstant $SQL_NC_START				#x0002)
(defconstant $SQL_NC_END					#x0004

;; SQL_FILE_USAGE values
)
(defconstant $SQL_FILE_NOT_SUPPORTED		#x0000)
(defconstant $SQL_FILE_TABLE				#x0001)
(defconstant $SQL_FILE_QUALIFIER			#x0002

;; SQL_GETDATA_EXTENSIONS values
)
(defconstant $SQL_GD_ANY_COLUMN			#x00000001L)
(defconstant $SQL_GD_ANY_ORDER			#x00000002L)
(defconstant $SQL_GD_BLOCK				#x00000004L)
(defconstant $SQL_GD_BOUND				#x00000008L

;; SQL_ALTER_TABLE values
)
(defconstant $SQL_AT_ADD_COLUMN			#x00000001L)
(defconstant $SQL_AT_DROP_COLUMN			#x00000002L

;; SQL_POSITIONED_STATEMENTS masks
)
(defconstant $SQL_PS_POSITIONED_DELETE	#x00000001L)
(defconstant $SQL_PS_POSITIONED_UPDATE	#x00000002L)
(defconstant $SQL_PS_SELECT_FOR_UPDATE	#x00000004L

;; SQL_GROUP_BY values
)
(defconstant $SQL_GB_NOT_SUPPORTED			#x0000)
(defconstant $SQL_GB_GROUP_BY_EQUALS_SELECT	#x0001)
(defconstant $SQL_GB_GROUP_BY_CONTAINS_SELECT	#x0002)
(defconstant $SQL_GB_NO_RELATION				#x0003
													
;; SQL_OWNER_USAGE masks
)
(defconstant $SQL_OU_DML_STATEMENTS		#x00000001L)
(defconstant $SQL_OU_PROCEDURE_INVOCATION #x00000002L)
(defconstant $SQL_OU_TABLE_DEFINITION 	#x00000004L)
(defconstant $SQL_OU_INDEX_DEFINITION 	#x00000008L)
(defconstant $SQL_OU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_QUALIFIER_USAGE masks
)
(defconstant $SQL_QU_DML_STATEMENTS		#x00000001L)
(defconstant $SQL_QU_PROCEDURE_INVOCATION #x00000002L)
(defconstant $SQL_QU_TABLE_DEFINITION 	#x00000004L)
(defconstant $SQL_QU_INDEX_DEFINITION 	#x00000008L)
(defconstant $SQL_QU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_SUBQUERIES masks
)
(defconstant $SQL_SQ_COMPARISON				#x00000001L)
(defconstant $SQL_SQ_EXISTS					#x00000002L)
(defconstant $SQL_SQ_IN						#x00000004L)
(defconstant $SQL_SQ_QUANTIFIED				#x00000008L)
(defconstant $SQL_SQ_CORRELATED_SUBQUERIES	#x00000010L

;; SQL_UNION masks
)
(defconstant $SQL_U_UNION						#x00000001L)
(defconstant $SQL_U_UNION_ALL					#x00000002L

;; SQL_BOOKMARK_PERSISTENCE values
)
(defconstant $SQL_BP_CLOSE				#x00000001L)
(defconstant $SQL_BP_DELETE				#x00000002L)
(defconstant $SQL_BP_DROP 				#x00000004L)
(defconstant $SQL_BP_TRANSACTION			#x00000008L)
(defconstant $SQL_BP_UPDATE				#x00000010L)
(defconstant $SQL_BP_OTHER_HSTMT			#x00000020L)
(defconstant $SQL_BP_SCROLL				#x00000040L

;; SQL_STATIC_SENSITIVITY values
)
(defconstant $SQL_SS_ADDITIONS			#x00000001L)
(defconstant $SQL_SS_DELETIONS			#x00000002L)
(defconstant $SQL_SS_UPDATES				#x00000004L

;; SQL_LOCK_TYPESL masks
)
(defconstant $SQL_LCK_NO_CHANGE			#x00000001L)
(defconstant $SQL_LCK_EXCLUSIVE			#x00000002L)
(defconstant $SQL_LCK_UNLOCK				#x00000004L

;; SQL_POS_OPERATIONS masks
|#

(defconstant $SQL_POS_POSITION 1) ;; #x00000001L
(defconstant $SQL_POS_REFRESH 2)  ;; #x00000002L
(defconstant $SQL_POS_UPDATE 4)   ;; #x00000004L
(defconstant $SQL_POS_DELETE 8)   ;; #x00000008L
(defconstant $SQL_POS_ADD 16)     ;; #x00000010L

#|
;; SQL_QUALIFIER_LOCATION values
)
(defconstant $SQL_QL_START				#x0001L)
(defconstant $SQL_QL_END					#x0002L

;; SQL_OJ_CAPABILITIES values

; #if (ODBCVER >= #x0201))
(defconstant $SQL_OJ_LEFT					#x00000001L)
(defconstant $SQL_OJ_RIGHT				#x00000002L)
(defconstant $SQL_OJ_FULL					#x00000004L)
(defconstant $SQL_OJ_NESTED				#x00000008L)
(defconstant $SQL_OJ_NOT_ORDERED			#x00000010L)
(defconstant $SQL_OJ_INNER				#x00000020L)
(defconstant $SQL_OJ_ALL_COMPARISON_OPS	#x00000040L
;; #endif	;; ODBCVER >= #x0201
;; #endif	;; ODBCVER >= #x0200

;; options for SQLGetStmtOption/SQLSetStmtOption)
(defconstant $SQL_QUERY_TIMEOUT			0)
(defconstant $SQL_MAX_ROWS				1)
(defconstant $SQL_NOSCAN					2)
(defconstant $SQL_MAX_LENGTH				3)
(defconstant $SQL_ASYNC_ENABLE			4)
(defconstant $SQL_BIND_TYPE				5
; #if (ODBCVER >= #x0200))
(defconstant $SQL_CURSOR_TYPE 			6)
(defconstant $SQL_CONCURRENCY 			7)
(defconstant $SQL_KEYSET_SIZE 			8)
(defconstant $SQL_ROWSET_SIZE 			9)
(defconstant $SQL_SIMULATE_CURSOR 		10)
(defconstant $SQL_RETRIEVE_DATA			11)
(defconstant $SQL_USE_BOOKMARKS			12)
(defconstant $SQL_GET_BOOKMARK			13	/*	GetStmtOption Only)
(defconstant $SQL_ROW_NUMBER				14	/*	GetStmtOption Only)
(defconstant $SQL_STMT_OPT_MAX			SQL_ROW_NUMBER
;; #else)
(defconstant $SQL_STMT_OPT_MAX			SQL_BIND_TYPE
;; #endif	;; ODBCVER >= #x0200
)
(defconstant $SQL_STMT_OPT_MIN			SQL_QUERY_TIMEOUT


;; SQL_QUERY_TIMEOUT options)
(defconstant $SQL_QUERY_TIMEOUT_DEFAULT	0UL

;; SQL_MAX_ROWS options)
(defconstant $SQL_MAX_ROWS_DEFAULT		0UL

;; SQL_NOSCAN options)
(defconstant $SQL_NOSCAN_OFF				0UL	/*	1.0 FALSE)
(defconstant $SQL_NOSCAN_ON				1UL	/*	1.0 TRUE)
(defconstant $SQL_NOSCAN_DEFAULT			SQL_NOSCAN_OFF

;; SQL_MAX_LENGTH options)
(defconstant $SQL_MAX_LENGTH_DEFAULT		0UL

;; SQL_ASYNC_ENABLE options)
(defconstant $SQL_ASYNC_ENABLE_OFF		0UL)
(defconstant $SQL_ASYNC_ENABLE_ON			1UL)
(defconstant $SQL_ASYNC_ENABLE_DEFAULT	SQL_ASYNC_ENABLE_OFF

;; SQL_BIND_TYPE options)
(defconstant $SQL_BIND_BY_COLUMN			0UL)
(defconstant $SQL_BIND_TYPE_DEFAULT		SQL_BIND_BY_COLUMN		;; Default value

;; SQL_CONCURRENCY options)
(defconstant $SQL_CONCUR_READ_ONLY		1)
(defconstant $SQL_CONCUR_LOCK 			2)
(defconstant $SQL_CONCUR_ROWVER			3)
(defconstant $SQL_CONCUR_VALUES			4)
(defconstant $SQL_CONCUR_DEFAULT			SQL_CONCUR_READ_ONLY	;; Default value

; #if (ODBCVER >= #x0200)
;; SQL_CURSOR_TYPE options)
(defconstant $SQL_CURSOR_FORWARD_ONLY 	0UL)
(defconstant $SQL_CURSOR_KEYSET_DRIVEN	1UL)
(defconstant $SQL_CURSOR_DYNAMIC			2UL)
(defconstant $SQL_CURSOR_STATIC			3UL)
(defconstant $SQL_CURSOR_TYPE_DEFAULT		SQL_CURSOR_FORWARD_ONLY	;; Default value

;; SQL_ROWSET_SIZE options)
(defconstant $SQL_ROWSET_SIZE_DEFAULT 	1UL

;; SQL_KEYSET_SIZE options)
(defconstant $SQL_KEYSET_SIZE_DEFAULT		0UL

;; SQL_SIMULATE_CURSOR options)
(defconstant $SQL_SC_NON_UNIQUE			0UL)
(defconstant $SQL_SC_TRY_UNIQUE			1UL)
(defconstant $SQL_SC_UNIQUE				2UL

;; SQL_RETRIEVE_DATA options)
(defconstant $SQL_RD_OFF					0UL)
(defconstant $SQL_RD_ON					1UL)
(defconstant $SQL_RD_DEFAULT				SQL_RD_ON

;; SQL_USE_BOOKMARKS options)
(defconstant $SQL_UB_OFF					0UL)
(defconstant $SQL_UB_ON					1UL)
(defconstant $SQL_UB_DEFAULT				SQL_UB_OFF

;; #endif	;; ODBCVER >= #x0200
|#

;; options for SQLSetConnectOption/SQLGetConnectOption)
(defconstant $SQL_ACCESS_MODE 101)
(defconstant $SQL_AUTOCOMMIT 102)
(defconstant $SQL_LOGIN_TIMEOUT 103)
(defconstant $SQL_OPT_TRACE 104)
(defconstant $SQL_OPT_TRACEFILE 105)
(defconstant $SQL_TRANSLATE_DLL 106)
(defconstant $SQL_TRANSLATE_OPTION 107)
(defconstant $SQL_TXN_ISOLATION 108) 
(defconstant $SQL_CURRENT_QUALIFIER 109)
;;#if (ODBCVER >= #x0200))
(defconstant $SQL_ODBC_CURSORS 110)
(defconstant $SQL_QUIET_MODE 111)
(defconstant $SQL_PACKET_SIZE 112)
(defconstant $SQL_CONN_OPT_MAX $SQL_PACKET_SIZE)
;; #else
;(defconstant $SQL_CONN_OPT_MAX $SQL_CURRENT_QUALIFIER)
;; #endif	;; ODBCVER >= #x0200)
(defconstant $SQL_CONNECT_OPT_DRVR_START 1000)

;;#define	SQL_CONN_OPT_MIN			SQL_ACCESS_MODE

;; SQL_ACCESS_MODE options
(defconstant $SQL_MODE_READ_WRITE 0) ; 0UL
(defconstant $SQL_MODE_READ_ONLY 1)  ; 1UL
(defconstant $SQL_MODE_DEFAULT $SQL_MODE_READ_WRITE)

;; SQL_AUTOCOMMIT options)
(defconstant $SQL_AUTOCOMMIT_OFF 0) ;0UL
(defconstant $SQL_AUTOCOMMIT_ON 1) ;1UL
(defconstant $SQL_AUTOCOMMIT_DEFAULT $SQL_AUTOCOMMIT_ON)

;; SQL_LOGIN_TIMEOUT options)
(defconstant $SQL_LOGIN_TIMEOUT_DEFAULT	15) ; 15UL

;; SQL_OPT_TRACE options)
(defconstant $SQL_OPT_TRACE_OFF 0) ; 0UL
(defconstant $SQL_OPT_TRACE_ON 1) ; 1UL
(defconstant $SQL_OPT_TRACE_DEFAULT $SQL_OPT_TRACE_OFF)
; #ifndef SQL_OPT_TRACE_FILE_DEFAULT
; (defconstant $SQL_OPT_TRACE_FILE_DEFAULT	"\\SQL.LOG"
;; #endif

; #if (ODBCVER >= #x0200)
;; SQL_ODBC_CURSORS options)
(defconstant $SQL_CUR_USE_IF_NEEDED 0) ; 0UL
(defconstant $SQL_CUR_USE_ODBC 1) ; 1UL
(defconstant $SQL_CUR_USE_DRIVER 2) ; 2UL
(defconstant $SQL_CUR_DEFAULT $SQL_CUR_USE_DRIVER)
;; #endif	;; ODBCVER >= #x0200

#|
;; Column types and scopes in SQLSpecialColumns. )
(defconstant $SQL_BEST_ROWID 1)
(defconstant $SQL_ROWVER 2)
)
(defconstant $SQL_SCOPE_CURROW			0)
(defconstant $SQL_SCOPE_TRANSACTION		1)
(defconstant $SQL_SCOPE_SESSION			2

;; Defines for SQLSetPos)
(defconstant $SQL_ENTIRE_ROWSET			0
|#

;; Operations in SQLSetPos

(defconstant $SQL_POSITION 0) ;; 1.0 FALSE
(defconstant $SQL_REFRESH 1)  ;; 1.0 TRUE
; #if (ODBCVER >= #x0200))
(defconstant $SQL_UPDATE 2)
(defconstant $SQL_DELETE 3)
(defconstant $SQL_ADD 4)
;; #endif	;; ODBCVER >= #x0200

;; Lock options in SQLSetPos)
(defconstant $SQL_LOCK_NO_CHANGE 0) ;; 1.0 FALSE
(defconstant $SQL_LOCK_EXCLUSIVE 1) ;; 1.0 TRUE
; #if (ODBCVER >= #x0200)
(defconstant $SQL_LOCK_UNLOCK 2)

;; SQLBindParameter extensions
; #if (ODBCVER >= #x0200)
(defconstant $SQL_DEFAULT_PARAM	-5)
(defconstant $SQL_IGNORE -6)
(defconstant $SQL_LEN_DATA_AT_EXEC_OFFSET -100)
;(defconstant $SQL_LEN_DATA_AT_EXEC(length) (-length+SQL_LEN_DATA_AT_EXEC_OFFSET)
; #endif	/* ODBCVER >= #x0200 */

;; Special return values for SQLGetData
(defconstant $SQL_NO_TOTAL -4)

#|
;; Macros for SQLSetPos)
(defconstant $SQL_POSITION_TO(hstmt,irow) SQLSetPos(hstmt,irow,SQL_POSITION,SQL_LOCK_NO_CHANGE))
(defconstant $SQL_LOCK_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_POSITION,fLock))
(defconstant $SQL_REFRESH_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_REFRESH,fLock))
(defconstant $SQL_UPDATE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_UPDATE,SQL_LOCK_NO_CHANGE))
(defconstant $SQL_DELETE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_DELETE,SQL_LOCK_NO_CHANGE))
(defconstant $SQL_ADD_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_ADD,SQL_LOCK_NO_CHANGE)
;; #endif	;; ODBCVER >= #x0200

; #ifndef RC_INVOKED

; #if (ODBCVER >= #x0200)
;;/*	This define is too large for RC)
(defconstant $SQL_ODBC_KEYWORDS \
"ABSOLUTE,ACTION,ADA,ADD,ALL,ALLOCATE,ALTER,AND,ANY,ARE,AS,"\
"ASC,ASSERTION,AT,AUTHORIZATION,AVG,"\
"BEGIN,BETWEEN,BIT,BIT_LENGTH,BOTH,BY,CASCADE,CASCADED,CASE,CAST,CATALOG,"\
"CHAR,CHAR_LENGTH,CHARACTER,CHARACTER_LENGTH,CHECK,CLOSE,COALESCE,"\
"COBOL,COLLATE,COLLATION,COLUMN,COMMIT,CONNECT,CONNECTION,CONSTRAINT,"\
"CONSTRAINTS,CONTINUE,CONVERT,CORRESPONDING,COUNT,CREATE,CROSS,CURRENT,"\
"CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,"\
"DATE,DAY,DEALLOCATE,DEC,DECIMAL,DECLARE,DEFAULT,DEFERRABLE,"\
"DEFERRED,DELETE,DESC,DESCRIBE,DESCRIPTOR,DIAGNOSTICS,DISCONNECT,"\
"DISTINCT,DOMAIN,DOUBLE,DROP,"\
"ELSE,END,END-EXEC,ESCAPE,EXCEPT,EXCEPTION,EXEC,EXECUTE,"\
"EXISTS,EXTERNAL,EXTRACT,"\
"FALSE,FETCH,FIRST,FLOAT,FOR,FOREIGN,FORTRAN,FOUND,FROM,FULL,"\
"GET,GLOBAL,GO,GOTO,GRANT,GROUP,HAVING,HOUR,"\
"IDENTITY,IMMEDIATE,IN,INCLUDE,INDEX,INDICATOR,INITIALLY,INNER,"\
"INPUT,INSENSITIVE,INSERT,INTEGER,INTERSECT,INTERVAL,INTO,IS,ISOLATION,"\
"JOIN,KEY,LANGUAGE,LAST,LEADING,LEFT,LEVEL,LIKE,LOCAL,LOWER,"\
"MATCH,MAX,MIN,MINUTE,MODULE,MONTH,MUMPS,"\
"NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NONE,NOT,NULL,NULLIF,NUMERIC,"\
"OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT,OVERLAPS,"\
"PAD,PARTIAL,PASCAL,PLI,POSITION,PRECISION,PREPARE,PRESERVE,"\
"PRIMARY,PRIOR,PRIVILEGES,PROCEDURE,PUBLIC,"\
"REFERENCES,RELATIVE,RESTRICT,REVOKE,RIGHT,ROLLBACK,ROWS,"\
"SCHEMA,SCROLL,SECOND,SECTION,SELECT,SEQUENCE,SESSION,SESSION_USER,SET,SIZE,"\
"SMALLINT,SOME,SPACE,SQL,SQLCA,SQLCODE,SQLERROR,SQLSTATE,SQLWARNING,"\
"SUBSTRING,SUM,SYSTEM_USER,"\
"TABLE,TEMPORARY,THEN,TIME,TIMESTAMP,TIMEZONE_HOUR,TIMEZONE_MINUTE,"\
"TO,TRAILING,TRANSACTION,TRANSLATE,TRANSLATION,TRIM,TRUE,"\
"UNION,UNIQUE,UNKNOWN,UPDATE,UPPER,USAGE,USER,USING,"\
"VALUE,VALUES,VARCHAR,VARYING,VIEW,WHEN,WHENEVER,WHERE,WITH,WORK,YEAR")
;; #endif	;; ODBCVER >= #x0200
|#

(defconstant $SQL_PARAM_TYPE_UNKNOWN 0)
(defconstant $SQL_PARAM_INPUT 1)
(defconstant $SQL_PARAM_INPUT_OUTPUT 2)
(defconstant $SQL_RESULT_COL 3)
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_PARAM_OUTPUT 4)
(defconstant $SQL_RETURN_VALUE 5)


;; Defines used by both Level 1 and Level 2 functions

;; generally useful constants
(defconstant $SQL_MAX_OPTION_STRING_LENGTH 256)

;; Additional return codes)
(defconstant $SQL_STILL_EXECUTING 2)
(defconstant $SQL_NEED_DATA 99)

;; SQL extended datatypes)
(defconstant $SQL_DATE 9)
(defconstant $SQL_TIME 10)
(defconstant $SQL_TIMESTAMP 11)
(defconstant $SQL_LONGVARCHAR -1)
(defconstant $SQL_BINARY -2)
(defconstant $SQL_VARBINARY -3)
(defconstant $SQL_LONGVARBINARY -4)
(defconstant $SQL_BIGINT -5)
(defconstant $SQL_TINYINT -6)
(defconstant $SQL_BIT -7)

(defconstant $SQL_INTERVAL_YEAR -80)
(defconstant $SQL_INTERVAL_MONTH -81)
(defconstant $SQL_INTERVAL_YEAR_TO_MONTH -82)
(defconstant $SQL_INTERVAL_DAY -83)
(defconstant $SQL_INTERVAL_HOUR -84)
(defconstant $SQL_INTERVAL_MINUTE -85)
(defconstant $SQL_INTERVAL_SECOND -86)
(defconstant $SQL_INTERVAL_DAY_TO_HOUR -87)
(defconstant $SQL_INTERVAL_DAY_TO_MINUTE -88)
(defconstant $SQL_INTERVAL_DAY_TO_SECOND -89)
(defconstant $SQL_INTERVAL_HOUR_TO_MINUTE -90)
(defconstant $SQL_INTERVAL_HOUR_TO_SECOND -91)
(defconstant $SQL_INTERVAL_MINUTE_TO_SECOND -92)
(defconstant $SQL_UNICODE -95)
(defconstant $SQL_TYPE_DRIVER_START $SQL_INTERVAL_YEAR)
(defconstant $SQL_TYPE_DRIVER_END $SQL_UNICODE)


;;#if (ODBCVER >= #x0200))
(defconstant $SQL_SIGNED_OFFSET	-20)
(defconstant $SQL_UNSIGNED_OFFSET -22)
;;; #endif	;; ODBCVER >= #x0200

;; C datatype to SQL datatype mapping
(defconstant $SQL_C_DATE $SQL_DATE)
(defconstant $SQL_C_TIME $SQL_TIME)
(defconstant $SQL_C_TIMESTAMP $SQL_TIMESTAMP)
(defconstant $SQL_C_BINARY $SQL_BINARY)
(defconstant $SQL_C_BIT $SQL_BIT)
(defconstant $SQL_C_TINYINT $SQL_TINYINT)
;;#if (ODBCVER >= #x0200))
(defconstant $SQL_C_SLONG (+ $SQL_C_LONG $SQL_SIGNED_OFFSET)) ;; SIGNED INTEGER
(defconstant $SQL_C_SSHORT (+ $SQL_C_SHORT $SQL_SIGNED_OFFSET)) ;; SIGNED SMALLINT
(defconstant $SQL_C_STINYINT (+ $SQL_TINYINT $SQL_SIGNED_OFFSET)) ;; SIGNED TINYINT
(defconstant $SQL_C_ULONG (+ $SQL_C_LONG $SQL_UNSIGNED_OFFSET)) ;; UNSIGNED INTEGER
(defconstant $SQL_C_USHORT (+ $SQL_C_SHORT $SQL_UNSIGNED_OFFSET)) ;; UNSIGNED SMALLINT
(defconstant $SQL_C_UTINYINT (+ $SQL_TINYINT $SQL_UNSIGNED_OFFSET)) ;;UNSIGNED TINYINT
(defconstant $SQL_C_BOOKMARK $SQL_C_ULONG) ;; BOOKMARK
;; #endif	;; ODBCVER >= #x0200

;; Options for SQLDriverConnect
(defconstant $SQL_DRIVER_NOPROMPT 0)
(defconstant $SQL_DRIVER_COMPLETE 1)
(defconstant $SQL_DRIVER_PROMPT 2)
(defconstant $SQL_DRIVER_COMPLETE_REQUIRED 3)

;; Level 2 Functions

;; SQLExtendedFetch "fFetchType" values
(defconstant $SQL_FETCH_NEXT 1)
(defconstant $SQL_FETCH_FIRST 2)
(defconstant $SQL_FETCH_LAST 3)
(defconstant $SQL_FETCH_PRIOR 4)
(defconstant $SQL_FETCH_ABSOLUTE 5)
(defconstant $SQL_FETCH_RELATIVE 6)
;#if (ODBCVER >= #x0200)
(defconstant $SQL_FETCH_BOOKMARK 8)
;#endif	/* ODBCVER >= #x0200 */

#|
;;/* SQLExtendedFetch "rgfRowStatus" element values */
(defconstant $SQL_ROW_SUCCESS 		0
(defconstant $SQL_ROW_DELETED 		1
(defconstant $SQL_ROW_UPDATED 		2
(defconstant $SQL_ROW_NOROW			3
#if (ODBCVER >= #x0200)
(defconstant $SQL_ROW_ADDED			4
(defconstant $SQL_ROW_ERROR			5
#endif	/* ODBCVER >= #x0200 */

;;/* Defines for SQLForeignKeys (returned in result set) */
(defconstant $SQL_CASCADE 			0
(defconstant $SQL_RESTRICT			1
(defconstant $SQL_SET_NULL			2

;;/* Defines for SQLBindParameter and
			   SQLProcedureColumns (returned in the result set) */
(defconstant $SQL_PARAM_TYPE_UNKNOWN  0
(defconstant $SQL_PARAM_INPUT         1
(defconstant $SQL_PARAM_INPUT_OUTPUT  2
(defconstant $SQL_RESULT_COL          3
#if (ODBCVER >= #x0200)
(defconstant $SQL_PARAM_OUTPUT		4
(defconstant $SQL_RETURN_VALUE		5
#endif	/* ODBCVER >= #x0200 */

;;/* Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter */
(defconstant $SQL_PARAM_TYPE_DEFAULT	$SQL_PARAM_INPUT_OUTPUT)
(defconstant $SQL_SETPARAM_VALUE_MAX	-1)

;;/* Defines for SQLStatistics */
(defconstant $SQL_INDEX_UNIQUE		0
(defconstant $SQL_INDEX_ALL			1

(defconstant $SQL_QUICK				0
(defconstant $SQL_ENSURE				1

;;/* Defines for SQLStatistics (returned in the result set) */
(defconstant $SQL_TABLE_STAT			0
(defconstant $SQL_INDEX_CLUSTERED 	1
(defconstant $SQL_INDEX_HASHED		2
(defconstant $SQL_INDEX_OTHER 		3

#if (ODBCVER >= #x0200)
;;/* Defines for SQLProcedures (returned in the result set) */
(defconstant $SQL_PT_UNKNOWN			0
(defconstant $SQL_PT_PROCEDURE		1
(defconstant $SQL_PT_FUNCTION 		2

;;/* Defines for SQLSpecialColumns (returned in the result set) */
(defconstant $SQL_PC_UNKNOWN			0
(defconstant $SQL_PC_NOT_PSEUDO		1
(defconstant $SQL_PC_PSEUDO			2
#endif	/* ODBCVER >= #x0200 */
|#



;#if (ODBCVER >= 0x0300)
(defconstant  $SQL_ATTR_ACCESS_MODE		$SQL_ACCESS_MODE)
(defconstant  $SQL_ATTR_AUTOCOMMIT			$SQL_AUTOCOMMIT)
(defconstant  $SQL_ATTR_CONNECTION_TIMEOUT	113)
(defconstant  $SQL_ATTR_CURRENT_CATALOG	$SQL_CURRENT_QUALIFIER)
(defconstant  $SQL_ATTR_DISCONNECT_BEHAVIOR	114)
(defconstant  $SQL_ATTR_ENLIST_IN_DTC		1207)
(defconstant  $SQL_ATTR_ENLIST_IN_XA		1208)
(defconstant  $SQL_ATTR_LOGIN_TIMEOUT		$SQL_LOGIN_TIMEOUT)
(defconstant  $SQL_ATTR_ODBC_CURSORS		$SQL_ODBC_CURSORS)
(defconstant  $SQL_ATTR_PACKET_SIZE		$SQL_PACKET_SIZE)
(defconstant  $SQL_ATTR_QUIET_MODE			$SQL_QUIET_MODE)
(defconstant  $SQL_ATTR_TRACE				$SQL_OPT_TRACE)
(defconstant  $SQL_ATTR_TRACEFILE			$SQL_OPT_TRACEFILE)
(defconstant  $SQL_ATTR_TRANSLATE_LIB		$SQL_TRANSLATE_DLL)
(defconstant  $SQL_ATTR_TRANSLATE_OPTION	$SQL_TRANSLATE_OPTION)
(defconstant  $SQL_ATTR_TXN_ISOLATION		$SQL_TXN_ISOLATION)
;#endif  /* ODBCVER >= 0x0300 */

(defconstant $SQL_ATTR_CONNECTION_DEAD	1209)

;/* values for SQL_ATTR_CONNECTION_DEAD */
(defconstant  $SQL_CD_TRUE	1) ;			1L		/* Connection is closed/dead */
(defconstant $SQL_CD_FALSE	0) ;			0L		/* Connection is open/available */


;; only for windows ?
(defconstant $SQL_WCHAR		 	-8)
(defconstant $SQL_WVARCHAR	 	-9)
(defconstant $SQL_WLONGVARCHAR 	-10)
(defconstant $SQL_C_WCHAR	$SQL_WCHAR)

