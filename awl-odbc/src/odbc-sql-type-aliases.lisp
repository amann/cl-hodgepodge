;;;; -*- Mode: Lisp;outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.ODBC-SYSTEM-SQL")


(defconstant string-ptr :pointer)

;;;;* Features

;;;; odbc2 compile for odbc version < 3.0
;;;; x86-64 compile for 64 bit
;;;; sb-unicode compile for unicode support

;;;;* SQL Types
;;;; API declaration data type

;;;; Types which are just aliases of basic types are defined using
;;;; defconstant and referenced in macros like with-foreign-object
;;;; by reader evaluation #.

(defconstant $::SQLCHAR        :uchar)
#-odbc2
(progn
  (defconstant $::SQLSCHAR     :char)
  (defconstant $::SQLDATE      :uchar)
  (defconstant $::SQLDECIMAL   :uchar)
  (defconstant $::SQLDOUBLE    :double)
  (defconstant $::SQLFLOAT     :double))
(defconstant $::SQLINTEGER     :long)
(defconstant $::SQLUINTEGER    :ulong)
#-odbc2
(defconstant $::SQLNUMERIC     :uchar)

(defconstant $::SQLPOINTER     :pointer)

#-odbc2
(defconstant $::SQLREAL        :float)

(defconstant $::SQLSMALLINT    :short)
(defconstant $::SQLUSMALLINT   :ushort)

#-odbc2
(progn
  (defconstant $::SQLTIME      :uchar)
  (defconstant $::SQLTIMESTAMP :uchar)
  (defconstant $::SQLVARCHAR   :uchar))

#+x86-64
(progn
  (defconstant $::SQLLEN        :int64)
  (defconstant $::SQLULEN       :uint64)
  (defconstant $::SQLSETPOSIROW :uint64))
#-x86-64
(progn
  (defconstant $::SQLLEN        $::SQLINTEGER)
  (defconstant $::SQLULEN       $::SQLUINTEGER)
  (defconstant $::SQLSETPOSIROW $::SQLUSMALLINT))

#+win32 ;; For backward compatibility
(progn
  (defconstant $::SQLROWCOUNT   $::SQLULEN)
  (defconstant $::SQLROWSETSIZE $::SQLULEN)
  (defconstant $::SQLTRANSID    $::SQLULEN)
  (defconstant $::SQLROWOFFSET  $::SQLLEN))


;;;;** Function return type
(defconstant $::SQLRETURN      $::SQLSMALLINT)

;;;;** Generic data structures
#-odbc2
(progn
  #+(or win32 x86-64)
  (defconstant $::SQLHANDLE    :pointer)
  #-(or win32 x86-64)
  (defconstant $::SQLHANDLE    $::SQLINTEGER)
  (defconstant $::SQLHENV      $::SQLHANDLE)
  (defconstant $::SQLHDBC      $::SQLHANDLE)
  (defconstant $::SQLHSTMT     $::SQLHANDLE)
  (defconstant $::SQLHDESC     $::SQLHANDLE))
#+odbc2
(progn
  #+(or win32 x86-64)
  (progn
    (defconstant $::SQLHENV    :pointer)
    (defconstant $::SQLHDBC    :pointer)
    (defconstant $::SQLHSTMT   :pointer))
  #-(or win32 x86-64)
  (progn
    (defconstant $::SQLHENV    $::SQLINTEGER)
    (defconstant $::SQLHDBC    $::SQLINTEGER)
    (defconstant $::SQLHSTMT   $::SQLINTEGER)))

;;;;** $::SQL portable types for C
(defconstant $::UCHAR          :uchar)
(defconstant $::SCHAR          :char)
(defconstant $::SQLSCHAR       $::SCHAR)
(defconstant $::SDWORD         :long)
(defconstant $::SWORD          :short)
(defconstant $::UDWORD         :ulong)
(defconstant $::UWORD          :ushort)

#+x86-64
(defconstant $::SQLUINTEGER    $::UDWORD)

(defconstant $::SLONG          :long)
(defconstant $::SSHORT         :short)
(defconstant $::ULONG          :ulong)
(defconstant $::USHORT         :ushort)
(defconstant $::SDOUBLE        :double)
(defconstant $::LDOUBLE        :double)
(defconstant $::SFLOAT         :float)

(defconstant $::PTR            :pointer)
(defconstant $::HENV           :pointer)
(defconstant $::HDBC           :pointer)
(defconstant $::HSTMT          :pointer)

(defconstant $::RETCODE        :short)


(defconstant $::SQLHWND        :pointer)
#+(or win32 os2)
(defconstant $::HWND           $::SQLHWND)
#+unix
(defconstant $::Widget         $::SQLHWND)

(defconstant $::SQLPOINTER     $::SQLHWND)

(defparameter $::*SQLSMALLINT   `(:pointer ,$::SQLSMALLINT))
(defparameter $::*SQLUSMALLINT  `(:pointer ,$::SQLUSMALLINT))
(defparameter $::*SQLINTEGER    `(:pointer ,$::SQLINTEGER))
(defparameter $::*SQLLEN        `(:pointer ,$::SQLLEN))
(defparameter $::*SQLULEN       `(:pointer ,$::SQLULEN))
(defparameter $::*SQLCHAR       `(:pointer ,$::SQLCHAR))

(defparameter $::*SQLHANDLE     `(:pointer ,$::SQLHANDLE))
(defparameter $::*SQLHENV       `(:pointer ,$::SQLHENV))
(defparameter $::*SQLHDBC       `(:pointer ,$::SQLHDBC))
(defparameter $::*SQLHSTMT      `(:pointer ,$::SQLHSTMT))

(defconstant $::*SQLPOINTER     :pointer)


(do-symbols (sym "$") (export sym "$"))

