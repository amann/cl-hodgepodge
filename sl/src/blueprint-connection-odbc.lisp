(defpackage #:ch.swisslife.blueprint-odbc
  (:use)
  (:nicknames #:bp-odbc)
  (:import-from #:ch.swisslife.blueprint-connection-system
                "GET-CONNECTION-SPEC" "GET-ALL-KNOWN-LEVELS" "GET-LEVEL")
  (:export "GET-CONNECTION-SPEC" "GET-ALL-KNOWN-LEVELS" "GET-LEVEL" "CONNECT" "CLOSE"))

(in-package #:ch.swisslife.blueprint-connection-system)

(define-connection-funs #:bp-odbc
    (lambda (&key driver dbq uid pwd &allow-other-keys)
      (plain-odbc:connect-generic :driver driver :dbq dbq
                                  :uid uid :pwd pwd))
  (lambda (connection)
    (plain-odbc:close-connection connection)))

(defmethod %host-name ((connection plain-odbc::odbc-connection))
  (plain-odbc::server-name connection))