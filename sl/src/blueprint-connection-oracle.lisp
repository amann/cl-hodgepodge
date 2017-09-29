(defpackage #:ch.swisslife.blueprint-oracle
  (:use)
  (:nicknames #:bp-oracle)
  (:import-from #:ch.swisslife.blueprint-connection-system
                "GET-CONNECTION-SPEC" "GET-ALL-KNOWN-LEVELS" "GET-LEVEL")
  (:export "GET-CONNECTION-SPEC" "GET-ALL-KNOWN-LEVELS" "GET-LEVEL" "CONNECT" "CLOSE"))

(in-package #:ch.swisslife.blueprint-connection-system)

(define-connection-funs #:bp-oracle
    (lambda (&key dbq uid pwd &allow-other-keys)
      (clsql-sys:connect (list dbq uid pwd)
                         :database-type :oracle
                         :if-exists :old
                         :make-default nil))
  (lambda (connection)
    (clsql-sys:disconnect :database connection)))

(defmethod %host-name ((connection clsql-oracle:oracle-database))
  (first (oam:split-string "/" (clsql-sys:database-name connection))))

