(defsystem "mdb"
  :description "Market Data Base"
  :version "0.1"
  :author "Olivier Amann <olivier.amann@swisslife.ch>"
  :components ((:module "mdb"
                :serial t
                :components ((:file "package")
                             (:file "in-memory-db")))))
