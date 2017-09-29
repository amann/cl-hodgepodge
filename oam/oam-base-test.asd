(in-package #:cl-user)

(pushnew :ch.amann-wolowyk.oam-base-test *features*)

(asdf:defsystem oam-base-test
  :name "OAM-BASE-TEST"
  :author "me"
  :components
  ((:file "oam-base"))
  :serial t)