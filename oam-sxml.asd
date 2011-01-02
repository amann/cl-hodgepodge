(in-package #:cl)

(setq *features* (remove :ch.amann-wolowyk.oam-sxml-system-test *features*))

(asdf:defsystem oam-sxml
  :name "OAM-SXML"
  :author "me"
  :components
  ((:file "oam-sxml"))
  :depends-on ("s-xml" "oam-util")
  :serial t)

  