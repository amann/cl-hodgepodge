(oam:define-project-package #:ch.amann-wolowyk.oam-bookkeeping #:book)
(in-package #:ch.amann-wolowyk.oam-bookkeeping-system)


(defclass book::record ()
  ((date :initarg :date)))

(defclass book::accounting-record (book::record)
  ((from :initarg :from)
   (to :initarg :to)
   (item :initarg :item)))




(defclass book::trade ()
  ((in :initarg :in)
   (out :initarg :out)))
