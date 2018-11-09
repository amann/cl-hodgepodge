(defpackage "LEDGER"
  (:use :cl))

(in-package "LEDGER")

(defvar *ledger* nil)

'(ledger (on "2018-11-1"
            (give 'postfinance
                  'krankenkasse
                  (scale 493.6
                         (item 'chf)))
            ))
(defvar *holidays* (make-hash-table))

(defun add-holidays (&rest holidays)
  (dolist (day holidays)
    (setf (gethash (awl:parse-date day :iso8601) *holidays*) t)))
(defun rem-holidays (&rest holidays)
  (dolist (day holidays)
    (remhash (awl:parse-date day :iso8601) *holidays*)))

(defun holiday-p (date-time &optional time-zone
                  &aux (date-time (awl:parse-date date-time :iso8601)))
  (or (awl:sat-sun-p date-time time-zone)
      (gethash date-time *holidays*)))

(defun bdc-forward (date-time)
  (awl:first-working-day date-time #'holiday-p))
(defun bdc-backward (date-time)
  (awl:last-working-day date-time #'holiday-p))
(defun bdc-modified-forward (date-time)
  (let ((bdc-forward (bdc-forward date-time)))
    (if (/= (awl:get-month date-time) (awl:get-month bdc-forward))
        (bdc-backward date-time)
        bdc-forward)))
(defun bdc-modified-backward (date-time)
  (let ((bdc-backward (bdc-backward date-time)))
    (if (/= (awl:get-month date-time) (awl:get-month bdc-backward))
        (bdc-forward date-time)
        bdc-backward)))

(defun at-business-date (date convention &aux (date (to-date-time date)))
  (ecase convention
    (:fwd (bdc-forward date))
    (:bwd (bdc-backward date))
    (:mfwd (bdc-modified-forward date))
    (:mbwd (bdc-modified-backward date))))


(defstruct date
  year month day)
(defgeneric to-date (date-time)
  (:method ((date-time date))
    date-time)
  (:method ((date-time string))
    (to-date (awl:parse-date date-time :iso8601)))
  (:method ((date-time awl:date-time))
    (make-date :year (awl:get-year date-time)
               :month (awl:get-month date-time)
               :day (awl:get-date date-time))))
(defgeneric to-date-time (date)
  (:method ((date awl:date-time))
    date)
  (:method ((date date))
    (awl:date-time (date-year date) (date-month date) (date-day date)))
  (:method ((date string))
    (awl:parse-date date :iso8601)))



(defstruct cycle
  "Represent a periodic cycle (sequence) from, from + step, from + 2 * step, ..., to. If to is nil, the sequence is infinite."
  anchor from step to)

(defun cycle (from step to &key (anchor from))
  (make-cycle :anchor anchor  :step step
              :from (first-after anchor step from)
              :to (last-before anchor step to)))
(defgeneric forwardp (cycle)
  (:method ((cycle cycle))
    (not (beforep (cycle-to cycle) (cycle-from cycle)))))
(defgeneric backwardp (cycle)
  (:method ((cycle cycle))
    (not (afterp (cycle-to cycle) (cycle-from cycle)))))

(defmethod awl:make-generator ((object cycle) &key eof step from &allow-other-keys)
  (let ((cycle (cycle (or from (cycle-from object)) (or step (cycle-step object)) (cycle-to object)
                      :anchor (cycle-anchor object))))
    (let* ((step (cycle-step cycle))
           (to (cycle-to cycle))
           (current (cycle-from cycle))
           (finish-test (if (forwardp cycle)
                            (lambda () (beforep to current))
                            (lambda () (afterp to current)))))
      (lambda ()
        (if (funcall finish-test)
            (or eof (error 'awl:no-next-item-error :generator cycle))
            (prog1
                current
              (setq current (awl:translate current step))))))))


(defun beforep (a b)
  (awl:date-< (to-date-time a) (to-date-time b)))
(defun afterp (a b)
  (beforep b a))


(defmacro do-while ((test &optional result) &body body)
  `(do () ((not ,test) ,result)
     ,@body))

(defun first-after (anchor step date)
  (let* ((next-step (awl:translate (to-date-time anchor) step))
         (fwdp (not (beforep next-step anchor)))
         (bwdp (not (afterp next-step anchor))))
    (if (or (and fwdp (not (beforep anchor date)))
            (and bwdp (not (afterp anchor date)))
            (and fwdp bwdp))
        anchor
        (first-after next-step step date))))
(defun last-before (anchor step date)
  (first-after anchor (awl:scale -1 step) date))

(defun add-businessdays (date nbr &optional time-zone (holidays :sat-sun))
  (if (zerop nbr)
      date
      (let* ((shift (signum nbr))
             (next-day (awl:add-days date shift time-zone)))
        (add-businessdays next-day (if (awl:holiday-p holidays next-day time-zone)
                                       nbr
                                       (- nbr shift))
                          time-zone))))
(defstruct payment
  date amount from to) 
