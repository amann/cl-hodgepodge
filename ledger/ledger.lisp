(defpackage "LEDGER"
  (:use :cl))

(in-package "LEDGER")

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

(defun adjust-date (date convention &aux (date (to-date-time date)))
  (case convention
    (:fwd (bdc-forward date))
    (:bwd (bdc-backward date))
    (:mfwd (bdc-modified-forward date))
    (:mbwd (bdc-modified-backward date))
    (t date)))


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

(defgeneric to-interval (object)
  (:method ((object awl:relative-interval))
    object)
  (:method ((object string))
    (awl:parse-interval object))
  (:method ((object symbol))
    (to-interval (symbol-name object))))


(defstruct cycle
  "Represent a periodic cycle (sequence) from, from + step, from + 2 * step, ..., to. If to is nil, the sequence is infinite."
  anchor from step to bdc)

(defun cycle (from step to &key (anchor from) bdc)
  (with-dates (from to)
    (with-intervals (step)
      (make-cycle :anchor anchor  :step step
                  :from (first-after from anchor step)
                  :to (last-before to anchor step)
                  :bdc bdc))))
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
           (bdc (cycle-bdc cycle))
           (current (cycle-from cycle))
           (finish-test (if (forwardp cycle)
                            (lambda () (beforep to current))
                            (lambda () (afterp to current)))))
      (lambda ()
        (if (funcall finish-test)
            (or eof (error 'awl:no-next-item-error :generator cycle))
            (prog1
                (adjust-date current bdc)
              (setq current (awl:translate current step))))))))


(defun beforep (a b)
  (awl:date-< (to-date-time a) (to-date-time b)))
(defun afterp (a b)
  (beforep b a))


(defmacro do-while ((test &optional result) &body body)
  `(do () ((not ,test) ,result)
     ,@body))

(defmacro with-dates ((&rest dates) &body body)
  `(let ,(mapcar (lambda (date)
                   `(,date (to-date-time ,date)))
                 dates)
     ,@body))
(defmacro with-intervals ((&rest intervals) &body body)
  `(let ,(mapcar (lambda (interval)
                   `(,interval (to-interval ,interval)))
                 intervals)
     ,@body))

(defun first-after (date anchor step)
  (with-dates (date anchor)
    (with-intervals (step)
      (multiple-value-bind (quotient rest-days)
          (date-floor date anchor step)
        (let ((candidate (awl:translate anchor (awl:scale quotient step))))
          (if (awl:time-< candidate date)
              (let ((quotient (+ quotient (signum rest-days))))
                (values (awl:translate anchor (awl:scale quotient step)) quotient))
              (values candidate quotient)))))))
(defun last-before (date anchor step)
  (with-dates (date anchor)
    (with-intervals (step)
      (multiple-value-bind (quotient rest-days)
          (date-floor date anchor step)
        (let ((candidate (awl:translate anchor (awl:scale quotient step))))
          (if (awl:time-< date candidate)
              (let ((quotient (- quotient (signum rest-days))))
                (values (awl:translate anchor (awl:scale quotient step)) quotient))
              (values candidate quotient)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+m+)
    (defconstant +m+ (awl:parse-interval "m")))
  (unless (boundp '+d+)
    (defconstant +d+ (awl:parse-interval "d")))
  (unless (boundp '+sec+)
    (defconstant +sec+ (awl:parse-interval "sec"))))

(defun date-floor (date anchor step &optional (prev-quotient 0))
  "Return as multiple values the largest integer `k` the non negative integer `d` and the non negative real `s` such that `date` == `anchor` + `k` * `step` + `d` * day + `s` * second."
  (let* ((next-step (awl:translate anchor step))
         (step-days (awl:nbr-days-between anchor next-step)))
    (multiple-value-bind (nbr-days seconds)
        (awl:nbr-days-between anchor date)
      (multiple-value-bind (quotient rest)
          (floor nbr-days step-days)
        (let* ((candidate (awl:translate anchor (awl:scale quotient step)))
               (quotient (+ quotient prev-quotient)))
          (if (awl:time-= date (awl:translate (awl:translate candidate (awl:scale rest +d+))
                                              (awl:scale seconds +sec+)))
              (values quotient rest seconds)
              (date-floor date candidate step quotient)))))))



(defun add-businessdays (date nbr &optional time-zone (holidays :sat-sun))
  (with-dates (date)
    (if (zerop nbr)
        date
        (let* ((shift (signum nbr))
               (next-day (awl:add-days date shift time-zone)))
          (add-businessdays next-day (if (awl:holiday-p holidays next-day time-zone)
                                         nbr
                                         (- nbr shift))
                            time-zone)))))
(defstruct payment
  date amount from to) 



(defvar *ledger* nil)

#+(or)
(with-ledger ()
  (dcfrom 'postfinance
        (on (awl:map-generators 'bdc-forward (cycle "2018-1-1" "m" "2018-12-1"))
            (give (scale 493.6 'chf)
                  :to 'krankenkasse))))

(defun mingle (predicate key &rest generators)
  (let ((generators (mapcar 'list (mapcar 'awl:make-generator generators))))
    (flet
        ((empty-p (gen)
           (null (cdr gen)))
         (current (gen)
           (apply 'values (cdr gen))))
      (flet ((next (gen)
               (multiple-value-prog1 (current gen)
                 (setf (cdr gen) (ignore-errors (multiple-value-list (funcall (car gen)))))))
             (some-not-empty-p ()
               (setq generators (remove-if #'empty-p generators)))
             (smallest (generators)
               (flet ((minimum (a b)
                        (if (funcall predicate (car a) (car b))
                            a
                            b)))
                 (cdr (reduce #'minimum (mapcar (lambda (gen)
                                                  (cons (funcall key (current gen)) gen))
                                                generators))))))
        (dolist (gen generators)
          (next gen))
        (let (this)
          (setq this (lambda ()
                       (if (some-not-empty-p)
                           (next (smallest generators))
                           (error 'awl:no-next-item-error :generator this)))))))))
