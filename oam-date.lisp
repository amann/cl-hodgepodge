;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;; $Id: oam-date.lisp,v 1.3 2010/09/09 10:26:17 amao Exp $
;;;;
;;;;** The Date Package
;;;;
;;;; The class date::date-time is a simple wrapper for the common-lisp
;;;; universal-time. The date-times are cached in a weak hash-table. The
;;;; variable plain-odbc::*universal-time-to-date-dataype* is set to
;;;; #'date::get-date-time and the variable
;;;; plain-odbc::*date-datatype-to-universal-time* is set to
;;;; #'(lambda (x) (slot-value x 'date::universal-time)) in order to convert
;;;; automatically sql-dates to date::date-time.
;;;;
;;;; Furthermore are implemented some utilities like date::add-seconds,
;;;; date::add-days and date::add-months.
(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.date
  (:nicknames #:date))
(defpackage #:ch.amann-wolowyk.date-system
  (:use))
(in-package #:ch.amann-wolowyk.date-system)

(defclass date::date-time ()
  ((date::universal-time :initform (error "No universal-time given.")
                         :type integer :reader date::universal-time<-date-time
                         :initarg :universal-time))
  (:documentation "Simple wrapper class for Common-Lisp universal-time.")
  (:metaclass oam-clos:cached-class)
  (:cache-fn #.(let ((cache #+(or clisp openmcl)
                          (make-hash-table :weak :value :test #'eql)
                          #+sbcl
                          (make-hash-table :test #'eql :weakness :value)))
               #'(lambda (mode key &optional value)
                   (let ((universal-time (nth-value 1 (get-properties key
                                                                      '(:universal-time)))))
                     (assert (integerp universal-time) ()
                             "Invalid universal-time: ~S" universal-time)
                     (ecase mode
                       (:get (gethash universal-time cache))
                       (:set (setf (gethash universal-time cache) value))))))))

(defun date::date-time<-universal-time (universal-time)
  (make-instance 'date::date-time :universal-time universal-time))
(defun date::make-date-time (year &optional (month 1) (date 1)
                             (hour 0) (minute 0) (second 0)
                             time-zone)
  "Return an instance of a DATE-TIME. Uses `CL:ENCODE-UNIVERSAL-TIME' and has the same interface."
  (make-instance 'date::date-time :universal-time
                 (encode-universal-time second minute hour
                                        date month year time-zone)))



(let ((cache #+(or clisp openmcl)
             (make-hash-table :weak :key :test #'eq)
             #+sbcl
             (make-hash-table :test #'eq :weakness :key)))
  (defun date::decode-date-time (date-time &optional time-zone)
    (if time-zone
        (decode-universal-time (date::universal-time<-date-time date-time) time-zone)
        (apply #'values (or (gethash date-time cache)
                            (setf (gethash date-time cache)
                                  (multiple-value-list (decode-universal-time (date::universal-time<-date-time date-time) time-zone))))))))
(defun date::get-second (date-time &optional time-zone)
  (nth-value 0 (date::decode-date-time date-time time-zone)))
(defun date::get-minute (date-time &optional time-zone)
  (nth-value 1 (date::decode-date-time date-time time-zone)))
(defun date::get-hour (date-time &optional time-zone)
  (nth-value 2 (date::decode-date-time date-time time-zone)))
(defun date::get-date (date-time &optional time-zone)
  (nth-value 3 (date::decode-date-time date-time time-zone)))
(defun date::get-month (date-time &optional time-zone)
  (nth-value 4 (date::decode-date-time date-time time-zone)))
(defun date::get-year (date-time &optional time-zone)
  (nth-value 5 (date::decode-date-time date-time time-zone)))
(defun date::get-day-of-week (date-time &optional time-zone)
  (nth-value 6 (date::decode-date-time date-time time-zone)))
(defun date::get-daylight-p (date-time &optional time-zone)
  (nth-value 7 (date::decode-date-time date-time time-zone)))
(defun date::get-time-zone (date-time &optional time-zone)
  (nth-value 8 (date::decode-date-time date-time time-zone)))

;;;;*** Add seconds
(defun date::add-seconds (date-time nbr-sec)
  "Return a new instance of date-time representing DATE-TIME shifted by NBR-SEC of seconds."
  (make-instance 'date::date-time
                 :universal-time (+ (date::universal-time<-date-time date-time)
                                    nbr-sec)))
;;;;*** Add days
(let ((+number-of-seconds-per-day+ #.(* 24 60 60)))
  (labels ((adjust-time (univ-time ref-univ-time time-zone)
             (+ univ-time
                (multiple-value-bind (ref-s ref-m ref-h)
                    (decode-universal-time ref-univ-time time-zone)
                  (multiple-value-bind (s m h)
                      (decode-universal-time univ-time time-zone)
                    (+ (- ref-s s)
                       (* 60 (- ref-m m))
                       (* #.(* 60 60) (- ref-h h))))))))
    (defun date::add-days (date-time nbr-days &optional time-zone)
      "Return a new instance of date-time representing DATE-TIME shifted by NBR-DAYS of days."
      (let* ((univ-time (date::universal-time<-date-time date-time))
             (univ-time+1day (+ univ-time (* nbr-days
                                             +number-of-seconds-per-day+))))
        (make-instance 'date::date-time
                       :universal-time (adjust-time univ-time+1day
                                                    univ-time time-zone))))
    (defun date::nbr-days-between (a b)
      (/ (- (date::universal-time<-date-time b)
            (date::universal-time<-date-time a))
         +number-of-seconds-per-day+))))

;;;;*** Add Months
;;;; The variable *end-of-month-convention* determines how to procede when
;;;; adding a month and the day is at the end of the month. E.g. does fall
;;;; the 30th of April fall onto the 31st of May when adding one month?
;;;; When *end-of-month-convention* >= 0 then yes, else no.
(defvar date::*end-of-month-convention* 1
  "Indicate the maximal position of a day from the end of the month (beginning with 0) to be preserved when adding months. Standard setting is 1, meaning that the last and the before last days of a month fall to the same relative position. To desactivate this feature, set it to negative or nil.")
(let ((+number-of-months-in-year+ 12))
  (labels
      ((leap-year-p* (year)
         (and (zerop (mod year 4))
              (or (not (zerop (mod year 100)))
                  (zerop (mod year 400)))))
       (days-in-current-month (year month)
         (aref (if (leap-year-p* year)
                   #.#(31 31 29 31 30 31 30 31 31 30 31 30 31)
                   #.#(31 31 28 31 30 31 30 31 31 30 31 30 31))
               month))
       (add-months* (year month day nbr)
         (multiple-value-bind (dy dm)
             (floor (+ month nbr -1) +number-of-months-in-year+)
           (let* ((pos-from-the-end (- (days-in-current-month year month)
                                       day))
                  (year  (+ year dy))
                  (month (+ 1 dm))
                  (day (if (<= pos-from-the-end
                               (or date::*end-of-month-convention* -1))
                           (- (days-in-current-month year month)
                              pos-from-the-end)
                           day)))
             (values year month day)))))
    (defun date::add-months (date-time nbr-months &optional time-zone)
      "Return a new instance of date-time representing DATE-TIME shifted by NBR-MONTHS of months. The behaviour of the days at the end of the month is determined by the variable `*END-OF-MONTH-CONVENTION*'."
      (multiple-value-bind (second minute hour day month year)
          (date::decode-date-time date-time time-zone)
        (multiple-value-call #'date::make-date-time
          (add-months* year month day nbr-months)
                             hour minute second time-zone)))
    (defun date::leap-year-p (date-time &optional time-zone)
      "Return T if DATE-TIME is a leap year, NIL else."
      (leap-year-p* (date::get-year date-time time-zone)))
    (defun date::number-of-days-in-current-month (date-time
                                                  &optional time-zone)
      "Return the number of days of the actual month indicated by DATE-TIME."
      (days-in-current-month (date::get-year date-time time-zone)
                             (date::get-month date-time time-zone)))))

(defun date::last-day (date-time &optional time-zone)
  "Return the date-time instance corresponding to the last day of the current month of DATE-TIME, keeping the same time of the day."
  (date::add-days date-time
                  (- (date::number-of-days-in-current-month date-time time-zone)
                     (date::get-date date-time time-zone)) time-zone))
;;;;*** Holidays
;;;; Holidays are defined by a generic predicate function date::holiday-p
;;;; dispatching on a date-time and a holiday calendar designator or object.
(defgeneric date::holiday-p (date-time holiday-calendar &optional time-zone)
  (:method (date-time holiday-calendar &optional time-zone)
    (declare (ignore date-time holiday-calendar time-zone))
    nil)
  (:documentation "Return T (or not nil) if DATE-TIME is a holiday according to the holiday calendar HOLIDAY-CALENDAR and NIL else."))

(defmethod date::holiday-p ((date-time date::date-time) (holiday-calendar (eql :sat-sun)) &optional time-zone)
  "Return T if DATE-TIME is not a Saturday nor a Sunday and NIL else."
  (<= 5 (date::get-day-of-week date-time time-zone)))

;;;; For doing the loop it would be nice to get first the effective method; e.g.:
;;;; (ccl:compute-effective-method #'holiday-p (ccl:generic-function-method-combination #'holiday-p)
;;;;                               (ccl:compute-applicable-methods #'holiday-p '(date-time holidays)))
(defun date::first-working-day (date-time holidays &optional time-zone)
  "Return the first working day following or equal given DATE-TIME according to the holiday calendar HOLIDAYS, keeping the same time of the day."
  (do ((date date-time (date::add-days date 1 time-zone)))
      ((not (date::holiday-p date holidays time-zone)) date)))
(defun date::last-working-day (date-time holidays &optional time-zone)
  "Return the last working day preceeding or equal given DATE-TIME according to the holiday calendar HOLIDAYS, keeping the same time of the day."
  (do ((date date-time (date::add-days date -1 time-zone)))
      ((not (date::holiday-p date holidays time-zone)) date)))

;;;;*** Relative Intervals
;;;; Relative intervals are time intervals whose time span is determined
;;;; only after setting one anchor date. For example the relative interval
;;;; one month determines a time span when giving one date t; the time span
;;;; is then [t, t + 1 Month].
;;;; Here we define the most basic relative intervals: second day month

(defun date::eval-interval-expr (value expr)
  (etypecase expr
    (null value)
    (cons
     (etypecase value
       (list (let ((item (first expr)))
               (etypecase item
                 (date::date-time (date::eval-interval-expr item (rest expr)))
                 (keyword (date::eval-interval-expr (append value (list (cons item 1))) (rest expr)))
                 (cons (date::eval-interval-expr (date::eval-interval-expr value item) (rest expr)))
                 (integer (date::eval-interval-expr
                           (append value
                                   (loop for item = (pop expr)
                                      while (integerp item)
                                      sum item into multiplier
                                      finally (return (etypecase item
                                                        (keyword (list (cons item multiplier)))
                                                        (cons (date::eval-interval-expr multiplier item))))))
                           expr)))))
       (integer (mapcar #'(lambda (x) (cons (car x) (* value (cdr x))))
                        (if (<= 0 value)
                            (date::eval-interval-expr nil expr)
                            (nreverse (date::eval-interval-expr nil expr)))))
       (date::date-time (loop for (interval . mult) in (date::eval-interval-expr nil expr)
                           do (setq value (funcall (ecase interval
                                                     (:s #'date::add-seconds)
                                                     (:d #'date::add-days)
                                                     (:m #'date::add-months))
                                                   value mult)))
                        value)))))

;;;;*** Format Date
;;;; Generic function to format the date into standardized formats.
(defgeneric date::format-date (date-time format &key time-zone stream
                                         &allow-other-keys)
  (:documentation "Send DATE-TIME in the format specified by FORMAT to the stream STREAM and return nil if STREAM is non nil. The default value of STREAM is nil, in which case FORMAT-DATE returns the formatted date as a string."))

(defmethod date::format-date ((date-time date::date-time)
                              (format (eql :iso8601))
                              &key full time-zone stream)
  (declare (ignore format))
  (multiple-value-bind (second minute hour day month year)
      (date::decode-date-time date-time time-zone)
    (format stream "~4D-~2,'0D-~2,'0D~:[~;T~@{~2,'0D~^:~}~]"
            year month day full hour minute second)))
(defmethod date::format-date ((date-time date::date-time)
                              (format (eql :MonYY))
                              &key time-zone stream)
  (declare (ignore format))
  (format stream "~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]~2,'0D"
          (date::get-month date-time time-zone)
          (mod (date::get-year date-time time-zone) 100)))
;;;;*** Parse Date
;;;; Generic function to parse string representations of dates. It is intended
;;;; that date::parse-date be an inverse function of the function date::format-date
;;;; when using the same FORMAT specifier and :stream is nil.
(defgeneric date::parse-date (string format &key time-zone
                                     &allow-other-keys)
  (:documentation "Parse STRING to return a date-time instance corresponding to the date contained in STRING in the format as if it would have been wrtitten by date::format-date using the format FORMAT. An error of type date-parse-error is thrown if parsing fails."))

(defmethod date::parse-date ((string string) (format (eql :iso8601))
                             &key time-zone)
  (destructuring-bind (y m d &optional (h 0) (mi 0) (s 0))
      (read-delimited-list #\) (make-string-input-stream (concatenate 'string (oam::string-replace "" " " '("-" ":" "T") string) ")")))
    (date:make-date-time y m d h mi s time-zone)))

(defun date::now ()
  "Return a date-time object representing the current time."
  (make-instance 'date::date-time :universal-time (get-universal-time)))



(let ((package (find-package '#:date)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))