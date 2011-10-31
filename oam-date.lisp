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
  (:use)
  (:nicknames #:date))
(defpackage #:ch.amann-wolowyk.date-system
  (:use #:cl))
(in-package #:ch.amann-wolowyk.date-system)

(defstruct (date::date-time (:conc-name date-time.)
                            (:constructor new-date-time (universal-time))
                            (:copier)
                            (:print-object print-date-time))
  (universal-time 0 :read-only t))

(defun print-date-time (date stream)
  (print-unreadable-object (date stream)
    (date::format-date date :iso8601 :stream stream :full t)))

(let ((cache #+(or clisp openmcl)
             (make-hash-table :weak :value :test #'eql)
             #+sbcl
             (make-hash-table :test #'eql :weakness :value)))
  (oam:fbind ((new-date-time (symbol-function 'new-date-time)))
   (defun date::date-time<-universal-time (universal-time)
     (or (gethash universal-time cache)
         (setf (gethash universal-time cache)
               (new-date-time universal-time))))))
(fmakunbound 'new-date-time)

(defun date::universal-time<-date-time (date-time)
  (date-time.universal-time date-time))

(defvar date::*time-zone* nil
  "Default time zone used for the decoding of DATE-TIME.")

(defun date::date-time (year &optional (month 1) (date 1)
                             (hour 0) (minute 0) (second 0)
                             (time-zone date::*time-zone*))
  "Return an instance of a DATE-TIME. Uses `CL:ENCODE-UNIVERSAL-TIME' and has the same interface."
  (date::date-time<-universal-time (encode-universal-time second minute hour
                                                          date month year
                                                          time-zone)))



(defun date::decode-date-time (date-time &optional (time-zone date::*time-zone*))
  (decode-universal-time (date-time.universal-time date-time)
                         time-zone))

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

(defun date::date-time-< (a b)
  (< (date::universal-time<-date-time a)
     (date::universal-time<-date-time b)))

;;;;*** Add seconds
(defun date::add-seconds (date-time nbr-sec)
  "Return a new instance of date-time representing DATE-TIME shifted by NBR-SEC of seconds."
  (date::date-time<-universal-time
   (+ (date::universal-time<-date-time date-time)
      nbr-sec)))
(defun date::nbr-secs-between (a b)
  (- (date::universal-time<-date-time b)
     (date::universal-time<-date-time a)))
;;;;*** Add days
;;;; TODO: The influence of the timezone is nebulous...
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
        (date::date-time<-universal-time (adjust-time univ-time+1day
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
        (multiple-value-call #'date::date-time
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
(defgeneric date::holiday-p (holiday-calendar date-time &optional time-zone)
  (:method (holiday-calendar date-time &optional time-zone)
    (declare (ignore date-time holiday-calendar time-zone))
    nil)
  (:documentation "Return T (or not nil) if DATE-TIME is a holiday according to the holiday calendar HOLIDAY-CALENDAR and NIL else."))

(defun sat-sun-p (date-time time-zone)
  "Return T if DATE-TIME, in TIME-ZONE, falls on a Saturday or a Sunday."
  (<= 5 (date::get-day-of-week date-time time-zone)))
(defmethod date::holiday-p ((holiday-calendar (eql :sat-sun)) (date-time date::date-time) &optional time-zone)
  "Return T if DATE-TIME is not a Saturday nor a Sunday and NIL else."
  (sat-sun-p date-time time-zone))


(defun date::first-working-day (date-time holiday-p &optional time-zone)
  "Return the first working day following or equal given DATE-TIME according to the holiday predicate HOLIDAY-P, keeping the same time of the day. HOLIDAY-P takes a DATE-TIME and a time-zone and returns T if the corresponding date is a holiday, NIL else."
  (oam:fbind (holiday-p)
    (do ((date date-time (date::add-days date 1 time-zone)))
        ((not (holiday-p date time-zone)) date))))
(defun date::last-working-day (date-time holiday-p &optional time-zone)
  "Return the last working day preceeding or equal given DATE-TIME according to the holiday HOLIDAY-P, keeping the same time of the day. HOLIDAY-P takes a DATE-TIME and a time-zone and returns T if the corresponding date is a holiday, NIL else."
  (oam:fbind (holiday-p)
    (do ((date date-time (date::add-days date -1 time-zone)))
        ((not (holiday-p date time-zone)) date))))

;;;;*** Relative Intervals
;;;; Relative intervals are time intervals whose time span is determined
;;;; only after setting one anchor date. For example the relative interval
;;;; one month determines a time span when giving one date t; the time span
;;;; is then [t, t + 1 Month].
;;;; Here we define the most basic relative intervals: second day month

(defun elem-interval-merge (a b)
  (destructuring-bind ((ua . na) (ub . nb))
      (list a b)
    (cond
      ((zerop na) (unless (zerop nb) (list b)))
      ((zerop nb) (list a))
      ((eq ua ub) (list (cons ua (+ na nb))))
      (t (list a b)))))
(defun l-merge (a b)
  (if a
      (append (butlast a) (elem-interval-merge (car (last a)) b))
      (list b)))

(defun reduce-interval-expression (expr)
  (reduce #'l-merge (rest expr)
          :initial-value (list (first expr))))

(defun merge-interval-expressions (expr1 expr2)
  (reduce-interval-expression (append expr1 expr2)))

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
                                   (loop :for item := (pop expr)
                                      :while (integerp item)
                                      :sum item :into multiplier
                                      :finally (return (etypecase item
                                                         (keyword (list (cons item multiplier)))
                                                         (cons (date::eval-interval-expr multiplier item))))))
                           expr)))))
       (integer (mapcar #'(lambda (x) (cons (car x) (* value (cdr x))))
                        (if (<= 0 value)
                            (date::eval-interval-expr nil expr)
                            (nreverse (date::eval-interval-expr nil expr)))))
       (date::date-time (loop :for (interval . mult) :in (date::eval-interval-expr nil expr)
                           :do (setq value (funcall (ecase interval
                                                      (:s #'date::add-seconds)
                                                      (:d #'date::add-days)
                                                      (:m #'date::add-months))
                                                    value mult)))
                        value)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((shift-fns (make-hash-table)))
    (defun get-shift-fn (interval-unit)
      (gethash interval-unit shift-fns
               (lambda (d m tz)
                 (declare (ignore d m tz))
                 (error "No translate method implemented for relative-interval ~S."
                        interval-unit))))
    (defun (setf get-shift-fn) (value interval-unit)
      (setf (gethash interval-unit shift-fns) value))
    (defmacro date::define-shift-fn (unit-symb (date mult time-zone) &body body)
      (check-type unit-symb symbol)
      (check-type date (and symbol (not (satisfies constantp)))
                  "DATE must be a symbol which is not declared constant.")
      (check-type mult (and symbol (not (satisfies constantp)))
                  "MULT must be a symbol which is not declared constant.")
      (check-type time-zone (and symbol (not (satisfies constantp)))
                  "TIME-ZONE must be a symbol which is not declared constant.")
      `(setf (get-shift-fn ',unit-symb) (lambda (,date ,mult ,time-zone)
                                          ,@body)))
    (defun date::bind-shift-fn (unit-symb fn)
      (check-type unit-symb symbol)
      (setf (gethash unit-symb shift-fns) fn))))

(oam:map*-plist #'date::bind-shift-fn `(date::second ,#'date::add-seconds
                                        date::day ,#'date::add-days
                                        date::month ,#'date::add-months))

(defvar date::holiday-p #'sat-sun-p 
  "Standard holiday predicate function used for business-day related intervals. Must be a function on two args, the first being a date-time, the second the time-zone shift.")

(date::define-shift-fn date::business-day
    (date mult time-zone)
  (oam:fbind ((next-working-day (if (< mult 0)
                                    #'date::last-working-day
                                    #'date::first-working-day)))
    (if (zerop mult)
        date
        (next-working-day (date::add-days (next-working-day date date::holiday-p time-zone)
                                          mult time-zone)
                          date::holiday-p time-zone))))

(defclass date::relative-interval ()
  ((interval-expr :reader interval-expr)))
(defmethod initialize-instance :after ((self date::relative-interval) &key interval-expr &allow-other-keys)
  (setf (slot-value self 'interval-expr) (reduce-interval-expression interval-expr)))

(defmethod oam::translate ((point date::date-time) (vector date::relative-interval)
                           &key time-zone &allow-other-keys)
  (reduce (oam:dlambda (date (unit . mult))
            (funcall (get-shift-fn unit) date mult time-zone))
          (interval-expr vector)
          :initial-value point))


;;;;**** Parse Relative-Interval



(let ((units '(("SEC" date::second 1)
               ("MIN" date::second 60)
               ("HOU" date::second #. (* 60 60))
               ("D" date::day 1)
               ("W" date::day 7)
               ("M" date::month 1)
               ("Q" date::month 3)
               ("S" date::month 6)
               ("Y" date::month 12)
               ("BD" date::business-day 1))))
  (flet ((get-unit (unit)
           (cdr (assoc unit units :test #'string=))))
   (oam:fbind ((parse-unit (oam:make-string-matcher (mapcar #'car units))))
     (defun parse-rel-interval (string)
       (let ((string (string-upcase string))
             (length (length string)))
         (do ((start 0)
              (expr nil))
             ((<= length start) (reduce-interval-expression (nreverse expr)))
           (multiple-value-bind (nbr pos)
               (handler-case
                   (parse-integer string :junk-allowed t :start start)
                 (parse-error ()
                   (values 1 start)))
             (multiple-value-bind (pos1 end unit)
                 (parse-unit string pos)
               (declare (ignore pos1))
               (setq start end)
               (destructuring-bind (unit mult)
                   (get-unit unit)
                 (push (cons unit (* mult (or nbr 1))) expr))))))))))

(defun date::parse-interval (string)
  (make-instance 'date::relative-interval :interval-expr (parse-rel-interval string)))


;;;;*** Format Date
;;;; Generic function to format the date into standardized formats.
(defgeneric date::format-date (date-time format &key time-zone stream
                                         &allow-other-keys)
  (:documentation "Send DATE-TIME in the format specified by FORMAT to the stream STREAM and return nil if STREAM is non nil. The default value of STREAM is nil, in which case FORMAT-DATE returns the formatted date as a string."))

(defmethod date::format-date ((date-time date::date-time)
                              (format (eql :iso8601))
                              &key full time-zone stream)
  (declare (ignore format))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (date::decode-date-time date-time time-zone)
    (declare (ignore day daylight-p))
    (format stream "~4D-~2,'0D-~2,'0D~:[~;T~{~2,'0D~^:~}~{~2,'0@D:~2,'0D~}~]"
            year month date full (list hour minute second) (multiple-value-bind (h m) (truncate zone)
                                                             (list h (abs (truncate (* m 60))))))))
(defmethod date::format-date ((date-time date::date-time)
                              (format (eql :MonYY))
                              &key time-zone stream)
  (declare (ignore format))
  (format stream "~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]~2,'0D"
          (date::get-month date-time time-zone)
          (mod (date::get-year date-time time-zone) 100)))
(defmethod date::format-date ((date-time date::date-time)
                              (format (eql :|ddMonyyyy|))
                              &key time-zone stream)
  (declare (ignore format))
  (format stream "~2,'0D~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]~4,'0D"
          (date::get-date date-time time-zone) (date::get-month date-time time-zone)
          (date::get-year date-time time-zone)))

;;;;*** Parse Date
;;;; Generic function to parse string representations of dates. It is intended
;;;; that date::parse-date be an inverse function of the function date::format-date
;;;; when using the same FORMAT specifier and :stream is nil.
(defgeneric date::parse-date (string format &key time-zone
                                     &allow-other-keys)
  (:documentation "Parse STRING to return a date-time instance corresponding to the date contained in STRING in the format as if it would have been written by date::format-date using the format FORMAT. An error of type date-parse-error is thrown if parsing fails."))

(defmethod date::parse-date ((string string) (format (eql :iso8601))
                             &key time-zone)
  (destructuring-bind (y m d &optional (h 0) (mi 0) (s 0))
      (read-delimited-list #\) (make-string-input-stream (concatenate 'string (oam::string-replace " " '("-" ":" "T") string) ")")))
    (date::date-time y m d h mi s time-zone)))

(defun date::now ()
  "Return a date-time object representing the current time."
  (date::date-time<-universal-time (get-universal-time)))



(let ((package (find-package '#:date)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))