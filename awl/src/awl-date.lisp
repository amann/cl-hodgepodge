;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
;;;;** The Date Package
;;;;
;;;; The class awl::date-time is a simple wrapper for the common-lisp
;;;; universal-time. The date-times are cached in a weak hash-table. 
;;;;
;;;; Furthermore are implemented some utilities like awl::add-seconds,
;;;; awl::add-days and awl::add-months.
(in-package "CL-USER")
(defpackage "CH.AMANN-WOLOWYK.DATE-SYSTEM"
  (:use "COMMON-LISP"))
(in-package "CH.AMANN-WOLOWYK.DATE-SYSTEM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
    "Number of seconds between 1/1/1900 and 1/1/1970."))

(declaim (ftype (function () (values integer rational))))
(defun awl::get-universal-time-of-day ()
  "Return as primary value the number of complete seconds elapsed now since 1/1/1900 (cl's universal-time) and as secondary value the fractions of the seconds."
  #+openmcl
  (rlet ((tv :timeval))
        (ccl::gettimeofday tv)
        (values (+ (the (unsigned-byte 32) (pref tv :timeval.tv_sec)) +unix-epoch+)
                (/ (the fixnum (pref tv :timeval.tv_usec)) 1000)))
  #+sbcl
  (multiple-value-bind (seconds microseconds)
      (sb-ext:get-time-of-day)
    (declare (type (unsigned-byte 32) seconds)
             (type fixnum microseconds))
    (values (+ seconds +unix-epoch+) (/ microseconds 1000000)))
  #+cmu
  (multiple-value-bind (successp seconds microseconds) (unix:unix-gettimeofday)
    (declare (type (unsigned-byte 32) seconds)
             (type fixnum microseconds))
    (assert successp () "unix:unix-gettimeofday reported failure.")
    (values (+ seconds +unix-epoch+) (/ microseconds 1000)))
  #-(or openmcl sbcl cmu)
  (values (get-universal-time) 0))


(defstruct (awl::date-time (:conc-name date-time.)
                            (:constructor new-date-time (universal-time))
                            (:copier)
                            (:print-object print-date-time))
  (universal-time 0d0 :read-only t
                  :type (double-float #.(coerce (- (encode-universal-time 0 0 0 15 10 (+ 1582 400) 0)
                                                   (encode-universal-time 0 0 0 1 1 (+ 1900 400) 0))
                                                'double-float))))

(defun print-date-time (date stream)
  (print-unreadable-object (date stream)
    (awl::format-date date :iso8601 :stream stream :full t)))

(let ((cache #+(or clisp openmcl)
             (make-hash-table :test #'equal :weak :value)
             #+sbcl
             (make-hash-table :test #'eql :weakness :value))
      (new-date-time (symbol-function 'new-date-time)))
  (defun awl::date-time<-universal-time (universal-time)
    (let* ((key (coerce universal-time 'double-float)))
      (or (gethash key cache)
          (setf (gethash key cache) (funcall new-date-time key))))))
(fmakunbound 'new-date-time)

(defun awl::universal-time<-date-time (date-time)
  (floor (date-time.universal-time date-time)))

(defmethod make-load-form ((date awl::date-time) &optional env)
  (declare (ignore env))
  (multiple-value-bind (universal-time fract)
      (awl::universal-time<-date-time date)
    `(awl::date-time<-universal-time ,(+ universal-time fract))))

(defvar awl::*time-zone* nil
  "Default time zone used for the decoding of DATE-TIME.")
(defgeneric time-zone-offset (time-zone date-time)
  (:documentation "Return the offset in hours from GMT as of DATE-TIME and as coded in the object TIME-ZONE or nil if the null time-zone is used. As secondary value, return the part of the offset due to day light saving in seconds or nil if no information about is available; as third value return the time-zone-offset in seconds or NIL in the case of the null time-zone. The null time-zone means: use the system's timezone information; this may be buggy on some implementations.")
  (:method ((time-zone null) (universal-time real))
    (values nil
            (if (plusp universal-time)
                (let ((universal-time (floor universal-time)))
                  (multiple-value-bind (sec min hrs day mth yrs dow d-p tzo)
                      (decode-universal-time universal-time)
                    (declare (ignore dow d-p))
                    (- universal-time
                       (encode-universal-time sec min hrs day mth yrs tzo))))
                0)))
  (:method ((time-zone null) (date-time awl::date-time))
    (time-zone-offset time-zone (awl::universal-time<-date-time date-time)))
  (:method ((time-zone rational) date-time) (values time-zone nil (truncate (* time-zone 3600)))))



(defun awl::date-time (year &optional (month 1) (date 1)
                        (hours 0) (minutes 0) (seconds 0d0)
                        (time-zone awl::*time-zone*))
  "Return an instance of a DATE-TIME. Uses `CL:ENCODE-UNIVERSAL-TIME' and has the same interface except that seconds may be a real."
  (declare (type (integer 1582 #.most-positive-fixnum) year)
           (optimize speed safety))
  (let ((seconds (coerce seconds 'double-float)))
    (declare (type (double-float 0d0 61d0) seconds))
    (multiple-value-bind (offset year)
        (floor (the fixnum (- year 1501)) 400)
      (multiple-value-bind (seconds fractional-seconds)
          (floor seconds)
        (declare (type (mod 61) seconds)
                 (type (double-float 0d0 1d0)))
        (awl::date-time<-universal-time (+ (* (1- offset) #.(coerce (encode-universal-time 0 0 0 1 1 (+ 1900 400) 0) 'double-float))
                                            (encode-universal-time seconds minutes hours
                                                                   date month (+ year 1901)
                                                                   time-zone)
                                            fractional-seconds))))))
(defun awl::decode-date-time (date-time &optional (time-zone awl::*time-zone*))
  "Return the decoded date as multiple values nanoseconds seconds minutes hours date month year day-of-week daylight-offset time-zone-offset."
  (declare (optimize speed))
  (multiple-value-bind (time-zone-offset daylight-offset)
      (time-zone-offset time-zone date-time)
    (multiple-value-bind (offset universal-time)
        (floor (date-time.universal-time date-time) #. (encode-universal-time 0 0 0 1 1 (+ 1900 400) 0))
      (declare (type fixnum offset))
      (multiple-value-bind (universal-time fractional-seconds)
          (floor universal-time)
        (multiple-value-bind (seconds minutes hours date month year day-of-week daylightp time-zone-offset)
            (decode-universal-time universal-time time-zone-offset)
          (declare (type fixnum year))
          (values (+ seconds fractional-seconds) minutes hours date month (+ year (* offset 400)) day-of-week
                  (or daylight-offset daylightp)
                  time-zone-offset
                  time-zone))))))

(declaim (inline awl::get-seconds awl::get-minutes awl::get-hours awl::get-date awl::get-month
                 awl::get-year awl::get-day-of-week awl::get-daylight-p awl::get-time-zone-offset))
(defun awl::get-seconds (date-time &optional time-zone)
  (nth-value 0 (awl::decode-date-time date-time time-zone)))
(defun awl::get-minutes (date-time &optional time-zone)
  (nth-value 1 (awl::decode-date-time date-time time-zone)))
(defun awl::get-hours (date-time &optional time-zone)
  (nth-value 2 (awl::decode-date-time date-time time-zone)))
(defun awl::get-date (date-time &optional time-zone)
  (nth-value 3 (awl::decode-date-time date-time time-zone)))
(defun awl::get-month (date-time &optional time-zone)
  (nth-value 4 (awl::decode-date-time date-time time-zone)))
(defun awl::get-year (date-time &optional time-zone)
  (nth-value 5 (awl::decode-date-time date-time time-zone)))
(defun awl::get-day-of-week (date-time &optional time-zone)
  (nth-value 6 (awl::decode-date-time date-time time-zone)))
(defun awl::get-daylight-p (date-time &optional time-zone)
  (nth-value 7 (awl::decode-date-time date-time time-zone)))
(defun awl::get-time-zone-offset (date-time &optional time-zone)
  (nth-value 8 (awl::decode-date-time date-time time-zone)))

(defun awl::date-time-< (a b)
  (< (date-time.universal-time a)
     (date-time.universal-time b)))

;;;;*** Add seconds
(defun awl::add-seconds (date-time nbr-sec &optional time-zone)
  "Return a new instance of date-time representing DATE-TIME shifted by NBR-SEC of seconds."
  (declare (ignore time-zone))
  (awl::date-time<-universal-time
   (+ (date-time.universal-time date-time)
      nbr-sec)))
(defun awl::nbr-secs-between (a b)
  (floor (- (date-time.universal-time b)
            (date-time.universal-time a))))
;;;;*** Add days
(let ((+number-of-seconds-per-day+ #.(* 24 60 60)))
  (defun awl::add-days (date-time nbr-days &optional time-zone)
    "Return a new instance of date-time representing DATE-TIME shifted by NBR-DAYS of days."
    (let* ((univ-time (date-time.universal-time date-time))
           (univ-time+n-days (+ univ-time (* nbr-days +number-of-seconds-per-day+)))
           (daylight-offset (- (or (nth-value 1 (time-zone-offset time-zone univ-time+n-days)) 0d0)
                               (or (nth-value 1 (time-zone-offset time-zone univ-time)) 0d0))))
      (awl::date-time<-universal-time (+ univ-time+n-days daylight-offset))))
  (defun awl::nbr-days-between (a b &optional time-zone)
    (floor (+ (awl::nbr-secs-between a b)
              (- (or (nth-value 1 (time-zone-offset time-zone a)) 0d0)
                 (or (nth-value 1 (time-zone-offset time-zone b)) 0d0)))
           +number-of-seconds-per-day+)))

;;;;*** Add Months
;;;; The variable *end-of-month-convention* determines how to procede when
;;;; adding a month and the day is at the end of the month. E.g. does fall
;;;; the 30th of April fall onto the 31st of May when adding one month?
;;;; When *end-of-month-convention* >= 0 then yes, else no.
(defvar awl::*end-of-month-convention* 1
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
                               (or awl::*end-of-month-convention* -1))
                           (- (days-in-current-month year month)
                              pos-from-the-end)
                           day)))
             (values year month day)))))
    (defun awl::add-months (date-time nbr-months &optional time-zone)
      "Return a new instance of date-time representing DATE-TIME shifted by NBR-MONTHS of months. The behaviour of the days at the end of the month is determined by the variable `*END-OF-MONTH-CONVENTION*'."
      (multiple-value-bind (seconds minutes hours day month year)
          (awl::decode-date-time date-time time-zone)
        (multiple-value-call #'awl::date-time
          (add-months* year month day nbr-months)
                             hours minutes seconds time-zone)))
    (defun awl::leap-year-p (date-time &optional time-zone)
      "Return T if DATE-TIME is a leap year, NIL else."
      (leap-year-p* (awl::get-year date-time time-zone)))
    (defun awl::number-of-days-in-current-month (date-time
                                                  &optional time-zone)
      "Return the number of days of the actual month indicated by DATE-TIME."
      (days-in-current-month (awl::get-year date-time time-zone)
                             (awl::get-month date-time time-zone)))))

(defun awl::last-day (date-time &optional time-zone)
  "Return the date-time instance corresponding to the last day of the current month of DATE-TIME, keeping the same time of the day."
  (awl::add-days date-time
                  (- (awl::number-of-days-in-current-month date-time time-zone)
                     (awl::get-date date-time time-zone)) time-zone))
;;;;*** Holidays
;;;; Holidays are defined by a generic predicate function awl::holiday-p
;;;; dispatching on a date-time and a holiday calendar designator or object.
(defgeneric awl::holiday-p (holiday-calendar date-time &optional time-zone)
  (:method (holiday-calendar date-time &optional time-zone)
    (declare (ignore date-time holiday-calendar time-zone))
    nil)
  (:documentation "Return T (or not nil) if DATE-TIME is a holiday according to the holiday calendar HOLIDAY-CALENDAR and NIL else."))

(defun awl::sat-sun-p (date-time time-zone)
  "Return T if DATE-TIME, in TIME-ZONE, falls on a Saturday or a Sunday."
  (<= 5 (awl::get-day-of-week date-time time-zone)))
(defmethod awl::holiday-p ((holiday-calendar (eql :sat-sun)) (date-time awl::date-time) &optional time-zone)
  "Return T if DATE-TIME is not a Saturday nor a Sunday and NIL else."
  (awl::sat-sun-p date-time time-zone))


(defun awl::first-working-day (date-time holiday-p &optional time-zone)
  "Return the first working day following or equal given DATE-TIME according to the holiday predicate HOLIDAY-P, keeping the same time of the day. HOLIDAY-P takes a DATE-TIME and a time-zone and returns T if the corresponding date is a holiday, NIL else."
  (awl::fbind (holiday-p)
    (do ((date date-time (awl::add-days date 1 time-zone)))
        ((not (holiday-p date time-zone)) date))))
(defun awl::last-working-day (date-time holiday-p &optional time-zone)
  "Return the last working day preceeding or equal given DATE-TIME according to the holiday HOLIDAY-P, keeping the same time of the day. HOLIDAY-P takes a DATE-TIME and a time-zone and returns T if the corresponding date is a holiday, NIL else."
  (awl::fbind (holiday-p)
    (do ((date date-time (awl::add-days date -1 time-zone)))
        ((not (holiday-p date time-zone)) date))))

;;;;*** Relative Intervals
;;;; Relative intervals are time intervals whose time span is determined
;;;; only after setting one anchor date. For example the relative interval
;;;; one month determines a time span when giving one date t; the time span
;;;; is then [t, t + 1 Month].
;;;; Here we define the most basic relative intervals: seconds day month

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
  (when expr
    (reduce #'l-merge (rest expr)
            :initial-value (list (first expr)))))

(defun merge-interval-expressions (expr1 expr2)
  (reduce-interval-expression (append expr1 expr2)))

(defun awl::eval-interval-expr (value expr)
  (etypecase expr
    (null value)
    (cons
     (etypecase value
       (list (let ((item (first expr)))
               (etypecase item
                 (awl::date-time (awl::eval-interval-expr item (rest expr)))
                 (keyword (awl::eval-interval-expr (append value (list (cons item 1))) (rest expr)))
                 (cons (awl::eval-interval-expr (awl::eval-interval-expr value item) (rest expr)))
                 (integer (awl::eval-interval-expr
                           (append value
                                   (loop :for item := (pop expr)
                                      :while (integerp item)
                                      :sum item :into multiplier
                                      :finally (return (etypecase item
                                                         (keyword (list (cons item multiplier)))
                                                         (cons (awl::eval-interval-expr multiplier item))))))
                           expr)))))
       (integer (mapcar #'(lambda (x) (cons (car x) (* value (cdr x))))
                        (if (<= 0 value)
                            (awl::eval-interval-expr nil expr)
                            (nreverse (awl::eval-interval-expr nil expr)))))
       (awl::date-time (loop :for (interval . mult) :in (awl::eval-interval-expr nil expr)
                           :do (setq value (funcall (ecase interval
                                                      (:s #'awl::add-seconds)
                                                      (:d #'awl::add-days)
                                                      (:m #'awl::add-months))
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
    (defmacro awl::define-shift-fn (unit-symb (date mult time-zone) &body body)
      (check-type unit-symb symbol)
      (check-type date (and symbol (not (satisfies constantp)))
                  "DATE must be a symbol which is not declared constant.")
      (check-type mult (and symbol (not (satisfies constantp)))
                  "MULT must be a symbol which is not declared constant.")
      (check-type time-zone (and symbol (not (satisfies constantp)))
                  "TIME-ZONE must be a symbol which is not declared constant.")
      `(setf (get-shift-fn ',unit-symb) (lambda (,date ,mult ,time-zone)
                                          ,@body)))
    (defun awl::bind-shift-fn (unit-symb fn)
      (check-type unit-symb symbol)
      (setf (gethash unit-symb shift-fns) fn))))

(awl::map*-plist #'awl::bind-shift-fn `(awl::seconds ,#'awl::add-seconds
                                        awl::day ,#'awl::add-days
                                        awl::month ,#'awl::add-months))

(defvar awl::holiday-p #'awl::sat-sun-p 
  "Standard holiday predicate function used for business-day related intervals. Must be a function on two args, the first being a date-time, the seconds the time-zone shift.")

(awl::define-shift-fn awl::business-day
    (date mult time-zone)
  (awl::fbind ((next-working-day (if (< mult 0)
                                    #'awl::last-working-day
                                    #'awl::first-working-day)))
    (if (zerop mult)
        date
        (next-working-day (awl::add-days (next-working-day date awl::holiday-p time-zone)
                                          mult time-zone)
                          awl::holiday-p time-zone))))

(defclass awl::relative-interval ()
  ((interval-expr :reader interval-expr)))
(defmethod initialize-instance :after ((self awl::relative-interval) &key interval-expr &allow-other-keys)
  (setf (slot-value self 'interval-expr) (reduce-interval-expression interval-expr)))
(defmethod print-object ((object awl::relative-interval) stream)
  (print-unreadable-object (object stream)
    (awl::print-interval object stream)))

(defgeneric awl::scale (factor vector &key &allow-other-keys))
(defmethod awl::scale ((factor integer) (vector awl::relative-interval) &key)
  (let ((interval-expr (mapcar (awl::dlambda ((unit . mult))
                                 (cons unit (* factor mult)))
                               (interval-expr vector))))
    (make-instance 'awl::relative-interval
                   :interval-expr (if (< factor 0)
                                      (nreverse interval-expr)
                                      interval-expr))))

(defgeneric awl::translate (point vector &key &allow-other-keys))
(defmethod awl::translate ((point awl::date-time) (vector awl::relative-interval)
                           &key time-zone &allow-other-keys)
  (reduce (awl::dlambda (date (unit . mult))
            (funcall (get-shift-fn unit) date mult time-zone))
          (interval-expr vector)
          :initial-value point))


;;;;**** Parse Relative-Interval



(let ((units '(("SEC" awl::seconds 1)
               ("MIN" awl::seconds 60)
               ("HOU" awl::seconds #. (* 60 60))
               ("D" awl::day 1)
               ("W" awl::day 7)
               ("M" awl::month 1)
               ("Q" awl::month 3)
               ("S" awl::month 6)
               ("Y" awl::month 12)
               ("BD" awl::business-day 1))))
  (flet ((get-unit (unit)
           (cdr (assoc unit units :test #'string=)))
         (div (elem-unit)
           (destructuring-bind (unit . mult)
               elem-unit
             (let* ((signum (signum mult))
                    (mult (* signum mult)))
               (let ((unit (find unit units :from-end t
                                            :test (lambda (unit name-unit-mult)
                                                    (and (eql unit (second name-unit-mult))
                                                         (<= (third name-unit-mult) mult))))))
                 (multiple-value-bind (mult rest)
                     (floor mult (third unit))
                   (values (format nil "~:[~;-~]~[~;~:;~:*~D~]~A" (< signum 0) mult (first unit))
                           (unless (zerop rest)
                             (cons (second unit) (* signum rest))))))))))
    (awl::fbind ((parse-unit (awl::make-string-matcher (mapcar #'car units))))
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
                (unless unit
                  (error "Invalid relative interval string ~S." (subseq string pos)))
                (setq start end)
                (destructuring-bind (unit mult)
                    (get-unit unit)
                  (unless (and nbr (zerop nbr))
                    (push (cons unit (* mult (or nbr 1))) expr))))))))
      (defun print-rel-interval (interval-expression stream)
        (when interval-expression
          (destructuring-bind (elem-unit &rest interval-expression)
              interval-expression
            (multiple-value-bind (div rest)
                (div elem-unit)
              (princ div stream)
              (print-rel-interval (if rest
                                      (cons rest interval-expression)
                                      interval-expression)
                                  stream))))))))

(defun awl::parse-interval (string)
  (make-instance 'awl::relative-interval :interval-expr (parse-rel-interval string)))
(defun awl::print-interval (interval stream)
  (let ((interval-expression (interval-expr interval)))
    (if interval-expression
        (print-rel-interval interval-expression stream)
        (princ "0d" stream))))

;;;;*** Format Date
;;;; Generic function to format the date into standardized formats.
(defgeneric awl::format-date (date-time format &key time-zone stream
                                                 &allow-other-keys)
  (:documentation "Send DATE-TIME in the format specified by FORMAT to the stream STREAM and return nil if STREAM is non nil. The default value of STREAM is nil, in which case FORMAT-DATE returns the formatted date as a string."))

(defmethod awl::format-date ((date-time awl::date-time)
                             (format (eql :iso8601))
                             &key full time-zone stream)
  (declare (ignore format))
  (multiple-value-bind (seconds minutes hours date month year day daylight-p zone)
      (awl::decode-date-time date-time time-zone)
    (declare (ignore day))
    (format stream "~4D-~2,'0D-~2,'0D~:[~;T~{~2,'0D:~}~6,3,0,,'0F~{~2,'0@D:~2,'0D~}~:[~;S~]~]"
            year month date full (list hours minutes) seconds (multiple-value-bind (h m) (truncate (- zone))
                                                                (list h (abs (truncate (* m 60)))))
            (and daylight-p (not (zerop daylight-p))))))
(defmethod awl::format-date ((date-time awl::date-time)
                             (format (eql :yymm))
                             &key time-zone stream)
  (declare (ignore format))
  (format stream "~2,'0D~2,'0D" (awl::get-year date-time time-zone) (awl::get-month date-time time-zone)))
(defmethod awl::format-date ((date-time awl::date-time)
                             (format (eql :MonYY))
                             &key time-zone stream)
  (declare (ignore format))
  (format stream "~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]~2,'0D"
          (awl::get-month date-time time-zone)
          (mod (awl::get-year date-time time-zone) 100)))
(defmethod awl::format-date ((date-time awl::date-time)
                             (format (eql :|ddMonyyyy|))
                             &key time-zone stream)
  (declare (ignore format))
  (format stream "~2,'0D~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~]~4,'0D"
          (awl::get-date date-time time-zone) (awl::get-month date-time time-zone)
          (awl::get-year date-time time-zone)))

;;;;*** Parse Date
;;;; Generic function to parse string representations of dates. It is intended
;;;; that awl::parse-date be an inverse function of the function awl::format-date
;;;; when using the same FORMAT specifier and :stream is nil.
(defgeneric awl::parse-date (string format &key time-zone
                                             &allow-other-keys)
  (:documentation "Parse STRING to return a date-time instance corresponding to the date contained in STRING in the format as if it would have been written by awl::format-date using the format FORMAT. An error of type date-parse-error is thrown if parsing fails."))
(defmethod awl::parse-date ((string awl::date-time) format &key &allow-other-keys)
  "In some cases it might be useful to put a date-time object into the parser."
  string)
(defmethod awl::parse-date ((string string) (format (eql :iso8601))
                            &key time-zone &allow-other-keys)
  "Try to parse a date written in the ISO8601 format."
  (destructuring-bind (y m d &optional (h 0) (mi 0) (s 0))
      (read-delimited-list #\) (make-string-input-stream (concatenate 'string (awl::string-replace " " '("-" ":" "T") string) ")")))
    (awl::date-time y m d h mi s time-zone)))
(defmethod awl::parse-date ((string string) (format (eql :yymm))
                            &key &allow-other-keys)
  (awl::date-time (+ 2000 (parse-integer string :start 0 :end 2)) (parse-integer string :start 2 :end 4)))
(defun awl::now ()
  "Return a date-time object representing the current time."
  (multiple-value-bind (seconds fraction)
      (awl::get-universal-time-of-day)
    (awl::date-time<-universal-time (+ seconds fraction))))

;;;;** Some utilities
;;;;*** Comparison of Dates
(declaim (inline awl::time-< awl::time-<= awl::time-> awl::time->= awl::time-= awl::time-/= awl::date-/=))
(defun awl::time-< (a &rest rest)
  "Return T if A and all remaing date in REST form a strictly increasing sequence and NIL else."
  (every (lambda (b)
           (prog1 (awl::date-time-< a b)
             (setq a b)))
         rest))
(defun awl::time-<= (a &rest rest)
  "Return T if A and all remaing date in REST form an increasing sequence and NIL else."
  (every (lambda (b)
           (prog1 (not (awl::date-time-< b a))
             (setq a b)))
         rest))
(defun awl::time-> (a &rest rest)
  "Return T if A and all remaing date in REST form a strictly decreasing sequence and NIL else."
  (every (lambda (b)
           (prog1 (awl::date-time-< b a)
             (setq a b)))
         rest))
(defun awl::time->= (a &rest rest)
  "Return T if A and all remaing date in REST form a decreasing sequence and NIL else."
  (every (lambda (b)
           (prog1 (not (awl::date-time-< a b))
             (setq a b)))
         rest))
(defun awl::time-= (a &rest rest)
  "Return T if A and all remaing dates in REST are two by two equal and NIL else."
  (every (lambda (b)
           (eq b a))
         rest))
(defun awl::time-/= (a &rest rest)
  "Return T if some of A or the remaing dates in REST are not two by two equal and NIL else."
  (not (apply 'awl::time-= a rest)))
(defun awl::date-= (a b &optional time-zone)
  "Return T if A and B are in the same day and NIL else."
  (and (= (awl::get-date a time-zone)
          (awl::get-date b time-zone))
       (= (awl::get-month a time-zone)
          (awl::get-month b time-zone))
       (= (awl::get-year a time-zone)
          (awl::get-year b time-zone))))
(defun awl::date-/= (a b &optional time-zone)
  "Return the result of (not (date:date-= A B))."
  (not (awl::date-= a b time-zone)))
(defun awl::date-< (a b &optional time-zone)
  "Return T if A is not after the day before the day of B, and NIL else."
  (and (awl::date-time-< a b)
       (awl::date-/= a b time-zone)))
(defun awl::date-> (a b &optional time-zone)
  "Return T if A is not before the day after the day of B, and NIL else."
  (awl::date-< b a time-zone))
(defun awl::date-<= (a b &optional time-zone)
  "Return T if A is before the day of B, and NIL else."
  (not (awl::date-> a b time-zone)))
(defun awl::date->= (a b &optional time-zone)
  "Return T if A is after the day of B, and NIL else."
  (not (awl::date-< a b time-zone)))

(defun awl::time-max (a &rest rest)
  (reduce (lambda (r x)
            (if (awl::date-time-< r x)
                x
                r))
          rest :initial-value a))
(defun awl::time-min (a &rest rest)
  (reduce (lambda (r x)
            (if (awl::date-time-< x r)
                x
                r))
          rest :initial-value a))


(do-symbols (sym '#:awl)
  (export sym '#:awl))
