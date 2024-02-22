(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:date
    (:documentation "The structure `date' contains an abstract absolute date in form of a slot `juliandays', which is a fixnum representing the offset in days from the \"Epoch\" date, and a slot `time', which is a (real 0 86400) representing the offset in seconds from midnight; no leap seconds are foreseen. The helper functions (LEAP-YEAR-P DATE-P MAKE-DATE VALID-GREG-DATE-P COPY-DATE DATE-JULIANDAYS JD<-GREG-M400-Y-M-D DATE-TIME GREG-M400-Y-M-D<-JD) are built in such a way that the Epoch is thought to be the date corresponding in the Gregorian Calendar to the 1st of January of a year which is a multiple of 400 plus 1, at 00:00 GMT; for common use the year 2001 will probably be best. The Gregorian dates used by these functions are then interpreted as offsets in years, months and days from the Epoch. For performance reason the years are sometimes split into the integers m400 and y satisfying (values m400 y) == (floor years).")
    (:use :cl :cl-user)
    (:export #:date
	     )))
(defpackage #:date-user (:use :date))
(in-package #:date)


(defconstant +seconds-in-day+ #.(* 24 60 60))

(defstruct date
  "An absolute date."
  (juliandays 0 :type fixnum :read-only t)
  (time 0 :type (real 0 #.(* 24 60 60)) :read-only t))




(defconstant +leap-year+ #.(apply 'vector (loop for i from 1 upto 400
					     collect (or (and (zerop (mod i 4))
							      (not (zerop (mod i 100))))
							 (zerop (mod i 400))))))
(defconstant +days-in-month+ #.#(31 28 31 30 31 30 31 31 30 31 30 31))
(defconstant +days-in-month-leap+ (let ((seq (copy-seq +days-in-month+)))
				 (setf (aref seq 1) 29)
				 seq))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((compute-days (days-in-month)
	   (apply 'vector (loop with days = 0
				for i below 12
				collect days
				do (incf days (aref days-in-month i))))))
    (defconstant +cumulated-days+ (compute-days +days-in-month+))
    (defconstant +cumulated-days-leap+ (compute-days +days-in-month-leap+))))

(defconstant +days<-year+ (apply 'vector (loop for leap-p across +leap-year+
					       with sum = 0
					       collect sum
					       do (incf sum (if leap-p
								366
								365)))))
(defconstant +number-days-in-400-years+ (+ (aref +days<-year+ 399) 366))
(defconstant +greg-y-m-d<-jd+ (make-array `(,+number-days-in-400-years+) :element-type '(unsigned-byte 18)
					  :initial-contents (loop with y = 0 and m = 0 and d = 0
								  for i below +number-days-in-400-years+
								  collect (encode-date-cache y m d)
								  do (multiple-value-bind (add-month day) (floor (1+ d) (aref (if (aref +leap-year+ y)
																  +days-in-month-leap+
																  +days-in-month+)
															      m))
								       (multiple-value-bind (add-year month) (floor (+ m add-month) 12)
									 (setq d day
									       m month
									       y (+ y add-year)))))))


(+ (* 1967 (expt 2 9))
   (* 09 (expt 2 5))
   23)
(defun encode-date-cache (y m d)
  (+ (* y #.(expt 2 9))
     (* m #.(expt 2 5))
     d))
(defun decode-date-cache (a)
  (multiple-value-bind (year rest) (floor a #.(expt 2 9))
    (multiple-value-bind (month day) (floor rest #.(expt 2 5))
      (list year month day))))

(defun leap-year-p (year)
  "Return t if `year' is a leap year, nil else."
  (aref +leap-year+ (mod year 400)))

(defun jd<-greg-400y-y-m-d (m400 y m d)
  "Return the julianday corresponding to the date given as offset from the Epoch in Gregorian years, months and days; More precisely the date corresponding to Epoch plus (+ (* 400 `m400') `y') years, plus `m' months and plus `d' days. The integers `m400', `y' and `d' can be arbitrary; the integer `m' must be (mod 12)"
  (+ (* m400 +number-days-in-400-years+)
     (aref +days<-year+ y)
     (aref (if (aref +leap-year+ y)
	       +cumulated-days-leap+
	       +cumulated-days+)
	   m)
     d))
(defun valid-greg-date-p (y m d)
  "Return t if `y' is an integer, `m' of type (mod 12)  and `d' of type (mod dm) with dm the number of days in month m+1 of year `y'."
  (declare (integer y m d))
  (and (typep m '(mod 12))
       (< -1 d (aref (if (aref +leap-year+ (mod y 400))
			+days-in-month-leap+
			+days-in-month+)
		    m))))
(defun greg-400y-y-m-d<-jd (jd)
  "Return the Gregorian date corresponding to the julianday `jd' in from of a list whose first value is ... "
  (multiple-value-bind (400y jd) (floor jd +number-days-in-400-years+)
    (cons 400y (decode-date-cache (aref +greg-y-m-d<-jd+ jd)))))
(defvar *end-of-month-convention* 2
  "Indicates at how many days from the end of month a date position relatively to the end of the month should be preserved. The default is 2, meaning that the last day of a month stays the last day of the new month after the adding of months as well as the before last day stays the before last day. If set negative, no end of month convention is used.")
(defun add-greg-days-to-jd (jd d)
  (+ jd d))
(defun days-in-month (y m)
  (if (leap-year-p y)
      (aref +days-in-month-leap+ m)
      (aref +days-in-month+ m)))
(defun add-greg-month-to-jd (jd nm)
  "Return a new date corresponding to `date' plus `n' Gregorian months."
  (destructuring-bind (400y y-m-d) (greg-400y-y-m-d<-jd jd)
    (let* ((y (aref y-m-d 0))
	   (m (aref y-m-d 1))
	   (d (aref y-m-d 2))
	   (pos-from-the-end (- (days-in-month y m) d)))
      (multiple-value-bind (ny m) (floor (+ nm m) 12)
	(multiple-value-bind (m400 y) (floor (+ ny y) 400)
	  (jd<-greg-400y-y-m-d (+ 400y m400) y m (if (<= pos-from-the-end *end-of-month-convention*)
						     (- (days-in-month y m) pos-from-the-end)
						     d)))))))
(defun add-greg-year-to-jd (jd ny)
  "Return a new date corresponding to `date' plus `n' Gregorian years."
  (destructuring-bind (400y y-m-d) (greg-400y-y-m-d<-jd jd)
    (let* ((y (aref y-m-d 0))
	   (m (aref y-m-d 1))
	   (d (aref y-m-d 2)))
      (multiple-value-bind (m400 y) (floor (+ ny y) 400)
	(jd<-greg-400y-y-m-d (+ 400y m400) y m d)))))


(defun day-of-week (jd)
  (mod jd 7))
(defun valid-time-p (h m s)
  (and (typep h '(mod 24))
       (typep m '(mod 60))
       (typep s '(real 0 60))))
(defun seconds<-h-m-s (h m s)
  (+ (* 60 (+ (* 60 h) m)) s))

(defun make-date-from-greg (year &optional (month 1) (day 1) (hours 0) (minutes 0) (seconds 0.0))
  (let ((m (1- month))
	(d (1- day)))
    (assert (valid-greg-date-p year m d))
    (assert (valid-time-p hours minutes seconds))
    (multiple-value-bind (400y y) (floor (- year 2001) 400)
      (make-date :juliandays (jd<-greg-400y-y-m-d 400y y m d)
		 :time (seconds<-h-m-s hours minutes seconds)))))




#| Intervals: We consider two kinds of intervals:
1. relative
2. absolute.

The relative intervals are a duration of time which are given independently of a time instance; the length of their duration however may depend on the time instance on which they are applied to define a shift of time. Typical relative intervals are days, years but also weeks, months or hours, minutes etc. Many of them depend on a definition of a calendar. In this implementation we consider only the Gregorian Calendar without any historical considerations (as the date of adoption etc.).

The absolute intervals are the duration between two instants of time (dates).

|#

