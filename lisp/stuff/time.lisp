(defpackage :datum
  (:use #:cl)
  (:export #:datum-univ
	   #:gregorian-date
	   ))

(inpackage #:datum)

(defstruct datum-univ
  "Hold the date w.r.t. the referential of the earth."
  julian-day;offset from the 1.3.2001
  day-time)

(let ((parameter-error-format "The parameter %s is %s%s; should be %s")
      (month-too-small " < 1")
      (month-too-big " > 12"))
  (defun gregorian-date (year month day &optional hours minutes seconds timezone)
    (cond
      ((< month 1)
       (error "The month %d < 1; should be 1 <= month <= 12." month))
      ((< 12 month)
       (error "The month %d > 12; should be 1 <= month <= 12." month))
      ((< day 1)
       (error "The day %d < 1; should be"))
      (make-datum-univ (days<-gdate year month day)))))

(defvar *timezone* nil
  "Hold the default timezone. If `nil' then use GMT.")
(defstruct time-zone
  (offset :type symbol))

(defvar *time-zones*
  (let ((zone-table(make-hash-table)))
    ()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun days-in-month (month)
  (elt [0 31 28 31 30 31 30 31 31 30 31 30 31] month))
(defun leap-year-p (year)
  (and (zerop (mod y 400))
       (not (zerop (mod y 100)))
       (zerop (mod y 4))))

(defmacro expand-eval (&rest body)
  (let ((macro-symbol (gensym)))
    `(macrolet
	 ((,macro-symbol ()
	    ,@body))
       (,macro-symbol))))

(let ((cumulated-days (gensym))
      (ref-year 2000)
      (ref-month 3)
      (ref-day 1)
      (ref-dow 3))
  (expand-eval
   (let ((sec/min 60)
	 (min/h 60)
	 (h/day 24))
     `(progn
	(defconst ,cumulated-days
	  ,(apply #'vector
		  (loop with sum = 0
		     for i from 2 below 14
		     collect sum
		     do (incf sum (days-in-month (1+ (mod i 12)))))))
	(defun ,cumulated-days (m)
	  (elt ,cumulated-days m))
	(defun* days<-gdate ((year month day))
	  (let ((y (if (< month ,ref-month)
		       (- year ,(+ ref-year 1))
		       (- year ,ref-year)))
		(m (mod (- month ,ref-month) 12))
		(d (- day ,ref-day)))
	    (+ (* 365 y)
	       (floor y 4)
	       (- (floor y 100))
	       (floor y 400)
	       (,cumulated-days (mod m 12))
	       d)))
	(defun gday-of-week (jd)
	  (+ (mod jd 7) ,ref-dow))
	(defun* sec<-h-m-s ((h m s))
	  (+ (* (+ (* h
		      ,min/h)
		   m)
		,sec/min)
	     s))
	(defun h-m-s<-sec (sec)
	  (let* ((h (floor sec ,(* sec/min min/h)))
		 (rest (- sec (* ,(* sec/min min/h) h)))
		 (m (floor rest ,sec/min))
		 (s (- rest (* ,sec/min m))))
	    (list h m s))))))
  (expand-eval
   (let ((days/400y (days<-gdate (list (+ ref-year 400) ref-month ref-day)))
	 (days/100y (days<-gdate (list (+ ref-year 100) ref-month ref-day)))
	 (days/4y   (days<-gdate (list (+ ref-year   4) ref-month ref-day)))
	 (days/y    (days<-gdate (list (+ ref-year   1) ref-month ref-day))))
     `(defun gdate<-days (jd)
	(let* ((y400 (floor jd ,days/400y))
	       (dy400 (* y400 ,days/400y))
	       (rest (- jd dy400))
	       (y100 (floor rest ,days/100y))
	       (dy100 (* y100 ,days/100y))
	       (rest (- rest dy100))
	       (y4 (floor rest ,days/4y))
	       (dy4 (* y4 ,days/4y))
	       (rest (- rest dy4))
	       (y1 (floor rest ,days/y))
	       (dy1 (+ (* y1 365)
		       (if (= 4 y1) 1 0)))
	       (y (+ (* 400 y400)
		     (* 100 y100)
		     (* 4 y4)
		     y1
		     ,ref-year))
	       (day-in-year (- jd dy400 dy100 dy4 dy1))
	       (p (position-if (lambda (el)
				 (<= el day-in-year))
			       ,cumulated-days
			       :from-end t))
	       (m (1+ (mod (+ p ,ref-month -1) 12)))
	       (d (+ (- day-in-year (,cumulated-days p)) ,ref-day)))
	  (list y m d))))))


(gdate<-days (days<-gdate '(11005 12 5)))
(h-m-s<-sec (sec<-h-m-s 6 34 12.5))
(gday-of-week (days<-gdate '(2000 3 1)))

(days<-gdate (gdate<-days 12345))
(sec<-h-m-s (h-m-s<-sec 8475.2))








