(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:date
    (:use :cl :cl-user)))
(in-package #:date)
(defstruct date
  (juliandays 0 :type fixnum :read-only t)
  (time 0 :type (real 0 86400) :read-only t))




(defconstant +leap-year+ #.(apply 'vector (loop for i below 400
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
(defconstant +number-days-in-400-years+ (do* ((y 0 (1+ y))
                                              (d 366 (+ d 365 (if (aref +leap-year+ y) 1 0))))
                                             ((>= y 399) d)))
(defconstant +greg-y-m-d<-jd+ (make-array `(,+number-days-in-400-years+) :element-type '(vector (unsigned-byte 9) 3)
                                          :initial-contents (loop with y = 0 and m = 0 and d = 0
							       for i below +number-days-in-400-years+
							       collect (make-array '(3) :element-type '(unsigned-byte 9)
										   :initial-contents (list y m d))
							       do (multiple-value-bind (add-month day) (floor (1+ d) (aref (if (aref +leap-year+ y)
                                                                                                                               +days-in-month-leap+
                                                                                                                               +days-in-month+)
                                                                                                                           m))
								    (multiple-value-bind (add-year month) (floor (+ m add-month) 12)
								      (setq d day
									    m month
									    y (+ y add-year)))))))
(defun leap-year-p (year)
  (aref +leap-year+ (mod year 400)))

(defun jd<-greg-m400-y-m-d (m400 y m d)
  (+ (* m400 +number-days-in-400-years+)
     (aref +days<-year+ y)
     (aref (if (aref +leap-year+ y)
	       +cumulated-days-leap+
	       +cumulated-days+)
	   m)
     d))
(defun valid-greg-date-p (y m d)
  (declare (integer y m d))
  (and (< -1 m 13)
       (< -1 d (aref (if (aref +leap-year+ (mod y 400))
                         +days-in-month-leap+
                         +days-in-month+)
                     m))))
(defun greg-m400-y-m-d<-jd (jd)
  (multiple-value-bind (m400 jd) (floor jd +number-days-in-400-years+)
    (cons m400 (aref +greg-y-m-d<-jd+ jd))))
