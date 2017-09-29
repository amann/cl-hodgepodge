



(let ((*user* :amao))
 (make-market-data-type swap :ccy :term :dcm :cp-freq))

(defun register-ids (stream)
  (let ((*package* (find-package :keyword)))
    (ignore-errors
      (loop :for item := (ignore-errors (read-delimited-list #\$ stream))
         :do (apply #'register item)))))


(defun read-data (stream)
  (let ((*package* (find-package :keyword)))
    (ignore-errors
      (loop :for (source-id value) := (read-delimited-list #\$ stream)
         :do (add-data-point source source-id ref-date value)))))

(defun make-swaprate-cashflows (ref-date source source-id)
  (destructuring-bind (&key value ccy term dcm cp-freq &allow-other-keys)
      (from-db ref-date source source-id)
    (declare (ignore ccy))
    (expand-cashflows 1 value ref-date term cp-freq dcm)))

(defvar *day-count-conventions* (make-hash-table))

(defmacro define-day-count-convention (name (date-a date-b &rest keys) &body body)
  (check-type name keyword)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,name *day-count-conventions*)
           (lambda (,date-a ,date-b
                    &key ,@(delete-if (lambda (key)
                                        (member key '(&key &allow-other-keys)))
                                      keys) &allow-other-keys)
             ,@body))))

(define-day-count-convention :a360 (a b)
  (/ (date:nbr-days-between a b) 360))
(define-day-count-convention :a365 (a b)
  (/ (date:nbr-days-between a b) 365))

(declaim (inline %year-part %get-dcm year-part))
(defun %year-part (dcm a b keys)
  (declare ((function dcm)))
  (apply dcm a b keys))
(defun %get-dcm (dcm)
  (or (gethash dcm *day-count-conventions*)
      (error "Unknown day count convention ~S." dcm)))

(define-compiler-macro year-part (dcm date-a date-b &rest keys &key &allow-other-keys)
  `(funcall %year-part ,(typecase dcm
                                  (keyword (%get-dcm dcm))
                                  (t dcm))
            ,date-a ,date-b ,keys))

(defun year-part (dcm date-a date-b &rest keys &key &allow-other-keys)
  (declare (optimize speed))
  (apply #'%year-part dcm date-a date-b keys))


(defun expand-cashflows (nominal rate start term cp-freq dcm
                         &aux (end (oam::translate start (date:parse-interval term)))
                         (freq (date:parse-interval cp-freq)))
  (declare (ignore dcm))
  (append (loop :for prev := start :then date
             :for date := (oam::translate prev freq)
             :until (date:date-time-< end date)
             :collect (cons date (* nominal rate (year-part :a365 prev date))))
          (list (cons end nominal))))

(defun make-discount-curve (ref-date ccy term-structure interpolation extrapolation)
  (declare (ignore interpolation extrapolation term-structure))
  (lambda (message &rest args)
    (declare (ignore args))
    (ecase message
      (:spot 1)
      (:fwd 1)
      (:ref-date ref-date)
      (:ccy ccy))))


