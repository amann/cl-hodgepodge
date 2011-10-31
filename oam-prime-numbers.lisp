(defun factor (n)
  "Return a list of factors of N."
  (when (> n 1)
    (loop :with max-d := (isqrt n)
       :for d = 2 :then (if (evenp d) (+ d 1) (+ d 2))
       :until (< max-d d)
       :do (when (zerop (rem n d))
             (return (cons d (factor (truncate n d)))))
       :finally (return (list n)))))


(defun sieve-of-eratosthenes (maximum)
  (declare (optimize speed)
           (type (unsigned-byte #.(floor (log array-total-size-limit 2))) maximum))
  (let ((composites (make-array (1+ maximum) :element-type 'bit
                                :initial-element 0)))
    (loop :for candidate :from 2 :to maximum
       :when (zerop (bit composites candidate))
       :collect candidate
       :and :do
       (loop :for composite :from (expt candidate 2) :to maximum :by candidate
          :do (setf (bit composites composite) 1)))))

(defun factor (n)
  (when (< 1 n)
    (let ((max-d (isqrt n)))
      (when (zerop (rem n 2))
        (return-from factor (cons 2 (factor (truncate n 2)))))
      (loop :for d :in (rest (sieve-of-eratosthenes max-d)) ;:from 3 :upto max-d :by 2
         :when (zerop (rem n d))
         :do (return-from factor (cons d (factor (truncate n d)))))
      (list n))))



(let ((prime-numbers (sieve-of-eratosthenes (- (floor (expt 2 (floor (log array-total-size-limit 2)))) 2))))
  (defun factor (n)
    (loop :with m fixnum := n :and factors := nil :and max-p := (isqrt n)
       :for p fixnum :in prime-numbers
       :until (< max-p p)
       :do (loop (multiple-value-bind (mul rem)
                     (truncate (the fixnum m) (the fixnum p))
                   (declare (type fixnum mul rem))
                   (if (zerop rem)
                       (progn (push p factors)
                              (setq m (the fixnum mul)))
                       (return))))
       :finally (return (nreverse factors)))))

(defun factor (n)
  (declare (optimize speed))
  (labels ((rec-factor (result spf n)
             (declare (integer spf n))
             (if (< 1 (the integer n))
                 (if (and (eql 2 (the integer spf))
                          (incf (the integer spf))
                          (evenp(the integer n)))
                     (rec-factor (cons 2 result) 2 (truncate (the integer n) 2))
                     (multiple-value-bind (l d)
                         (loop :with max-d integer := max-d 
                            :for d integer :in (rest prime-numbers)
                                        ;:from (the integer spf) :upto (the integer max-d) :by 2
                            :do (multiple-value-bind (l rem)
                                    (truncate (the integer n) (the integer d))
                                  (when (zerop rem)
                                    (return (values (the integer l) (the integer d)))))
                            :finally (return (values 1 (the integer n))))
                       (declare (integer l d))
                       (rec-factor (cons (the integer d) result) (the integer d) (the integer l))))
                 (nreverse result))))
    (rec-factor nil 2 n)))

(rem 3 4)