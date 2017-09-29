
(defun count-consecutive-items (vector &key (test #'equal))
  (let ((nothing '#:nothing))
    (loop :with counter := 0
       :with current-item := nothing
       :with result = nil
       :for item :across vector
       :do (if (funcall test item current-item)
               (incf counter)
               (progn
                 (push (list counter current-item) result)
                 (setq current-item item counter 1)))
       :finally (return (rest (nreverse (push (list counter current-item) result)))))))

(defun transform-to-string (item-count)
  (format nil "~:{~A~A~}" item-count))

(defun sum-digits (integer)
  (let ((integer (abs integer)) digit)
    (loop :until (zerop integer)
       :do (multiple-value-setq (integer digit) (floor integer 10))
       :sum digit)))

(defun compute-age (init)
  (dotimes (i 9 (sum-digits (parse-integer init)))
    (setq init (transform-to-string (count-consecutive-items init)))))

(compute-age "1")
