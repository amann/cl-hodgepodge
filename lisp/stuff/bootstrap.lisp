(defun make-discounting-function (date instruments prices
                                  &key basis interpolation)
  (let ((instruments (sort (cons (cons 0 (lambda (d-fn)
                                           (declare (ignore d-fn)) 0))
                                 (mapcar #'(lambda (instr price)
                                             (cons (funcall basis date
                                                            (get-maturity instr))
                                                   (lambda (d-fn)
                                                     (- (fair-value instr date d-fn)
                                                        price))))
                                         instruments prices))
                           #'< :key #'car)))
      (labels
          ((bootstrap (grid instruments)
             (if (null instruments)
                 (let ((int-fct (make-interpolation grid)))
                   (lambda (t)
                     (funcall int-fct (funcall basis date t))))
                 (let* ((instrument (car instruments))
                        (t (car instrument))
                        (v (cdr instrument))
                        (d (find-zero
                            (lambda (d)
                              (funcall v (bootstrap (cons (cons t d)
                                                          grid)
                                                    nil))) 1)))
                   (bootstrap (cons (cons t d) grid)
                              (cdr instruments))))))
        (bootstrap (list (cons 0 1)) instruments))))



(mapcar #'- '(36 102 40 37 55) '(18 43 20 18 28))


(reduce #'+  '(18 43 20 18 28))