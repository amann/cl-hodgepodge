;;;;1006
(defparameter scen-nbr '(4165
                         5682
                         4420
                         8849
                         996
                         8360
                         4015
                         5469
                         8279
                         3328
                         295
                         4386
                         7430
                         3123
                         8554
                         755
                         8852
                         7589
                         964
                         1851
                         6882
                         825
                         5565
                         4577
                         1375
                         7086
                         1478
                         8737
                         125
                         5358
                         1596
                         7041
                         488
                         2477
                         3458
                         2205
                         2800
                         1956
                         4417
                         6487
                         8438
                         7761
                         1140
                         2618
                         6317
                         264
                         1719
                         1252
                         7145
                         2488
                         166
                         9992
                         7111
                         8170
                         169
                         9798
                         8428
                         873
                         786
                         2831
                         1458
                         789
                         2434
                         7592
                         5957
                         8649
                         2737
                         314
                         1358
                         5375
                         364
                         4525
                         81
                         4735
                         1196
                         1729
                         8581
                         7675
                         2665
                         674
                         5869
                         9909
                         1246
                         8444
                         3896
                         2615
                         9001
                         9291
                         7679
                         494
                         1259
                         3388
                         1631
                         5481
                         7782
                         3345
                         2976
                         7154
                         9104
                         4032))
;;; 1009
(defparameter scennum '(4137
                        1446
                        7139
                        1193
                        1662
                        8579
                        7929
                        4783
                        3394
                        1536
                        7985
                        6749
                        6686
                        7032
                        1493
                        3415
                        2253
                        3894
                        1611
                        3143
                        8216
                        4863
                        5946
                        5028
                        3815
                        9315
                        7039
                        9808
                        373
                        4474
                        4152
                        9411
                        8819
                        7815
                        6693
                        3633
                        3797
                        4173
                        5380
                        4507
                        7014
                        2897
                        6476
                        8313
                        9333
                        9605
                        4272
                        8966
                        5877
                        4134
                        6700
                        3680
                        1521
                        520
                        9295
                        9372
                        8688
                        4068
                        9014
                        3777
                        8280
                        6048
                        3104
                        8617
                        5849
                        3629
                        2132
                        9145
                        1442
                        53
                        4138
                        1741
                        5989
                        1624
                        7026
                        5335
                        8074
                        4028
                        1162
                        8242
                        9458
                        8757
                        829
                        2459
                        3094
                        5851
                        7525
                        6595
                        3297
                        9588
                        1268
                        1734
                        3520
                        6133
                        2055
                        8959
                        1646
                        6593
                        1175
                        3026))




#+ (or) (with-open-file (out #p "c:/temp/market-data-SLSH-1006.csv" :direction :output)
  (format out "~{~A~^,~}~%" (sort scen-nbr #'<))
  (with-open-file (in #p"c:/temp/Copie de market data SH 3006 SIM analysis.csv" :direction :input)
    (let ((*readtable* (copy-readtable nil))
          (scen-numbers (make-hash-table))
          (max-scen (apply #'max scen-nbr)))
      (dolist (nbr scen-nbr)
        (setf (gethash nbr scen-numbers) t))
      (set-syntax-from-char #\, #\Space)
      (labels ((fetch-row ()
                 (do* ((row (read-line in nil))
                       (line (when row (make-string-input-stream row)))
                       (item (read line nil) (read line nil))
                       (index 1 (1+ index))
                       (sep ""))
                      ((or (< max-scen index) (null item)) row)
                   (when (gethash index scen-numbers)
                     (format out "~A~F" sep item)
                     (setq sep ",")))))
        (loop
           while (fetch-row)
           do (format out "~%"))))))



(defun extract-columns (column-nbrs row)
  (let ((row (if (typep row 'cons)
                 (let* ((n (length row))
                        (vct (make-array `(,n))))
                   (loop for i below n
                      do (setf (elt vct i) (pop row)))
                   vct)
                 row)))
    (when row
      (map 'list
           #'(lambda (i)
               (elt row (1- i)))
           column-nbrs))))


(defun fetch-row (input-stream &key (delimiter #\~) (eof-error-p t) eof-value)
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char delimiter #\Space)
    (let ((line (read-line input-stream eof-error-p eof-value)))
      (unless (eq line eof-value)
        (loop with line = (make-string-input-stream line)
           for item = (read line nil line)
           until (eq item line)
           collect item)))))


(defun permute (list permutation)
  (mapcar #'car (sort (mapcar #'cons list permutation) #'< :key #'cdr)))


(defun extract-scenarios (scen-nbrs input-file output-file)
 (with-open-file (out output-file :direction :output)
   (format out "~{~A~^,~}~%" scen-nbrs)
   (with-open-file (in input-file :direction :input)
     (loop for row = (extract-columns scen-nbrs (fetch-row in :delimiter #\, :eof-error-p nil))
          while row
          do (format out "~{~A~^,~}~%" row)))))

(defun extract-scenarios2 (scen-nbrs input-file output-file)
  (let ((table (with-open-file (in input-file :direction :input)
                 (loop for row = (extract-columns scen-nbrs (fetch-row in :delimiter #\, :eof-error-p nil))
                    while row
                    collect row))))
    (with-open-file (out output-file :direction :output)
      (format out "~{~A~^,~}~%" scen-nbrs)
      (loop for row in table
         do (format out "~{~A~^,~}~%" row)))))

(defun extract-scenarios3 (scen-nbrs input-file output-file)
  (let ((readtable (copy-readtable nil))
        (table (let (table)
                 (with-open-file (in input-file :direction :input)
                   (handler-case
                       (loop (push (read-line in) table))
                     (end-of-file () table))))))
    (set-syntax-from-char #\, #\Space readtable)
    (loop for line in (prog1 table (setq table nil))
       do (push (extract-columns scen-nbrs (let ((*readtable* readtable)
                                                 (line (make-string-input-stream line)))
                                             (loop for item = (read line)
                                                until (eq item line)
                                                collect item))) table))
    (with-open-file (out output-file :direction :output)
      (format out "~{~A~^,~}~%" scen-nbrs)
      (loop for row in table
         do (format out "~{~A~^,~}~%" row)))))

#p "c:/temp/market-data-SLSH-1006-2.csv"
 #p"c:/temp/Copie de market data SH 3006 SIM analysis.csv"

