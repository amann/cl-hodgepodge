
(define-condition foo-error (error) ())

(defun check-foo (x)
  (loop (if (eq :foo x)
            (return x)
            (with-simple-restart (new-foo "foo.")
                (error 'foo-error)))))
(defun check-foo (x)
  (loop (if (eq :foo x)
            (return x)
            (restart-case
                (error 'foo-error)
              (new-foo (new-foo)
                :report "enter new foo."
                :interactive (lambda ()
                               (list (eval (read))))
                (setq x new-foo))))))


(defun check-foo (x)
  (assert (eq :foo x) (x)
          'foo-error x)
  x)

(defun correct-to-foo (x)
  (handler-bind ((foo-error (lambda (c &aux (r (or (find-restart 'new-foo c)
                                                   (find-restart 'continue c))))
                          (compute-restarts)
                             #+ (or) (invoke-restart r :foo))))
    (check-foo x)))
(defun correct-to-foo (x)
  (handler-case (check-foo x)
    (foo-error (c)
      (compute-restarts)
      #+ (or) (invoke-restart r :foo))))

(correct-to-foo :bar)


(defun validate-url (string)
  "The URL of the page; should start with http:// or https://."
  (unless (or (string= "http://" string :end2 7)
              (string= "https://" string :end2 8))
    (csv-error "URL invalid." :value string)))


(defun validate-rating (string)
  "String should contain an integer between 1 and 5, inclusive."
  (let ((rating (parse-integer string :junk-allowed t)))
    (unless (and (integerp rating) (<= 1 rating 5))
      (csv-error "Rating not an integer in range." :value string))))


(defun validate-visitors (string)
  "The number of visitors to the page; string should contain an
integer more than or equal to zero."
  (let ((visitors (parse-integer string :junk-allowed nil)))
    (unless (and (integerp visitors) (>= visitors 0))
      (csv-error "Number of visitors invalid." :value string))))


(defun validate-date (string)
  "The published date of the URL. Should be in yyyy-mm-dd format."
  (let ((split (oam:split-string "-" string)))
    (flet ((!valid-number-of-digits-p (string n)   ; See note 3
             (and (every #'digit-char-p string)
                  (= (length string) n))))
      (unless (and (!valid-number-of-digits-p (first split) 4)
                   (!valid-number-of-digits-p (second split) 2)
                   (!valid-number-of-digits-p (third split) 2))
        (csv-error "Published date not in valid format." :value string)))))



(define-condition csv-error (error)
  ((message
    :initarg :message
    :accessor csv-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor csv-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (line-number
    :initarg :line-number
    :accessor csv-error-line-number
    :initform nil
    :documentation "The line number of the row in for the error was signalled.")))


;; Do something more useful than the default printer behaviour
(defmethod print-object ((object csv-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[L~A ~]~S~@[: ~S~]"
            (csv-error-line-number object)
            (csv-error-message object)
            (csv-error-value object))))


;; We use this function to signal our validation error
(defun csv-error (message &key value line-number)
  (error 'csv-error
         :message message
         :value value
         :line-number line-number))

(defun validate-field (header value)
  (flet ((!header-matches (string)
           (string-equal header string)))
    (cond
      ((!header-matches "url") (validate-url value))
      ((!header-matches "rating") (validate-rating value))
      ((!header-matches "visitors") (validate-visitors value))
      ((!header-matches "date") (validate-date value))
      (t (csv-error "Invalid header." :value header)))))



(defun validate-csv (file)
  (destructuring-bind (headers . rows)
      (parse-csv-file file)
    (loop
       for row in rows
       for line-number upfrom 2
       do
       ;; If this restart is invoked, validation will continue on
       ;; the next row
       (with-simple-restart (continue-next-row "Continue validation on next row.")
         (when (/= (length row) (length headers))
           (csv-error "Number of fields doesn't equal number of headers."
                      :line-number line-number))
         (loop
            for header in headers
            for field in row
            do
            (handler-bind
                ((csv-error #'(lambda (c)
                                (setf (csv-error-line-number c) line-number))))
              ;; If this restart is invoked, validation will continue
              ;; on the next field in the row
              (with-simple-restart (continue-next-field "Continue validation on next field.")
                (validate-field header field))))))))


(defun validate-csv (file)
  (restart-case (validate-csv-aux file)
    (retry-file ()
      :report (lambda (stream)
                (format stream "Retry validating the file ~A." file))
      (validate-csv file))))


(defun validate-csv-aux (file)
  (destructuring-bind (headers . rows)
      (parse-csv-file file)
    (loop
       for row in rows
       for line-number upfrom 2
       do
       (with-simple-restart (continue-next-row "Continue validation on next row.")
         (when (/= (length row) (length headers))
           (csv-error "Number of fields doesn't equal number of headers."
                      :line-number line-number))
         (loop
            for header in headers
            for field in row
            do
            (handler-bind
                ((csv-error #'(lambda (c)
                                (setf (csv-error-line-number c) line-number))))
              (restart-case (validate-field header field)
                (use-new-ice-cream (new-ice-cream)
                  :report "Use a new ice cream."
                  :interactive read-new-value  
                  (setq field new-ice-cream)))))))))
(defun read-new-value ()
   (format t "Enter a new value: ")
   (multiple-value-list (eval (read))))


(defun list-csv-errors (file)
  (let ((result nil))
    (handler-bind
        ((csv-error #'(lambda (c)
                        (let ((restart (or (find-restart 'use-new-ice-cream)
                                           (find-restart 'continue-next-field)
                                           (find-restart 'continue-next-row))))
                          (when restart
                            (push c result)
                            (invoke-restart restart +666))))))

      (validate-csv file))
    (nreverse result)))


(defun parse-csv-file (file)
  (with-open-stream (f file)
    (loop
       for line = (read-line f nil)
       while line
       collect (oam:split-string "," line))))

(with-input-from-string (file
                         "rating,url,visitors,date
4,http://chaitanyagupta.com/home,-1233445,2000-01-01
5,http://chaitanyagupta.com/blog,33333,2006-02-02
5,http://chaitanyagupta.com/code,2121212,2007-03-03
")
  (list-csv-errors file))