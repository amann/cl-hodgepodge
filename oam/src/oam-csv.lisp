(oam:define-project-package #:ch.amann-wolowyk.csv #:csv)
(in-package #:ch.amann-wolowyk.csv-system)


(defconstant csv::+ms-eol+ (if (boundp 'csv::+ms-eol+)
                                 csv::+ms-eol+
                                 (format nil "~A~A" #\Return #\Newline))
  "End of line string for Windows or MS-DOS.")

(defconstant csv::+unix-eol+ #\Newline
  "End of line character for *nix.")

(defvar csv::*default-eol* #+(or windows win32) csv::+ms-eol+
        #+(or unix) csv::+unix-eol+)


(defun csv::default-item-parser (item-string)
  "Simple parser using CL:READ-FROM-STRING and offering two restarts RETURN-NIL and RETURN-VALUE, the latter taking a new value to be returned instead of the unparseable item."
  (declare (optimize speed))
  (restart-case
      (read-from-string item-string)
    (csv::return-nil ()
      :report "Continue by returning NIL."
      nil)
    (csv::return-value (new-value)
      :report "Continue by returning a new value instead."
      :interactive (lambda ()
                     (format t "Enter a new value to return: ")
                     (eval (read)))
      new-value)))


(defun make-csv-header-map (stream eol delimiters escape-char)
  "Read (and consuming) the first row of a CSV file through STREAM considering it as the header of the columns. Return a hash-table mapping each column name with the column position (beginning with 0)."
  (declare (optimize speed))
  (let ((hash (make-hash-table :test #'equal))
        (delimiters (etypecase delimiters (character (list delimiters)) (cons delimiters)))
        (eol@ (elt eol 0))
        (eol* (elt eol (1- (length eol)))))
    (oam:fbind ((map-chunk (oam::make-chunk-mapper (list* eol@ delimiters) escape-char)))
      (loop :with delim := nil
         :for item := (with-output-to-string (*standard-output*)
                        (setq delim (map-chunk #'write-char stream)))
         :for i :upfrom 0
         :until (char= eol@ delim) :do (setf (gethash item hash) i)
         :finally (handler-case
                      (unless (eql eol* delim)
                        (peek-char eol* stream)
                        (read-char stream))
                    (end-of-file ())))
      (lambda (col-name)
        (or (gethash col-name hash)
            (error "No such column name."))))))

(defun csv::parse-csv (stream col-parsers &key (eol csv::*default-eol*) (delimiters #\~) (escape-char #\\) header)
  "Parse a CSV file by mapping the items of the columns to (a/-) function(-/s) given in COL-PARSERS. COL-PARSERS is either one function to parse all columns with or a list of pairs (col-designator parse-fn) where col-designator is either an integer or may be a string if HEADER is true. If HEADER is nil the parsing function COL-PARSERS takes two arguments: the first is the item in form of a string to be parsed, the second is a boolean being true if and only if the current item is the last of the current row. If HEADER is true the parsing functions take as one single argument the string to be parsed."
  (declare (optimize speed))
  (let* ((eol (string eol)))
    (oam:fbind ((header-pos (if header (make-csv-header-map stream eol delimiters escape-char))))
      (etypecase col-parsers
        (function (csv::map-items-of-csv-rows col-parsers stream :delimiters delimiters :eol eol
                                                :escape-char escape-char))
        (cons (let ((col-nbrs (mapcar (lambda (pair)
                                        (let ((col-designator (car pair)))
                                          (etypecase col-designator
                                            (integer col-designator)
                                            (string (header-pos col-designator)))))
                                      col-parsers))
                    (col-parsers (mapcar #'cadr col-parsers)))
                (rplacd (last col-parsers) col-parsers)
                (csv::map-csv-rows-of-cols (lambda (item)
                                               (funcall (pop col-parsers) item))
                                           stream
                                           col-nbrs :delimiters delimiters :eol eol))))
      #'header-pos)))

(defun csv::load-csv (stream col-parsers &key (eol csv::*default-eol*) (delimiters #\~) (escape-char #\\) header)
  "Parse a CSV file with parse-csv and loading it as a list of rows which themselves are lists of items."
  (declare (optimize speed))
  (let (result)
    (csv::parse-csv stream
                       (etypecase col-parsers
                         (function (let ((first-col-p t))
                                     (lambda (item eolp)
                                       (when first-col-p
                                         (setq first-col-p nil)
                                         (push nil result))
                                       (push item (car result))
                                       (when eolp
                                         (setf (car result) (nreverse (car result))
                                               first-col-p t)))))
                         (cons (mapcar (let ((max-pos (1- (length col-parsers)))
                                             (pos -1))
                                         (lambda (col-parser)
                                           (incf pos)
                                           (list (first col-parser)
                                                 (cond
                                                   ((= 0 pos max-pos)
                                                    (setq pos -1)
                                                    (lambda (item)
                                                      (push nil result)
                                                      (push (funcall (second col-parser) item) (car result))
                                                      (setf (car result) (nreverse (car result)))))
                                                   ((= 0 pos)
                                                    (lambda (item)
                                                      (push nil result)
                                                      (push (funcall (second col-parser) item) (car result))))
                                                   ((= pos max-pos)
                                                    (setq pos -1)
                                                    (lambda (item)
                                                      (push (funcall (second col-parser) item) (car result))
                                                      (setf (car result) (nreverse (car result)))))
                                                   (t
                                                    (lambda (item)
                                                      (push (funcall (second col-parser) item) (car result))))))))
                                       col-parsers)))
                       :eol eol :delimiters delimiters :escape-char escape-char :header header)
    (nreverse result)))



(defun csv::map-items-of-csv-rows (fct stream &key delimiters (eol csv::*default-eol*) escape-char)
  "Map the function FCT to each CSV-value one by one and row by row. The function FCT takes two arguments: the first is a string containing the current CSV-value, the second is a boolean which is true iff the current value is the last one of the current row."
  (let* ((eol (string eol))
         (eol@ (elt eol 0))
         (eol* (elt eol (1- (length eol)))))
    (oam:fbind ((map-chunk (oam::make-chunk-mapper (list* eol@ (etypecase delimiters
                                                                   (character (list delimiters))
                                                                   (cons delimiters)))
                                                     escape-char)))                                
      (loop
         (loop :with last-delim := t
            :until (or (null last-delim)
                       (eql eol@ last-delim))
            :do (funcall fct
                         (with-output-to-string (*standard-output*)
                           (setq last-delim (map-chunk #'write-char stream)))
                         (or (null last-delim)
                             (eql eol@ last-delim)))
            :finally (handler-case
                         (if (eql last-delim eol*)
                             (peek-char nil stream)
                             (progn
                               (peek-char eol* stream)
                               (read-char stream)))
                       (end-of-file () (return-from csv::map-items-of-csv-rows))))))))


(define-condition csv::end-of-row (error)
  ((missing-columns :initarg :missing-columns :reader csv::missing-columns)
   (treated-columns :initarg :treated-columns :reader csv::treated-columns)))

(defun csv::map-csv-rows-of-cols (fct stream col-numbers &key delimiters (eol csv::*default-eol*) escape-char)
  "Map the function FCT to each item of some columns of a CSV file input by STREAM row by row. The columns used are designated by their position from left (beginning with 0) in the list COL-NUMBERS. The values of the CSV are separated by characters given in DELIMITERS, the end of line markers are given in EOL. The values of the CSV may contain delimiter characters when escaped by the ESCAPE-CHAR."
  (let* ((eol (string eol))
         (eol@ (elt eol 0))
         (eol* (elt eol (1- (length eol))))
         (nbr-of-cols (length col-numbers))
         (extr-cols (make-array (list nbr-of-cols)
                                :element-type 'string
                                :initial-element ""))
         (sorted-col-nbrs (sort (mapcar (let ((i -1)) (lambda (n) (cons n (incf i)))) col-numbers)
                                #'< :key #'car)))
    (oam:fbind ((map-chunk (oam::make-chunk-mapper (list* eol@ (etypecase delimiters
                                                                   (character (list delimiters))
                                                                   (cons delimiters)))
                                                     escape-char)))
      (labels ((eolp (delim)
                 (or (null delim)
                     (eql eol@ delim)))
               (raise-end-of-row-error (remaining-cols)
                 (error 'csv::end-of-row :missing-columns remaining-cols
                        :treated-columns (loop :for i :below nbr-of-cols
                                            :collect (if (member i remaining-cols :key #'cdr :test #'=)
                                                         :missing
                                                         (elt extr-cols i)))))
               (skip-n-chunks (n delim remaining-cols)
                 (loop :repeat n
                    :do (if (eolp delim)
                            (raise-end-of-row-error remaining-cols)
                            (setq delim (map-chunk nil stream)))
                    :finally (return delim))))
        (restart-case
            (loop
               (let ((last-delim t))
                 (restart-case
                     (loop :with curr-pos := 0 :with extr-cols := extr-cols
                        :for remaining-cols :on sorted-col-nbrs
                        :do (destructuring-bind (col-nbr . rank) (first remaining-cols)
                              (restart-case
                                  (if (eolp (setq last-delim (skip-n-chunks (- col-nbr curr-pos)
                                                                            last-delim
                                                                            remaining-cols)))
                                      (raise-end-of-row-error remaining-cols)
                                      (setf (elt extr-cols rank)
                                            (with-output-to-string (*standard-output*)
                                              (setq last-delim
                                                    (map-chunk #'write-char stream)
                                                    curr-pos (1+ col-nbr)))))
                                (csv::use-values (cols-to-use)
                                  :report "Use other entries instead."
                                  :interactive (lambda ()
                                                 (format t "Enter the columns to use ~
                                                          in form of a sequence: ")
                                                 (multiple-value-list (eval (read))))
                                  (setq extr-cols cols-to-use))))
                        :finally (map nil fct extr-cols))
                   (csv::skip-row ()
                     :report "Skip this row."))
                 (handler-case
                     (progn (unless (eql last-delim eol*)
                              (peek-char eol* stream)
                              (read-char stream))
                            (peek-char nil stream))
                   (end-of-file () (return-from csv::map-csv-rows-of-cols)))))
          (csv::terminate ()
            :report "Terminate reading."))))))

(oam:export-interface '#:csv)