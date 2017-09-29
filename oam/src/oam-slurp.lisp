(oam:define-project-package #:ch.amann-wolowyk.slurp #:slurp)
(in-package #:ch.amann-wolowyk.slurp-system)


(defconstant slurp::+ms-eol+ (if (boundp 'slurp::+ms-eol+)
                                 slurp::+ms-eol+
                                 (format nil "~A~A" #\Return #\Newline))
  "End of line string for Windows or MS-DOS.")

(defconstant slurp::+unix-eol+ #\Newline
  "End of line character for *nix.")

(defvar slurp::*default-eol* #+(or windows win32) slurp::+ms-eol+
        #+(or unix) slurp::+unix-eol+)

(defun slurp::slurp-file (pathname)
  "Read the file at PATHNAME into a single string and return this string."
  (with-open-file (stream pathname)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun slurp::read-rows (stream)
  "Read STREAM into a list of rows and return this list."
  (loop :for line = (read-line stream nil nil)
     :while line :collect line))

(defun slurp::read-rows2 (pathname)
  (funcall (oam:make-string-splitter #\Newline #\\) (slurp::slurp-file pathname)))



(defun slurp::make-chunk-mapper (delimiters &optional escape-char)
  "Return a function taking a function FCT in one character and optionally an input character stream defaulting to *standard-input*. The returned function maps at each call the function FCT to each character of the next chunk of the stream. A chunk is the sequence of character between two occurences of DELIMITERS which are not escaped by ESCAPE-CHAR. DELIMITERS and ESCAPE-CHAR can be either a predicate function, a single character or a list of characters. In the latter case a delimiter or an escape-character is any of that list. The behaviour is unspecified if a character may be at the same time a delimiter and an escape-character."
  (declare (optimize speed))
  (flet ((select-check-form (item)
           (etypecase item
             (function `(funcall ,item (the character char)))
             (character `(char= (the character char) ,item))
             (cons `(find (the character char) ',item :test #'char=)))))
    (let ((delimiter-check-form (list (list (select-check-form delimiters)
                                            :delimiter)))
          (escape-check-form (when escape-char
                               (list (list (select-check-form escape-char)
                                           '(setq escapedp t)
                                           :escape)))))
      (eval `(macrolet
                 ((check (char)
                    (declare (ignorable char))
                    `(if escapedp
                         (progn (setq escapedp nil)
                                :take-it)
                         ,',(append '(cond)
                                    delimiter-check-form
                                    escape-check-form
                                    '((t :take-it))))))
               (lambda (fct &optional (stream *standard-input*))
                 (declare (optimize speed))
                 (handler-case
                     (let (escapedp)
                       (loop :for char character := (read-char stream)
                          :do (case (check char)
                                (:take-it (funcall fct (the character char)))
                                (:delimiter (return char))
                                (:escape))))
                   (end-of-file ()))))))))



(defun slurp::default-item-parser (item-string)
  "Simple parser using CL:READ-FROM-STRING and offering two restarts RETURN-NIL and RETURN-VALUE, the latter taking a new value to be returned instead of the unparseable item."
  (declare (optimize speed))
  (restart-case
      (read-from-string item-string)
    (slurp::return-nil ()
      :report "Continue by returning NIL."
      nil)
    (slurp::return-value (new-value)
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
    (oam:fbind ((map-chunk (slurp::make-chunk-mapper (list* eol@ delimiters) escape-char)))
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

(defun slurp::parse-csv (stream col-parsers &key (eol slurp::*default-eol*) (delimiters #\~) (escape-char #\\) header)
  "Parse a CSV file by mapping the items of the columns to (a/-) function(-/s) given in COL-PARSERS. COL-PARSERS is either one function to parse all columns with or a list of pairs (col-designator parse-fn) where col-designator is either an integer or may be a string if HEADER is true. If HEADER is nil the parsing function COL-PARSERS takes two arguments: the first is the item in form of a string to be parsed, the second is a boolean being true if and only if the current item is the last of the current row. If HEADER is true the parsing functions take as one single argument the string to be parsed."
  (declare (optimize speed))
  (let* ((eol (string eol)))
    (oam:fbind ((header-pos (if header (make-csv-header-map stream eol delimiters escape-char))))
      (etypecase col-parsers
        (function (slurp::map-items-of-csv-rows col-parsers stream :delimiters delimiters :eol eol
                                                :escape-char escape-char))
        (cons (let ((col-nbrs (mapcar (lambda (pair)
                                        (let ((col-designator (car pair)))
                                          (etypecase col-designator
                                            (integer col-designator)
                                            (string (header-pos col-designator)))))
                                      col-parsers))
                    (col-parsers (mapcar #'cadr col-parsers)))
                (rplacd (last col-parsers) col-parsers)
                (slurp::map-csv-rows-of-cols (lambda (item)
                                               (funcall (pop col-parsers) item))
                                             stream
                                             col-nbrs :delimiters delimiters :eol eol))))
      #'header-pos)))

(defun slurp::load-csv (stream col-parsers &key (eol slurp::*default-eol*) (delimiters #\~) (escape-char #\\) header)
  "Parse a CSV file with parse-csv and loading it as a list of rows which themselves are lists of items."
  (declare (optimize speed))
  (let (result)
    (slurp::parse-csv3 stream
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



(defun slurp::map-items-of-csv-rows (fct stream &key delimiters (eol slurp::*default-eol*) escape-char)
  "Map the function FCT to each CSV-value one by one and row by row. The function FCT takes two arguments: the first is a string containing the current CSV-value, the second is a boolean which is true iff the current value is the last one of the current row."
  (let* ((eol (string eol))
         (eol@ (elt eol 0))
         (eol* (elt eol (1- (length eol)))))
    (oam:fbind ((map-chunk (slurp::make-chunk-mapper (list* eol@ (etypecase delimiters
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
                       (end-of-file () (return-from slurp::map-items-of-csv-rows))))))))


(define-condition slurp::end-of-row (error)
  ((missing-columns :initarg :missing-columns :reader slurp::missing-columns)
   (treated-columns :initarg :treated-columns :reader slurp::treated-columns)))

(defun slurp::map-csv-rows-of-cols (fct stream col-numbers &key delimiters (eol slurp::*default-eol*) escape-char)
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
    (oam:fbind ((map-chunk (slurp::make-chunk-mapper (list* eol@ (etypecase delimiters
                                                                   (character (list delimiters))
                                                                   (cons delimiters)))
                                                     escape-char)))
      (labels ((eolp (delim)
                 (or (null delim)
                     (eql eol@ delim)))
               (raise-end-of-row-error (remaining-cols)
                 (error 'slurp::end-of-row :missing-columns remaining-cols
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
                                (slurp::use-values (cols-to-use)
                                  :report "Use other entries instead."
                                  :interactive (lambda ()
                                                 (format t "Enter the columns to use ~
                                                          in form of a sequence: ")
                                                 (multiple-value-list (eval (read))))
                                  (setq extr-cols cols-to-use))))
                        :finally (map nil fct extr-cols))
                   (slurp::skip-row ()
                     :report "Skip this row."))
                 (handler-case
                     (progn (unless (eql last-delim eol*)
                              (peek-char eol* stream)
                              (read-char stream))
                            (peek-char nil stream))
                   (end-of-file () (return-from slurp::map-csv-rows-of-cols)))))
          (slurp::terminate ()
            :report "Terminate reading."))))))

(oam:fbind ((map-chunk (slurp::make-chunk-mapper #'upper-case-p)))
  (defun slurp::camel-style-to-lisp (string)
    (string-upcase (let ((string (with-output-to-string (*standard-output*)
                                   (with-input-from-string (*standard-input* string)
                                     (loop :for upcase-char := (map-chunk #'write-char)
                                        :while upcase-char
                                        :do (format t "~A~A" #\- upcase-char))))))
                     (subseq string (min 1 (length string)))))))


;;;; Old stuff


#+ (or)
(defun slurp::read-csv (pathname item-parser-list &key (separator #\~) (escape-char #\\) header)
  (oam:fbind ((make-item-splitter (oam:make-string-splitter-factory separator :escape-char escape-char)))
    (with-open-file (file pathname)
      (let* ((header (if header (oam:fbind ((next-item (make-item-splitter (read-line file))))
                                  (loop :for item = (next-item) :while item :collect item)))))
        (values (loop :for row = (read-line file nil) :while row
                   :collect (oam:fbind ((next-item (make-item-splitter row)))
                              (loop :for item-parser in item-parser-list
                                 :collect (funcall item-parser (next-item)))))
                header)))))

#+ (or)
(defun slurp::make-csv-row-parser (row-splitter item-parser-list)
  #'(lambda (row)
      (let ((item-parser-list item-parser-list))
        (oam:fbind ((next-item (funcall row-splitter row)))
          (loop :for item := (next-item) :and item-parser := (pop item-parser-list)
             :while (and item item-parser)
             :collect (funcall item-parser item))))))

#+ (or)
(defun slurp::parse-csv-string (string item-parser-list &key (eol slurp::*default-eol*) (delimiters #\~) (escape-char #\\) header)
  (oam:fbind ((next-row (funcall (oam:make-string-splitter-factory eol :escape-char escape-char) string))
              (make-item-splitter (oam:make-string-splitter-factory delimiters :escape-char escape-char)))
    (let* ((header (if header (let ((hash (make-hash-table :test #'equal)))
                                (oam:fbind ((next-item (make-item-splitter (next-row))))
                                  (loop :for item = (next-item) :for i :upfrom 0
                                     :while item :do (setf (gethash item hash) i))
                                  hash)))))
      (values (loop :for row = (next-row) :while row
                 :collect (oam:fbind ((next-item (make-item-splitter row)))
                            (loop :for item-parser in item-parser-list
                               :collect (funcall item-parser (next-item)))))
              header))))


#+ (or)
(defun slurp::parse-csv-string-2 (string item-parser-list &key (end-of-line slurp::*default-eol*) (separator #\~) (escape-char #\\) header-parser-list)
  (let ((row-splitter (oam:make-string-splitter-factory separator :escape-char escape-char)))
    (oam:fbind ((next-row (funcall (oam:make-string-splitter-factory end-of-line :escape-char escape-char) string))                
                (parse-csv-row (slurp::make-csv-row-parser row-splitter item-parser-list)))
      (let* ((header (when header-parser-list
                       (funcall (slurp::make-csv-row-parser row-splitter header-parser-list)
                                (next-row)))))
        (values (loop :for row := (next-row) :while row :collect (parse-csv-row (next-row)))
                header)))))

#+ (or)
(defun slurp::make-chunk-mapper (delimiters &optional escape-char)
  (eval `(macrolet ((check (char)
                      (oam:with-gensyms (g!char)
                        `(let ((,g!char ,char))
                           (declare (type character ,g!char))
                           ,,(if (characterp escape-char)
                                  ``(if status
                                        (progn (setq status nil)
                                               :take-it)
                                        (cond
                                          (,,(etypecase delimiters
                                                         (character ``(char= (the character ,g!char) ,,delimiters))
                                                         (cons ``(find (the character ,g!char) ',',delimiters :test #'char=)))
                                           :delimiter)
                                          ((char= (the character ,g!char) ,,escape-char)
                                           (setq status :escaped)
                                           :escape)
                                          (t
                                           :take-it)))
                                  ``(cond
                                      ((find (the character ,g!char) ',',delimiters :test #'char=)
                                       :delimiter)
                                      (t
                                       :take-it)))))))
           (lambda (fct &optional (stream *standard-input*))
             (declare (optimize speed))
             (oam:fbind ((fct (or fct (lambda (x) (declare (ignore x))))))
               (handler-case
                   (loop :with status := nil
                      :for char character := (read-char stream)
                      :do (case (check (the character char))
                            (:take-it (fct (the character char)))
                            (:delimiter (return char))
                            (:escape)))
                 (end-of-file ())))))))


(oam:export-interface '#:slurp)