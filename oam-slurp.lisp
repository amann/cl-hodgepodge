(oam:define-project-package #:ch.amann-wolowyk.slurp #:slurp)
(in-package #:ch.amann-wolowyk.slurp-system)


(defconstant slurp::+ms-eol+ (format nil "~A~A" #\Return #\Newline)
  "End of sine string for Windows or MS-DOS.")
(defconstant slurp::+unix-eol+ #\Newline
  "End of line character for *nix.")

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


(defun slurp::make-csv-row-parser (item-splitter item-parser-list)
  #'(lambda (row)
      (mapcar #'funcall item-parser-list (funcall item-splitter row))))


(defun slurp::parse-csv (string item-parser-list &key (end-of-line #\Newline) (separator #\~) (escape-char #\\) header)
  (oam:fbind ((next-row (funcall (oam:make-string-splitter-factory end-of-line :escape-char escape-char) string))
              (make-item-splitter (oam:make-string-splitter-factory separator :escape-char escape-char)))
    (let* ((header (if header (oam:fbind ((next-item (make-item-splitter (next-row))))
                                (loop :for item = (next-item) :while item :collect item)))))
      (values (loop :for row = (next-row) :while row
                 :collect (oam:fbind ((next-item (make-item-splitter row)))
                            (loop :for item-parser in item-parser-list
                               :collect (funcall item-parser (next-item)))))
              header))))

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




(oam:export-interface '#:slurp)