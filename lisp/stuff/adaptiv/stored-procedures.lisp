(in-package #:adaptiv)

(defun match-string (target string)
  "Return the position in STRING (beginnig with 0) of the first (from the left) character of the first substring of STRING matching TARGET. If there is no match, nil is returned."
  (etypecase target
    (character
     (position target string))
    (string
     (let* ((tar0 (elt target 0))
            (n (length target)))
       (do ((position (position tar0 string) (let ((pos (position tar0 (subseq string (1+ position)))))
                                               (and pos (+ position pos 1)))))
           ((or (null position)
                (string= target (subseq string position (+ position n))))
            position))))))

(defun string-replace (prefix replace target string)
  "Return the concatenation of PREFIX and the string obtained form STRING by replacing in STRING all occurencies of TARGET by REPLACE."
  (if (string= "" string)
      prefix
      (let* ((position (match-string target string))
             (prefix (concatenate 'string prefix (if position (concatenate 'string (subseq string 0 position) replace) string)))
             (string (if position (subseq string (+ position (if (stringp target)
                                                                 (length target)
                                                                 1))) "")))
        (string-replace prefix replace target string))))

(defgeneric to-sql (object)
  (:documentation "return a string which represents OBJECT in SQL syntax.")
  (:method ((object null)) "null")
  (:method ((object string)) (format nil "'~A'" (string-replace "" "''" "'"  object)))
  (:method ((object real)) (format nil "~F" object))
  (:method ((object symbol)) (to-sql (symbol-name object)))
  (:method ((object list)) (format nil "(~{~A~#[~:;, ~]~})" (mapcar #'to-sql object)))
  (:method ((object date)) (to-sql (subseq (format-time nil object) 0 23)))
  (:method ((object wall-time)) (to-sql (subseq (format-time nil object) 0 23))))

(defun map-values (&rest arglist)
  (mapcar #'to-sql  arglist))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-list-keyword-p (arg)
    (member arg '(&optional &key &allow-other-keys &aux &rest)))
  (defun extract-mandatory-args (arglist)
    (loop for arg in arglist until (lambda-list-keyword-p arg) collect arg))
  (defun extract-optional-args (arglist)
    (loop for arg in (cdr (member '&optional arglist))
       until (lambda-list-keyword-p arg)
       collect (typecase arg
                 (symbol (list arg nil))
                 (cons arg))))
  (defun extract-keyword-args (arglist)
    (loop for arg in (cdr (member '&key arglist))
       until (lambda-list-keyword-p arg)
       collect (typecase arg
                 (symbol (list (intern (symbol-name arg) :keyword) arg))
                 (cons (destructuring-bind (a &optional init-form &rest rest) arg
                         (let ((key (etypecase a
                                      (symbol (intern (symbol-name a) :keyword))
                                      (cons (car a))))
                               (var (etypecase a
                                      (symbol a)
                                      (cons (cadr a)))))
                           (append (list key (if init-form
                                                 (list var init-form)
                                                 var))
                                   rest)))))))
  (defun extract-rest-arg (arglist)
    (second (member '&rest arglist)))
  (defun extract-aux-args (arglist)
    (loop for arg in (cdr (member '&aux arglist))
       until (lambda-list-keyword-p arg)
       collect (typecase arg
                 (symbol (list arg nil))
                 (cons arg))))
  (defun extract-var-symbols (arglist)
    (append (extract-mandatory-args arglist)
            (mapcar (lambda (arg)
                      (etypecase arg
                        (symbol arg)
                        (cons (car arg))))
                    (extract-optional-args arglist))
            (mapcar (lambda (arg)
                      (let ((arg (second arg)))
                        (etypecase arg
                          (symbol arg)
                          (cons (car arg)))))
                    (extract-keyword-args arglist))
            (mapcar (lambda (arg)
                      (etypecase arg
                        (symbol arg)
                        (cons (car arg))))
                    (extract-aux-args arglist)))))

(defmacro define-stored-procedure (name arglist &key (stored-name (substitute #\- #\_ (string-downcase name)))
                                   documentation
                                   gives-results catalog (var-symbols (extract-var-symbols arglist) var-symb-p))
  `(defun ,name ,arglist
     ,(format nil "Stored procedure ~A on database ~A.~@[~&~A~]" stored-name catalog documentation)
     ,@(when var-symb-p
             (let* ((declared-vars (extract-var-symbols arglist))
                    (special-vars (set-difference var-symbols declared-vars))
                    (ignored-vars (set-difference declared-vars var-symbols)))
               `((declare ,@(when ignored-vars
                                  `((ignorable ,@ignored-vars)))
                          ,@(when special-vars
                                  `((special ,@special-vars)))))))
     (,(if gives-results 'query 'execute-command)
       (apply #'format nil ,(format nil "exec ~A ~{~~A~*~#[~:;, ~]~}" stored-name var-symbols)
              (map-values ,@var-symbols))
       :database (connect-to ,catalog))))












(define-stored-procedure list-table-names (catalog
                                           &key
                                           @table-name
                                           @table-owner
                                           @table-qualifier
                                           @table-type
                                           @fusepattern)
  :stored-name "sp_tables" :gives-results t :catalog catalog
  :var-symbols (@table-name @table-owner @table-qualifier @table-type @fusepattern))

(define-stored-procedure list-table-columns (catalog
                                             @table-name
                                             &key
                                             @table-owner
                                             @table-qualifier
                                             @column-name
                                             @ODBCVer)
  :stored-name "sp_columns" :gives-results t :catalog catalog
  :var-symbols (@table-name @table-owner @table-qualifier @column-name @ODBCVer))

(define-stored-procedure list-sproc-columns (catalog
                                             &optional
                                             @procedure-name
                                             &key
                                             @procedure-owner
                                             @procedure-qualifier
                                             @column-name
                                             @ODBCVer
                                             @fUsePattern)
  :stored-name "sp_sproc_columns" :gives-results t :catalog catalog
  :var-symbols (@procedure-name @procedure-owner @procedure-qualifier @column-name @ODBCVer @fUsePattern))