(oam:define-project-package #:ch.amann-wolowyk.oam-sql #:sql)
(in-package #:ch.amann-wolowyk.oam-sql-system)

(defun sql::to-sql (expr)
  (etypecase expr
    (null "NULL")
    (symbol (let ((string (oam:string-replace #\_ #\- (string expr))))
              (if (and (string/= string "?")
                       (notevery #'upper-case-p string))
                  (format nil "~S" string)
                  string)))
    (string (format nil "'~A'" expr))
    (integer (format nil "~D" expr))
    (rational (sql::to-sql (coerce expr 'long-float)))
    (real (format nil "~,,,,,,'EG" expr))
    (cons (let ((first (first expr))
                (rest (rest expr)))
            (typecase first
              (symbol
               (let ((first (intern (string first) :keyword)))
                 (ecase first
                   ((:and :or)
                    (if rest
                        (format nil (format nil "(~~{~~A~~^ ~A ~~})" first) (mapcar #'sql::to-sql rest))
                        (case first
                          (:and "1=1")
                          (t "1=0"))))
                   ((:= :<= :< :>= :>)
                    (case (length rest)
                      ((0 1)
                       "1=1")
                      (2
                       (format nil "~A ~A ~A" (sql::to-sql (first rest)) first (sql::to-sql (second rest))))
                      (t
                       (sql::to-sql (cons 'and (mapcar (let ((prev (first rest)))
                                                         (lambda (curr)
                                                           (list first prev (setq prev curr))))
                                                       (rest rest)))))))
                   ((:not)
                    (format nil "~A ~A" first (sql::to-sql (first rest))))
                   ((:in :member)
                    (if (consp (second rest))
                        (format nil "~A IN ~A" (sql::to-sql (first rest)) (sql::to-sql (second rest)))
                        "0=1"))
                   (:like
                    (let ((second-rest (second rest)))
                      (if (and (stringp second-rest)
                               (some (lambda (c) (or (char= c #\%)
                                                     (char= c #\_)))
                                     second-rest))
                          (format nil "~A ~A ~A" (sql::to-sql (first rest) first (sql::to-sql second-rest)))
                          (sql::to-sql (list* := rest))))))))
              (t
               (format nil "(~A~{, ~A~})" (if first (sql::to-sql first) "") (mapcar #'sql::to-sql rest))))))))

(oam:export-interface '#:sql)