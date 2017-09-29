
(oam:define-project-package "CH.AMANN-WOLOWYK.MDB" "MDB")

(in-package "CH.AMANN-WOLOWYK.MDB-SYSTEM")

(defvar mdb::*user*)
(defvar *data* (db:make-table :data-id :ref-date :value :creation-date :creator))
(defvar *source* (db:make-table :data-id :source :source-id :type-id :type-spec-id :creation-date :creator))
(defvar *type-spec* (db:make-table :type-spec-id :key :value :creation-date :creator))
(defvar *types* (make-hash-table))

(defun data-type (name)
  (gethash name *types*))
(defsetf data-type (name) (value)
  `(setf (gethash ,name *types*) ,value))
(defun mdb::make-data-type (name &rest keys)
  "KEYS is a list of items of the form keyword or (keyword :optional)."
  (check-type name symbol)
  (unless (data-type name)
    (let* ((key-list (mapcar (lambda (key)
                               (etypecase key
                                 ((cons keyword (cons * null))
                                  (cons (first key) (unless (eq :optional (second key)) t)))
                                 (keyword (cons key t))))
                             keys)))
      (setf (data-type name) key-list))))

(defun mdb::register (data-type source source-id &rest parameters &key &allow-other-keys)
  (let* ((type-spec-id (gensym (oam:mkstr data-type "-" source "-" source-id "-")))
         (date (date:now))
         (key-list (data-type data-type))
         (given-keys (oam:map-plist (lambda (key val) (declare (ignore val)) key) parameters))
         (mandatory-keys (mapcar #'car (remove-if-not #'cdr key-list)))
         (optional-keys (mapcar #'car (remove-if #'cdr key-list))))
    (map nil (lambda (key) (assert (member key given-keys) () "Key ~S is mandatory." key)) mandatory-keys)
    (map nil (lambda (key) (assert (member key (append mandatory-keys optional-keys)) () "Key ~S is invalid." key)) given-keys)
    (oam:map*-plist (lambda (key value)
                      (db:insert *type-spec*
                                 :type-spec-id type-spec-id :key key :value value
                                 :creation-date date :creator mdb::*user*))
                    parameters)
    (db:insert *source* :data-id (gensym (oam:mkstr source "-" source-id "-"))
               :source source :source-id source-id
               :type-id data-type :type-spec-id type-spec-id
               :creation-date date :creator mdb::*user*)))


(defun get-id (source source-id date)
  (ignore-errors
    (db:with-table-cursor
        ((s (db:select-from (s *source*) (s.type-id s.type-spec-id s.data-id)
                            :where (and (equal s.source source)
                                        (equal s.source-id source-id)
                                        (not (date:date-time-< date s.creation-date)))
                            :order-by (db:compare (lambda (a b) (date:date-time-< b a)) creation-date))))
      (oam:next rows-s)
      (values s.data-id s.type-id s.type-spec-id))))
(defun get-type-spec-id (source source-id date)
  (nth-value 2 (get-id source source-id date)))
(defun get-type-id (source source-id date)
  (nth-value 1 (get-id source source-id date)))

(defun get-type-spec (source source-id date)
  (let ((type-spec-id (get-type-spec-id source source-id date)))
    (db:with-table-cursor ((s (db:select-from (s *type-spec*) (s.key s.value)
                                              :where (eq s.type-spec-id type-spec-id))))
      (loop :while (ignore-errors (oam:next rows-s))
         :nconc (when s.value (list s.key s.value))))))

(defun get-data-point (ref-date source source-id date)
  (let ((data-id (get-id source source-id date)))
    (db:with-table-cursor ((s (db:select-from (s *data*) (s.value)
                                              :where (and (eq s.data-id data-id)
                                                          (eq s.ref-date ref-date)
                                                          (not (date:date-time-< date s.creation-date)))
                                              :order-by (db:compare (lambda (a b) (date:date-time-< b a)) creation-date))))
      (oam:next rows-s)
      s.value)))

(defun mdb::add-data-point (source source-id reference-date value &aux (date (date:now)))
  (let ((data-id (get-id source source-id date)))
    (when data-id
      (db:insert *data* :data-id data-id :ref-date reference-date
                 :value value :creation-date date :creator mdb::*user*)
      t)))

;;;;

(defun mdb::get-data (ref-date source source-id &optional (validity-date (date:now)))
  (list* (get-type-id source source-id validity-date)
         :value (get-data-point ref-date source source-id validity-date)
         (get-type-spec source source-id validity-date)))



(oam:export-interface "MDB")