(oam:define-project-package "CH.AMANN-WOLOWYK.OAM-DB" "DB")

(in-package "CH.AMANN-WOLOWYK.OAM-DB-SYSTEM")


;;;;** FIFO Queue
(defclass oam::fifo-pipe ()
  ((list    :initform (list nil)   :reader   fifo-list)
   (length  :initform 0            :accessor fifo-length)
   (pointer :accessor fifo-pointer)))
(defmethod reinitialize-instance :before ((self oam::fifo-pipe) &key &allow-other-keys)
  (setf (slot-value self 'list) (list nil)))
(defmethod shared-initialize :after ((self oam::fifo-pipe) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (fifo-pointer self) (last (fifo-list self))))
(defmethod oam::add-to ((fifo oam::fifo-pipe) value)
  (prog1 (setf (cdr (fifo-pointer fifo)) (list value))
    (incf (fifo-length fifo))
    (pop (fifo-pointer fifo))))
(defmethod oam::pop ((fifo oam::fifo-pipe))
  (prog1 (pop (cdr (fifo-list fifo)))
    (decf (fifo-length fifo))))
(defmethod oam::length ((seq oam::fifo-pipe))
  (length (cdr (fifo-list seq))))
(defmethod oam::dump ((seq oam::fifo-pipe))
  (prog1 (cdr (fifo-list seq))
    (reinitialize-instance seq)))

(defmethod oam::gmap (type fn (seq oam::fifo-pipe))
  (map type fn (cdr (fifo-list seq))))

(defmethod oam::find (item (sequence oam::fifo-pipe) &rest keys &key from-end test test-not start end key &allow-other-keys)
  (declare (ignore from-end test test-not start end key))
  (apply #'find item (cdr (fifo-list sequence)) keys))




;;;;** Table
;;;;
;;;; A table is a data structure consisting of rows of entries. Each row has the same number of entries
;;;; (possibly empty -- symbolized by NIL) forming thus columns. To every column can be associated a name.
;;;; An entry of a given row can referenced by its position on the row -- the column-index, short cidx --
;;;; or via the name of the column which is a string.
;;;;
;;;; The rows are generally not accessed directly but via an iterator. This means that rows can normally
;;;; only be treated sequentially. (The physical tables being an exception; here the rows have an ID -- the
;;;; row-index, short ridx.)
;;;;
;;;; Physical tables are implemented as a vector of vectors, the latter ones being the rows of the table.
;;;; Normally the vectors for the rows have all the same length. Since it is possible to add or remove
;;;; columns, it may happen however that newer rows have more elements than older ones (or vice versa).
;;;; Columns for which a particular row has no entry get as default value NIL.
;;;;
;;;; Further 

(define-condition db::no-such-column-error (error)
  ((name :initform (error "No name given.") :type string :reader db::name :initarg :name)
   (table :initform (error "No table given.") :type table :reader db::table :initarg :table))
  (:report (lambda (c s)
             (format s "No column with name ~S in table ~A." (db::name c) (db::table c))))
  (:documentation "Error condition signaled when atempting to reference a column with a non-existant name."))

(defun make-map-column<-name ()
  "Return a map column-name -> column to be stored in the slot COLUMN<-NAME of a table object."
  (let ((name->column (make-hash-table :test #'equal)))
    (lambda (action name &optional assignment)
      "Depending on ACTION do following:
:get Return the column associated to the name NAME; ASSIGNMENT is ignored;
:set Set the association of the name NAME to the column ASSIGNMENT;
:rem Remove the association of NAME; ASSIGNMENT is ignored;
:rename Rename the association."
      (let ((name (string name)))
        (ecase action
          (:get (gethash name name->column))
          (:rename (let ((column (gethash name name->column)))
                     (when column
                       (remhash name name->column)
                       (setf (gethash assignment name->column) column))))
          (:set (setf (gethash name name->column) assignment))
          (:rem (remhash name name->column)))))))


(defclass table ()
  ((columns :initform nil :accessor columns
            :documentation "Contains the list of column headers. It is implemented in inverse order so that the last column is first.")
   (column<-name :initform (make-map-column<-name)
                 :documentation "Map containing the associations column name -> column for fast access.")
   (rows :reader rows :documentation "Contains the rows of the table."))
  (:metaclass oam-clos:abstract-class))



(c2mop:finalize-inheritance (find-class 'table))

(oam::with-slot-definition-locations (column<-name) table
  (defun column<-name (table name action &optional assignment)
    (funcall (c2mop:standard-instance-access table column<-name) action name assignment)))

(defclass physical-table (table)
  ((deleted-rows :initform (make-instance 'oam::fifo-pipe) :reader deleted-rows
                 :documentation "Contains the row-numbers of those rows which are maked as deleted. It is implemented as a fifo-pipe, so the rows which are marked first are also overwritten first.")
   (free-indices :initform (make-instance 'oam::fifo-pipe) :reader free-indices
                 :documentation "List containing the indices of the row objects for which currently no column header is associated because of deletion. When creating a new column header for attribution of the index first the availability of a free index is checked for, if none exists, the index = (length (columns (db::table self))) is used.")))

(defmethod initialize-instance :after ((self physical-table) &key column-names &allow-other-keys)
  (dolist (column-name column-names)
    (make-instance 'physical-column :table self :name column-name))
  (setf (slot-value self 'rows) (make-array 0 :element-type 'physical-row :adjustable t :fill-pointer t)))

(defclass select-table (table) ())
(defmethod initialize-instance :after ((self select-table) &key &allow-other-keys)
  (setf (slot-value self 'rows) (make-array 0 :element-type 'select-row :adjustable t :fill-pointer t)))

(defclass joined-table (table) ())
(defmethod initialize-instance :after ((self joined-table) &key &allow-other-keys)
  (setf (slot-value self 'rows) (make-array 0 :element-type 'joined-row :adjustable t :fill-pointer t)))




(defgeneric use-deleted-row-index (table)
  (:method ((table physical-table))
    (oam::pop (deleted-rows table)))
  (:method ((table table))
    nil))


(defun db::column-names (table)
  "Return the names of the columns of TABLE in the correct order."
  (nreverse (mapcar #'db::name (columns table))))

(defun use-free-col-index (table)
  "Return and consumes a free index for associating a column to the corresponding entry in the row object."
  (or (oam::pop (free-indices table))
      (1- (length (columns table)))))

(defun get-max-col-index (table)
  "Return the largest index associated to columns."
  (+ (length (columns table))
     (oam::length (free-indices table))
     -1))

(defclass column ()
  ((name :initform "" :type string :reader db::name)
   (table :initform (error "No table given.") :type table :initarg :table :reader db::table))
  (:metaclass oam-clos:abstract-class))

(defmethod c2mop:validate-superclass ((class c2mop:funcallable-standard-class) (superclass oam-clos:abstract-class))
  t)

(defmethod initialize-instance :after ((self column)
                                       &key name (position (length (columns (db::table self))))
                                       &allow-other-keys)
  (let ((table (db::table self)))
    (setf (db::name self) name
          (db::position self) position)
    (column<-name table name :set self)))



(defclass physical-column (column)
  ((index :type (unsigned-byte #.(floor (log array-total-size-limit 2)))
          :reader index))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A column header specification object for a table. The column is linked to an index on a row."))

(defmethod initialize-instance :after ((self physical-column)
                                       &key &allow-other-keys)
  (let ((table (db::table self)))
    (setf (slot-value self 'index) (use-free-col-index table))
    (c2mop:set-funcallable-instance-function self (compile nil `(lambda (row)
                                                                  (ignore-errors
                                                                    (aref (entries row) ,(index self))))))))

(defclass virtual-column (column)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((self virtual-column)
                                       &key access-fn
                                       &allow-other-keys)
  (c2mop:set-funcallable-instance-function self access-fn))

(defgeneric column (table specifier)
  (:documentation "Return the column of table TABLE which corresponds to the specifier SPECIFIER."))

(defmethod column ((table table) (column-name string))
  (or (column<-name table column-name :get)
      (error 'db::no-such-column-error :table table :name column-name)))
(defmethod column ((table table) (column-name symbol))
  (column table (string column-name)))
(defmethod column ((table table) (position fixnum))
  (car (last (columns table) (1+ position))))

(define-condition db::invalid-table-name (error)
  ((name :initform (error "No name given.") :type symbol :reader db::name :initarg :name))
  (:report (lambda (c s)
             (format s "The Name ~S is invalid for a table. A table name should contain no dot (.)." (db::name c))))
  (:documentation "Error condition signaled when atempting to create an invalid table name."))
(define-condition db::column-name-already-used-error (error)
  ((name :initform (error "No name given.") :type symbol :reader db::name :initarg :name)
   (table :initform (error "No table given.") :type table :reader db::table :initarg :table)
   (column :initform (error "No column given.") :type column :reader db::column :initarg :column))
  (:report (lambda (c s)
             (format s "Name ~S already used for column ~A in table ~A." (db::name c) (db::column c) (db::table c))))
  (:documentation "Error condition signaled when atempting to create a column name which is already attributed to some column of the table."))



(defun check-table-name (name)
  "Check if NAME contains no occurence of SEPARATOR."
  (loop (if (find #\. name :test #'char=)
            (restart-case
                (error 'db::invalid-table-name :name name)
              (new-name (new-name)
                :report "Use a new name."
                :interactive (lambda ()
                               (princ "Enter a new name: ")
                               (list (eval (read))))
                (setq name new-name)))
            (return name))))

(defun check-column-name (table name)
  "Check if the name is a valid column name i.e. not already used. If not valid then signal an error of type DB::NAME-ALREADY-USED-ERROR."
  (loop (let ((column (ignore-errors (column table name))))
          (if column
              (restart-case
                  (error 'db::column-name-already-used-error :table table :column column :name name)
                (new-name (new-name)
                  :report "Use a new name."
                  :interactive (lambda ()
                                 (princ "Enter a new name: ")
                                 (list (eval (read))))
                  (setq name new-name)))
              (return (string name))))))

(defmethod (setf db::name) (value (object column))
  "Set the name of a COLUMN object."
  (block nil
    (let* ((table (db::table object))
           (old-name (db::name object))
           (new-name (check-column-name table value)))
      (when (eq new-name old-name)
        (return old-name))
      (column<-name table old-name :rename new-name)
      (setf (slot-value object 'name) new-name))))

(defgeneric db::position (object)
  (:documentation "Return the position of OBJECT."))
(defmethod db::position ((object column))
  "Return the position in the associated table of the COLUMN object."
  (position object (columns (db::table object))))

(defmethod (setf db::position) (value (object column))
  "Set the position in the associated table of the COLUMN object. If VALUE is null the nothing is done."
  (when value
    (let* ((table (db::table object))
           (columns (oam:remove object (columns table)))
           (position (let ((max-pos (length columns)))
                       (- max-pos (max 0 (min max-pos value)))))
           (columns (oam:ninsert position columns object)))
      (setf (columns table) columns)
      position)))

(defmethod db::delete-column ((column column))
  "Delete the column from the associated table and makes unbound the slots table and index. A deleted column is useless and should be garbage collected."
  (let ((table (db::table column)))
    (setf (slot-value table 'columns) (delete column (columns table)))
    (column<-name table (db::name column) :rem)
    (oam::add-to (free-indices table) (index column))
    (slot-makunbound column 'table)
    (slot-makunbound column 'index)))
(defun db::add-column (table name &optional position)
  (apply #'make-instance 'column :table table :name name (when position (list :position position))))

;;;;** Rows
(defclass row ()
  ((table :initform (error "No table given.") :type table :initarg :table :reader db::table)
   (index :type (unsigned-byte #.(floor (log array-total-size-limit 2))) :accessor index)
   (entries :reader entries))
  (:metaclass oam-clos:abstract-class)
  (:documentation "An object representing a row in the table."))
(defmethod initialize-instance :after ((self row) &key &allow-other-keys)
  (let* ((table (db::table self))
         (rows (rows table))
         (free-row-index (use-deleted-row-index table)))
    (if free-row-index
        (setf (aref rows free-row-index) self
              (index self) free-row-index)
        (progn (vector-push-extend self rows)
               (setf (index self) (1- (length rows)))))))

(defclass physical-row (row)
  ((entries :type vector :reader entries)))


(defmethod initialize-instance :after ((self physical-row) &key entries &allow-other-keys)
  (setf (slot-value self 'entries) (apply #'make-array (1+ (get-max-col-index (db::table self)))
                                          :adjustable t :fill-pointer t
                                          (if entries
                                              (list :initial-contents entries)
                                              (list :initial-element nil)))))


(defclass virtual-row (row) ())

(defclass select-row (virtual-row)
  ((entries :type row :initarg :linked-row)))


(defclass joined-row (virtual-row)
  ((entries :type (cons (or row null) (or row null)))))
(defmethod initialize-instance :after ((self joined-row) &key left-row right-row &allow-other-keys)
  (setf (slot-value self 'entries) (cons left-row right-row)))



(defun delete-row (table row-num)
  "Mark the row with in position index of table TABLE as deleted."
  (when (integerp row-num)
    (oam::add-to (deleted-rows table) row-num)))
(defun deletedp (table index)
  "Return true iff row in position INDEX in table TABLE is marked as deleted." 
  (not (null (oam::find index (deleted-rows table)))))

(defun entry (row col)
  "Return the entry of column COL from ROW."
  (when row (funcall col row)))
(defmethod (setf entry) (value (row physical-row) (idx fixnum))
  "Set the entry of index IDX from ROW to VALUE."
  (setf (aref (entries row) idx) value))
(defmethod (setf entry) (value (row physical-row) (col physical-column))
  "Set the entry from ROW corresponding to column COL to VALUE."
  (setf (aref (entries row) (index col)) value))


;;;;** Constructors for Physical Tables
(defun db::make-table* (column-names)
  (make-instance 'physical-table :column-names column-names))
(defun db::make-table (&rest column-names)
  (db::make-table* column-names))


;;;;*** Printing


(defmethod print-object ((o row) stream)
  (let (next)
    (map nil (lambda (col)
               (when next
                 (princ ", " stream))
               (setq next t)
               (print-object (funcall col o) stream))
         (reverse (columns (db::table o))))))

(defmethod print-object ((o column) stream)
  (princ (db::name o) stream))


(defmethod print-object ((o table) stream)
  (fresh-line stream)
  (let (next)
    (map nil (lambda (col)
               (when next
                 (princ ", " stream))
               (setq next t)
               (print-object col stream))
         (reverse (columns o))))
  (map nil (lambda (row)
             (terpri stream)
             (print-object row stream))
       (rows o)))


;;;;** Insert -----------------------------------
(defun insert* (table col-idxs values)
  (let ((row (make-instance 'physical-row :table table)))
    (loop :for col-idx :in col-idxs
       :for value :in values
       :do (setf (entry row col-idx) value))))

(defmethod db::make-inserter ((table physical-table) column-names)
  "Return a function taking a list of values and inserting into table TABLE a row whose entries are each value of the list of values for the corresponding column in the list COLUMN-NAMES; those entries for which no value have been given, NIL is set."
  (compile nil `(lambda (values)
                  (insert* ,table ',(mapcar (lambda (col-name)
                                              (index (column table col-name)))
                                            column-names)
                           values))))

(defmethod db::insert* ((table physical-table) column-names values)
  "Insert into table a row whose entries are for each column in the list COLUMN the value in the list VALUES which has the same position."
  (insert* table (mapcar (lambda (col-name)
                           (index (column table col-name)))
                         column-names) values))
(defun db::insert (table &rest entries &key &allow-other-keys)
  "Insert the entries into table. ENTRIES is a plist of form [column-name value]* for those columns which do not appear in the ENTRIES list, the value NIL is set."
  (let ((columns (oam::map-plist (lambda (col-name value)
                                   (declare (ignore value))
                                   col-name)
                                 entries))
        (values (oam::map-plist (lambda (col-name value)
                                  (declare (ignore col-name))
                                  value)
                                entries)))
    (db::insert* table columns values)))


;;;;** Select, Group and Join -------------------------------------
(defun select-from* (column-names fns table test)
  "Create and return a new table with columns COLUMN-NAMES and whose rows are generated by mapping the rows of the table TABLE which satisfy the predicate TEST to the each function of the list of functions FNS in respective order."
  (let ((new-table (make-instance 'select-table)))
    (map nil
         (compile nil
                  `(lambda (col-name fn)
                     (make-instance 'virtual-column
                                    :table ,new-table
                                    :name col-name
                                    :access-fn (compile nil
                                                        `(lambda (row)
                                                           (funcall ,fn (entries row)))))))
         column-names
         fns)
    (oam::pmap
         (compile nil
                  `(lambda (row)
                     (when (funcall ,test row)
                       (make-instance 'select-row
                                      :table ,new-table
                                      :linked-row row))))
         (rows table))
    new-table))

(defun make-generalized-unary-fn (fn)
  "Return a function which behaves like FN but returning NIL for those args which would generate an error if applied to FN."
  (compile nil `(lambda (x) (ignore-errors (funcall ,fn x)))))
(defun make-generalized-binary-fn (fn)
  "Return a function which generalizes the function FN in following way: FN is a function in at least two args where the second must be optional; the returned function behaves like FN, but returns NIL for all those args which would generate an error if applied to FN."
  (compile nil `(lambda (x &optional y)
                  (or (ignore-errors (funcall ,fn x y))
                      (ignore-errors (funcall ,fn x))
                      (ignore-errors (funcall ,fn y))))))

(flet ((make-reducing-fn (reducing-fn new-col extr-fn &aux (fn (make-generalized-binary-fn reducing-fn)))
         (compile nil `(lambda (new-row row)
                         (funcall ,fn
                                  (entry new-row ,new-col)
                                  (funcall ,extr-fn row))))))
  (defun group-from* (group-names group-fns column-names reducing-fns extraction-fns table test)
    "Create and return a new table with columns group-names and column-names and whose rows are generated from the rows of the table TABLE in following way: All rows from TABLE which satisfy the predicate TEST are grouped by comparing with CL:EQL the results of applying them GROUP-FNS and for each such group a row is created. The columns of that row designated by group-names contain the result of each corresponding function in GROUP-FNS. (Hence GROUP-NAMES and GROUP-FNS must be of same length.) The columns designated by COLUMN-NAMES are the result of the reduction over the corresponding group by the corresponding function (generalized by MAKE-GENERALIZED-BINARY-FN) in REDUCING-FNS. (Hence COLUMN-NAMES and REDUCING-FNS must be of same length.)"
    (let* ((new-table (make-instance 'physical-table))
           (row-hash (oam:make-hash-tree :test #'equal))
           (hash-lock (bt:make-lock "HASH-LOCK"))
           (grp-cols (loop :for col-name :in group-names
                        :collect (make-instance 'physical-column :table new-table :name col-name)))
           (red-cols (loop :for col-name :in column-names
                        :collect (make-instance 'physical-column :table new-table :name col-name)))
           (red-fns (mapcar #'make-reducing-fn reducing-fns red-cols extraction-fns)))
      (oam::pmap (compile nil `(lambda (row)
                                 (when (funcall ,test row)
                                   (let* ((keys (mapcar (lambda (fn) (funcall fn row)) ',group-fns)))
                                     (destructuring-bind (new-row . entry-locks)
                                         (let (fresh-row)
                                           (prog1
                                               (bt:with-lock-held (,hash-lock)
                                                 (or (oam:get-hash* ,row-hash keys)
                                                     (let ((entry-locks (mapcar (oam:f-comp #'bt:make-lock #'db::name) ',red-cols))
                                                           (new-row (make-instance 'physical-row :table ,new-table)))
                                                       (setf fresh-row new-row
                                                             (oam:get-hash* ,row-hash keys) (cons new-row entry-locks)))))
                                             (when fresh-row (loop :for key :in keys :for col :in ',grp-cols
                                                                :do (setf (entry fresh-row col) key)))))
                                       (loop :for fn :in ',red-fns :for col :in ',red-cols :for lock :in entry-locks
                                          :do (bt:with-lock-held (lock)
                                                (setf (entry new-row col) (funcall fn new-row row)))))))))
                 (rows table))
      new-table)))

(defun select-from** (group-names group-fns column-names reducing-fns extraction-fns table test)
  (if group-names
      (group-from* group-names group-fns column-names reducing-fns extraction-fns table test)
      (select-from* column-names extraction-fns table test)))

;;;;Sketch for implementing a group-by select statement:
#+(or)
(let ((rows '((1 2 3 4)
              (5 6 7 8)
              (5 7 'a 9)
              (5 4 8 4)
              (9 0 1 2)
              (9 1 2 3)
              (0 2 3 4))))
  (let ((new-rows (oam:make-hash-tree)))
    (let ((col-defs (list (lambda (row)
                            (declare (special new-row))
                            (or (ignore-errors (max (elt new-row 1) (elt row 1)))
                                (elt new-row 1)
                                (ignore-errors (max (elt row 1)))))
                          (lambda (row)
                            (declare (special new-row))
                            (or (ignore-errors (+ (elt new-row 2) (elt row 2)))
                                (elt new-row 2)
                                (ignore-errors (+ (elt row 2)))))
                          (lambda (row)
                            (declare (special new-row))
                            (or (ignore-errors (* (elt new-row 3) (elt row 3)))
                                (elt new-row 3)
                                (ignore-errors (* (elt row 3))))))))
      (loop :for row :in rows
         :do (let ((a (elt row 0)))
               (let ((new-row (or (oam:get-hash new-rows a) (make-array 4 :initial-element nil)))) 
                 (declare (special new-row))
                 (setf (elt new-row 0) a)
                 (loop :for i :from 1 :to 3
                    :do (setf (elt new-row i) (funcall (elt col-defs (1- i)) row)))
                 (setf (oam:get-hash new-rows a) new-row))))
      (let (result)
        (oam:map-hash (lambda (key &optional value)
                        (when value (push value result)))
                      new-rows)
        result))))



(defun db::join* (left-table right-table test &key type left-table-name right-table-name)
  "Create and return a new table resulting from the join of LEFT-TABLE and RIGHT-TABLE on the predicate function TEST. TEST must take two ROW objects. The key :TYPE can be one of :outer :left :right or nil (the default) indicating that the join is respectively an outer, a left-outer, a right-outer or an inner join."
  (let ((joined-table (make-instance 'joined-table))
        (left-columns (reverse (columns left-table)))
        (right-columns (reverse (columns right-table))))
    (loop :for col :in left-columns
       :do (make-instance 'virtual-column :table joined-table :name (oam:symb left-table-name "." (db::name col))
                          :access-fn (compile nil `(lambda (row) (entry (car (entries row)) ,col)))))
    (loop :for col :in right-columns
       :do (make-instance 'virtual-column :table joined-table :name (oam:symb right-table-name "." (db::name col))
                          :access-fn (compile nil `(lambda (row) (entry (cdr (entries row)) ,col)))))
    (let* ((right-join-p (member type '(:right :outer)))
           (left-join-p  (member type '(:left :outer)))
           (rows1 (slot-value (if right-join-p right-table left-table) 'rows))
           (rows2 (slot-value (if right-join-p left-table right-table) 'rows)))
      (oam:fbind ((test (compile nil (if right-join-p
                                         `(lambda (row1 row2)
                                            (funcall ,test row2 row1))
                                         `(lambda (row1 row2)
                                            (funcall ,test row1 row2)))))
                  (add-joined-rows (compile nil (if right-join-p
                                                    `(lambda (row1 row2)
                                                       (make-instance 'joined-row :table ,joined-table
                                                                      :left-row row2 :right-row row1))
                                                    `(lambda (row1 row2)
                                                       (make-instance 'joined-row :table ,joined-table
                                                                      :left-row row1 :right-row row2))))))
        (loop :for row1 :across rows1
           :do (loop :with empty-p := t
                  :for row2 :across rows2
                  :do (when (test row1 row2)
                        (setq empty-p nil)
                        (add-joined-rows row1 row2))
                  :finally (when (and (or right-join-p left-join-p)
                                      empty-p)
                             (add-joined-rows row1 nil))))
        (when (and right-join-p left-join-p)
          (loop :for row2 :across rows2
             :do (when (notany (lambda (row1) (test row1 row2)) rows1)
                   (add-joined-rows nil row2))))))
    joined-table))



(deftype oam::variable () '(and symbol (not (satisfies constantp))))
(defun oam::variablep (x) (typep x 'oam::variable))
(defun oam::var-binding-pair (pair)
  "Return (list PAIR PAIR) if PAIR is of type oam::variable, return PAIR if PAIR is a pair with first element of type oam::variable or signals an error of type type-error otherwise."
  (etypecase pair
    (oam::variable (list pair pair))
    ((cons oam::variable (cons * null)) pair)))

(labels ((col-name-p (table-names object)
           "Return a list (OBJECT T C) if OBJECT is a symbol whose name is of the form T.C where T is one of the names in TABLE-NAMES and C is the name of a column of table T, and NIL else."
           (and (oam::variablep object)
                (loop :with obj-name := (string object)
                   :with obj-name-length := (length obj-name)
                   :for table-name :in table-names
                   :for prefix := (oam:mkstr table-name ".")
                   :for prefix-length := (length prefix)
                   :when (string= prefix obj-name :end2 (min prefix-length obj-name-length))
                   :do (return (list object table-name (subseq obj-name prefix-length))))))
         (extract-col-names (table-names expression)
           "Return a list of tripplets (column-symbol table-symbol column-name) where column-symbol is the symbol representing a column as found in EXPRESSION, table-symbol is the symbol naming the table to which the column belongs and column-name is the string-name of the column in that table."
           (loop :for symb :in (remove-duplicates (oam:flatten (list expression)))
              :when (setq symb (col-name-p table-names symb)) :collect symb))
         (make-expression-parser (table-names table-g!syms)
           "Return a function taking an expression and returning a new expression which creates a function in one variable which is a row and blabla...
Return a function parsing an expression by associating a symbol of the form N.C to a symbol-macrolet"
           (let* ((tbl-rowvar-alist (mapcar (lambda (table-symbol)
                                              (cons table-symbol (gensym (oam:mkstr "ROW-" table-symbol))))
                                            table-names))
                  (tbl-name-g!sym-alist (mapcar #'cons table-names table-g!syms))
                  (rowvars (mapcar #'cdr tbl-rowvar-alist)))
             (compile nil
                      `(lambda (expression)
                         (let* ((col-names (funcall ,#'extract-col-names ',table-names expression))
                                (g!col-vars (mapcar (oam:f-comp #'gensym #'string #'first) col-names))
                                (col-bindings (mapcar (oam:dlambda ((col-symb tbl-symb col-name) g!col)
                                                        (declare (ignore col-symb))
                                                        `(,g!col (column ,(cdr (assoc tbl-symb ',tbl-name-g!sym-alist)) ,col-name)))
                                                      col-names g!col-vars)))
                           `(let (,@col-bindings)
                              (lambda (,@',rowvars)
                                (let (,@(mapcar (oam:dlambda ((col-symb tbl-symb col-name) g!col)
                                                  (declare (ignore col-name))
                                                  `(,col-symb (entry ,(cdr (assoc tbl-symb ',tbl-rowvar-alist)) ,g!col)))
                                                col-names g!col-vars))
                                  ,expression)))))))))
  (defmacro db::join ((left-table right-table &key type) &key on)
    "Create and return a new table resulting from the join of LEFT-TABLE and RIGHT-TABLE on test forms of the ON-CLAUSE. LEFT-TABLE and RIGHT-TABLE are either a symbol which is bound to a table or a pair (symbol form) where form evaluates to a table. In the lexical environment of the ON-CLAUSE, those symbols are bound to the respective table. Furthermore, in this lexical environment, the symbols of the form X.Y where X is the name of one of the before mentioned symbols are bound to the entry of the row corresponding to the column named Y of table X when processing all rows of the tables to be joined. The key :TYPE can be one of :outer :left :right or nil (the default) indicating that the join is respectively an outer, a left-outer, a right-outer or an inner join."
    (let* ((left-table-binding (oam::var-binding-pair left-table))
           (right-table-binding (oam::var-binding-pair right-table))
           (left-table-name (car left-table-binding))
           (right-table-name (car right-table-binding))
           (left-table-g!sym (gensym "LEFT-TABLE"))
           (right-table-g!sym (gensym "RIGHT-TABLE"))
           (table-g!binds (list (list left-table-g!sym left-table-name)
                                (list right-table-g!sym right-table-name))))
      (oam:fbind ((parse (make-expression-parser (list left-table-name right-table-name) (list left-table-g!sym right-table-g!sym))))
        `(let (,left-table-binding
               ,right-table-binding
               ,@table-g!binds)
           (db::join* ,left-table-name ,right-table-name
                      ,(parse on)
                      :type ,type
                      :left-table-name ',left-table-name
                      :right-table-name ',right-table-name)))))
  (defmacro db::select-from (table (&rest columns) &key where order-by group-by)
    "Create and return a new table with columns COLUMN-NAMES and whose rows are generated by mapping the rows of the table TABLE which satisfy the predicate TEST to the each function of the list of functions FNS in respective order."
    (declare (ignore order-by))
    (let* ((table-binding (oam::var-binding-pair table))
           (table-name (car table-binding))
           (table-g!sym (gensym "TABLE"))
           (table-g!bind (list table-g!sym table-name)))
      (oam:fbind ((parse (make-expression-parser (list table-name) (list table-g!sym))))
        (let* ((group-bindings (mapcar (oam:f-comp #'oam::var-binding-pair
                                                   (lambda (col)
                                                     (typecase col
                                                       (oam::variable
                                                        (list (make-symbol (or (second (oam::split-string #\. (string col)
                                                                                                          :max-chunks 2))
                                                                               (error "Invalid column name: ~S." col)))
                                                              col))
                                                       (t col))))
                                       group-by))
               (group-expressions (mapcar #'second group-bindings))
               (group-fns (mapcar (let (extra-bindings)
                                    (lambda (extra-bind expr)
                                      (prog1
                                          (parse (if extra-bindings
                                                     `(let (,@extra-bindings)
                                                        (declare (ignorable ,@(mapcar #'car extra-bindings)))
                                                        ,expr)
                                                     expr))
                                        (push extra-bind extra-bindings))))
                                  group-bindings group-expressions))
               (group-names (mapcar #'first group-bindings))
               (new-col-bindings (mapcar (oam:f-comp #'oam::var-binding-pair
                                                     (lambda (col)
                                                       (typecase col
                                                         (oam::variable
                                                          (list (make-symbol (or (second (oam::split-string #\. (string col)
                                                                                                            :max-chunks 2))
                                                                                 (error "Invalid column name: ~S." col)))
                                                                col))
                                                         (t col))))
                                         columns))
               (new-col-expressions (mapcar (if group-by
                                                (lambda (x &aux (x (second x)))
                                                  (and (equal "REDUCE" (string (first x)))
                                                       (third x)))
                                                #'second)
                                            new-col-bindings))
               (new-col-reduction-fns (when group-by (mapcar #'cadadr new-col-bindings)))
               (new-col-extraction-fns (mapcar (if group-by
                                                   (lambda (extra-bind expr)
                                                     (declare (ignore extra-bind))
                                                     (parse expr))
                                                   (let (extra-bindings)
                                                     (lambda (extra-bind expr)
                                                       (prog1
                                                           (parse (if extra-bindings
                                                                      `(let (,@extra-bindings)
                                                                         (declare (ignorable ,@(mapcar #'car extra-bindings)))
                                                                         ,expr)
                                                                      expr))
                                                         (push extra-bind extra-bindings)))))
                                               new-col-bindings new-col-expressions))
               (new-col-names (mapcar #'first new-col-bindings)))
          `(let* (,table-binding
                  ,table-g!bind)
             (select-from** ',group-names
                            (list ,@group-fns)
                            ',new-col-names
                            (list ,@new-col-reduction-fns)
                            (list ,@new-col-extraction-fns)
                            ,table-g!sym
                            ,(if where
                                 (parse `(let (,@(if group-by
                                                     group-bindings
                                                     new-col-bindings))
                                           (declare (ignorable ,@(mapcar #'car (if group-by
                                                                                   group-bindings
                                                                                   new-col-bindings))))
                                           ,where))
                                 (compile nil '(lambda (row) (declare (ignore row)) t))))))))))


;;;;** Load Tables from CSV-Files
(flet ((make-insert-fn (table column-names)
         (let ((insert (db::make-inserter table column-names)))
           (funcall (compile nil `(lambda ()
                                    (let (entries)
                                      (lambda (item eolp)
                                        (push item entries)
                                        (when eolp
                                          (funcall ,insert (nreverse entries))
                                          (setq entries nil))))))))))
  (defun db::load-table-from-csv (stream &key delimiters eol escape-char)
    (let (table fn)
      (let ((header-fn (let (column-names)
                         (lambda (item eolp)
                           (push (oam:symb item) column-names)
                           (when eolp
                             (setq column-names (nreverse column-names)
                                   table (db::make-table* column-names)
                                   fn (make-insert-fn table column-names)))))))
        (setq fn header-fn))
      (apply #'csv:map-items-of-csv-rows (lambda (item eolp)
                                           (funcall fn item eolp))
             stream
             (append (when delimiters
                       (list :delimiters delimiters))
                     (when eol
                       (list :eol eol))
                     (when escape-char
                       (list :escape-char escape-char))))
      table)))


(oam:export-interface "DB")

;;;;==============================================
#||


(defstruct (data (:conc-name data.))
  source
  ticker
  type
  point-id
  spec-id)



(defstruct (data-spec (:conc-name data-spec.))
  id
  key
  creation-date
  value
  creator)

(defstruct (data-point (:conc-name data-point.))
  id
  ref-date
  creation-date
  value
  creator)

(defvar *data-points*)
(defvar *ids*)
(defvar *data-specs*)

(defmacro defcounters (&body names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(let ((count -1))
                    (defun ,name () (incf count))))
               names)
     nil))

(defcounters
    new-point-id
  new-spec-id)




(defun get-id-struct (source ticker)
  (let ((ids (remove-if (lambda (elt)
                          (and (eq source (data.source elt))
                               (eq ticker (data.ticker elt))))
                        *ids*)))
    (when (< 1 (length ids))
      (error "Corrupt table!!"))
    (first ids)))

(defun get-spec-struct (&key id param-key))


(defun find-all-if (test seq &rest ))


(defun currencyp (o)
  (member o '(:chf :eur :usd)))
(deftype type-id ()
  '(satisfies currencyp))


(defgeneric db::define-data-point-spec (source ticker type-id &key &allow-other-keys))

(defmethod db::define-data-point-spec ((data-type (eql :spot-fx)) source ticker
                                   &key from-currency to-currency)
  (let ((id-struct (get-id-struct source ticker)))
    (if id-struct
        (error "SOURCE = ~A and TICKER =~A already allocated." source ticker)
        (make-data :source source
                   :ticker ticker
                   :point-id (new-point-id)
                   :type data-type
                   :spec-id (or (get-spec-struct))))))

||#