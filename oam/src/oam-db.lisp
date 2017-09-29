(oam:define-project-package "CH.AMANN-WOLOWYK.OAM-DB" "DB")

(in-package "CH.AMANN-WOLOWYK.OAM-DB-SYSTEM")


(define-condition db::unsupported-operation (error) ())

(define-condition db::invalid-table-name (error)
  ((name :initform (error "No name given.") :reader db::name :initarg :name))
  (:report (lambda (c s)
             (format s "The Name ~S is invalid for a table. A table name should contain no dot (.)." (db::name c))))
  (:documentation "Error condition signaled when attempting to create an invalid table name."))

(define-condition db::invalid-column-name (error)
  ((name :initform (error "No name given.") :reader db::name :initarg :name)
   (table :initform (error "No table given.") :reader db::table :initarg :table))
  (:report (lambda (c s)
             (format s "Invalid column name ~S in table ~A." (db::name c) (db::table c))))
  (:documentation "Error condition signaled when attempting to create an invalid column name."))

(define-condition db::no-such-column-error (db::invalid-column-name) ()
  (:report (lambda (c s)
             (format s "No column with name ~S in table ~A." (db::name c) (db::table c))))
  (:documentation "Error condition signaled when attempting to reference a column with a non-existant name."))

(define-condition db::column-name-already-used-error (db::invalid-column-name)
  ((column :initform (error "No column given.") :reader db::column :initarg :column))
  (:report (lambda (c s)
             (format s "Name ~S already used for column ~S in table ~A." (db::name c) (db::column c) (db::table c))))
  (:documentation "Error condition signaled when attempting to create a column name which is already attributed to some column of the table."))



;;;;** Table
(defun make-map-column<-name ()
  "Return a map column-name -> column to be stored in the slot COLUMN<-NAME of a table object."
  (let ((name->column (make-hash-table :test #'equal #+sbcl :weakness #+(or clisp ccl) :weak #+(or sbcl clisp ccl) :value)))
    (compile nil `(lambda (action name &optional column)
                    "Depending on ACTION do following:
:get Return the column associated to the name NAME; COLUMN is ignored;
:set Set the association of the name NAME to the column COLUMN;
:rem Remove the association of NAME; COLUMN is ignored; Return the associated COLUMN or NIL."
                    (declare (keyword action)
                             (string name)
                             (optimize speed))
                    (ecase action
                      (:get (gethash (the string name) ,name->column))
                      (:set (setf (gethash (the string name) ,name->column) column))
                      (:rem (prog1 (gethash (the string name) ,name->column)
                              (remhash (the string name) ,name->column))))))))

(defun make-map-name<-column ()
  "Return a map column -> column-name to be stored in the slot NAME<-COLUMN of a table object."
  (let ((column->name (make-hash-table :test #'eq #+sbcl :weakness #+(or clisp ccl) :weak #+(or sbcl clisp ccl) :key)))
    (compile nil `(lambda (action column &optional name)
                    "Depending on ACTION do following:
:get Return the column associated to the name COLUMN; NAME is ignored;
:set Set the association of COLUMN to NAME;
:rem Remove the association of COLUMN; NAME is ignored; return COLUMN;"
                    (declare (keyword action)
                             (optimize speed))
                    (ecase action
                      (:get (gethash column ,column->name))
                      (:set (setf (gethash column ,column->name) name))
                      (:rem (remhash column ,column->name)
                            column))))))

(defclass db::table ()
  ((columns :initform (cons (bt:make-lock) nil) :reader columns
            :documentation "Contains the list of column headers. It is implemented in inverse order so that the last column is first.") 
   (column<-name :initform (make-map-column<-name) :reader column<-name
                 :documentation "Map containing the associations column name -> column for fast access.")
   (name<-column :initform (make-map-name<-column) :reader name<-column
                 :documentation "Map containing the associations column name -> column for fast access.")
   (rows :initform (make-array 0 :adjustable t :fill-pointer t) :reader rows :documentation "Contains the rows of the table.")))


(defclass db::physical-table (db::table)
  ((lock :initform (bt:make-lock) :reader lock)
   (deleted-columns :initform (oam:make-queue) :reader deleted-columns)))

(defun db::make-table* (column-names)
  (let ((table (make-instance 'db::physical-table)))
    (map nil (lambda (name)
               (make-physical-column table (string name)))
         column-names)
    table))
(defun db::make-table (&rest column-names)
  (db::make-table* column-names))


(defclass db::virtual-table (db::table) ())


(defun db::column-count (table)
  (length (cdr (columns table))))
(defun column-iterator (table)
  (oam:make-generator-from-list (reverse (cdr (columns table)))))
(defun db::column-iterator (table)
  (let ((col-iter (column-iterator table)))
    (lambda ()
      (column-name table (funcall col-iter)))))
(defun db::row-count (table)
  (length (rows table)))


(defun add-row (table row)
  (vector-push-extend row (rows table))
  row)





;;;;** Columns

(declaim (inline column))
(defun column (table name &aux (column<-name (column<-name table)))
  "Return the column assosciated to NAME. If none such exists, signal an error of type DB::NO-SUCH-COLUMN-ERROR."
  (declare (string name)
           ((function (keyword string)) column<-name)
           (optimize speed))
  (or (funcall column<-name :get name)
      (error 'db::no-such-column-error :table table :name name)))

(defsetf column (table name) (value)
  `(let ((columns (columns ,table)))
     (bt:with-lock-held ((car columns))
       (let ((old-col (ignore-errors (column ,table ,name))))
         (unless (eq ,value old-col)
           (when old-col
             (setf (cdr columns) (delete old-col (cdr columns)))
             (oam:enqueue (deleted-columns ,table) old-col)
             (funcall (column<-name ,table) :rem ,name)
             (funcall (name<-column ,table) :rem old-col))
           (when ,value
             (pushnew ,value (cdr columns))
             (funcall (column<-name ,table) :set ,name ,value)
             (funcall (name<-column ,table) :set ,value ,name)))))
     ,value))

(defun column-name (table column &aux (name<-column (name<-column table)))
  "Return the name of COLUMN in the table TABLE."
  (declare ((function (keyword *)) name<-column)
           (optimize speed))
  (or (funcall name<-column :get column)
      (error "Column ~S not registered in table ~S." column table)))


(defun check-column-name (table name)
  "Check if the name is a valid column name i.e. not already used. If not valid then signal an error of type DB::COLUMN-NAME-ALREADY-USED-ERROR and provide the restart NEW-NAME."
  (loop (restart-case
            (if (stringp name)
                (let ((column (ignore-errors (column table name))))
                  (if column
                      (error 'db::column-name-already-used-error :table table :column column :name name)
                      (return name)))
                (error 'db::invalid-column-name :table table :name name))
          (db::new-name (new-name)
            :report "Use a new name."
            :interactive (lambda ()
                           (princ "Enter a new name: ")
                           (list (eval (read))))
            (setq name new-name)))))

(defsetf column-name (table column) (value)
  `(progn
     (if ,column
         (let ((old-name (column-name table column))
               (new-name (check-column-name ,table ,value)))
           (unless old-name
             (pushnew ,column (cdr (columns ,table))))
           (unless (equal old-name new-name)
             (funcall (column<-name ,table) :rem old-name)
             (funcall (column<-name ,table) :set new-name ,column)
             (funcall (name<-column ,table) :set ,column new-name)) 
           new-name)
         (error "Invalid column ~S." ,column))
     ,value))

(defmacro make-column (table name (entries value) &key getter setter)
  "Create a new column instance and add it to the table TABLE under the name NAME after checking for the validity of NAME. The keywords GETTER and SETTER allow to define the code used to get and set a value from a row; for this the variables given at the places ENTRIES and VALUE are lexically bound to the entries of a row and the value to set the corresponding entry to in the setter code. If SETTER is not used (setf GETTER <VALUE>) is used instead."
  (let ((g!table (gensym "TABLE"))
        (g!name (gensym "NAME"))
        (g!valuep (gensym "VALUEP")))
    `(let* ((,g!table ,table)
            (,g!name (check-column-name ,g!table ,name)))
       (setf (column ,g!table ,g!name)
             (lambda (,entries &optional (,value nil ,g!valuep))
               (declare (optimize speed)
                        (ignorable ,entries ,value))
               (if ,g!valuep
                   ,(if setter
                        setter
                        `(setf ,getter ,value))
                   ,getter))))))

(defun make-physical-column (table name)
  (or (oam:dequeue (deleted-columns table))
      (let ((index (db::column-count table)))
        (make-column table name (entries value)
                     :getter (svref entries index)))))

(defun make-join-column (table name column &optional leftp)
  (let ((splitter (if leftp #'car #'cdr)))
    (make-column table name (entries value)
                 :getter (let ((entries (funcall splitter entries)))
                           (when entries
                             (funcall column entries)))
                 :setter (error 'db::unsupported-operation))))

;;;;** Rows
(defun make-row (table entries)
  (add-row table (lambda (&optional column (value nil valuep))
                   (if column
                       (if valuep
                           (funcall column entries value)
                           (funcall column entries))
                       entries))))
(declaim (inline entries))
(defun entries (row)
  (declare ((function ()) row)
           (optimize speed))
  (funcall row))
(defun make-physical-row (table)
  (let ((entries (make-array (db::column-count table) :initial-element nil)))
    (make-row table entries)))
(defun join-rows (table left-row right-row)
  (let ((entries (cons (entries left-row) (entries right-row))))
    (make-row table entries)))

;;;;** Entry
(declaim (inline entry))
(defun entry (row col)
  (declare ((function (*)) row)
           (optimize speed))
  (funcall row col))

(defsetf entry (row col) (value)
  (declare (optimize speed))
  `(funcall ,row ,col ,value))





;;;;** Insert -----------------------------------

(defun set-row (row columns values)
  (declare (optimize speed))
  (map nil (lambda (col val)
             (setf (entry row col) val))
       columns values))

(defgeneric insert-row (table columns values)
  (:method (table columns values) (declare (ignore table columns values)) (error 'db::unsupported-operation)))
(defmethod insert-row ((table db::physical-table) columns values)
  (let ((row (make-physical-row table)))
    (set-row row columns values)))

(defgeneric db::make-inserter (table column-names)
  (:method (table column-names) (declare (ignore table column-names)) (error 'db::unsupported-operation)))
(defmethod db::make-inserter ((table db::physical-table) column-names)
  "Return a function taking a list of values and inserting into table TABLE a row whose entries are each value of the list of values for the corresponding column in the list COLUMN-NAMES; those entries for which no value have been given, NIL is set."
  (let ((columns (mapcar (lambda (name)
                           (funcall (column<-name table) name))
                         column-names)))
    (compile nil `(lambda (values)
                    (set-row (make-physical-row ,table) ',columns values)))))

(defun db::insert* (table column-names values)
  "Insert into table a row whose entries are for each column in the list COLUMN the value in the list VALUES which has the same position."
  (insert-row table (mapcar (lambda (col-name) (column table (string col-name))) column-names) values))

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
;;;;*** Select-from
(defun select-from* (column-names fns table test)
  "Create and return a new table with columns whose names are the strings in COLUMN-NAMES and whose rows are generated by mapping the rows of the table TABLE which satisfy the predicate TEST to the each function of the list of functions FNS in respective order."
  (let ((new-table (make-instance 'db::virtual-table)))
    (map nil
         (compile nil
                  `(lambda (col-name fn)
                     (make-column ,new-table col-name (entries value)
                                  :getter (funcall fn entries)
                                  :setter (error 'db::unsupported-operation))))
         column-names
         fns)
    (oam::pmap
         (compile nil
                  `(lambda (row)
                     (when (funcall ,test row)
                       (make-row ,new-table row))))
         (rows table))
    new-table))
;;;;*** Group
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
    (let* ((new-table (make-instance 'db::physical-table))
           (row-hash (oam:make-hash-tree :test #'equal))
           (hash-lock (bt:make-lock "HASH-LOCK"))
           (grp-cols (loop :for col-name :in group-names
                        :collect (make-physical-column new-table col-name)))
           (red-cols (loop :for col-name :in column-names
                        :collect (make-physical-column new-table col-name)))
           (red-fns (mapcar #'make-reducing-fn reducing-fns red-cols extraction-fns)))
      (oam::pmap (compile nil `(lambda (row)
                                 (when (funcall ,test row)
                                   (let* ((keys (mapcar (lambda (fn) (funcall fn row)) ',group-fns)))
                                     (destructuring-bind (new-row . entry-locks)
                                         (let (fresh-row)
                                           (prog1
                                               (bt:with-lock-held (,hash-lock)
                                                 (or (oam:get-hash* ,row-hash keys)
                                                     (let ((entry-locks (mapcar (lambda (red-col) (bt:make-lock (column-name ,new-table red-col)))
                                                                                ',red-cols))
                                                           (new-row (make-physical-row ,new-table)))
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






(defun db::join* (left-table right-table test &key type left-table-name right-table-name)
  "Create and return a new table resulting from the join of LEFT-TABLE and RIGHT-TABLE on the predicate function TEST. TEST must take two ROW objects. The key :TYPE can be one of :outer :left :right or nil (the default) indicating that the join is respectively an outer, a left-outer, a right-outer or an inner join."
  (declare (ignore left-table-name))
  (let ((joined-table (make-instance 'db::virtual-table)))
    (loop :with left-columns := (column-iterator left-table)
       :for col := (oam:next left-columns)
       :do (make-join-column joined-table (db::name col) col t))
    (loop :with right-columns := (column-iterator right-table)
       :for col := (oam:next right-columns)
       :do (handler-bind ((db::column-name-already-used-error (lambda (c)
                                                                (invoke-restart (find-restart 'db::new-name c)
                                                                                (oam:mkstr right-table-name "." (db::name c))))))
               (make-join-column joined-table (db::name col) col nil)))
    (let* ((right-join-p (member type '(:right :outer)))
           (left-join-p  (member type '(:left :outer)))
           (rows1 (rows (if right-join-p right-table left-table)))
           (rows2 (rows (if right-join-p left-table right-table))))
      (oam:fbind ((test (compile nil (if right-join-p
                                         `(lambda (row1 row2)
                                            (declare (optimize speed))
                                            (funcall ,test row2 row1))
                                         `(lambda (row1 row2)
                                            (declare (optimize speed))
                                            (funcall ,test row1 row2)))))
                  (add-joined-rows (compile nil (if right-join-p
                                                    `(lambda (row1 row2)
                                                       (declare (optimize speed))
                                                       (join-rows ,joined-table row2 row1))
                                                    `(lambda (row1 row2)
                                                       (declare (optimize speed))
                                                       (join-rows ,joined-table row1 row2))))))
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



;;;;* Convenience Macros

(deftype oam::variable () '(and symbol (not (satisfies constantp))))
(defun oam::variablep (x) (typep x 'oam::variable))
(defun oam::var-binding-pair (pair)
  "Return (list PAIR PAIR) if PAIR is of type oam::variable, return PAIR if PAIR is a pair with first element of type oam::variable or signals an error of type type-error otherwise."
  (etypecase pair
    (oam::variable (list pair pair))
    ((cons oam::variable (cons * null)) pair)))


(defun check-table-name (name)
  "Check if NAME contains no occurence of SEPARATOR."
  (loop (if (or (not (oam::variablep name))
                (find #\. (string name) :test #'char=))
            (restart-case
                (error 'db::invalid-table-name :name name)
              (new-name (new-name)
                :report "Use a new name."
                :interactive (lambda ()
                               (princ "Enter a new name: ")
                               (list (eval (read))))
                (setq name new-name)))
            (return name))))


;;;;** Result Set Cursor


(defun table-cursor (table)
  (oam::make-cursor-from-vector (rows table)))


(defmacro db::with-table-cursor ((table &key cursor-name (separator ".")) &body body)
  "Create a lexical environment in which the columns of the table TABLE are bound to symbols of the form P::T.C, where P::T is the symbol locally bound to TABLE, either by refering within WITH-COLUMNS to TABLE using that symbol or by explicitly binding the symbol to the table by writing at the place of TABLE the list (P::T <table-reference>), and C is the name of the column. The entries accessed by the columns are those of the current state of the row-cursor. The initial state is the first row. For navigating within the rows the locally defined functions NEXT, BACK and USE-ROW can be used."
  (let* ((table-binding (oam::var-binding-pair table))
         (table-binding (list (check-table-name (first table-binding)) (second table-binding)))
         (table-var (first table-binding))
         (cursor-name (or cursor-name (intern (oam:mkstr "ROWS-" table-var))))
         (tbl-var-str (string table-var))
         (package (unless (find-symbol tbl-var-str)
                    (symbol-package table-var)))
         (col-symbs.names (remove-duplicates (mapcan (lambda (symbol)
                                                       (let* ((sym-str (string symbol))
                                                              (split-sym-str (oam:split-string separator sym-str))
                                                              (col-name (second split-sym-str)))
                                                         (when (and (find-symbol sym-str package)
                                                                    (string= tbl-var-str (first split-sym-str))
                                                                    (< 0 (length col-name)))
                                                           (list (cons symbol col-name)))))
                                                     (oam:flatten body))
                                             :key #'car))
         (g!cols (mapcar (lambda (c)
                           (gensym (string (car c))))
                         col-symbs.names))
         (g!table (gensym "TABLE"))
         (g!cursor (gensym "CURSOR")))
    `(let* ((,g!table ,(second table-binding))
            (,g!cursor (table-cursor ,g!table))
            (,cursor-name ,g!cursor))
       (let (,@(mapcar (lambda (g-col col-symb.name)
                         `(,g-col (funcall ,#'column ,g!table ,(cdr col-symb.name))))
                       g!cols col-symbs.names))
         (symbol-macrolet
             (,@(mapcar (lambda (g-col col-symb.name)
                          `(,(car col-symb.name) (entry (oam:current ,g!cursor) ,g-col)))
                        g!cols col-symbs.names))
           ,@body)))))


;;;; Join and Select-from


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
  (flet ((col-bind (col)
           (oam::var-binding-pair (typecase col
                                    (oam::variable
                                     (list (make-symbol (or (second (oam::split-string #\. (string col)
                                                                                       :max-chunks 2))
                                                            (error 'db::invalid-column-name :table nil :name (string col))))
                                           col))
                                    (t col)))))
    (defmacro db::select-from (table (&rest columns) &key where order-by group-by)
      "Create and return a new table with columns COLUMNS and whose rows are generated by mapping the rows of the table TABLE which satisfy the predicate TEST to the each function of the list of functions FNS in respective order."
      (let* ((table-binding (oam::var-binding-pair table))
             (table-name (car table-binding))
             (table-g!sym (gensym "TABLE"))
             (table-g!bind (list table-g!sym table-name))
             (new-table-g!sym (gensym "NEW-TABLE")))
        (oam:fbind ((parse (make-expression-parser (list table-name) (list table-g!sym))))
          (flet ((make-function-binder ()
                   (let (extra-bindings)
                     (lambda (extra-bind expr)
                       (prog1
                           (parse (if extra-bindings
                                      `(let (,@extra-bindings)
                                         (declare (ignorable ,@(mapcar #'car extra-bindings)))
                                         ,expr)
                                      expr))
                         (push extra-bind extra-bindings))))))
            (let* ((group-bindings (mapcar #'col-bind group-by))
                   (group-expressions (mapcar #'second group-bindings))
                   (group-fns (mapcar (make-function-binder)
                                      group-bindings group-expressions))
                   (group-names (mapcar (lambda (col-bind) (string (first col-bind))) group-bindings))
                   (new-col-bindings (mapcar #'col-bind columns))
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
                                                       (make-function-binder))
                                                   new-col-bindings new-col-expressions))
                   (new-col-names (mapcar (lambda (col-bind) (string (first col-bind))) new-col-bindings)))
              `(let* (,table-binding
                      ,table-g!bind
                      (,new-table-g!sym (select-from** ',group-names
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
                                                            (compile nil '(lambda (row) (declare (ignore row)) t))))))
                 ,(when order-by
                        `(let ((pred (make-sort-pred ',order-by ,new-table-g!sym
                                                     ',(append group-names new-col-names))))
                           (setf (slot-value ,new-table-g!sym 'rows) (sort (rows ,new-table-g!sym) pred))))
                 ,new-table-g!sym))))))))
(defun make-sort-pred (expr table col-names)
  (let ((g!row1 (gensym "ROW1"))
        (g!row2 (gensym "ROW2"))
        (colnames (intersection (oam:flatten expr) col-names
                                :key #'string :test #'equal)))
    (compile nil `(lambda (,g!row1 ,g!row2)
                    (macrolet
                        ((db::compare (pred expr)
                           `(funcall ,pred
                                     (symbol-macrolet
                                         ,',(mapcar (lambda (col)
                                                      `(,col (entry ,g!row1 ,(column table (string col)))))
                                                    colnames)
                                       ,expr)
                                     (symbol-macrolet
                                         ,',(mapcar (lambda (col)
                                                      `(,col (entry ,g!row2 ,(column table (string col)))))
                                                    colnames)
                                       ,expr))))
                      ,expr)))))

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
