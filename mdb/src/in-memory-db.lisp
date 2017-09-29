(in-package "MDB-SYSTEM")



(defclass repository ()
  (cadb
   commits
   refs))



(defun contains-p (repository item))

(defun find-branch (repository name))

(defun find-tag (repository name))

(defun find-commit (repository name))

(defun list-branches (repository))
(defun list-tags (repository))
(defun list-commits (repository))

(defun enum-items (repository))


(defclass index ()
  (repository))

(defun ())

(defclass commit ()
  ((index :type index)
   (parents :type list)
   info))

(defun make-staging-area (commit)
  (make-instance 'staging-area 'commit commit))

(defclass staging-area ()
  ((commit :initarg commit)
   (addings :initform (make-hash-table))
   (removals :initform nil)))

(defun add-item (staging-area key item)
  "Add ITEM to the STAGING-AREA associating to KEY. Return `true' as primary value, if ITEM has been newly added to the STAGING-AREA, return `true' as secondary value, if ITEM is also new to the repository's data-base. Return NIL else."
  (with-slots (addings)
      staging-area
    (setf (gethash key addings) item)))

(defun rem-item (staging-area key)
  (with-slots (addings removals)
      staging-area
    (remhash key addings)
    (pushnew key removals)))




(defun commit (staging-area)
  (let ((dataset (slot-value (slot-value staging-area 'commit) 'dataset))
        (cadb (slot-value (slot-value staging-area 'repository) 'cadb)))
    (maphash (lambda (key value &aux (pos (cadb-add (cons key value))))
               (when pos
                 (setq dataset (dpb 1 (byte 1 pos) dataset))))
             (slot-value staging-area 'addings))
    ()))





(defun check-out (commit)) 

(defclass check-out ()
  (commit))

(defun get-item (check-out key))




(defmacro mlet (bindings &body body)
  (if bindings
      (let ((binding (first bindings)))
        (etypecase binding
          (cons `(multiple-value-bind ,(car binding)
                     ,(cadr binding)
                   (mlet ,(rest bindings) ,@body)))
          (symbol `(let (,binding)
                     ,@body))
          (null `(progn ,@body))))
      `(progn ,@body)))



















(defmacro do-bits ((var x) &body body)
  "Evaluate BODY forms after binding VAR to each set bit in X."
  (let ((k (gensym)))
    `(do ((,k ,x (logand ,k (1- ,k))))
         ((= ,k 0))
       (let ((,var (logand ,k (- ,k))))
         ,@body))))



(defstruct (repository (:copier nil)
                       (:constructor)
                       (:conc-name "%REPOSITORY-"))
  ((databases nil :type list)
   (commits nil :type vector)
   (branches nil :type list)
   (tags nil :type list)))

(defun %add-database (repository db)
  (setf (%repository-databases repository) (append (%repository-databases repository) (list db))))





(defstruct (commit (:copier nil)
                   (:constructor %make-commit (parent dataset))
                   (:conc-name "%COMMIT-"))
  ((repository nil :type repository :read-only t)
   (datasets nil :type list :read-only t)
   (parent nil :type commit :read-only t)
   (info nil :type info :read-only t)))

(defclass info ()
  (author
   author-date
   date
   notes))

(defstruct branch
  ((tip nil :type commit)
   (info nil :type info)))
(defstruct tag
  ((commit nil :type commit)
   (info nil :type info)))


(let ((zero-tail #1= (0 . #1#)))
  (defun make-datasets (integers)
    (nconc integers zero-tail)))


(defun map-datasets (function dataset &rest more-datasets)
  (apply 'make-dataset
         (do (result
              (ds dataset (rest ds))
              (more-ds more-datasets (mapl (lambda (l) (pop (car l))) more-datasets)))
             ((every (lambda (d) (eq ds d)) more-ds)
              (nreverse result))
           (apply function (first ds) (mapcar 'first more-ds)))))
(define-compiler-macro map-datasets (function dataset &rest more-datasets)
  (let ((ds-var (gensym))
        (more-ds-vars (mapcar 'gensym more-datasets)))
    `(make-datasets
      (do (result
           (,ds-var ,dataset (rest ,ds-var))
           ,@(mapcar (lambda (ds-var dataset)
                       `(,ds-var ,dataset (rest ,ds-var)))
                     more-ds-vars more-datasets))
          ((and ,@(mapcar (lambda (mds-var) `(eq ,ds-var ,mds-var))
                          more-ds-vars))
           (nreverse result))
        (apply function (first ,ds-var) (mapcar 'first ,more-ds-vars))))))


(defun dataset-union (dataset &rest more-datasets)
  (map-datasets 'logior dataset more-datasets))


;;;;** Versioned Data Base VDB

(defstruct (vdb (:copier nil)
                (:constructor)
                (:conc-name "%VDB-"))
  cadb
  indices)


;;;;** Content Addressed Data Base CADB
(defstruct (cadb (:copier nil)
                 (:constructor make-cadb (id-fn))
                 (:conc-name "%CADB-"))
  (vector (make-array 0 :adjustable t))
  (free-indices nil :type list)
  (index (make-hash-table) :type hash-table :read-only t)
  (id-fn nil :type function :read-only t))

(defun cadb-vector (cadb)
  "Return the underlying vector of CADB."
  (%cadb-vector cadb))

(defmacro with-cadb ((var cadb) &body body)
  (let ((%vector (gensym "VECTOR")))
    `(flet ((contains ())))))

(defun %cadb-position (cadb id)
  "Return the position of ITEM in the underlying vector if ITEM is contained in this CADB and NIL else."
  (gethash id (%cadb-index cadb)))

(defun cadb-id (cadb item)
  "Return the id under which ITEM would be stored in this CADB."
  (funcall (%cadb-id-fn cadb) item))
(defun cadb-item (cadb id)
  "Return as primary value the item in CADB having ID and as secondary value the index in the underlying vector."
  (declare (inline %cadb-position))
  (let ((idx (%cadb-position id)))
    (when idx
      (values (aref (%cadb-vector cadb) idx)
              idx))))
(defun cadb-add (cadb item)
  "Add ITEM to CADB, if there is not already an object in the CADB for which CADB-ID returns the same id under EQL, and return the index of the position in the underlying vector. In the case of a collision, do nothing and return NIL."
  (let ((id (cadb-id cadb id))
        (index (%cadb-index cadb)))
    (unless (gethash id index)
      (let ((idx (let ((idx (first (%cadb-free-indices cadb))))
                   (if idx
                       (prog1 idx
                         (setf (aref (%cadb-vector cadb) idx) item))
                       (vector-push-extend (%cadb-vector cadb) item)))))
        (setf (gethash id index) idx)))))
(defun cadb-rem (cadb id)
  "Remove the object stored under ID in this CADB and return the freed index in the underlying vector, if such object is present.  Do nothing and return NIL else."
  (let ((index (%cadb-index cadb)))
    (let ((idx (gethash id index)))
      (when idx
        (remhash id index)
        (let ((vector (%cadb-vector cadb)))
          (setf (aref vector idx) nil))
        (if (= idx (1- (fill-pointer vector)))
            (setf (fill-pointer vector) idx)
            (push idx (%cadb-free-indices cadb)))
        idx))))
(defun cadb-shrink (cadb)
  "Shrink the underlying vector of CADB."
  (let ((vector (%cadb-vector cadb)))
    (setf (%cadb-free-indices cadb) (sort (%cadb-free-indices cadb) '>))
    (flet ((pop-item ()
             (do ((last-idx (1- (fill-pointer vector))
                            (1- (decf (fill-pointer vector))))
                  (biggest-free-idx (first (%cadb-free-indices cadb))
                                    (progn (pop (%cadb-free-indices cadb))
                                           (first free-indices))))
                 ((< biggest-free-idx last-idx) 
                  (prog1
                      (aref vector last-idx)
                    (setf (aref vector last-idx) nil
                          (fill-pointer vector) last-idx))))))
      (let ((free-indices (reverse (%cadb-free-indices cadb)))
            (index (%cadb-index cadb)))
        (do ((item (pop-item)
                   (pop-item))
             (free-index (pop free-indices)
                         (progn
                           (setf (aref vector free-index) item)
                           (pop free-indices))))
            ((when (and free-index
                        (< (fill-pointer cadb) free-index))
               (vector-push-extend item)))))))
  (length (setf (%cadb-vector cadb) (adjust-array vector (fill-pointer vector) :fill-pointer t))))










(defun copy-hash-table (hash-table)
  (with-hash-table-iterator (orig hash-table)
    (do ((copy (make-hash-table :test (hash-table-test hash-table)
                                :size (hash-table-size hash-table)
                                :rehash-size (hash-table-rehash-size hash-table)
                                :rehash-threshold (hash-table-rehash-threshold hash-table)
                                :weakness (hash-table-weakness hash-table))))
        ((multiple-value-bind (more-p key value)
             orig
           (or (not more-p)
               (setf (gethash key copy) value)))
         copy))))
