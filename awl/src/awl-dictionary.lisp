;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;============================================================================
;;;;* Dereferenced Dictionaries
;;;; An object of the type AWL::DICTIONARY associates values to keys.  Such a
;;;; dictionary can reference to a list of parent directories, meaning that a key
;;;; is looked up sequencially, first in the own table, then in each of the parents,
;;;; one after the other along the list, until the key (if any) is found.  If the
;;;; parents themselves refer to other parents, the dictionary has a tree structure
;;;; and a look up in the dictionary is a depts first search.  A special case,
;;;; where the lists of parent directories contain each maximally one element would
;;;; be comparable to environments.
;;;;
;;;; In this implementation, dictionaries use hash-tables and a dictionary without
;;;; parents is a simple hash-table.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass awl::dictionary () ((tables :initform nil))))

(deftype dictionary () '(or awl::dictionary hash-table))

(defun initialize-hash-table (initargs)
  (apply 'make-hash-table
         (mapcan (lambda (key)
                   (let ((value (getf initargs key)))
                     (when value
                       (list key value))))
                 '(:test :rehash-size :size :rehash-threshold
                   :hash-function :weakness :synchronized))))

(declaim (inline check-table-type))
(defun check-table-type (table)
  (check-type table dictionary))

(defgeneric add-table (dictionary table)
  (:method ((dictionary awl::dictionary) table)
    (with-slots (tables) dictionary
      (setq tables (add-table tables table)))
    dictionary)
  (:method ((dictionary list) table)
    (append dictionary (list table)))
  (:method (dictionary table)
    (make-instance 'awl::dictionary :tables (list dictionary table))))
(defgeneric rem-table (dictionary table)
  (:method ((dictionary awl::dictionary) table)
    (with-slots (tables) dictionary
      (setq tables (rem-table tables table)))
    dictionary)
  (:method ((dictionary list) table)
    (remove table dictionary))
  (:method (dictionary table)
    (unless (eql dictionary table)
      dictionary)))
(defgeneric push-table (dictionary table)
  (:method ((dictionary awl::dictionary) table)
    (with-slots (tables) dictionary
      (setq tables (push-table tables table)))
    dictionary)
  (:method ((dictionary list) table)
    (cons table dictionary))
  (:method ((dictionary hash-table) table)
    (make-instance 'awl::dictionary :tables (list table dictionary))))
(defgeneric pop-table (dictionary)
  (:method ((dictionary awl::dictionary))
    (with-slots (tables) dictionary
      (setq tables (pop-table tables)))
    dictionary)
  (:method ((dictionary list))
    (rest dictionary))
  (:method (dictionary)))

(defmethod shared-initialize :after ((instance awl::dictionary) slot-names
                                     &rest initargs
                                     &key parents &allow-other-keys)
  (add-table instance (initialize-hash-table initargs))
  (dolist (parent parents)
    (check-table-type parent)
    (add-table instance parent)))


;;;;** Functions for managing entries in dictionaries
(define-condition awl::dictionary-error (error)
  ((dictionary :initarg :dictionary :reader awl::dictionary-error-dictionary)))
(define-condition awl::dictionary-key-present-error (awl::dictionary-error)
  ((key :initarg :key :reader awl::dictionary-error-key)))

(defgeneric pushnew-dict (dictionary key value)
  (:method ((dictionary awl::dictionary) key value)
    (with-slots (tables) dictionary
      (setf tables (pushnew-dict tables key value))))
  (:method ((dictionary null) key value))
  (:method ((dictionary list) key value)
    (pushnew-dict (first dictionary) key value))
  (:method (dictionary key value)
    (multiple-value-bind (value present-p)
        (getdict dictionary key)
      (if present-p
          (error 'awl::dictionary-key-present-error :dictionary dictionary
                                                    :key key)
          (setdict dictionary key value)))))

(defgeneric remdict (dictionary key)
  (:method ((dictionary awl::dictionary) key)
    (with-slots (own-table parents)
        dictionary
      (or (remdict own-table key)
          (remdict parents key))))
  (:method ((dictionary list) key)
    (or (remdict (first dictionary) key)
        (remdict (rest dictionary) key)))
  (:method ((dictionary null) key))
  (:method ((dictionary hash-table) key)
    (remhash key dictionary)))

(defgeneric getdict (dictionary key)
  (declare (optimize speed))
  (:method ((dictionary null) key))
  (:method ((dictionary list) key)
    (multiple-value-bind (value present-p)
        (getdict (first dictionary) key)
      (if present-p
          (values value present-p)
          (getdict (rest dictionary) key))))
  (:method ((dictionary awl::dictionary) key)
    (with-slots (tables) dictionary
      (getdict tables key)))
  (:method ((dictionary hash-table) key)
    (gethash key dictionary)))

(defgeneric setdict (dictionary key value)
  (declare (optimize speed))
  (:method ((dictionary hash-table) key value)
    (setf (gethash key dictionary) value))
  (:method ((dictionary null) key value))
  (:method ((dictionary list) key value)
    (setdict (first dictionary) key value))
  (:method ((dictionary awl::dictionary) key value)
    (with-slots (tables) dictionary
      (setdict tables key value))))

(defgeneric get-tables (dictionary)
  (:method ((dictionary hash-table))
    (list dictionary))
  (:method ((dictionary list))
    dictionary)
  (:method ((dictionary awl::dictionary))
    (mapcan 'get-tables (slot-value dictionary 'tables))))

;;;;** Dictionary API
(defun awl::make-dictionary (parents &rest options &key test &allow-other-keys)
  "Make a new dictionary.  PARENTS is a list of other dictionaries which should be included.  The options OPTIONS refer to the freshly created hash-table used for holding new associations and have no influence on the parent tables."
  (declare (ignore test))
  (if parents
      (apply 'make-instance 'awl::dictionary :parents parents options)
      (initialize-hash-table options)))

(defun awl::add-dict-table (dictionary table)
  (check-table-type dictionary)
  (check-table-type table)
  (add-table dictionary table))
(defun awl::push-dict-table (dictionary table)
  (check-table-type table)
  (push-table dictionary table))


(defun awl::getdict (dictionary key &optional default)
  "Return the value in the dictionary DICTIONARY associated with the key KEY.  If non such exists, return DEFAULT.  The secondary value is a boolean being true if KEY is present in DICTIONARY and false else."
  (multiple-value-bind (value present-p)
      (getdict dictionary key)
    (values (if present-p
                value
                default)
            present-p)))
(defsetf awl::getdict (dictionary key &optional default) (value)
  "Associate in the dictionary DICTIONARY the value VALUE to the key KEY.  The argument DEFAULT is ignored."
  (declare (ignore default))
  `(setdict ,dictionary ,key ,value))

(defun awl::remdict (dictionary key)
  "Remove the entry in DICTIONARY labeled with KEY.  If DICTIONARY is a AWL::DICTIONARY, the nearest entry is removed possibly revealing an entry shadowed by the removed entry.  To remove the next entry, recall remdict.  Return true if an entry has been removed, nil else."
  (remdict dictionary key))

(defun awl::pushnew-dict (dictionary key value)
  "Add VALUE associated to KEY to the DICTIONARY (at innermost level) and return VALUE and true if succeded.  If KEY is already present at innermost level, an error of type AWL::DICTIONARY-KEY-PRESENT-ERROR is signaled."
  (pushnew-dict dictionary key value))

(defmacro awl::do-dictionary ((key value &optional result) dictionary &body body)
  "Iterate over the key-value associations of a dictionary with KEY and VALUE bound in the scope of BODY to the key respectively the value of the association.  The body of DO-DICTIONARY is like a tagbody surrounded by an implicit block named NIL.  It consists of a series of tags and statements and evaluated once for each association unless a non local exit is invoked.  If terminating normally, the form RESULT is evaluated with the variables KEY and VALUE bound to NIL."
  (awl:with-gensyms (g!seen-tables g!table g!iterator g!more?)
    `(let (,g!seen-tables ,key ,value)
       (declare (ignorable ,key ,value)
                (dynamic-extent ,g!seen-tables))
       (dolist (,g!table (get-tables ,dictionary) ,result)
         (block ,g!table
           (return (with-hash-table-iterator (,g!iterator ,g!table)
                     (loop (multiple-value-bind (,g!more? ,key ,value)
                               (,g!iterator)
                             (unless ,g!more?
                               (setq ,g!seen-tables (append ,g!seen-tables (list ,g!table)))
                               (return-from ,g!table))
                             (unless (nth-value 1 (getdict ,g!seen-tables ,key))
                               ,@body))))))))))


;;;;* Multi Hash-Table

(defun awl::get-hash* (hash keys &optional default)
  "Return the object in HASH whose keys is the same as keys under the hash's equivalence test and T as secondary value if such object exists. Return nil, nil else."
  (if default (multiple-value-bind (value not-empty-p followed-path remaining-path)
                  (funcall hash :get keys nil)
                (values (if not-empty-p value default) not-empty-p followed-path remaining-path))
      (funcall hash :get keys nil)))
(define-compiler-macro awl::get-hash* (hash keys &optional (default nil defaultp))
  (if defaultp
      `(multiple-value-bind (value not-empty-p followed-path remaining-path)
           (funcall ,hash :get ,keys nil)
        (values (if not-empty-p value ,default) not-empty-p followed-path remaining-path))
      `(funcall ,hash :get ,keys nil)))
(defun awl::get-hash (hash &rest keys)
  "Return the object in HASH whose keys is the same as keys under the hash's equivalence test and T as secondary value if such object exists. Return nil, nil else."
  (awl::get-hash* hash keys))
(defsetf awl::get-hash* (hash keys &optional default) (value)
  "Set the value of the hash to the given value."
  (declare (ignorable default))
  `(funcall ,hash :set ,keys ,value))
(defsetf awl::get-hash (hash &rest keys) (value)
  "Set the value of the hash to the given value."
  `(funcall ,hash :set ,keys ,value))
(defun awl::rem-hash* (hash keys)
  "Remove the entry for KEYS in HASH, if any. Returns true if there was such an entry, or false otherwise."
  (funcall hash :rem keys nil))
(defun awl::rem-hash (hash &rest keys)
  "Remove the entry for KEYS in HASH, if any. Returns true if there was such an entry, or false otherwise."
  (awl::rem-hash* hash keys))
(defun awl::clr-hash (hash)
  "Remove all entries in HASH, if any, and returns the empty HASH."
  (funcall hash :clr nil nil)
  hash)
(defun awl::hash-count (hash)
  "Return the number of entries in HASH. If HASH has just been created or newly cleared (see clr-hash) the entry count is 0."
  (funcall hash :cnt nil nil))
(defun awl::map-hash (fct hash)
  "Map HASH to the function FCT in depth first manner. FCT takes two arguments: the first is the key the second must be optional and takes the value. blabla."
  (funcall hash :map fct nil))



(let ((empty '#:nil))
  (labels
      ((follow-path (nodes path followed-path)
         (let ((node (first nodes)))
           (if node
               (handler-case
                   (destructuring-bind (step &rest remaining-path) path
                     (follow-path (cons (gethash step (cdr node)) nodes) remaining-path (cons step followed-path)))
                 (error ()
                   (values nodes followed-path path)))
               (error "No node."))))
       (map-children (fct node)
         (let ((hash (cdr node)))
           (when hash
             (maphash fct (cdr node)))))
       (map-tree (fct node)
         (unless (eq empty (car node))
           (funcall fct nil (car node)))
         (map-children (lambda (s n)
                         (funcall fct s)
                         (map-tree fct n))
                       node)))
    (defun make-hash-tree (options)
      (let ((root (cons empty nil)))
        (lambda (action path value)
          (ecase action
            (:get (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (let* ((value (car (first nodes)))
                           (emptyp (or (eq empty value) remaining-path)))
                      (values (unless emptyp value) (not emptyp) followed-path remaining-path))))
            (:set (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (declare (ignore followed-path))
                    (let ((node (first nodes)))
                      (dolist (step remaining-path)
                        (unless (cdr node)
                          (setf (cdr node) (apply #'make-hash-table options)))
                        (let ((hash (cdr node)))
                          (setf (gethash step hash) (cons empty nil))
                          (setq node (gethash step hash))))
                      (setf (car node) value))))
            (:rem (multiple-value-bind (nodes followed-path remaining-path)
                      (follow-path (list root) path nil)
                    (unless remaining-path
                      (setf (car (first nodes)) empty)
                      (loop :for (node . prev-nodes) :on nodes
                         :for step :in followed-path
                         :do (if (and (eq empty (car node))
                                      (null (cdr node)))
                                 (remhash step (cdr (first prev-nodes)))
                                 (return)))
                      t)))
            (:clr (setq root (cons empty nil)))
            (:cnt (let ((counter 0))
                    (labels ((count-non-empty (node)
                               (unless (eq empty (car node))
                                 (incf counter))
                               (map-children (lambda (s n)
                                               (declare (ignore s))
                                               (count-non-empty n))
                                             node)))
                      (count-non-empty root))
                    counter))
            (:map (progn
                    (map-tree path root)
                    nil))))))))



(defclass awl::hash-tree (c2mop:funcallable-standard-object) ()
  (:metaclass c2mop:funcallable-standard-class))
(let ((hash-tree-class (find-class 'awl::hash-tree)))
  (unless (c2mop:class-finalized-p hash-tree-class)
    (c2mop:finalize-inheritance hash-tree-class)))
(defmethod initialize-instance :after ((self awl::hash-tree) &key options)
  (c2mop:set-funcallable-instance-function self (make-hash-tree options)))
(defun awl::make-hash-tree (&rest options &key test size rehash-size
                            rehash-threshold #+(or ccl clisp sbcl) weak &allow-other-keys)
  (declare (ignore test size rehash-size rehash-threshold #+(or ccl clisp sbcl) weak))
  #+sbcl (progn
           (setf (getf options :weakness) (getf options :weak))
           (remf options :weak))
  (make-instance 'awl::hash-tree :options options ))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))

;;;; Depreciated stuff -- should be eliminated.
#+ (or)
(progn
  (defgeneric make-multi-hash-getter-setter (class options))

  (let ((empty '#:nil))
    (defun get-multi-hash (hash-table key other-keys)
      (handler-case
          (if other-keys
              (get-multi-hash (cdr (gethash key hash-table))
                              (first other-keys) (rest other-keys))
              (let ((hash (gethash key hash-table)))
                (if hash
                    (let ((value (car hash)))
                      (if (eq empty value)
                          (values nil nil :empty-inner-node)
                          (values value t)))
                    (values nil nil :empty-leaf))))
        (type-error ()
          (values nil nil :error))))
    (labels ((new-hash (key hash-table hash)
               (etypecase hash
                 ((cons t null) hash)
                 (null (let ((hash (cons empty nil)))
                         (setf (gethash key hash-table) hash)
                         hash)))))
      (defun set-multi-hash (hash-table options value key other-keys)
        (let ((hash (gethash key hash-table)))
          (if other-keys
              (set-multi-hash (cdr (typecase hash
                                     ((cons t hash-table) hash)
                                     (t (rplacd (new-hash key hash-table hash)
                                                (apply #'make-hash-table options)))))
                              options value (first other-keys) (rest other-keys))
              (rplaca (typecase hash
                        ((cons t hash-table) hash)
                        (t (new-hash key hash-table hash)))
                      value))))))


  (defclass awl::multi-hash-table ()
    (get-fn set-fn rem-fn count-fn))
;;;; SBCL (and CMUCL?) finalize their classes late in order to allow forward referenced superclasses
  (let ((multi-hash-table-class (find-class 'awl::multi-hash-table)))
    (unless (c2mop:class-finalized-p multi-hash-table-class)
      (c2mop:finalize-inheritance multi-hash-table-class)))
  (defmethod make-multi-hash-getter-setter ((type awl::multi-hash-table) options)
    (let ((hash-table (apply #'make-hash-table options)))
      (values (lambda (key other-keys)
                (get-multi-hash hash-table key other-keys))
              (lambda (value key other-keys)
                (set-multi-hash hash-table options value key other-keys)))))
  (defmethod initialize-instance :after ((self awl::multi-hash-table) &rest initargs
                                         &key options &allow-other-keys)
    (declare (ignore initargs))
    (multiple-value-bind (get-fn set-fn rem-fn count-fn)
        (make-multi-hash-getter-setter self options)
      (setf (slot-value self 'get-fn) get-fn
            (slot-value self 'set-fn) set-fn
            (slot-value self 'rem-fn) rem-fn
            (slot-value self 'count-fn) count-fn)))

  (defun awl::make-multi-hash-table (&rest options &key &allow-other-keys)
    (make-instance 'awl::multi-hash-table :options options))

  (awl::with-slot-definition-locations (get-fn set-fn rem-fn count-fn)
                                       awl::multi-hash-table
    (defun awl::get-multi-hash (hash-table key &rest other-keys)
      "Return the value of the multi hash corresponding to KEY and OTHER-KEYS."
      (funcall (c2mop:standard-instance-access hash-table get-fn) key other-keys))
    (defun awl::get-multi-hash* (hash-table keys)
      "Return the value of the multi hash corresponding to KEY and OTHER-KEYS."
      (funcall (c2mop:standard-instance-access hash-table get-fn) (first keys) (rest keys)))
    (defsetf awl::get-multi-hash (hash-table key &rest other-keys) (value)
      "Set the value of the multi hash to the given value."
      `(progn
         (funcall (c2mop:standard-instance-access ,hash-table ,set-fn)
                  ,value ,key ,other-keys)
         ,value))
    (defsetf awl::get-multi-hash* (hash-table keys) (value)
      "Set the value of the multi hash to the given value."
      `(destructuring-bind (key &rest other-keys) ,keys
         (funcall (c2mop:standard-instance-access ,hash-table ,set-fn)
                  ,value key other-keys)
         ,value))
    (defun awl::rem-multi-hash (hash-table key &rest other-keys)
      "Remove KEYS from HASH-TABLE."
      (funcall (c2mop:standard-instance-access hash-table rem-fn) key other-keys))
    (defun awl::rem-multi-hash* (hash-table keys)
      "Remove KEYS from HASH-TABLE."
      (funcall (c2mop:standard-instance-access hash-table rem-fn) (first keys) (rest keys)))
    (defun awl::multi-hash-count (hash-table)
      "Return the number of entries in HASH-TABLE."
      (funcall (c2mop:standard-instance-access hash-table count-fn))))

;;;** Fixed Multi Hash Tables
  
  (defun get-fixed-multi-hash (hash-table keys)
    (handler-case
        (loop with foundp
              for key in keys
              do (multiple-value-setq (hash-table foundp)
                   (gethash key hash-table))
              finally (return (values hash-table foundp)))
      (type-error ()
        (values nil nil))))
  (defun set-fixed-multi-hash (hash-table options value key other-keys)
    (if other-keys
        (set-fixed-multi-hash (or (gethash key hash-table)
                                  (setf (gethash key hash-table)
                                        (apply #'make-hash-table (first options))))
                              (rest options) value (first other-keys) (rest other-keys))
        (setf (gethash key hash-table) value)))
  (defun rem-fixed-multi-hash (hash-table keys)
    (handler-case
        (loop :with continue-p := t :and removed-p := nil
              :for (key hash) :in (nreverse
                                   (loop :with hash := hash-table
                                         :for key :in keys
                                         :collect (list key
                                                        (prog1 hash
                                                          (setq hash (gethash key hash))))))
              :while continue-p
              :do (progn
                    (setq removed-p (remhash key hash))
                    (setq continue-p (zerop (hash-table-count hash))))
              :finally (return removed-p))
      (type-error ())))
  (defun clr-fixed-multi-hash (hash-table)
    (clrhash hash-table))

  (defclass awl::fixed-range-multi-hash-table (awl::multi-hash-table) ())
  (labels ((count-values (node sum)
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (if (hash-table-p v)
                            (setq sum (count-values v sum))
                            (incf sum)))
                      node)
             sum))
    (defmethod make-multi-hash-getter-setter ((type awl::fixed-range-multi-hash-table)
                                              options)
      (let ((hash-table (apply #'make-hash-table (first options)))
            (nbr-keys (length options)))
        (values
         (lambda (key other-keys)
           (if (= nbr-keys (1+ (length other-keys)))
               (get-fixed-multi-hash hash-table (cons key other-keys))
               (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                      (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
         (lambda (value key other-keys)
           (if (= nbr-keys (1+ (length other-keys)))
               (set-fixed-multi-hash hash-table (rest options) value
                                     key other-keys)
               (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                      (< (1+ (length other-keys)) nbr-keys) (cons key other-keys)
                      nbr-keys)))
         (lambda (key other-keys)
           (if (= nbr-keys (1+ (length other-keys)))
               (rem-fixed-multi-hash hash-table (cons key other-keys))
               (error "~:[Too many~;Not enough~] keys given: ~S; expected ~D."
                      (< (1+ (length other-keys)) nbr-keys) other-keys nbr-keys)))
         (lambda ()
           (count-values hash-table 0))))))

  (defun awl::make-fixed-multi-hash-table (&rest options)
    (funcall #'make-instance 'awl::fixed-range-multi-hash-table :options options)))