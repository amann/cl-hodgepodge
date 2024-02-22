(in-package #:adaptiv)
(defun make-map (data-list)
  "Creates a hash-table from the list `data-list' which is a list of lists whose first element must be a symbol or a string. This first element is upcased as string and interned as a symbol in the keyword package and used as key of the hash-table. The rest of the list is taken as value."
  (let ((c-h (make-hash-table)))
    (map nil #'(lambda (data-entry)
                 (setf (gethash (intern (string-upcase (car data-entry)) :keyword) c-h)
                       (mapcar #'(lambda (item)
                                   (typecase item
                                     (string (string-trim " " item))
                                     (t item)))
                               (cdr data-entry))))
         data-list)
    c-h))

