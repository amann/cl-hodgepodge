
(defun parse-state-definitions (body)
  (loop :with block-name (gensym)
     :with parameters = '()
     :with states = '()
     :with forms = '()
     :for state :in body
     :do (destructuring-bind (state-name state-lambda-list &body body)
             state
           (let* ((state-params (awl:get-lambda-variables state-lambda-list))
                  (state-g!params (mapcar 'awl:g!sym state-params))
                  (state-g!name (awl:g!sym state-name)))
             (setf parameters (append parameters state-g!params))
             (push (list* state-name state-g!name
                          (mapcan 'list state-g!params state-params)
                          state-lambda-list) states)
             (push state-g!name forms)
             (push `(return-from ,block-name ,@body) forms)))
     :finally (return (values block-name parameters states (nreverse forms)))))


(defmacro with-states (states &body body)
  (multiple-value-bind (block-name state-parameters states tag-body) 
      (parse-state-definitions states)
    (let ((expand-state (lambda (state)
                          (`((awl:macrolet*
                                 ((to-state (state-name &rest args)
                                            (let ((state (find state-name ',states :key 'first)))
                                              (if state
                                                  (destructuring-bind (state state-params &rest state-lambdalist)
                                                      (rest state)
                                                    `(progn (destructuring-bind ,state-lambdalist
                                                                ',args
                                                              (psetf ,@state-params))
                                                            (go ,state)))
                                                  `(to-state ,state-name ,@args)))))
                               (block block-name (tagbody ,@tag-body))))))))`(let ,state-parameters
        (labels ,(mapcar expand-state states))))))

(state-machine
 (prelude () (print 'start)
          (change-state start 1))
 (start (count) (print count) 
        (if (< count 4)
            (change-state start (1+ count))
            (change-state read-next "Hi")))
 (read-next (message) (print message)))
prints:
START 
1 
2 
3 
4 
"Hi" 
--> NIL




(defpackage :my-enum (:use :cl :iterate))

(in-package :my-enum)

(defun get-key (plist value)
  (iter (generating element in plist)
        (let ((current-key (next element))
              (current-value (next element)))
          (when (= current-value value)
            (return current-key)))))

(defun duplicate-value-check (enum)
  (let* ((values
          (iter
           (for element in enum)
           (when (numberp element)
             (collect element))))
         (sorted-values (sort values #'<))
         (position (mismatch sorted-values
                             (remove-duplicates sorted-values))))
    (when position
      (let ((value (nth position sorted-values)))
        (error "key ~A has a value ~D duplicated"
               (get-key enum value) value)))))

(defun enumerate (symbol-list)
  (iter (for symbol in symbol-list)
        (for value from 0)
        (nconcing (list symbol value))))

;; could type #.(getf *enum* key) each time, but it's a bit tedious
;; lets write $symbol instead
(let (old-readtable -enum-)

  (defun enable-shorthand (enum)
    (setf -enum- enum)
    (duplicate-value-check -enum-)
    (setf old-readtable (copy-readtable))
    (set-macro-character #\$ #'(lambda (stream char)
                                 (declare (ignore char))
                                 (getf -enum- (read stream)))))

  (defun disable-shorthand ()
    (setf *readtable* old-readtable)))

#--------------------------------------------------------------

With this you could write

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *state* (enumerate '(:first :second :third))))

(enable-shorthand *state*)

(defun machine (...)
  (tagbody
     (case state
       ($:first (go :first))
       ($:second (go :second))
       ...)
   :first
   ...))

(disable-shorhand)

and thus use names for states without needing many lines of ugly
defconstant's and #.





;;;;==========

(defmacro argtags (block-name &rest labels-and-forms)
  (check-type block-name symbol)
  (let (labels forms thunks thunk-gensyms)
    (dolist (item labels-and-forms)
      (cond
        ((symbolp item)
         (push `(,item () () ,item) labels)
         (push item forms))
        ((and (consp item)
              (eq (first item) 'label))
         (unless (and (symbolp (second item))
                      (listp (rest (rest item)))
                      (every #'symbolp (rest (rest item))))
           (error "ARGTAGS: bad label syntax ~a in block ~a" item block-name))
         (destructuring-bind (op label &rest vars) item
           (let ((gensyms (mapcar (lambda (var)
                                    (gensym (symbol-name var)))
                                  vars))
                 (thunk-label (gensym (symbol-name label))))
             (push `(,label ,vars ,gensyms ,thunk-label) labels)
             (push thunk-label thunks)
             (push
              `(psetf ,@(mapcan (lambda (realvar gensym)
                                  `(,realvar ,gensym))
                                vars gensyms))
              thunks)
             (push `(go ,label) thunks)
             (setf thunk-gensyms (nconc gensyms thunk-gensyms))
             (push label forms))))
        (t
         (push item forms))))
    `(macrolet ((goto (label &rest args)
                  (let* ((labels ',labels)
                         (matching-label (find label labels :key #'first)))
                    (unless matching-label
                      (error "ARGTAGS: goto undefined label ~a in block ~a"
                             label ',block-name))
                    (destructuring-bind (name vars gensyms thunk-label)
                        matching-label
                      (declare (ignore name))
                      (when (/= (length args) (length vars))
                        (error "ARGTAGS: label ~a caled with wrong argument count in block ~a"
                               label ',block-name))
                      `(progn
                         ,@(if args `((psetf ,@(mapcan (lambda (gensym arg) `(,gensym ,arg)) gensyms args))))
                         (go ,thunk-label))))))
       (block ,block-name
         (let (,@thunk-gensyms)
           (tagbody
              ,@(nreverse forms)
              (return-from ,block-name)
              ,@(nreverse thunks)))))))
.





(define my-a
    (lambda (input)
      (letrec
       ((init (lambda (stream)
                (or (null? stream)
                    (case (car stream)
                      ((c) (loop (cdr stream)))))))
        (loop (lambda (stream)
                (or (null? stream)
                    (case (car stream)
                      ((a) (loop (cdr stream)))
                      ((d) (loop (cdr stream)))
                      ((r) (end (cdr stream)))
                      (else #f)))))
        (end (lambda (stream)
               (or (null? stream)
                   (case (car stream)
                     (else #f))))))
       (init input))))

Into CL first. In doing so, we blow away a lot of Scheme braindamage
and cut it nearly in half:

(defun my-a (input)
  (labels
      ((init (stream)
         (case (first stream)
           ((c) (loop (rest stream)))))
       (loop (stream)
          (case (first stream)
            ((a d) (loop (rest stream)))
            ((r) (end (rest stream)))))
       (end (stream)
         (null stream)))
    (init input)))

Now the ARGTAGS version:

(defun my-a (stream)
  (argtags nil
           (label init stream)
           (case (first stream)
             ((c) (goto loop (rest stream))))
           (return nil)

           (label loop stream)
           (case (first stream)
             ((a d) (goto loop (rest stream)))
             ((r) (goto end (rest stream))))
           (return nil)
           
           (label end stream)
           (return (null stream))))

(defun fac-0 (n)
  (declare (optimize speed)
           (fixnum n))
  (let (curr1400 count1401)
    (labels
        ((compute (curr count)
           (declare (fixnum count curr))
           (tagbody
              (setf curr1400 curr
                     count1401 count)
              (go compute1399)
            compute1399
              (return-from compute
                (let ((curr curr1400) (count count1401))
                  (declare (fixnum count curr))
                  (if (< 0 count)
                      (progn
                        (psetf curr1400 (- count curr)
                               count1401 (1- count))
                        (go compute1399))
                      curr))))))
      (compute 0 n))))
(defun fac (n)
  (declare (optimize speed)
           (fixnum n))
  (awl::labels* ((compute (curr count)
             (declare (fixnum count curr))
                          (if (< 0 count)
                              (awl::goto compute (- count curr) (1- count))
                              curr)))
   (compute 0 n)))
(defun fac-1 (n)
  (declare (optimize speed)
           (fixnum n))
  (awl::labels* ((compute (curr count)
             (declare (fixnum count curr))
                          (if (< 0 count)
                              (compute (- count curr) (1- count))
                              curr)))
   (compute 0 n)))
(defun fac-2 (n)
  (declare (optimize speed)
           (fixnum n))
  (labels ((compute (curr count)
             (declare (fixnum count curr))
             (if (< 0 count)
                 (compute (- count curr) (1- count))
                 curr)))
   (compute 0 n)))
(defun fac-t (n)
  (declare (optimize speed)
           (fixnum n))
  (let ((count n) (result 0))
    (declare (fixnum count result))
    (tagbody
     next
       (when (< 0 count)
         (setf result (- count result))
         (decf count)
         (go next)))
    result))


(defun awl::make-destructurer (lambda-list)
  (compile nil `(lambda (value-list)
                  (destructuring-bind ,lambda-list
                      value-list
                    (values ,@(awl:get-lambda-variables lambda-list))))))
(defun awl::destructure-values (lambda-list value-list)
  "Return the values determined by destruturing VALUE-LIST by the lambda list LAMBDA-LIST in the order as given by AWL:GET-LAMBDA-VARIABLES"
  (funcall (awl::make-destructurer lambda-list)
           value-list))

(defun parse-label-defs (label-defs)
  (loop
     :for (tag arglist . body1) :in label-defs
     :for g!tag := (awl:g!sym tag)
     :for tag-vars := (awl:get-lambda-variables arglist)
     :for tag-g!vars := (mapcar 'awl:g!sym tag-vars)
     :for (body decls docs) := (multiple-value-list (awl:parse-body body1))
     :collect (list tag g!tag arglist decls tag-vars tag-g!vars) :into parsed-label-defs
     :nconc tag-g!vars :into label-parameters 
     :nconc `(,g!tag (return-from ,tag (let ,(mapcar 'list tag-vars tag-g!vars),@decls ,@body))) :into tag-bodies
     :finally (return (values parsed-label-defs label-parameters tag-bodies))))
(defmacro awl::labels* ((&rest label-defs) &body body)
  "Establish lexical labels as with CL:LABELS. Inside the bodies but not in the lambda-lists of the label definitions provide a local macro, AWL:GOTO label &rest args, allowing to jump to the label as with CL:GO in a CL:TAGBODY."
  (multiple-value-bind (parsed-label-defs label-parameters tag-bodies)
      (parse-label-defs label-defs)
    (awl:with-protected-macros (awl:macrolet*)
      `(let ,label-parameters
         (labels
             ,(mapcar (awl:dlambda ((tag g!tag arglist decls tag-vars tag-g!vars))
                        `(,tag ,arglist
                               ,@decls
                               (awl:macrolet*
                                   ((awl::goto (&whole form label &rest args)
                                               (destructuring-bind
                                                     (&optional tag g!tag arglist decls tag-vars tag-g!vars)
                                                   (assoc label ',parsed-label-defs) 
                                                 (declare (ignore decls tag-vars))
                                                 (if tag
                                                     `(progn
                                                        (setf ,@(mapcan 'list tag-g!vars
                                                                        (multiple-value-list (awl::destructure-values arglist args))))
                                                        (go ,g!tag))
                                                     form))))
                                 (tagbody
                                    (progn
                                      (setf ,@(mapcan 'list tag-g!vars tag-vars))
                                      (go ,g!tag))
                                    ,@tag-bodies))))
                      parsed-label-defs)
           ,@body)))))




(defun fac (n)
  (declare (optimize speed) (fixnum n))
  (awl::labels* ((compute (curr count)
                   (declare (fixnum count curr))
                   (if (< 0 count)
                       (awl::goto compute (- count curr) (1- count))
                       curr)))
   (compute 0 n)))



(defun fac-d (n)
  (declare (optimize speed) (fixnum n))
  (labels ((compute (curr count)
             (declare (fixnum curr count))
             (if (< 0 count)
                 (compute (- count curr) (1- count))
                 curr)))
    (compute 0 n)))

(defun fac-a (n)
  (let (curr1022 count1023)
    (labels ((compute (curr count)
               (declare (fixnum count curr))
               (tagbody
                  (progn
                    (progn (setq curr1022 curr) (setq count1023 count))
                    (go compute1021))
                compute1021
                  (return-from compute
                    (let ((curr curr1022) (count count1023))
                      (declare (fixnum count curr))
                      (if (< 0 count)
                          (let ((whole1042 (list (- count curr) (1- count))))
                            (declare (type list whole1042))
                            (let* ()
                              (declare (muffle-conditions code-deletion-note))
                              (let* ((curr (car whole1042))
                                     (count (car (cdr whole1042))))
                                (progn
                                  (setq curr1022 curr)
                                  (setq count1023 count))
                                (go compute1021))))
                          curr))))))
      (compute 0 n))))

(defun fac-b (n)
  (let (curr1022 count1023)
    (labels ((compute (curr count)
               (declare (fixnum count curr))
               (tagbody
                  (progn
                    (progn (setq curr1022 curr) (setq count1023 count))
                    (go compute1021))
                compute1021
                  (return-from compute
                    (let ((curr curr1022) (count count1023))
                      (declare (fixnum count curr))
                      (if (< 0 count)
                          (progn
                            (setq count1023 (1- count))
                            (setq curr1022 (- count curr))
                            (go compute1021))
                          curr))))))
      (compute 0 n))))

(defun fac-c (n)
  (declare (optimize speed) (fixnum n))
  (let ((curr1022 0) (count1023 0))
    (declare (fixnum curr1022 count1023))
    (tagbody
       (setq curr1022 0) (setq count1023 n)
     compute1021
       (return-from fac-c
         (let ((curr curr1022) (count count1023))
           (declare (fixnum count curr))
           (if (< 0 count)
               (progn
                 (setq count1023 (1- count))
                 (setq curr1022 (- count curr))
                 (go compute1021))
               curr))))))

