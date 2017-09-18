;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;* Multi Cases
(defun %mcase-valid-clause-tests-p (nbr-keys tests)
  (or (null tests)
      (and (consp tests)
           (= nbr-keys (length tests)))))

(defun %mcase-otherwise-clause-tests-p (tests)
  (and (list tests)
       (every (lambda (x)
                (or (eql t x)
                    (equal '(:not) x)))
              tests)))
(defun %mcase-clause-expr (mcase nbr-keys g!keys clause may-be-otherwise-clause-p)
  (let ((clause-error-format-string "~@<~IBad ~S clause:~:@_ ~S.~@[~:@_ ~@?~]~:@>"))
    (unless (consp clause)
      (error clause-error-format-string mcase clause nil))
    (destructuring-bind (tests . body)
        clause
      (unless (%mcase-valid-clause-tests-p nbr-keys tests)
        (error clause-error-format-string mcase clause
               "Test must consist of the same number of key designators ~
              as the number of keys, in occurrence ~A." nbr-keys))
      (when (and (not may-be-otherwise-clause-p)
                 (%mcase-otherwise-clause-tests-p tests))
        (error clause-error-format-string mcase clause
               "{nil|({t|(:not)}*)} allowed as key designators only in ~
              the final otherwise-clause, not in a normal-clause."))
      (let ((test-expr `(and ,.(mapcar (lambda (g!key test)
                                         (typecase test
                                           (list (let* ((negatep (eq :not (first test)))
                                                        (test `(or ,.(mapcar (lambda (item)
                                                                               `(eql ,g!key ',item))
                                                                             (if negatep (rest test) test)))))
                                                   (if negatep
                                                       `(not ,test)
                                                       test)))
                                           ((eql t) t)
                                           (t `(eql ,g!key ',test))))
                                       g!keys tests))))
        (values `(,test-expr . ,body)
                test-expr)))))
;;;;TODO:
;;;; 1. Give a better documentation string.
;;;; 2. Make a cmcase macro.
;;;; 3. Refactor out common code.
(defmacro awl::mcase (keys &body clauses)
  "MCASE keyforms normal-clause* otherwise-clause?
  keyforms == (keyform{k})
  normal-clause == ((trivial-test{l} normal-test trivial-test{k-l-1}) form*)
  otherwise-clause == ({ nil | (trivial-test{k})} form*)
  normal-test == (:not? key*)
  trivial-test == t | (:not)
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
  (typecase keys
    (cons
     (let ((g!keys (mapcar 'awl:g!sym  keys))
           (nbr-keys (length keys)))
       `(let ,(mapcar 'list g!keys keys)
          (declare (ignorable ,@g!keys))
          (cond
            ,.(mapcar (lambda (clause)
                        (%mcase-clause-expr 'mcase nbr-keys g!keys clause nil))
                      (butlast clauses))
            ,(%mcase-clause-expr 'mcase nbr-keys g!keys (first (last clauses)) t)))))
    (t `(case ,keys ,.clauses))))

(defmacro awl::emcase (keys &body clauses)
  "EMCASE keyforms normal-clause normal-clause*
  keyforms == (keyform{k})
  normal-clause == ((trivial-test{l} normal-test trivial-test{k-l-1}) form*)
  normal-test == (:not? key*)
  trivial-test == t | (:not)
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the clauses is taken, an error is signaled."
  (typecase keys
    (cons
     (let ((g!keys (mapcar 'awl:g!sym  keys))
           (nbr-keys (length keys))
           test-exprs)
       `(let ,(mapcar 'list g!keys keys)
          (declare (ignorable ,@g!keys))
          (cond
            ,.(mapcar (lambda (clause)
                        (multiple-value-bind (clause-expr test-expr)
                            (%mcase-clause-expr 'mcase nbr-keys g!keys clause nil)
                          (push test-expr test-exprs)
                          clause-expr))
                      clauses)
            (t (error 'type-error
                      :expected-type '(or . ,(nreverse test-exprs))
                      :datum ,g!keys))))))))

;;;;==================================================================

;;;;* Event Handling
;;;;
;;;; One possible way for handling events in Common Lisp, is to use
;;;; the conditions system.  Signalling a condition is signalling an
;;;; event and the different condition handlers which are bound to
;;;; the condition via HANDLER-BIND handle that event.  The only
;;;; concern i have in using conditions, is that the triggering of each
;;;; event implies the instanciation of a condition object.  There
;;;; might be a lighter way to do: by associating to each event a
;;;; hook of event handlers which is finally nothing else than a list
;;;; of functions stored in a special variable and mapped to #'funcall
;;;; with the appropriate arguments at each triggered event.

(defclass event ()
  ((events :initform nil :allocation :class)
   (name :initarg :name :reader event-name)
   (lambda-list :initarg :lambda-list)
   hook-symbol))
(define-symbol-macro %events%
  (slot-value (c2mop:class-prototype (find-class 'event t environment)) 'events))
(defun %get-event (name environment)
  (getf %events% name))
(defun %add-event (name event environment)
  (setf (getf %events% name) event))
(defun %rem-event (name environment)
  (remf %events% name))

(defmethod initialize-instance :after ((instance event) &key name environment &allow-other-keys)
  (proclaim `(special ,(setf (slot-value instance hook-symbol) (gensym (string name)))))
  (%add-event name instance environment))

(defun find-event (name &optional (errorp t) environment) 
  (or (%get-event name environment)
      (when errorp
        (error "Event ~S not found." name))))
(defsetf find-event (name &optional (errorp t) environment) (value)
  `(if value
       (progn
         (check-type value event "a valid event.")
         (%add-event name value environment))
       (%rem-event name)))



(defmacro define-event (name (&rest lambda-list))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'event :name name
                           :lambda-list lambda-list)))
(defmacro event-handler-bind ((event (&rest args) &body body)
                              &body forms &environment environment)
  (with-slots (hook-symbol lambda-list)
      (find-event event t environment)
    (assert (awl:sub-lambda-list-p args lambda-list) ()
            "The lambda list ~S is not compatible with the ~
             event lambda list ~S." args lambda-list)
    `(let ((,hook-symbol (cons (lambda ,lambda-list . ,body)
                               ,hook-symbol)))
       . ,forms)))
(defun event (name &rest args)
  (with-slots (hook-symbol)
      (find-event name)
    (dolist (handler (symbol-value hook-symbol))
      (apply handler args))))
(define-compiler-macro event (&whole form name &rest args &environment environment)
  (if (constantp name)
      (with-slots (hook-symbol lambda-list)
          (find-event name t environment)
        (assert (awl:satisfies-lambda-list-p args lambda-list))
        `(dolist (handler ,hook-symbol)
           (funcall handler . ,args)))
      form))



;;;;* State Machine
;;;;
;;;; Let us take a small example of a use case: parsing XML:

(flet ((next)
       (look (&optional type))
       ())
  (labels
      ((token ()
         (with-output-to-string (*standard-output*)
           (loop :while (non-terminating-p (peek))
                 :do (wri-c))))
       (tag (c)
         (declare (ignore c))
         (let ((c (next)))
           (ecase c
             (#\? (<? c))
             (#\! (<! c))
             (#\/ (</ c))
             (t (element c)))))
       (<? ())
       (<! ())
       (</ ())
       (element ()))
    (ecase c
      (#\< ))))







;;;;* Data Validation by XML Schema
;;;;
;;;; Unfortunately XML is used everywhere and with it the brain
;;;; dead XML Schema for data validation.  Here, we try to implement
;;;; the minimum needed to verify and handle the Blueprint Silverbook
;;;; XML Schema.
;;;;
;;;;
