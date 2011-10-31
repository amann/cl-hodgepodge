
;;;;; Cursor interface
;;;;
;;;; Interface for cursors. A cursor is a (anonymous) function which when successively called
;;;; returns a new object. It may also be judicable to store the new value into a common (mutable)
;;;; object to avoid repeated consing. When no further object can be returned (eof), an error of type
;;;; `no-next-element-error' must be thrown or an eof object returned.
(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.cursor
  (:use)
  (:nicknames #:cursor))
(defpackage #:ch.amann-wolowyk.cursor-system
  (:use #:common-lisp))
(in-package #:ch.amann-wolowyk.cursor-system)

(define-condition cursor::no-next-element-error (error)
  ((cursor :initarg :cursor))
  (:report (lambda (condition stream)
             (format stream "There is no next element for cursor ~A."
                     (slot-value condition 'cursor))))
  (:documentation "Condition of supertype `error' which is signaled by the cursor when no next element can be generated."))

(defun cursor::map-cursors (type fn cursor &rest cursors)
  "Map the output of CURSORS to the function FN and return a sequence of the type TYPE containing the results of the mapping. If TYPE is nil, nil is returned. The loop terminates as soon as one of the cursors throws a `no-next-element-error'. It is therefore expected that (at least one of) the cursors throws a `no-next-element-error' to terminate the loop. TYPE is one of nil, list, vector or string."
  (let ((cursors (cons cursor cursors)))
    (ecase type
      ((nil) (handler-case
                 (loop (apply fn (mapcar #'funcall cursors)))
               (cursor::no-next-element-error ())))
      (list (let (collection)
              (handler-case
                  (loop (push (apply fn (mapcar #'funcall cursors))
                              collection))
                (cursor::no-next-element-error ()
                  (nreverse collection)))))
      ((vector string)
       (let ((collection (case type
                           (vector
                            (make-array '(1)
                                        :adjustable t :fill-pointer 0))
                           (string
                            (make-array '(1)
                                        :adjustable t :fill-pointer 0
                                        :element-type 'character)))))
         (handler-case
               (loop (vector-push-extend
                      (apply fn (mapcar #'funcall cursors))
                      collection))
           (cursor::no-next-element-error ()
             collection)))))))


(defgeneric cursor::make-cursor (object &key eof &allow-other-keys)
  (:documentation "Return a cursor taking OBJECT as base and returning EOF if non nil or throwing a `no-next-element-error' if EOF is nil. A cursor is a (anonymous) function which when successively called returns a new object. The specializer OUTPUT-TYPE may be used to specify the output type by implementing an adequate conversion."))

(defun cursor::make-list-cursor (list &key (step 1) key eof)
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(null list))
	 (eof-signal (or eof '(error 'cursor::no-next-element-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next `(dotimes (i ,step) (pop list))))
    (funcall (compile nil `(lambda ()
                             (let ((list ',list))
                               (lambda ()
                                 ,(if test
                                      `(if ,test
                                           ,eof-signal
                                           (prog1 ,get
                                             ,next))
                                      `(prog1 ,get
                                         ,next)))))))))

(defun cursor::make-number-cursor (n &key to (step 1) (eof nil))
  "Return a cursor producing numbers from N below TO by steps STEP. If TO <= N and 0 < STEP or N <= TO and STEP < 0 or if TO is not a number the cursor never terminates. If STEP is not a number it is set to 1. A STEP of 0 produces an infinite sequence of the constant N (Not really useful). If N is no a number it is set to 0. If EOF is not nil it is returned when reaching the terminating condition is reached instead of throwing a `no-next-element-error'."
  (let* ((n (if (numberp n)
		n
		0))
         (step (if (numberp step)
		   step
		   1))
	 (to (when (and (numberp to)
			(or (< (- n to) 0 step)
			    (< step 0 (- n to))))
	       to))
	 (test (when to `(<= ,to n)))
	 (eof-signal (or eof '(error 'cursor::no-next-element-error)))
	 (get 'n)
	 (next `(incf n ,step)))
    (eval `(let ((n ,n))
             #'(lambda ()
                 ,(if test
                      `(if ,test
                           ,eof-signal
                           (prog1
                               ,get
                             ,next))
                      `(prog1 ,get
                         ,next)))))))

(defmethod cursor::make-cursor ((n number) &key eof (step 1) to
                                &allow-other-keys)
  (cursor::make-number-cursor n :to to :step step :eof eof))
(defmethod cursor::make-cursor ((list list)&key eof (step 1) key
                                &allow-other-keys)
  (cursor::make-list-cursor list :step step :key key :eof eof))



(let ((package (find-package '#:ch.amann-wolowyk.cursor)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))