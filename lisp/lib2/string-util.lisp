(in-package #:cl-user)
(defpackage #:oam-utils
  (:use #:cl)
  (:export #:match-string
           #:string-replace
           #:if*
           #:when*))

(in-package #:oam-utils)

;;;;; IF* and WHEN* utilities
(defmacro if* (test then &optional else)
  "Like `if' but the return value of TEST is locally bound to the variable IT or <VAR> if the argument test is given in the form (<VAR> test)."
  (let* ((var (or (and (consp test) (= 2 (length test)) (car test))
                  'it))
         (test (if (eq var 'it)
                   test
                   (cadr test))))
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro when* (test &body then)
  "Like `when' but the return value of TEST is locally bound to the variable IT or <VAR> if the argument test is given in the form (<VAR> test)."
  (let* ((var (or (and (consp test) (= 2 (length test)) (car test))
                  'it))
         (test (if (eq var 'it)
                   test
                   (cadr test))))
    `(let ((,var ,test))
       (when ,var ,@then))))

;;;;; String Utilities
(defun match-string (target string)
  "Return the position in STRING (beginnig with 0) of the first (from the left) character of the first substring of STRING matching TARGET. If there is no match, nil is returned."
  (etypecase target
    (character
     (position target string))
    (string
     (let* ((tar0 (elt target 0))
            (n (length target)))
       (do ((position (position tar0 string) (let ((pos (position tar0 (subseq string (1+ position)))))
                                               (and pos (+ position pos 1)))))
           ((or (null position)
                (string= target (subseq string position (+ position n))))
            position))))))

(defun string-replace (prefix replace target string)
  "Return the concatenation of PREFIX and the string obtained form STRING by replacing in STRING all occurencies of TARGET by REPLACE."
  (if (string= "" string)
      prefix
      (let* ((position (match-string target string))
             (prefix (concatenate 'string prefix (if position (concatenate 'string (subseq string 0 position) replace) string)))
             (string (if position (subseq string (+ position (length target))) "")))
        (string-replace prefix replace target string))))


;;;;; Cursor interface
;;;;
;;;; Interface for cursors. A cursor is a (anonymous) function which when successively called
;;;; returns a new object. It may also be judicable to store the new value into a common (mutable)
;;;; object to avoid repeated consing. When no further object can be returned (eof), an error of type
;;;; `no-next-element-error' must be thrown or an eof object returned.
(in-package #:cl-user)
(defpackage #:cursor
  (:use #:common-lisp #:utils)
  (:export #:no-next-element-error
           #:map-cursors
           #:make-list-cursor
           #:make-mumber-cursor))
(in-package #:cursor)

(define-condition no-next-element-error (error)
  ((cursor :initarg :cursor))
  (:report (lambda (condition stream)
             (format stream "There is no next element for cursor ~A."
                     (slot-value condition 'cursor))))
  (:documentation "Condition of supertype `error' which is signaled by the cursor when no next element can be generated."))

(defun map-cursors (type fn &rest cursors)
  "Map the output of CURSORS to the function FN and return a sequence of the type TYPE containing the results of the mapping. If TYPE is nil, nil is returned. The loop terminates as soon as one of the cursors throws a `no-next-element-error'. It is therefore expected that (at least one of) the cursors throws a `no-next-element-error' to terminate the loop."
  (flet ((next-element ()
           (mapcar #'funcall cursors)))
    (ecase type
      ((vector string)
       (loop with result = (apply #'make-array '(1) :adjustable t :fill-pointer 0
                                  (case type (string (list :element-type 'character))))
          do (vector-push-extend (apply fn (next-element)) result)
          finally (return result)))
      (list
       (loop collect (apply fn (next-element))))
      ((nil)
       (loop (apply fn (next-element)))))))

(defun make-list-cursor (list &key (step 1) key eof)
  (let* ((list list)
         (step (if (numberp step)
		   step
		   1))
	 (test `(<= (length list) ,step))
	 (eof-signal (or eof '(error 'no-next-element-error)))
	 (get (if key
                  `(funcall ,key (car list))
                  '(car list)))
	 (next `(dotimes (i ,step) (pop list))))
    (eval `(let ((list ',list))
             #'(lambda ()
                 ,(if test
                      `(if ,test
                           ,eof-signal
                           (prog1
                               ,get
                             ,next))
                      `(prog1 ,get
                         ,next)))))))

(defun make-number-cursor (n &key to (step 1) (eof nil))
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
	 (eof-signal (or eof '(error 'no-next-element-error)))
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

