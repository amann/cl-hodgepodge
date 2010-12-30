;;;; -*- outline-regexp:";;;;[*]+ +" -*-

;;;;* The Unit testing Framework
;;;; This package offers a simple framework for unit testing.
(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.oam-unit-testing
  (:use)
  (:nicknames #:utest))
(defpackage #:ch.amann-wolowyk.oam-unit-testing-system
  (:use #:cl))
(in-package #:ch.amann-wolowyk.oam-unit-testing-system)

(defmacro utest::define-unit-test (name (&rest parameters)
                                   &body forms)
  "Define a unit test function of name NAME executing the forms one by one and reporting if they failed, i.e. their return value was NIL, or passed."
  `(defun ,name (,@parameters)
     (declare (special *indent*))
     (let ((*indent* (if (boundp '*indent*) *indent* 0)))
       (declare (special *indent*))
       (format t "~&~vT~A~@[ with parameters: ~:{~A = ~A~}~]"
               *indent* ',name (list ,@(mapcar #'(lambda (p)
                                                   `(list ',p ,p))
                                               parameters)))
       (let ((result (notany #'null
                             (let ((*indent* (+ 2 *indent*)))
                               (declare (special *indent*))
                               (list ,@(mapcar #'(lambda (form)
                                                   `(handler-case
                                                        (report-result ,form ',form)
                                                      (error (c)
                                                        (report-result nil ',form c))))
                                               forms))))))
         (when (= 0 *indent*)
           (report-result result ',name))
         result))))

(defun report-result (result form &optional c)
  (declare (special *indent*))
  (format t "~&~v,0T~v<~A~; ... ~;~:[FAILED~;passed~].~>~@[ Condition ~S was signalled.~]" *indent* 80 form result c)
  result)

(eval-when (:load-toplevel :execute)
  (let ((package (find-package '#:utest)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (export symbol package)))))
