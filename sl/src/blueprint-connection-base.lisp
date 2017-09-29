(defpackage #:ch.swisslife.blueprint-connection-system
  (:use #:common-lisp) (:shadow #:close))
(in-package #:ch.swisslife.blueprint-connection-system)

(defvar *connection-specs*)
(load "blueprint-connection-spec")

(defun get-connection-spec (bp-level)
  (cdr (assoc bp-level *connection-specs*)))

(defun get-all-known-levels ()
  (mapcar #'car *connection-specs*))

(let ((allowed-levels (get-all-known-levels)))
  (defun check-level (level)
    (assert (member level allowed-levels :test #'eq) (level)
            "Level must be ~[~;the ~:;one of the possible ~]value~:P~{ ~A~#[~; or~:;,~]~}: "
            (length allowed-levels) allowed-levels)))

(oam:defmacro! define-connection-funs (package o!connection-fn o!close-fn)
  `(let ((cache nil))
     (flet ((get-connection (level)
              (cdr (assoc level cache)))
            (set-connection (level connection)
              (push (cons level connection) cache)
              connection))
       (defun ,(intern "CONNECT" package) (bp-level)
         (funcall ,#'check-level bp-level)
         (or (get-connection bp-level)
             (set-connection bp-level (apply ,g!connection-fn (get-connection-spec bp-level)))))
       (defun ,(intern "CLOSE" package) (bp-level)
         (funcall ,#'check-level bp-level)
         (let ((connection (get-connection bp-level)))
           (when connection (funcall ,g!close-fn connection)))))))


(defgeneric %host-name (connection))
(defun get-level (connection)
  (loop :with host-name := (%host-name connection)
     :for level :in (get-all-known-levels)
     :when (destructuring-bind (&key dbq &allow-other-keys)
               (get-connection-spec level)
             (string= dbq host-name))
     :do (return level)))
