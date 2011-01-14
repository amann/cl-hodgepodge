;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.active-object-pattern
  (:use)
  (:nicknames #:aop))
(defpackage #:ch.amann-wolowyk.active-object-pattern-system
  (:use #:cl))
(in-package #:ch.amann-wolowyk.active-object-pattern-system)


(defclass aop::scheduler () ()
  (:metaclass 'openmcl-mop:funcallable-standard-class))

(defun aop::scheduler-active-p (scheduler)
  (funcall scheduler :active-p))

(defun aop::stop-scheduler (scheduler)
  (funcall scheduler :stop))

(defmethod initialize-instance :after ((self aop::scheduler)
                                       &key &allow-other-keys)
  (setf (slot-value self 'thread)
        (bt:make-thread #'(lambda ()
                            (with-slots (continue-p) self
                              )))))
(defvar aop::*current-future* nil
  "This special variable is bound to the future associated to the scheduled function at the moment of the call. It can then be used in the called function to send status information to the future.")
(defun start-new-scheduler (message-fn get-future-fn guard-fn priority-order-fn)
  (let ((lock (bt:make-lock))
        (continue-p t) thread
        add-request-fn pop-next-request-fn
        queue-empty-p-fn list-all-requests-fn interrupt-fn)
    (multiple-value-setq (add-request-fn pop-next-request-fn queue-empty-p-fn list-all-requests-fn interrupt-fn)
      (make-queue))
    (labels ((continue-p ()
               (bt:with-lock-held (lock)
                 continue-p))
             (dispatch ()
               (funcall pop-next-request-fn #'(lambda (request)
                                                ()))))
     (setq thread (bt:make-thread #'(lambda ()
                                      (loop
                                         (let ((request (funcall pop-next-request-fn
                                                                 guard-fn priority-order-fn)))
                                           (unless (continue-p) (return))
                                           (let ((aop::*current-future* (funcall get-future-fn request)))
                                             (funcall message-fn request))))))))
    #'(lambda (message &optional request)
        (ecase message
          (:add-request (funcall add-request-fn request))
          (:active-p (continue-p))
          (:empty-p (funcall queue-empty-p-fn))
          (:stop (unless (funcall queue-empty-p-fn)
                   (restart-case
                       (error "There are still pending requests.")
                     (force-stop ()
                       :report "Force stopping the scheduler."
                       (setq continue-p nil)) 
                     (let-it-be ()
                       :report "Leave the scheduler running."
                       (setq continue-p t))))
                 (bt:with-lock-held (lock)
                   (setq continue-p nil)))))))

(define-condition aop::queue-interruption ()
  ((id :initarg :id :reader aop::id)
   (snapshot :initarg :snapshot :reader aop::snapshot))
  (:report (lambda (interruption stream)
             (format stream "Treatment of the queue ~A has been interrupted. ~
                     The status of the queue at the moment of the interruption was: ~&~A"
                     (aop::id interruption)
                     (aop::snapshot interruption)))))
(defun make-queue (id)
  (let (input-queue output-queue
        (i-lock (bt:make-lock))
        (o-lock (bt:make-recursive-lock)))
    (labels ((update-output-queue (order-predicate)
               (let ((input-queue (bt:with-lock-held (lock)
                                    (prog1 input-queue
                                      (setq input-queue nil)))))
                 (bt:with-recursive-lock-held (o-lock)
                   (setq output-queue (let ((output-queue (concatenate 'list output-queue
                                                                       (nreverse input-queue))))
                                        (if order-predicate
                                            (stable-sort output-queue order-predicate)
                                            output-queue)))))))
      (values
       ;; add-item()
       #'(lambda (item)
           (bt:with-lock-held (lock)
             (push item input-queue)))
       ;; pop-next-item(guard priority-order)
       #'(lambda (guard order-predicate)
           (let (item)
             (bt:with-recursive-lock-held (o-lock)
               (multiple-value-setq (output-queue item)
                 (oam::remove-if #'(lambda (item)
                                     (funcall guard item))
                                 (update-output-queue priority-order)
                                 :count 1)))
             (first item)))
       ;; queue-empty-p()
       #'(lambda () (null (update-output-queue nil)))
       ;; list-all-items()
       #'(lambda () (map 'list #'identity (update-output-queue nil)))
       ;; interrupt()
       #'(lambda ()
           (bt:with-recursive-lock-held (o-lock)
             (restart-case
                 (signal (make-condition 'aop::queue-interruption :id id
                                         :snapshot (update-output-queue nil)))
               (continue ()
                 :report "Continue.")
               (replace-queue (new-queue)
                 :report "Replace the queue."
                 :interactive #'(lambda ()
                                  (format t "Enter a sequence as replacement for the queue: ")
                                  (eval (read)))))))))))

;;;;** Request
(defun make-request (fn guard priority future-setter)
  #'(lambda (type)
      (ecase type
        (:function fn)
        (:guard guard)
        (:priority priority)
        (:future-setter future-setter))))

;;;;** Future
(define-condition future-status ()
  ())
(define-condition aop::future-status-pending (future-status)
  ())
(define-condition aop::future-status-failed (future-status)
  ((reason :reader aop::reason)))
(defclass aop::future () ()
  (:metaclass 'openmcl-mop:funcallable-standard-class))
(defun aop::try-fetch-values (future)
  "Return the values of the future if available, else returns nil and signals a condition of type `FUTURE-STATUS-PENDING' if the processing of the request is still pending or a condition of type `FUTURE-STATUS-FAILED' if the processing failed. If the call of the requested function signals an error, this error is transfered to the caller of `TRY-FETCH-VALUES' via `FUTURE-STATUS-FAILED'."
  (funcall future :try))
(defun aop::wait-for-values (future)
  "Like `TRY-FETCH-VALUES' but is waiting until the servant side notified a changement of the status."
  (funcall future :wait))

(defun make-future ()
  (let (values
        (condition (make-condition 'aop::future-status-pending))
        (lock (bt:make-lock))
        (condition-variable (bt:make-condition-variable)))
    (labels ((try-fetch-value ()
               (when (bt:acquire-lock lock nil)
                           (handler-case
                               (if condition
                                   (signal condition)
                                   (prog1
                                       (values-list values)
                                     (bt:release-lock lock)))
                             (condition (c)
                               (bt:release-lock lock)
                               (typecase c
                                 (error (error c))
                                 (t (signal c))))))))
     (values (let ((future (make-instance 'aop::future)))
               (openmcl-mop:set-funcallable-instance-function
                future
                #'(lambda (action)
                    (ecase action
                      (:try (try-fetch-value))
                      (:wait (bt:with-lock-held (lock)
                               (bt:condition-wait condition-variable lock))
                             (try-fetch-value)))))
               future)
             #'(lambda (message-fn)
                 (bt:with-lock-held (lock)
                   (handler-case
                       (setq values (multiple-value-list (funcall message-fn))
                             condition nil)
                     ())
                   (ecase action
                     (:set-condition (setq condition v))
                     (:set-value (setq values v
                                       condition nil)
                                 (bt:condition-notify condition-variable)))))))))





(defun send-request (scheduler fn guard)
  (multiple-value-bind (future future-setter)
      (make-future)
    (bt:make-thread #'(lambda ()
                          ()))))


(defun aop::attach-function-to-scheduler (fn scheduler &optional guard (priority 0))
  #'(lambda (&rest args)
       (send-request scheduler fn guard)))



(defmacro aop::define-scheduled-function (name guard (&rest args) &body body)
  `(progn
     (defun ,name ,(list* scheduler args))
     (setf (symbol-function ,name) ,(list* 'make-scheduled-function scheduler guard args body))))



;;;;--------------------------------------------------------
(let ((package (find-package '#:aop)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))