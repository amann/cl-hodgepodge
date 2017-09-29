;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(in-package #:cl-user)
(defpackage #:ch.amann-wolowyk.active-object-pattern
  (:use)
  (:nicknames #:aop))
(defpackage #:ch.amann-wolowyk.active-object-pattern-system
  (:use #:cl))
(in-package #:ch.amann-wolowyk.active-object-pattern-system)
;;;;------------------------------------------------------

;;;;* Concurrency Utils
;;;;
;;;; This package contains utilities for concurrency. They are thread
;;;; save using mutexes and condition-variables. Following objects are
;;;; implemented:
;;;; - event-channel: a buffer of 'events' (items) which can be
;;;;   manipulated by two provided functions:
;;;;   - fetch-events(action &optional flush) action in (:try :wait)
;;;;     Fetch all events in the channel if available; when FLUSH is
;;;;     non nil, empty the channel.
;;;;   - add-event(event): Add event to the channel
;;;; - etc...

;;;;** Event Channel
;;;;
;;;; An event channel allows for the sending of events from one thread to another.

(defun make-event-channel ()
  "Create an event channel and return two functions as multiple-values:
- fetch-events(action &optional flush): Try to return the current content of the event queue. The argument ACTION must be either :try or :wait. If ACTION is :try then tries to acquire the lock and return the queue; if ACTION is :wait then blocks waiting until the event queue is filled. When the optional argument FLUSH is not nil then the queue is emptied when fetching.
- add-event(event): Add event to the event channel."
  (let ((events)
        (lock (bt:make-lock))
        (condition-variable (bt:make-condition-variable)))
    (labels ((fetch-events (flush)
               (prog1
                   (reverse events)
                 (when flush (setq events nil))))
             (try-fetch-events (flush)
               (when (bt:acquire-lock lock nil)
                 (unwind-protect
                      (fetch-events flush)
                   (bt:release-lock lock))))
             (wait-fetch-events (flush)
               (bt:with-lock-held (lock)
                 (or (fetch-events flush)
                     (progn
                       (bt:condition-wait condition-variable lock)
                       (fetch-events)))))
             (push-event (event)
               (bt:with-lock-held (lock)
                 (push event events)
                 (bt:condition-notify condition-variable))))
      (values
       ;; fetch-events(action &optional flush) action in (:try :wait)
       #'(lambda (action &optional flush)
           (ecase action
             (:try (try-fetch-events flush))
             (:wait (wait-fetch-events flush))))
       ;; add-event(event)
       #'(lambda (event)
           (bt:make-thread #'(lambda () (push-event event))))))))


;;;;** Queue
;;;;

;;;;*** Queue Interruption

(define-condition aop::queue-interruption ()
  ((queue-id :initarg :queue-id :reader aop::queue-id)
   (snapshot :initarg :snapshot :reader aop::snapshot))
  (:report (lambda (interruption stream)
             (format stream "Treatment of the queue ~A has been interrupted. ~
                     The status of the queue at the moment of the interruption was: ~&~A"
                     (aop::queue-id interruption)
                     (aop::snapshot interruption)))))

;;;;*** The Queue Constructor
(defun make-queue (id &optional default-order)
  "Create a thread save queue and return as multiple values four functions:
- add-item(item): push ITEM into the queue
- pop-next-request-fn(guard order-predicate): return the first item according to ORDER-PREDICATE with guard(item) not nil
- queue-empty-p(): return T if the queue is empty, nil else
- interrupt(): signal a condition of type `queue-interruption' and provides two restarts CONTINUE and REPLACE-QUEUE. The restart CONTINUE resumes and returns a snapshot of the current queue; the restart REPLACE-QUEUE expects a sequence of items and replace the current queue with it."
  (multiple-value-bind (fetch-input-queue add-to-input-queue)
      (make-event-channel)
    (fbind (fetch-input-queue add-to-input-queue)
        (let (output-queue
              (lock (bt:make-recursive-lock)))
          (labels ((update-output-queue (order-predicate)
                     (bt:with-recursive-lock-held (lock)
                       (setq output-queue (let ((output-queue (concatenate 'list output-queue
                                                                           (fetch-input-queue :wait t))))
                                            (if order-predicate
                                                (stable-sort output-queue order-predicate)
                                                output-queue)))))
                   (snapshot ()
                     (map 'list #'identity (update-output-queue default-order))))
            (values
             ;; add-item(item)
             #'add-to-input-queue
             ;; pop-next-item(guard order-predicate)
             #'(lambda (guard order-predicate)
                 (let (item)
                   (bt:with-recursive-lock-held (lock)
                     (multiple-value-setq (output-queue item)
                       (oam::remove-if #'(lambda (item)
                                           (funcall guard item))
                                       (update-output-queue order-predicate)
                                       :count 1)))
                   (first item)))
             ;; queue-empty-p()
             #'(lambda () (null (update-output-queue default-order)))
             ;; interrupt()
             #'(lambda ()
                 (bt:with-recursive-lock-held (lock)
                   (restart-case
                       (let ((snapshot (snapshot)))
                         (signal (make-condition 'aop::queue-interruption
                                                 :queue-id id
                                                 :snapshot snapshot))
                         snapshot)
                     (continue ()
                       :report "Continue."
                       snapshot)
                     (replace-queue (new-queue)
                       :report "Replace the queue."
                       :interactive (lambda ()
                                      (format t "Enter a sequence as replacement for the queue: ")
                                      (eval (read)))
                       (setq output-queue new-queue)))))))))))



;;;;** Future Calls
;;;;
;;;; The future call object calls a function with no arguments in another thread
;;;; and allows to 'communicate' with this thread by listening to signals, invoking
;;;; restarts and finally get the return value of the function. The signals can be
;;;; used for example to receive progress information.
;;;;
;;;;*** Conditions
;;;;
;;;;**** Condition Wrappers
;;;;
;;;; Condition wrappers wrap the condition signalled by the thread.
;;;; Those wrappers are then sent via an event channel to the listening
;;;; thread where the original condition is unwrapped and signalled again.
;;;; For restartable conditions a list of the available restarts is also wrapped.

(define-condition request-condition-wrapper ()
  ((condition :initarg :condition :reader aop::condition))
  (:documentation "Condition wrapping a condition or error signalled by the call of the request. This condition is transfered to the emitant of the request. The reader CONDITION returns the wrapped condition."))
(define-condition request-restartable-condition-wrapper (request-condition-wrapper)
  ((restarts :initarg :restarts :reader aop::restarts))
  (:documentation "Condition wrapping a condition or error signalled by the call of the request. This condition is transfered to the emitant of the request. The reader RESTARTS allows to review all available restarts."))

;;;;**** Status Signals
;;;;
;;;; Status are a special case of condition wrappers. They are handled
;;;; in such a way that they only transfer information and immediately
;;;; invoke a continue restart. It is intended that those status info
;;;; be sent by the function send-status.

(define-condition request-status () ()
  (:documentation "Conditions for signalling the status of the request."))
(define-condition aop::request-status-pending (request-status) ()
  (:documentation "Condition to signal that the request is still pending and not failed. For signaling more information, like a progression status of an estimated time, convenient subclasses may be defined. This condition is intended to be signalled by the request function itself."))
(define-condition aop::request-status-succeed (request-status)
  ((return-values)))
(define-condition aop::request-status-failed (request-status request-condition-wrapper) ()
  (:documentation "Condition to signal the failure of the request. No restart is available here. This condition is intended to be signalled by the request function itself. The reader CONDITION returns the original condition which may be at the origin of the failure."))

(defmethod aop::condition ((status request-status))
  nil)
(defmethod aop::restarts ((status request-status))
  nil)
(defun aop::send-status (request-status &rest args)
  "Send the present status of the request. REQUEST-STATUS must inherit `request-status-pending'."
  (assert (typep request-status 'aop::request-status-pending) nil
          "REQUEST-STATUS = ~S is of type ~A and does not inherit `request-status-pending' as expected."
          request-status (class-name (class-of request-status)))
  (restart-case
      (apply #'signal request-status args)
    (continue-request ())))

;;;;*** Future Constructor

(defun make-future-call (fn)
  (multiple-value-bind (fetch-value push-value)
      (make-event-channel)
    (multiple-value-bind (fetch-restart push-restart)
        (make-event-channel)
      (oam:fbind (fetch-value push-value fetch-restart push-restart)
        (let ((already-called-fn #'(lambda ()
                                     (error "Function has already been called.")))
              (call-fn #'(lambda ()
                           (bt:make-thread
                            #'(lambda ()
                                (push-value
                                 (multiple-value-list 
                                  (handler-case
                                      (funcall fn)
                                    (aop::request-status-pending (status)
                                      (push-value status)
                                      (invoke-restart (find-restart 'continue-request)))
                                    (aop::request-status-failed (status)
                                      (push-value status)
                                      nil)
                                    (condition (c)
                                      (fetch-value :wait t)
                                      (fetch-restart :wait t)
                                      (push-value (make-condition 'request-restartable-condition-wrapper
                                                                  :condition c
                                                                  :restarts (remove-duplicates
                                                                             (mapcar #'restart-name (compute-restarts c)))))
                                      (let ((restart (fetch-restart :wait t)))
                                        (when restart (invoke-restart (find-restart restart c)))))))))))))
          (values
           ;; fetch-values(action) action in (:try :wait)
           #'(lambda (action)
               (or (multiple-value-list (fetch-value action))
                   (let ((condition (fetch-value action)))
                     (if (typep condition 'request-status)
                         (signal condition)
                         (let ((orig-cond (aop::condition condition))
                               (restarts (restarts condition)))
                           (restart-case
                               (typecase orig-cond
                                 (error (error condition))
                                 (t (signal condition)))
                             (chose-restart (restart)
                               :report (lambda (s)
                                         (let ((i -1))
                                           (format s "Following restarts are available:")
                                           (map nil #'(lambda (r)
                                                        (format s "~&~D:~8T~S" (incf i) r))
                                                restarts)))
                               :interactive (lambda ()
                                              (format t "Chose a restart (Enter the appropriate number):")
                                              (nth (read) restarts))
                               (funcall push-restart-fn restart))
                             (abort-request ()
                               :report "Abort request."
                               (funcall push-restart-fn nil))))))))
           ;; call()
           #'(lambda ()
               (prog1
                   (funcall call-fn)
                 (setq call-fn already-called-fn)))))))))


(defun aop::try-fetch-values (future)
  "Return the values of the future if available, else returns nil and signals a condition of type `REQUEST-STATUS-PENDING' if the processing of the request is still pending or a condition of type `REQUEST-STATUS-FAILED' if the processing failed. If the call of the requested function signals an error, this error is transfered to the caller of `TRY-FETCH-VALUES' via `REQUEST-STATUS-FAILED'."
  (funcall future :try))
(defun aop::wait-for-values (future)
  "Like `TRY-FETCH-VALUES' but is waiting until the servant side notified a changement of the status."
  (funcall future :wait))




;;;;** The Scheduler
;;;;
;;;; The scheduler is the main agent of the active object pattern: it dispaches
;;;; the queued requests.
(defun make-new-scheduler (id request-constructor-fn request-caller-fn guard-fn priority-order-fn)
  "Create a new scheduler and return two functions as multiple-values of one mandatory argument. The argument MESSAGE of the first function can take one of the following values having following meanings:
- :active-p    - return T if the scheduler is active, nil else
- :empty-p     - return T if the queue is empty, nil else
- :start       - start the scheduler after creating it or if previously stopped
- :stop        - return a snapshot of the request queue and signaling of type `queue-interruption' allowing to use among others following restarts:
                 - CONTINUE : continue and leave everything as is
                 - REPLACE-QUEUE : allowing to replace the queue by a new sequence of requests. The requests should be of the same type
                 - STOP : stop the scheduler; notice that to stop the scheduler, the signal `queue-interruption' must be handled.
The second function adds a new request, hold in the argument REQUEST, to the queue.

The args of MAKE-NEW-SCHEDULER are following:
- ID                - An identifyer for the scheduler
- CALL-REQUEST-FN   - A function taking a request object and applying the inherent message function to the future setter function
- GUARD-FN          - A function taking a request object and returning T if the request is ready or allowed to be treated
- PRIORITY-ORDER-FN - A function taking two request objects r1 and r2 and returning t or non nil if r1 has higher priority than r2 and nil else"
  (let ((lock (bt:make-lock))
        (continue-p t) thread
        add-request-fn pop-next-request-fn
        queue-empty-p-fn list-all-requests-fn interrupt-fn)
    (multiple-value-setq (add-request-fn pop-next-request-fn queue-empty-p-fn interrupt-fn)
      (make-queue id guard-fn priority-order-fn))
    (labels ((continue-p ()
               (bt:with-lock-held (lock)
                 continue-p))
             (stop ()
               (bt:with-lock-held (lock)
                 (setq continue-p nil)
                 (loop while (bt:thread-alive-p thread)
                    finally (setq thread nil))))
             (start ()
               (bt:with-lock-held (lock)
                 (unless (and (bt:threadp thread) (bt:thread-alive-p thread))
                   (setq thread (bt:make-thread #'(lambda ()
                                                    (loop
                                                       (let ((request (funcall pop-next-request-fn guard-fn priority-order-fn)))
                                                         (unless (continue-p) (return))
                                                         (funcall request-caller-fn request))))))))))
      (values #'(lambda (message &optional request)
                  (ecase message
                    (:active-p (not (null thread)))
                    (:empty-p (funcall queue-empty-p-fn))
                    (:start (start))
                    (:stop (unless (funcall queue-empty-p-fn)
                             (restart-case
                                 (funcall interrupt-fn)
                               (stop ()
                                 :report "Stop the scheduler."
                                 (stop)))))))
              #'(lambda (fn guard priority)
                  (multiple-value-bind (future caller)
                      (make-future fn)
                   (bt:make-thread #'(lambda ()
                                       (funcall add-request-fn
                                                (make-request caller guard priority))))
                   future))))))
(defun aop::scheduler-active-p (scheduler)
  (funcall scheduler :active-p))
(defun aop::stop-scheduler (scheduler)
  (funcall scheduler :stop))




;;;;** Request
(defun make-request (fn guard priority future-setter)
  #'(lambda (type)
      (ecase type
        (:function fn)
        (:guard guard)
        (:priority priority)
        (:future-setter future-setter))))



(defun send-request (scheduler fn guard priority)
  (multiple-value-bind (future future-setter)
      (make-future)
    (bt:make-thread #'(lambda ()
                        (funcall scheduler :add-request (make-request fn guard priority future-setter))))
    future))

(defvar aop::*scheduler* nil
  "Special variable containing the default scheduler. It is intended to be locally bound.")
(defun aop::make-scheduled-function (fn &optional guard)
  #'(lambda (priority &rest args)
       (send-request aop::*scheduler*
                     #'(lambda ()
                         (apply fn args))
                     guard priority)))



(defmacro aop::define-scheduled-function (name (&rest args) (&key documentation (guard #'(lambda () t))) &body body)
  `(prog1
       (defun ,name ,(list* 'priority args)
         ,(format nil "~@[~A~] The used scheduler is taken from the variable *scheduler*." documentation)
         (declare (ignore priority ,@args)))
     (setf (symbol-function ',name)
           (aop::make-scheduled-function #'(lambda ,args ,@body) ,guard))))



;;;;--------------------------------------------------------
(let ((package (find-package '#:aop)))
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (export symbol package))))
