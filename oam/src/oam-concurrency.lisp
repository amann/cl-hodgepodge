;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(in-package #:cl-user)
(oam::define-project-package #:ch.amann-wolowyk.concurrency #:ccy)
(in-package #:ch.amann-wolowyk.concurrency-system)
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
;;;; It provides two functions: 
;;;;   - fetch-events(action &optional flush) action in (:try :wait)
;;;;     Fetch all events in the channel if available; when FLUSH is
;;;;     non nil, empty the channel.
;;;;   - add-event(event): Add event to the channel

(defun ccy::make-event-channel ()
  "Create an event channel and return two functions as multiple-values:
- fetch-events(&optional timeout flush-p): Return the current content of the event queue. The argument TIMEOUT must be either NIL or a non-negative number representing seconds. If TIMEOUT is NIL then tries to acquire the lock and return the queue; if TIMEOUT is non NIL then blocks waiting at most TIMEOUT seconds or until a new event is added to the chain. When the optional argument FLUSH-P is not NIL then the queue is emptied when fetching.
- add-event(event): Add EVENT to the event channel."
  (let ((events)
        (lock (bt:make-lock))
        (condition-variable (bt:make-condition-variable)))
    (labels ((fetch-events (flush-p)
               (prog1
                   events
                 (when flush-p (setq events nil))))
             (try-fetch-events (flush-p)
               (bt:with-lock-held (lock)
                 (fetch-events flush-p)))
             (wait-fetch-events (timeout flush-p)
               (handler-case
                   (bt:with-timeout (timeout)
                     (bt:with-lock-held (lock)
                       (or (fetch-events flush-p)
                           (progn
                             (bt:condition-wait condition-variable lock)
                             (fetch-events flush-p)))))
                 (bt:timeout ()
                   nil)))
             (push-event (event)
               (bt:with-lock-held (lock)
                 (push event events)
                 (bt:condition-notify condition-variable))))
      (values
       ;; fetch-events(&optional timeout flush-p) timeout in seconds
       #'(lambda (&optional timeout flush-p)
           "Return the current content of the event queue. The argument TIMEOUT must be either NIL or a non-negative number representing seconds. If TIMEOUT is NIL then try to acquire the lock and return the queue; if TIMEOUT is non NIL then block waiting at most TIMEOUT seconds or until a new event is added to the chain. When the optional argument FLUSH-P is not NIL then the queue is emptied when fetching."
           (if timeout
               (wait-fetch-events timeout flush-p)
               (try-fetch-events flush-p))
           (fetch-events flush-p))
       ;; add-event(event)
       #'(lambda (event)
           "Add EVENT to the event channel."
           (bt:make-thread #'(lambda () (push-event event))))))))



;;;;** Queue
;;;;

;;;;*** Queue Interruption

(define-condition ccy::queue-interruption ()
  ((queue-id :initarg :queue-id :reader ccy::queue-id)
   (snapshot :initarg :snapshot :reader ccy::snapshot))
  (:report (lambda (interruption stream)
             (format stream "Treatment of the queue ~A has been interrupted. ~
                     The status of the queue at the moment of the interruption was: ~&~A"
                     (ccy::queue-id interruption)
                     (ccy::snapshot interruption)))))

;;;;*** The Queue Constructor
(defun ccy::make-queue (id &optional default-order)
  "Create a thread save queue and return as multiple values four functions:
- add-item(item): push ITEM into the queue
- pop-next-request-fn(guard order-predicate): return the first item according to ORDER-PREDICATE with guard(item) not nil
- queue-empty-p(): return T if the queue is empty, NIL else
- interrupt(): signal a condition of type `queue-interruption' and provides two restarts CONTINUE and REPLACE-QUEUE. The restart CONTINUE resumes and returns a snapshot of the current queue; the restart REPLACE-QUEUE expects a sequence of items and replace the current queue with it."
  (multiple-value-bind (fetch-input-queue add-to-input-queue)
      (ccy::make-event-channel)
    (oam:fbind (fetch-input-queue add-to-input-queue)
        (let (output-queue
              (lock (bt:make-recursive-lock)))
          (labels ((update-output-queue (order-predicate)
                     (bt:with-recursive-lock-held (lock)
                       (setq output-queue (let* ((snapshot (fetch-input-queue nil t))
                                                 (output-queue (concatenate 'list output-queue snapshot)))
                                            (if (and snapshot order-predicate)
                                                (stable-sort output-queue order-predicate)
                                                output-queue)))))
                   (snapshot ()
                     (map 'list #'identity (update-output-queue default-order))))
            (values
             ;; add-item(item)
             #'add-to-input-queue
             ;; pop-next-item(guard order-predicate)
             #'(lambda (guard order-predicate)
                 "Return the first item according to ORDER-PREDICATE with GUARD(item) not nil."
                 (let (item)
                   (bt:with-recursive-lock-held (lock)
                     (multiple-value-setq (output-queue item)
                       (oam::remove-if #'(lambda (item)
                                           (funcall guard item))
                                       (update-output-queue order-predicate)
                                       :count 1)))
                   (first item)))
             ;; queue-empty-p()
             #'(lambda () "Return T if the queue is empty, NIL else." (null (update-output-queue default-order)))
             ;; interrupt()
             #'(lambda ()
                 "Signal a condition of type `queue-interruption' and provides two restarts CONTINUE and REPLACE-QUEUE. The restart CONTINUE resumes and returns a snapshot of the current queue; the restart REPLACE-QUEUE expects a sequence of items, replaces the current queue with this sequence and returns it."
                 (bt:with-recursive-lock-held (lock)
                   (let ((snapshot (snapshot)))
                     (restart-case
                         (progn
                           (signal (make-condition 'ccy::queue-interruption
                                                   :queue-id id
                                                   :snapshot snapshot))
                           snapshot)
                       (continue ()
                         :report "Continue."
                         (list snapshot))
                       (replace-queue (new-queue)
                         :report "Replace the queue."
                         :interactive (lambda ()
                                        (format t "Enter a sequence as replacement for the queue: ")
                                        (eval (read)))
                         (setq output-queue (map 'list #'identity new-queue))
                         (list new-queue))))))))))))



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
  ((condition :initarg :condition :reader ccy::condition))
  (:documentation "Condition wrapping a condition or error signalled by the call of the request. This condition is transfered to the emitant of the request. The reader CONDITION returns the wrapped condition."))
(define-condition request-restartable-condition-wrapper (request-condition-wrapper)
  ((restarts :initarg :restarts :reader ccy::restarts))
  (:documentation "Condition wrapping a condition or error signalled by the call of the request. This condition is transfered to the emitant of the request. The reader RESTARTS allows to review all available restarts."))

;;;;**** Status Signals
;;;;
;;;; Status are a special case of condition wrappers. They are handled
;;;; in such a way that they only transfer information and immediately
;;;; invoke a continue restart. It is intended that those status info
;;;; be sent by the function send-status.

(define-condition request-status () ()
  (:documentation "Conditions for signalling the status of the request."))
(define-condition ccy::request-status-call-pending (request-status) ()
  (:documentation "Condition to signal that the calling request is still pending and not yet called."))
(define-condition ccy::request-status-called (request-status) ()
  (:documentation "Condition to signal that the function has been called. For signaling more information, like a progression status of an estimated time, convenient subclasses may be defined. This condition is intended to be signalled by the request function itself."))
(define-condition ccy::request-status-succeed (request-status)
  ((return-values)))
(define-condition ccy::request-status-failed (request-status request-condition-wrapper) ()
  (:documentation "Condition to signal the failure of the request. No restart is available here. This condition is intended to be signalled by the request function itself. The reader CONDITION returns the original condition which may be at the origin of the failure."))

(defmethod ccy::condition ((status request-status))
  nil)
(defmethod ccy::restarts ((status request-status))
  nil)
(defun ccy::send-status (request-status &rest args)
  "Send the present status of the request. REQUEST-STATUS must inherit `request-status-called'."
  (assert (typep request-status 'ccy::request-status-called) nil
          "REQUEST-STATUS = ~S is of type ~A and does not inherit `request-status-called' as expected."
          request-status (class-name (class-of request-status)))
  (restart-case
      (apply #'signal request-status args)
    (continue-process ())))

;;;;*** Future Call Constructor

(defun ccy::make-future-call (fn)
  (multiple-value-bind (fetch-value push-value)
      (ccy::make-event-channel)
    (multiple-value-bind (fetch-restart push-restart)
        (ccy::make-event-channel)
      (oam:fbind (fetch-value push-value fetch-restart push-restart)
        (let ((lock (bt:make-lock))
              (call-pending-condition (make-condition 'ccy::request-status-call-pending))
              (already-called-fn #'(lambda ()
                                     (error "Function has already been called.")))
              (call-fn #'(lambda ()
                           (bt:make-thread
                            #'(lambda ()
                                (push-value
                                 (multiple-value-list 
                                  (handler-case
                                      (funcall fn)
                                    (ccy::request-status-called (status)
                                      (push-value status)
                                      (invoke-restart (find-restart 'continue-process)))
                                    (ccy::request-status-failed (status)
                                      (push-value status)
                                      nil)
                                    (condition (c)
                                      (fetch-value nil t)
                                      (fetch-restart nil t)
                                      (push-value (make-condition 'request-restartable-condition-wrapper
                                                                  :condition c
                                                                  :restarts (remove-duplicates
                                                                             (mapcar #'restart-name (compute-restarts c)))))
                                      (let ((restart (fetch-restart nil t)))
                                        (when restart (invoke-restart (find-restart restart c)))))))))))))
          (values
           ;; fetch-values(timeout)
           #'(lambda (&optional timeout)
               (let ((condition (fetch-value timeout t)))
                 (typecase condition
                   (null (signal call-pending-condition))
                   (request-status (signal condition))
                   (t (let ((orig-cond (ccy::condition condition))
                            (restarts (fetch-restart condition)))
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
                            (push-restart restart))
                          (abort-request ()
                            :report "Abort request."
                            (push-restart nil))))))))
           ;; call()
           #'(lambda ()
               (bt:with-lock-held (lock)
                 (prog1
                     (funcall call-fn)
                   (setq call-fn already-called-fn))))))))))


(defun ccy::try-fetch-values (future)
  "Return the values of the future if available, else returns nil and signals a condition of type `REQUEST-STATUS-PENDING' if the processing of the request is still pending or a condition of type `REQUEST-STATUS-FAILED' if the processing failed. If the call of the requested function signals an error, this error is transfered to the caller of `TRY-FETCH-VALUES' via `REQUEST-STATUS-FAILED'."
  (funcall future nil))
(defun ccy::wait-for-values (future timeout)
  "Like `TRY-FETCH-VALUES' but is waiting until the servant side notified a changement of the status."
  (funcall future timeout))




;;;;** The Scheduler
;;;;
;;;; The scheduler is the main agent of the active object pattern: it dispaches
;;;; the queued requests.
(defun ccy::make-scheduler (id guard-fn priority-order-fn)
  "Create a new scheduler and return two functions as multiple-values of one mandatory argument. The argument MESSAGE of the first function can take one of the following values having following meanings:
- :active-p    - return T if the scheduler is active, nil else
- :empty-p     - return T if the queue is empty, nil else
- :start       - start the scheduler after creating it or if previously stopped
- :stop        - return a snapshot of the request queue and signaling of type `queue-interruption' allowing to use among others following restarts:
                 - CONTINUE : continue and leave everything as is
                 - REPLACE-QUEUE : allowing to replace the queue by a new sequence of requests. The requests should be of the same type
                 - STOP : stop the scheduler; notice that to stop the scheduler, the signal `queue-interruption' must be handled.
The second function adds a new request, hold in the argument REQUEST, to the queue.

The args of MAKE-NEW-SCHEDULER are the following:
- ID                - An identifyer for the scheduler
- GUARD-FN          - A function taking the argument GUARD given while sending the request and returning T if the request is ready or allowed to be treated
- PRIORITY-ORDER-FN - A function taking the priorities of two request r1 and r2 and returning t or non nil if r1 has higher priority than r2 and nil else"
  (let ((lock (bt:make-lock))
        (continue-p t) thread)
    (multiple-value-bind (add-request pop-next-request queue-empty-p interrupt)
        (ccy::make-queue id)
      (oam:fbind (add-request pop-next-request queue-empty-p interrupt)
       (labels ((make-request (future-call guard priority)
                  #'(lambda (action-type)
                      (ecase action-type
                        (:call (funcall future-call))
                        (:guard guard)
                        (:priority priority))))
                (continue-p ()
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
                      (setq continue-p t
                            thread (bt:make-thread #'(lambda ()
                                                       (loop
                                                          (oam:fbind ((request (pop-next-request #'(lambda (request)
                                                                                                     (funcall guard-fn (funcall request :guard)))
                                                                                                 #'(lambda (r1 r2)
                                                                                                     (funcall priority-order-fn
                                                                                                              (funcall r1 :priority)
                                                                                                              (funcall r2 :priority))))))
                                                            (unless (continue-p) (return))
                                                            (request :call))))))))))
         (values #'(lambda (message &optional request)
                     "The argument MESSAGE of the function can take one of the following values having following meanings:
- :active-p    - return T if the scheduler is active, nil else
- :empty-p     - return T if the queue is empty, nil else
- :start       - start the scheduler after creating it or if previously stopped
- :stop        - return a snapshot of the request queue and signaling of type `queue-interruption' allowing to use among others following restarts:
                 - CONTINUE : continue and leave everything as is
                 - REPLACE-QUEUE : allowing to replace the queue by a new sequence of requests. The requests should be of the same type
                 - STOP : stop the scheduler; notice that to stop the scheduler, the signal `queue-interruption' must be handled."
                     (ecase message
                       (:active-p (not (null thread)))
                       (:empty-p (queue-empty-p))
                       (:start (start))
                       (:stop (unless (queue-empty-p)
                                (restart-case
                                    (interrupt)
                                  (stop ()
                                    :report "Stop the scheduler."
                                    (stop)))))))
                 #'(lambda (fn guard priority)
                     "Send a request to call the function with no argument FN. The argument GUARD will be the argument of GUARD-FN given to the constructor of the scheduler. The argument PRIORITY will be used as one of the agruments for the function PRIORITY-ORDER-FN."
                     (multiple-value-bind (future future-call)
                         (make-future fn)
                       (add-request (make-request future-call guard priority))
                       future))))))))

(defclass ccy::scheduler ()
  ((control-fn :type function))
  (:metaclass c2mop:funcallable-standard-class))
(defmethod initialize-instance :after ((self ccy::scheduler)
                                       &key guard-fn priority-order-fn)
  (multiple-value-bind (control-fn send-request-fn)
      (make-scheduler self guard-fn priority-order-fn)
    (setf (slot-value self 'control-fn) control-fn)
    (c2mop:set-funcallable-instance-function self send-request-fn)))


(defun ccy::scheduler-active-p (scheduler)
  (funcall (slot-value scheduler 'control-fn) :active-p))
(defun ccy::scheduler-empty-p (scheduler)
  (funcall (slot-value scheduler 'control-fn) :empty-p))
(defun ccy::start-scheduler (scheduler)
  (funcall (slot-value scheduler 'control-fn) :start))
(defun ccy::stop-scheduler (scheduler)
  (funcall (slot-value scheduler 'control-fn) :stop))


(defun ccy::send-call-request (scheduler fn guard priority)
  (funcall scheduler fn guard priority))

(defvar ccy::*scheduler* nil
  "Special variable containing the default scheduler. It is intended to be locally bound.")

(defun ccy::make-scheduled-function (fn &optional guard (scheduler ccy::*scheduler*))
  "Return a function whose calls are transferred to the scheduler SCHEDULER."
  #'(lambda (priority &rest args)
      (ccy::send-call-request scheduler
                              #'(lambda ()
                                  (apply fn args))
                              guard priority)))

(defmacro ccy::define-scheduled-function (name (&rest args) (&key documentation (guard #'(lambda () t))) &body body)
  `(prog1
       (defun ,name ,(list* 'priority args)
         ,(format nil "~@[~A~] The used scheduler is taken from the variable *scheduler*." documentation)
         (declare (ignore priority ,@args)))
     (setf (symbol-function ',name)
           (ccy::make-scheduled-function #'(lambda ,args ,@body) ,guard))))



;;;;--------------------------------------------------------
(oam:export-interface '#:ccy)
