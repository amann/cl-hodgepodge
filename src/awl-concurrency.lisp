;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(declaim (optimize debug safety))
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")
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

;;;;** Parallel Mapping Functions

(defun displace-split (vector size)
  "Return a list of displaced sub vectors of VECTOR of size SIZE."
  (let* ((l (length vector))
         (d (min (max size 1) l))
         (c (ceiling l d))
         (c-1 (1- c)))
    (append (loop :for i :below c-1
                  :collect (make-array d :displaced-to vector :displaced-index-offset (* d i)))
            (list (make-array (- l (* c-1 d)) :displaced-to vector :displaced-index-offset (* d c-1))))))

(defun list-pointers (list size)
  (let* ((l (length list))
         (d (min (max size 1) l))
         (c (ceiling l d))) 
    (map-into (make-list (min c l))
              (let ((rest list))
                (lambda ()
                  (cons rest (setq rest (nthcdr d rest))))))))


(defvar *cpu-count* #+ccl (ccl:cpu-count) #-ccl 4)
(defvar *max-running-threads* 60)
(let ((thread-count 0)
      (lock (bt:make-lock)))
  (defun running-threads ()
    thread-count)
  (defun thread-count-inc-if (pred)
    (bt:with-lock-held (lock)
      (when (funcall pred thread-count) (incf thread-count)))))
(defun determine-split-factor (vects)
  "Return the number of sub vectors the vectors in VECTS should be split into for parallelization."
  (let ((min-size (reduce #'min (mapcar #'length vects))))
    (min (* 3/2 *cpu-count*) (ceiling min-size 1000))))

(defun pmap-into-list ())

(defun awl::pmap-into (sequence fn &rest vects &aux (count (determine-split-factor vects)))
  "Like cl:map-into but parallelize the process over several threads. Unlike with cl:map-into, the order in which the elements of VECTS are applied to FN is not known. The number of threads which are spawned is determined using the special variable *parallel-thread-count*."
  (labels (()))
  (when vects 
    (dolist (thread (apply #'mapcar
                           (lambda (sv &rest svs)
                             (bt:make-thread (compile nil
                                                      `(lambda ()
                                                         (map-into ,sv ,fn ,@svs)))))
                           (displace-split vector count)
                           (mapcar (lambda (vect)
                                     (displace-split vect count))
                                   vects)))
      (bt:join-thread thread)))
  vector)
(defun awl::pmap (fn &rest vects &aux (count (determine-split-factor vects)))
    "Like cl:map but parallelize the process over several threads and applies only to vectors. Unlike with cl:map, the order in which the elements of VECTS are applied to FN is not known. The number of threads which are spawned is hold in the special variable *parallel-thread-count*."
    (when vects 
      (map nil #'bt:join-thread
           (apply #'mapcar
                  (lambda (&rest svs)
                    (bt:make-thread (compile nil
                                             `(lambda ()
                                                (map nil ,fn ,@svs)))))
                  (mapcar (lambda (vect)
                            (displace-split vect count))
                          vects)))))

;;;;** Event Channel
;;;;
;;;; An event channel allows for the sending of events from one thread to another.
;;;; It provides two functions: 
;;;;   - fetch-events(action &optional flush) action in (:try :wait)
;;;;     Fetch all events in the channel if available; when FLUSH is
;;;;     non nil, empty the channel.
;;;;   - add-event(event): Add event to the channel


(define-condition awl::event-channel-fetch-timeout (bt:timeout) ())

(defun awl::make-event-channel ()
  "Create an event channel and return two functions as multiple-values:
- fetch-events(&optional timeout flush-p): Return the current content of the event queue. The argument TIMEOUT must be either NIL or a non-negative number representing seconds. If TIMEOUT is NIL then tries to acquire the lock and return the queue; if TIMEOUT is non NIL then blocks waiting at most TIMEOUT seconds or until a new event is added to the chain. When the optional argument FLUSH-P is not NIL then the queue is emptied when fetching.
- add-event(event): Add EVENT to the event channel."
  (let (events)
    (let ((lock (bt:make-lock))
          (condition-variable (bt:make-condition-variable)))
      (values
       (lambda (&key timeout (flush-p t))
         "Return the current content of the event queue. The argument TIMEOUT must be either NIL or a non-negative number representing seconds. If TIMEOUT is NIL then try to acquire the lock and return the queue; if TIMEOUT is non NIL then block waiting at most TIMEOUT seconds or until a new event is added to the chain. When the optional argument FLUSH-P is not NIL (the default) then the queue is emptied when fetching."
         (let ((result (if timeout
                           (bt:with-timeout (timeout)
                             (bt:with-lock-held (lock)
                               (do () (events events)
                                 (bt:condition-wait condition-variable lock))))
                           (bt:with-lock-held (lock)
                             events))))
           (if flush-p
               (prog1 (nreverse result)
                 (setq events nil))
               (reverse result))))
       (lambda (event)
         "Add EVENT to the event channel."
         (bt:with-lock-held (lock)
           (push event events))
         (bt:condition-notify condition-variable))))))

(defmacro awl::with-event-channel ((fetch-fn-name push-fn-name) &body body)
  "Instanciate a fresh event channel and establish within the lexical scope of BODY function bindings of the symbol given with FETCH-FN-NAME to the event fetch function and to the symbol given with PUSH-FN-NAME to the function to push new events to the event channel."
  (awl::with-gensyms (g!fetch-fn g!push-fn)
    `(multiple-value-bind (,g!fetch-fn ,g!push-fn)
         (awl::make-event-channel)
       (flet ((,fetch-fn-name (&rest args)
                (apply ,g!fetch-fn args))
              (,push-fn-name (event)
                (funcall ,g!push-fn event)))
         ,@body))))



;;;;* Queues
(defun awl::make-queue (&key (synchronized t) &aux (synchronized (when synchronized t)))
  "Return a queue which is synchronized by default unless the key argument :SYNCHRONIZED is set to nil. It provides a function in one variable which is one of the symbols :enqueue :dequeue :length :peeker and :dump and which returns the corresponding method."
  (let (queue
        (lock (bt:make-lock "QUEUE"))
        (length 0))
    (declare (integer length))
    (compile nil `(lambda (method)
                    (ecase method
                      (:enqueue ,(if synchronized
                                     (lambda (value)
                                       "Enqueue value to the queue."
                                       (bt:with-lock-held (lock)
                                         (let ((elt (cons value (cdr queue))))
                                           (if queue
                                               (setf (cdr queue) elt)
                                               (setf (cdr elt) elt))
                                           (setf queue elt)
                                           (the integer (incf length)))))
                                     (lambda (value)
                                       "Enqueue value to the queue."
                                       (let ((elt (cons value (cdr queue))))
                                         (if queue
                                             (setf (cdr queue) elt)
                                             (setf (cdr elt) elt))
                                         (setf queue elt)
                                         (the integer (incf length))))))
                      (:dequeue ,(if synchronized
                                     (lambda ()
                                       "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
                                       (bt:with-lock-held (lock)
                                         (when queue
                                           (decf length)
                                           (values (if (eq queue (cdr queue))
                                                       (prog1 (car queue)
                                                         (setf queue nil))
                                                       (pop (cdr queue)))
                                                   t))))
                                     (lambda ()
                                       "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
                                       (when queue
                                           (decf length)
                                           (values (if (eq queue (cdr queue))
                                                       (prog1 (car queue)
                                                         (setf queue nil))
                                                       (pop (cdr queue)))
                                                   t)))))
                      (:length ,#'(lambda ()
                                    "Return the length of the queue."
                                    length))
                      (:peeker (let* ((eof "eof")
                                      (generator (awl::make-generator-from-circle (funcall ,(lambda () (cdr queue))) :eof eof)))
                                 (lambda (&aux (item (funcall generator)))
                                   "Iterate through the queue without changing its state. This function is not synchronized."
                                   (unless (eq eof item)
                                     (values item t)))))
                      (:dump ,(if synchronized
                                  (lambda ()
                                    "Empty the queue."
                                    (bt:with-lock-held (lock)
                                      (setq queue nil)))
                                  (lambda ()
                                    "Empty the queue."
                                    (setq queue nil))))
                      (:synchronizedp ,(lambda () synchronized)))))))
(declaim (inline awl::get-queue-method awl::enqueue awl::dequeue awl::queue-length awl::queue-dump awl::queue-synchronizedp awl::get-iterator))
(defun awl::get-queue-method (queue method)
  "Return the method METHOD of queue QUEUE."
  (declare ((function (symbol) (function)) queue)
           (optimize speed))
  (funcall queue method))
(defun awl::enqueue (queue value)
  "Enqueue value to the queue."
  (declare ((function (symbol) (function (*) integer)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :enqueue) value))
(defun awl::dequeue (queue)
  "Dequeue the next element from the queue. Return as secondary value NIL if queue was empty, T otherwise"
  (declare ((function (symbol) (function () *)) queue)
           (optimize speed))
  (funcall (the (function ()) (awl::get-queue-method queue :dequeue))))
(defun awl::queue-length (queue)
  "Return the length of the queue."
  (declare ((function (symbol) (function () integer)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :length)))
(defun awl::queue-dump (queue)
  "Empty the queue."
  (declare ((function (symbol) (function () null)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :dump)))
(defun awl::queue-synchronizedp (queue)
  "Return T if the queue is synchronized and NIL else."
  (declare ((function (symbol) (function () null)) queue)
           (optimize speed))
  (funcall (awl::get-queue-method queue :synchronizedp)))
(defun awl::queue-iterator (queue)
  "Iterate through the queue without changing its state. This function is not synchronized."
  (declare ((function (symbol) (function () integer)) queue)
           (optimize speed))
  (awl::get-queue-method queue :peeker))


;;;;** Queue
;;;;

;;;;*** Queue Interruption

(define-condition awl::queue-interruption ()
  ((queue :initarg :queue :reader awl::queue-interruption-queue)
   (snapshot :initarg :snapshot :reader awl::queue-interruption-snapshot))
  (:report (lambda (interruption stream)
             (format stream "Treatment of the queue ~A has been interrupted. ~
                     The status of the queue at the moment of the interruption was: ~&~A"
                     (awl::queue-interruption-queue interruption)
                     (awl::queue-interruption-snapshot interruption)))))


(defun awl::make-queue (&optional default-order)
  ())

;;;;*** The Queue Constructor
(defun awl::make-queue (id &optional default-order)
  "Create a thread save queue and return as multiple values four functions:
- add-item(item): push ITEM into the queue
- pop-next-request-fn(guard order-predicate): return the first item according to ORDER-PREDICATE with guard(item) not nil
- queue-empty-p(): return T if the queue is empty, NIL else
- interrupt(): signal a condition of type `queue-interruption' and provides two restarts CONTINUE and REPLACE-QUEUE. The restart CONTINUE resumes and returns a snapshot of the current queue; the restart REPLACE-QUEUE expects a sequence of items and replaces the current queue with it."
  (awl::with-event-channel (fetch-input-queue add-to-input-queue)
    (let (output-queue (last-order default-order) (lock (bt:make-recursive-lock)))
      (flet ((update-output-queue (&optional (order-predicate default-order))
               (bt:with-recursive-lock-held (lock)
                 (setq output-queue (let* ((snapshot (fetch-input-queue))
                                           (output-queue (append output-queue snapshot)))
                                      (if (and snapshot order-predicate)
                                          (stable-sort output-queue order-predicate)
                                          output-queue))))))
        (flet ((snapshot ()
                 (map 'list #'identity (update-output-queue default-order))))
          (values
           ;; add-item(item)
           #'add-to-input-queue
           ;; pop-next-item(guard order-predicate)
           (lambda (&optional guard order-predicate)
             "Return the first item according to ORDER-PREDICATE with GUARD(item) not nil."
             (let (item)
               (bt:with-recursive-lock-held (lock)
                 (multiple-value-setq (output-queue items)
                   (awl::remove-if* (lambda (item)
                                      (funcall guard item))
                                    (update-output-queue order-predicate)
                                    :count 1)))
               (first items)))
           ;; queue-empty-p()
           (lambda () "Return T if the queue is empty, NIL else." (null (update-output-queue default-order)))
           ;; interrupt()
           (lambda ()
             "Signal a condition of type `queue-interruption' and provides two restarts CONTINUE and REPLACE-QUEUE. The restart CONTINUE resumes and returns a snapshot of the current queue; the restart REPLACE-QUEUE expects a sequence of items, replaces the current queue with this sequence and returns it."
             (bt:with-recursive-lock-held (lock)
               (let ((snapshot (snapshot)))
                 (restart-case
                     (progn
                       (signal (make-condition 'awl::queue-interruption
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
  ((condition :initarg :condition :reader awl::condition))
  (:documentation "Condition wrapping a condition or error signalled by the call of the request. This condition is transfered to the emitant of the request. The reader CONDITION returns the wrapped condition."))
(define-condition request-restartable-condition-wrapper (request-condition-wrapper)
  ((restarts :initarg :restarts :reader awl::restarts))
  (:documentation "Condition wrapping a condition or error signalled by the call of the request. This condition is transfered to the emitant of the request. The reader RESTARTS allows to review all available restarts."))

;;;;**** Status Signals
;;;;
;;;; Status are a special case of condition wrappers. They are handled
;;;; in such a way that they only transfer information and immediately
;;;; invoke a continue restart. It is intended that those status info
;;;; be sent by the function send-status.

(define-condition request-status () ()
  (:documentation "Conditions for signalling the status of the request."))
(define-condition awl::request-status-call-pending (request-status) ()
  (:documentation "Condition to signal that the calling request is still pending and not yet called."))
(define-condition awl::request-status-called (request-status) ()
  (:documentation "Condition to signal that the function has been called. For signaling more information, like a progression status of an estimated time, convenient subclasses may be defined. This condition is intended to be signalled by the request function itself."))
(define-condition awl::request-status-succeed (request-status)
  ((return-values)))
(define-condition awl::request-status-failed (request-status request-condition-wrapper) ()
  (:documentation "Condition to signal the failure of the request. No restart is available here. This condition is intended to be signalled by the request function itself. The reader CONDITION returns the original condition which may be at the origin of the failure."))

(defmethod awl::condition ((status request-status))
  nil)
(defmethod awl::restarts ((status request-status))
  nil)
(defun awl::send-status (request-status &rest args)
  "Send the present status of the request. REQUEST-STATUS must inherit `request-status-called'."
  (check-type request-status awl::request-status-called)
  (restart-case
      (apply #'signal request-status args)
    (continue-process ())))

;;;;*** Future Call Constructor

(defun awl::make-future-call (fn)
  (multiple-value-bind (fetch-value push-value)
      (awl::make-event-channel)
    (multiple-value-bind (fetch-restart push-restart)
        (awl::make-event-channel)
      (oam:fbind (fetch-value push-value fetch-restart push-restart)
        (let ((lock (bt:make-lock))
              (call-pending-condition (make-condition 'awl::request-status-call-pending))
              (already-called-fn (lambda ()
                                   (error "Function has already been called.")))
              (call-fn (lambda ()
                         (bt:make-thread
                          (lambda ()
                            (push-value
                             (multiple-value-list 
                              (handler-case
                                  (funcall fn)
                                (awl::request-status-called (status)
                                  (push-value status)
                                  (invoke-restart (find-restart 'continue-process)))
                                (awl::request-status-failed (status)
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
           (lambda (&optional timeout)
             (let ((condition (fetch-value timeout t)))
               (typecase condition
                 (null (signal call-pending-condition))
                 (request-status (signal condition))
                 (t (let ((orig-cond (awl::condition condition))
                          (restarts (fetch-restart condition)))
                      (restart-case
                          (typecase orig-cond
                            (error (error condition))
                            (t (signal condition)))
                        (chose-restart (restart)
                          :report (lambda (s)
                                    (let ((i -1))
                                      (format s "Following restarts are available:")
                                      (map nil (lambda (r)
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
           (lambda ()
             (bt:with-lock-held (lock)
               (prog1
                   (funcall call-fn)
                 (setq call-fn already-called-fn))))))))))


(defun awl::try-fetch-values (future)
  "Return the values of the future if available, else returns nil and signals a condition of type `REQUEST-STATUS-PENDING' if the processing of the request is still pending or a condition of type `REQUEST-STATUS-FAILED' if the processing failed. If the call of the requested function signals an error, this error is transfered to the caller of `TRY-FETCH-VALUES' via `REQUEST-STATUS-FAILED'."
  (funcall future nil))
(defun awl::wait-for-values (future timeout)
  "Like `TRY-FETCH-VALUES' but is waiting until the servant side notified a changement of the status."
  (funcall future timeout))




;;;;** The Scheduler
;;;;
;;;; The scheduler is the main agent of the active object pattern: it dispaches
;;;; the queued requests.
(defun awl::make-scheduler (id guard-fn priority-order-fn)
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
        (awl::make-queue id)
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

(defclass awl::scheduler ()
  ((control-fn :type function))
  (:metaclass c2mop:funcallable-standard-class))
(defmethod initialize-instance :after ((self awl::scheduler)
                                       &key guard-fn priority-order-fn)
  (multiple-value-bind (control-fn send-request-fn)
      (make-scheduler self guard-fn priority-order-fn)
    (setf (slot-value self 'control-fn) control-fn)
    (c2mop:set-funcallable-instance-function self send-request-fn)))


(defun awl::scheduler-active-p (scheduler)
  (funcall (slot-value scheduler 'control-fn) :active-p))
(defun awl::scheduler-empty-p (scheduler)
  (funcall (slot-value scheduler 'control-fn) :empty-p))
(defun awl::start-scheduler (scheduler)
  (funcall (slot-value scheduler 'control-fn) :start))
(defun awl::stop-scheduler (scheduler)
  (funcall (slot-value scheduler 'control-fn) :stop))


(defun awl::send-call-request (scheduler fn guard priority)
  (funcall scheduler fn guard priority))

(defvar awl::*scheduler* nil
  "Special variable containing the default scheduler. It is intended to be locally bound.")

(defun awl::make-scheduled-function (fn &optional guard (scheduler awl::*scheduler*))
  "Return a function whose calls are transferred to the scheduler SCHEDULER."
  #'(lambda (priority &rest args)
      (awl::send-call-request scheduler
                              #'(lambda ()
                                  (apply fn args))
                              guard priority)))

(defmacro awl::define-scheduled-function (name (&rest args) (&key documentation (guard #'(lambda () t))) &body body)
  `(prog1
       (defun ,name ,(list* 'priority args)
         ,(format nil "~@[~A~] The used scheduler is taken from the variable *scheduler*." documentation)
         (declare (ignore priority ,@args)))
     (setf (symbol-function ',name)
           (awl::make-scheduled-function #'(lambda ,args ,@body) ,guard))))



;;;;--------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (symb "AWL")
    (export symb "AWL")))
