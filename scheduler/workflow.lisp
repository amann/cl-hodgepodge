;;;; -*- outline-regexp:";;;;[*]+ +" -*-

;;;;* Hook
(defclass hook ()
  ((lambda-list :initarg :lambda-list)
   (function-list :initform nil))
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod initialize-instance :after ((self hook)&key &allow-other-keys)
  (with-slots (function-list)
      self
    (sb-mop:set-funcallable-instance-function
     self (compile nil (lambda (&rest args)
			 (etypecase function-list
			   (cons
			    (dolist (function function-list)
			      (apply function args)))
			   ((or symbol function)
			    (apply function-list args))))))))
(defmacro make-hook (&rest lambda-list)
  `(make-instance 'hook :lambda-list ',lambda-list))
(defgeneric add-hook (hook function &optional append)
  (:method ((hook hook) function &optional append)
    (check-type function (or symbol function))
    (with-slots (function-list)
        hook
      (let ((existing-functions (typecase function-list
                                  (cons function-list)
                                  (t (list function-list)))))
        (unless (member function function-list)
          (setf function-list
                (if append
                    (append existing-functions (list function))
                    (cons function existing-functions))))))))
(defgeneric remove-hook (hook function)
  (:method ((hook hook) function)
    (with-slots (function-list)
        hook
      (setf function-list (delete function function-list)))))
(defgeneric clear-hook (hook)
  (:method ((hook hook))
    (setf (slot-value hook 'function-list) nil)))

;;;;* Queue
(defvar *timeout* nil
  "Timeout in seconds.")

(defun make-queue-code (&key (synchronized t))
  `(let (queue ,@(when synchronized
		   '((lock (bt:make-lock))
		     (cvar (bt:make-condition-variable)))))
     (flet ((enqueue (x)
	      (let ((new-cell (cons x (cdr queue))))
		(if queue
		    (setf (cdr queue) new-cell)
		    (setf (cdr new-cell) new-cell))
		(setf queue new-cell))
	      x)
	    (dequeue ()
	      (if queue
		  (values (if (eql queue (cdr queue))
			      (prog1
				  (car queue)
				(setf queue nil))
			      (pop (cdr queue)))
			  t)
		  (values nil nil))))
       (values ,(if synchronized
		    `(lambda (x)
		       (bt:with-lock-held (lock)
			 (enqueue x)
			 (bt:condition-notify cvar)))
		    '#'enqueue)
	       ,(if synchronized
		    `(lambda (&optional timeout)
		       (bt:with-lock-held (lock)
			 (multiple-value-bind (item successp)
			     (dequeue)
			   (if successp
			       (values item t)
			       (progn
				 (bt:condition-wait cvar lock :timeout timeout)
				 (dequeue))))))
		    '#'dequeue)))))
(defun make-queue (&key (synchronized t))
  (funcall (compile nil `(lambda () ,(make-queue-code :synchronized synchronized)))))


(defmacro with-queue ((enqueue dequeue &key (synchronized t)) &body body)
  "Establish within the lexical scope of BODY a function ENQUEUE and a function DEQUEUE
which act on a freshly instanciated queue which is synchronized if the key argument SYNCHRONIZED is true (the default).

- The function ENQUEUE takes one mandatory argument, the item to be enqueued, and returns that item.
- The function DEQUEUE takes no or an optional argument TIMEOUT if SYNCHRONIZED is true, and returns as principal value the first item in the queue and as secondary value T, if such item is found, or NIL else. If the real number TIMEOUT is given, the functions waits at most as many seconds before returning."
  (let ((g!enqueue (gensym "ENQUEUE"))
	(g!dequeue (gensym "DEQUEUE")))
    `(multiple-value-bind (,g!enqueue ,g!dequeue)
	 ,(make-queue-code :synchronized synchronized)
       (flet ((,enqueue (x) (funcall ,g!enqueue x))
	      (,dequeue ,(when synchronized `(&optional timeout))
		(funcall ,g!dequeue ,@(when synchronized `(timeout)))))
	 (declare (inline ,enqueue ,dequeue))
	 ,@body))))

;;;;* DO-PARALLEL

(defun %make-task-code (body)
  (let ((g!tag (gensym "TAG")))
    `(lambda (condition-callback)
       (lambda ()
         (catch ',g!tag
           (let ((*debugger-hook* (lambda (condition hook)
                                    (declare (ignore hook))
                                    (funcall condition-callback condition)
                                    (throw ',g!tag nil))))
             (handler-bind
                 ((condition condition-callback))
               ,@body)))))))



(defmacro do-parallel (&body forms)
  "Evaluate each form in its own thread and return their principal values as multiple values. The forms are evaluated in the lexical context of the DO-PARALLEL form but in the dynamic context of each spawned thread. The global variable BT:*DEFAULT-SPECIAL-BINDINGS* may be used to transfer special bindings to the dynamic scope of each form."
  (let* ((g!condition-enqueue (gensym "ENQUEUE"))
         (g!condition-dequeue (gensym "DEQUEUE"))
         (threads (mapcar (lambda (form)
                            `(,(gensym) (bt:make-thread (,(%make-task-code (list form)) ,g!condition-enqueue)
                                                        :name ,(format nil "~S" form))))
                          forms))
         (g!threads (gensym "THREADS")))
    `(multiple-value-bind (,g!condition-enqueue ,g!condition-dequeue)
         (make-queue)
       (let* (finishedp
              (,g!threads (bt:make-thread
                           (lambda ()
                             (unwind-protect
                                  (let ,threads
                                    (values ,@(mapcar (lambda (thread)
                                                        `(bt:join-thread ,(car thread)))
                                                      threads)))
                               (setf finishedp t))))))
         (do ()
             (finishedp (bt:join-thread ,g!threads))
           (signal (funcall ,g!condition-dequeue)))))))

;;;;* Workflow

(defpackage "MCT"
  (:export "WAIT-DELIVERY"
           "LAST-DELIVERY"
           "STD-PORTFOLIO"
           "CALCULATE"))

(defun mct:wait-delivery (market-unit situation-date delivery-type &optional superceeding)
  "Wait until a delivery for MARKET-UNIT and SITUATION-DATE of type DELIVERY-TYPE is present in
 the MCT data base. If SUPERCEEDING is a delivery ID, wait for a delivery which is more recent."
  nil)
(defun mct:last-delivery (market-unit situation-date delivery-type)
  "Return the delivery ID of the last delivery present in the MCT data base."
  nil)

(defun mct:std-portfolio (market-unit situation-date &key :asset-delivery :rpf-delivery)
  "Return a standard portfolio definition found in the MCT data base for MARKET-UNIT at SITUATION-DATE and for the ASSET-DELIVERY and RPF-DELIVERY IDs. At least one of the ASSET-DELIVERY or RPF-DELIVERY must been indicated."
  nil)

(defun mct:calculate (configuration portfolio &optional (base-date (situation-date portfolio)))
  "Do a calculation on PORTFOLIO using CONFIGURATION."
  nil)



(defpackage "CRT"
  (:export "CALCULATE"))
(defun crt:calculate (configuration portfolio &optional (base-date (situation-date portfolio)))
  "Do a calculation on PORTFOLIO using CONFIGURATION."
  nil)

(defpackage "AGT"
  (:export "CALCULATE"
           "GET-PARAMETERS"))
(defun agt:calculate (configuration parameters market-risk credit-risk)
  "Do a calculation on PARAMETERS MARKET-RISK and CREDIT-RISK using CONFIGURATION."
  nil)
(defun agt:get-parameters (configuration market-unit situation-date)
  "Return the parameters for MARKET-UNIT and SITUATION-DATE needed for a calculation under CONFIGURATION.")


(defpackage "RDB"
  (:export "*PURPOSE*"
           "LOAD"
           "PRINT-REPORT"))
(defvar rdb:*purpose* nil)
(defun rdb:load (object purpose)
  "Load OBJECT into the risk data base under the label PURPOSE."
  nil)
(defun rdb:print-report (market-unit situation-date purpose)
  "Print a report for MARKET-UNIT und SITUATION-DATE under PURPOSE."
  nil)

(defpackage "MANUAL"
  (:export "IS-OK"
           "SELECT"))
(defun manual:is-ok ()
  "Wait for an external (manual) interaction to return true or false."
  nil)
(defun manual:select (items)
  "Wait for an external (manual) interaction to return an item chosen amongst ITEMS. It is possible to return NIL if none has been chosen."
  nil)


(defun rdb:make-report (purpose market-unit situation-date)
  (labels
      ((sign-off-assets (&optional asset-delivery)
         (mct:wait-delivery market-unit situation-date :assets asset-delivery)
         (let* ((asset-delivery (mct:last-delivery market-unit situation-date :assets))
                (portfolio (mct:std-portfolio market-unit situation-date asset-delivery)))
           (rdb:load (mct:calculate (rdb:find-config :mct :sensitivities purpose) portfolio) purpose)
           (rdb:load portfolio purpose)
           (rdb:print-report market-unit situation-date purpose)
           (if (manual:is-ok)
               (labels
                   ((calculate-target-capital (&optional rpf-delivery)
                      (mct:wait-delivery market-unit situation-date :rpf rpf-delivery)
                      (let ((portfolio (mct:std-portfolio market-unit situation-date
                                                          :asset-delivery asset-delivery
                                                          :rpf-delivery (mct:last-delivery market-unit
                                                                                           situation-date
                                                                                           :rpf)))
                            (agt-config (rdb:find-config :agt :aggregation purpose)))
                        (let* ((agt-results (multiple-value-call
                                               'agt:calculate
                                             agt-config
                                             (do-parallel
                                               (agt:get-parameters agt-config market-unit situation-date)
                                               (mct:calculate (rdb:find-config :mct :simulation purpose) portfolio)
                                               (crt:calculate (rdb:find-config :crt :simulation purpose) portfolio))))
                               (chosen-rpf (get-rpf-delivery (agt:portfolio (manual:select agt-results))))
                               (chosen-agt-result (agt:extract-sub-result agt-result chosen-rpf)))
                          (if chosen-rpf
                              (do-parallel
                                (rdb:load agt-result purpose)
                                (progn
                                  (gral:reload-rpf chosen-rpf)
                                  (let ((rpf-delivery (mct:last-delivery market-unit situation-date :rpf)))
                                    (do-parallel
                                      (rdb:load (mct:std-portfolio market-unit situation-date
                                                                   :rpf-delivery rpf-delivery)
                                                purpose)
                                      (rdb:load (mct:calculate (rdb:find-config :mct :vsc purpose)
                                                               (mct:std-portfolio market-unit situation-date
                                                                                  :asset-delivery asset-delivery
                                                                                  :rpf-delivery rpf-delivery))
                                                purpose)))))
                              (progn
                                (mct:wait-delivery market-unit situation-date :rpf rpf-delivery)
                                (calculate-target-capital (mct:last-delivery market-unit situation-date :rpf))))))))
                 (calculate-target-capital))
               (sign-off-assets asset-delivery)))))
    (sign-off-assets)))
