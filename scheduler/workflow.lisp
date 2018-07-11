;;;; -*- outline-regexp:";;;;[*]+ +" -*-
(in-package "CL-USER")
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

(in-package "CL-USER")

(defpackage "DISPATCHER"
  (:use "CL")
  (:export "SEND-JOB"))
(defun send-job (jobdef))
(defun make-jobdef (sender time)
  `(*top* (@)
          (|Job| (@(|Sender| ,sender)
                   (|SendTime| ,time))
                 (|Starter|)
                 ())))


(defpackage "GRAL"
  (:use "CL")
  (:export "WAIT-DELIVERY"
           "LAST-DELIVERY"
           "STD-PORTFOLIO"
           "RELOAD-RPF"))
(in-package "GRAL")

(defvar *databases* '(:uat ("CRRISK" "crrisk_uat_user" "CM_plc2$1uat" "IX-SQL0181")
                      :pav ("CRRISK" "crrisk_pav_user" "C0M_plcr$P17" "IX-SQL0306")))
(defvar *stage* :uat)

(defmacro with-db (&body body)
  `(destructuring-bind (database user password host)
       (getf *databases* *stage*)
     (mssql:with-connection (database user password host)
       ,@body)))
(defun query (format query &rest args)
  (with-db
    (mssql:query (apply 'format nil query args) :format format)))
(sb-int:defconstant-eqx +market-units+ '(:ch :de :fr :lu :slgroup) #'equalp)
(deftype market-unit ()
  `(member ,@+market-units+))
(defun gral:wait-delivery (market-unit situation-date delivery-type &optional superceeding)
  "Wait until a delivery for MARKET-UNIT and SITUATION-DATE of type DELIVERY-TYPE is present in
 the MCT data base and return its last delivery ID. If SUPERCEEDING is a delivery ID, wait for a
 delivery which is more recent."
  (let ((superceeding (or superceeding -1)))
    (do ((delivery (last-delivery market-unit situation-date delivery-type)
                   (last-delivery market-unit situation-date delivery-type)))
        ((and delivery (< superceeding delivery)) delivery)
      (sleep 30))))
(defun gral:last-delivery (market-unit situation-date delivery-type)
  "Return the delivery ID of the last delivery present in the MCT data base."
  (destructuring-bind (database user password host)
      (getf gral::*databases* *stage*)
    (mssql:with-connection (database user password host)
      (mssql:query (format nil "select max(delivery) from t_activedeliverys
 where situationdate = '~A'
 and orgunit = '~A'
 and deliverytype = '~A'" situation-date market-unit delivery-type) :format :single))))

(defclass gral:std-portfolio ()
  ((market-unit :market-unit)
   (situation-date :situation-date)
   (asset-delivery :asset-delivery)
   (rpf-delivery :rpf-delivery)))
(defgeneric get-where-clause (portfolio report-type)
  (:method ((portfolio std-portfolio) report-type)
    nil)
  (:method :around ((portfolio std-portfolio) report-type)
    (with-slots (situation-date market-unit asset-delivery rpf-delivery)
        portfolio
      (format nil "SITUATIONDATE = '~A' AND ORGUNIT = '~A' AND DELIVERY IN (~{~S~^,~S~})~@[ AND ~A~]"
              situation-date market-unit (remove nil (list asset-delivery rpf-delivery)) (call-next-method)))))
(defun gral:std-portfolio (market-unit situation-date &key asset-delivery rpf-delivery)
  "Return a standard portfolio definition found in the MCT data base for MARKET-UNIT at SITUATION-DATE and for the ASSET-DELIVERY and RPF-DELIVERY IDs. At least one of the ASSET-DELIVERY or RPF-DELIVERY must been indicated."
  (assert (or asset-delivery rpf-delivery))
  (make-instance 'std-portfolio :market-unit market-unit :situation-date situation-date
                                :asset-delivery asset-delivery :rpf-delivery rpf-delivery))
(defun gral:get-delivery (delivery-type portfolio)
  "Return the delivery ID of DELIVERY-TYPE of the PORTFOLIO."
  (ecase delivery-type
   (:asset (slot-value portfolio 'asset-delivery))
   (:rpf (slot-value portfolio 'rpf-delivery))))


(defpackage "MCT"
  (:use "CL" "CL-USER")
  (:export "CALCULATE"))
(in-package "MCT")
(defclass configuration ()
  ((situation-date :initarg :situation-date)
   (calibration :initarg :call)
   (ics-configuration) calculator))
(defclass target-capital-configuration (configuration)
  ())
(defclass sensitivity-configuration (configuration)
  ())
(defclass vsc-configuration (configuration)
  ())
(defun mct:calculate (configuration portfolio &optional (base-date (situation-date portfolio)))
  "Do a calculation on PORTFOLIO using CONFIGURATION."
  nil)

(in-package "CL-USER")
(defpackage "CRT"
  (:use "CL" "CL-USER")
  (:export "CALCULATE"))
(in-package "CRT")
(defun crt:calculate (configuration portfolio &optional (base-date (situation-date portfolio)))
  "Do a calculation on PORTFOLIO using CONFIGURATION."
  nil)

(in-package "CL-USER")
(defpackage "AGT"
  (:use "CL" "CL-USER")
  (:export "CALCULATE"
           "GET-PARAMETERS"))
(in-package "AGT")
(defun agt:calculate (configuration parameters market-risk credit-risk)
  "Do a calculation on PARAMETERS MARKET-RISK and CREDIT-RISK using CONFIGURATION."
  nil)
(defun agt:get-parameters (configuration market-unit situation-date)
  "Return the parameters for MARKET-UNIT and SITUATION-DATE needed for a calculation under CONFIGURATION."
  nil)

(in-package "CL-USER")
(defpackage "RDB"
  (:use "CL" "CL-USER")
  (:export "*PURPOSE*"
           "LOAD-OBJECT"
           "PRINT-REPORT"
           "FIND-CONFIG"))
(in-package "RDB")
(defvar *purpose* :frm)
(defvar *configurations* '(:sst (:mct (:sensitivities (make-instance 'mct::sensitivity-configuration)
                                       :target-capital (make-instance 'mct::target-capital-configuration)
                                       :vsc (make-instance 'mct::vsc-configuration))
                                 :crt (:target-capital (make-instance 'crt::target-capital-configuration))
                                 :agt (:aggregation nil))
                           :frm (:mct (:sensitivities (make-instance 'mct::sensitivity-configuration)
                                       :target-capital (make-instance 'mct::target-capital-configuration)
                                       :vsc (make-instance 'mct::vsc-configuration))
                                 :crt (:target-capital (make-instance 'crt::target-capital-configuration))
                                 :agt (:aggregation nil))
                           :grc (:mct (:sensitivities (make-instance 'mct::sensitivity-configuration)
                                       :target-capital (make-instance 'mct::target-capital-configuration)
                                       :vsc (make-instance 'mct::vsc-configuration))
                                 :crt (:target-capital (make-instance 'crt::target-capital-configuration))
                                 :agt (:aggregation nil))))
(defun rdb:find-config (tool calculation purpose)
  (or (getf (getf (getf *configurations* purpose) tool) calculation)
      (error "Configuration for purpose=~A, tool=~A and calculation=~A not found."
             purpose tool calculation)))
(defgeneric rdb:load-object (object purpose)
  "Load OBJECT into the risk data base under the label PURPOSE."
  nil)
(defun rdb:print-report (market-unit situation-date purpose)
  "Print a report for MARKET-UNIT und SITUATION-DATE under PURPOSE."
  nil)


(in-package "CL-USER")
(defpackage "MANUAL"
  (:use "CL" "CL-USER")
  (:export "IS-OK"
           "SELECT"))
(in-package "MANUAL")
(defun manual:is-ok ()
  "Wait for an external (manual) interaction to return true or false."
  (yes-or-no-p "Is everything OK?"))
(defun manual:select (items)
  "Wait for an external (manual) interaction to return an item chosen amongst ITEMS. It is possible to return NIL if none has been chosen."
  (let ((i -1))
    (format t "Please chose one among following possibilities by typing-in the corresponding number:~
~:{~&~3D - ~A~}"
            (mapcar (lambda (item) (list (incf i) item))
                    items))
    (loop
      (with-simple-restart
          (redo-choice "Redo coice.")
        (format t "~&Your choice: ")
        (let ((choice (read)))
          (unless (typep choice `(or null (integer 0 ,i)))
            (error "The value ~S is not a valid choice. Enter a number from 0 to ~D or NIL." choice i))
          (return (when choice (elt items choice))))))))


(in-package "CL-USER")
(defun rdb:make-report (purpose market-unit situation-date)
  (labels
      ((sign-off-assets (&optional asset-delivery)
         (let* ((asset-delivery (gral:wait-delivery market-unit situation-date :assets asset-delivery))
                (portfolio (gral:std-portfolio market-unit situation-date asset-delivery)))
           (rdb:load (mct:calculate (rdb:find-config :mct :sensitivities purpose) portfolio) purpose)
           (rdb:load portfolio purpose)
           (rdb:print-report market-unit situation-date purpose)
           (if (manual:is-ok)
               (labels
                   ((calculate-target-capital (&optional rpf-delivery)
                      (gral:wait-delivery market-unit situation-date :rpf rpf-delivery)
                      (let* ((portfolio (gral:std-portfolio market-unit situation-date
                                                           :asset-delivery asset-delivery
                                                           :rpf-delivery (gral:last-delivery market-unit
                                                                                             situation-date
                                                                                             :rpf)))
                             (agt-config (rdb:find-config :agt :aggregation purpose))
                             (agt-results (multiple-value-call
                                              'agt:calculate
                                            agt-config
                                            (do-parallel
                                              (agt:get-parameters agt-config market-unit situation-date)
                                              (mct:calculate (rdb:find-config :mct :target-capital purpose)
                                                             portfolio)
                                              (crt:calculate (rdb:find-config :crt :target-capital purpose)
                                                             portfolio)))))
                        (chosen-rpf (gral:get-delivery :rpf (agt:portfolio (manual:select agt-results))))
                        (chosen-agt-result (agt:extract-sub-result agt-result chosen-rpf))
                        (if chosen-rpf
                            (do-parallel
                              (rdb:load agt-result purpose)
                              (progn
                                (gral:reload-rpf chosen-rpf)
                                (let ((rpf-delivery (gral:last-delivery market-unit situation-date :rpf)))
                                  (do-parallel
                                    (rdb:load (gral:std-portfolio market-unit situation-date
                                                                  :rpf-delivery rpf-delivery)
                                              purpose)
                                    (rdb:load (mct:calculate (rdb:find-config :mct :vsc purpose)
                                                             (gral:std-portfolio market-unit situation-date
                                                                                 :asset-delivery asset-delivery
                                                                                 :rpf-delivery rpf-delivery))
                                              purpose)))))
                            (calculate-target-capital (gral:wait-delivery market-unit situation-date
                                                                          :rpf rpf-delivery))))))
                 (calculate-target-capital))
               (sign-off-assets asset-delivery)))))
    (sign-off-assets)))
