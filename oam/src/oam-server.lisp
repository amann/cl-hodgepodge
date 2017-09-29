
(defun make-server-handler (log)
  (let ((remote))
   (lambda (stream)
     ;(setq remote (format nil "~S:~D" usocket:*remote-host* usocket:*remote-port*))
     (format log "~&Got connection from ~A." remote)
     (handler-case
         (loop :for received := (read stream)
            :do (flush-stream stream)
            :do (format log "~&~A Rcvd: ~S" remote received)
            :do (format log "~&~A Sent: ~S" remote (print (ignore-errors (eval received)) stream))
            :do (force-output stream))
       (end-of-file ()))
     (format log "~&Lost connection from ~A." remote))))

(defun flush-stream (stream)
  (loop :while (read-char-no-hang stream nil nil)))


(defun start-server (host port)
  (usocket:socket-server host port (make-server-handler *standard-output*) nil :in-new-thread t :multi-threading t))

(defun listen-connection (host port &optional (log *standard-output*))
  (let ((server (usocket:socket-listen host port)))
    (unwind-protect
	 ;; infinite loop, terminate with Control+C
	 (loop (format log "~&Waiting for a connection on ~S:~D~%"
                       (usocket:get-local-name server)
                       (usocket:get-local-port server))
            (let ((socket (usocket:socket-accept server)))
              (format log "~&Got connection: ~S:~D -- ~S:~D~%"
                      (usocket:get-peer-name socket) (usocket:get-peer-port socket)
                      (usocket:get-local-name socket) (usocket:get-local-port socket))
              ;; loop is terminated when the remote host closes the connection
              ;; or on EXT:EXIT
              (with-open-stream (stream (usocket:socket-stream socket))
                (loop
                   (let* ((received (read stream)))
                     ;; flush everything left in stream
                     (loop :for c = (read-char-no-hang stream nil nil) :while c)
                     (format log "~&Received: ~S" received)
                     (let ((sent (ignore-errors (eval received))))
                       (print sent stream)
                       (format log "~&Sent:     ~S" sent)
                       (terpri stream)
                       (force-output stream)))))))
      ;; make sure server is closed
      (usocket:socket-close server))))


(defun connect-to (host port)
  (with-open-stream (server (usocket:socket-stream (usocket:socket-connect host port)))
    (format t "~A> "
            (progn
              (write '(car (package-nicknames *package*))
                     :stream server)
              (terpri server)
              (force-output server)
              (read server)))
    (loop :for c = (read)
       :while c
       :do (progn
             (write c :stream server :readably t)
             (terpri server)
             (force-output server)
             (prin1 (read server))
             (format t "~&~A> "
                     (progn
                       (write '(car (package-nicknames *package*))
                              :stream server)
                       (terpri server)
                       (force-output server)
                       (read server)))))))



#+(or)
(with-open-file (log #p"test.log"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
  (listen-connection log))