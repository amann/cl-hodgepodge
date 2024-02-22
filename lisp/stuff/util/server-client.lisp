(defun listen-connection (&optional (log *standard-output*))
  (let ((server (socket:socket-server)))
    (unwind-protect
	 ;; infinite loop, terminate with Control+C
	 (loop (format t "~&Waiting for a connection on ~S:~D~%"
                       (socket:socket-server-host server)
                       (socket:socket-server-port server))
            (with-open-stream (socket (socket:socket-accept server))
              (multiple-value-bind (local-host local-port)
                  (socket:socket-stream-local socket)
                (multiple-value-bind (remote-host remote-port)
                    (socket:socket-stream-peer socket)
                  (format log "~&Connection: ~S:~D -- ~S:~D~%"
                          remote-host remote-port local-host local-port)))
              ;; loop is terminated when the remote host closes the connection
              ;; or on EXT:EXIT
              (loop (when (eq :eof (socket:socket-status (cons socket :input)))
                      (return))
                 (let* ((received (read socket)))
                   ;; flush everything left in socket
                   (loop :for c = (read-char-no-hang socket nil nil) :while c)
                   (format log "~&Received: ~S" received)
                   (let ((sent (ignore-errors (eval received))))
                     (print sent socket)
                     (format log "~&Sent:     ~S" sent)
                     (terpri socket))))))
      ;; make sure server is closed
      (socket:socket-server-close server))))


(defun connect-to (host port)
  (with-open-stream (server (socket:socket-connect port host))
    (format t "~A> "
            (progn
              (write '(car (package-nicknames *package*))
                     :stream server)
              (read server)))
    (loop :for c = (read)
       :while c
       :do (progn
             (write c :stream server)
             (prin1 (read server))
             (format t "~&~A> "
                     (progn
                       (write '(car (package-nicknames *package*))
                              :stream server)
                       (read server)))))))



#+IGNORE(with-open-file (log #p"test.log"
				   :direction :output
				   :if-does-not-exist :create
				   :if-exists :overwrite)
  (listen-connection log))