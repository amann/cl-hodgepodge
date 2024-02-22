(defun listen-to-connection ()
  (let ((server (socket:socket-server)))
    (format t "~&Waiting for a connection on ~S:~D~%"
            (socket:socket-server-host server)
            (socket:socket-server-port server))
    (unwind-protect
         ;; infinite loop, terminate with Control+C
         (loop (with-open-stream (socket (socket:socket-accept server))
                 (multiple-value-bind (local-host local-port)
                     (socket:socket-stream-local socket)
                   (multiple-value-bind (remote-host remote-port)
                       (socket:socket-stream-peer socket)
                     (format t "~&Connection: ~S:~D -- ~S:~D~%"
                             remote-host remote-port local-host local-port)))
                 ;; loop is terminated when the remote host closes the connection or on EXT:EXIT
                 (loop (when (eq :eof (socket:socket-status (cons socket :input)))
                         (return))
                    (print (eval (read socket)) socket)
                    ;; flush everything left in socket
                    (loop :for c = (read-char-no-hang socket nil nil) :while c)
                    (terpri socket))))
      ;; make sure server is closed
      (socket:socket-server-close server))))