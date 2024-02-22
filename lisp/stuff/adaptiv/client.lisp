(defun connect-to (host port)
  (with-open-stream (server (socket:socket-connect port host))
    (format t ">")
    (loop :for c = (read)
       :while c
       :do (progn
             (write c :stream server)
             (prin1 (read server))
             (format t "~&>")))))