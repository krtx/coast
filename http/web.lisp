(in-package :coconuts)

(require :hunchentoot)

(setq cl-who:*prologue* "<!DOCTYPE html>")

(export-symbol :web :client)
(defclass web:client (client)
  ((acceptor :initform nil
             :accessor client-acceptor)
   (reply :initform nil
          :accessor client-reply)))

(export-symbol :web :keitai)
(defclass web:keitai (web:client) ())

(export-symbol :web :server)
(defclass web:server (server) ())

(export-symbol :web :twitter)
(defclass web:twitter (web:server) ())

(export-symbol :web :message)
(defclass web:message (message)
  ((request :initform nil
            :initarg :request
            :accessor message-request)))


(defmethod on-client-connection ((proxy proxy) (client web:client))
  (setf (connection-incoming-external-format (connection client))
        (list :utf8 :eol-style :crlf))
  (setf (connection-outgoing-external-format (connection client))
        (list :utf8 :eol-style :crlf))
  (setf (client-acceptor client)
        (make-instance 'hunchentoot:acceptor
                       :persistent-connections-p nil
                       :request-dispatcher 'dispatch-request))
  (let ((hunchentoot:*hunchentoot-default-external-format*
         (apply #'flexi-streams:make-external-format
                (connection-outgoing-external-format (connection client))))
        (hunchentoot:*default-content-type* "text/html; charset=utf-8"))
    (setf (client-reply client) (make-instance 'hunchentoot:reply))))


;; irc:messageのreconstructは落とす
(defmethod reconstruct ((proxy proxy) (msg irc:message)
                        (server irc:server) (client web:client)))


;; 接続時のclientからのrequestを取ってくる
(defmethod read-message ((client web:client) &key hang)
  (declare (ignore hang))
  (with-slots (connection) client
    (if (listen (connection-stream connection))
        (let ((chunga:*accept-bogus-eols* t)
              (input-stream (flexi-streams:make-flexi-stream
                             (connection-stream connection)
                             :element-type 'flexi-streams:octet)))
          (multiple-value-bind (headers-in method url-string protocol)
                               (hunchentoot::get-request-data input-stream)
            (let ((hunchentoot:*acceptor* (client-acceptor client))
                  (hunchentoot:*reply* (client-reply client)))
              (make-instance 'web:message
               :request (make-instance 'hunchentoot:request
                         :remote-addr (connection-host connection)
                         :remote-port (connection-port connection)
                         :acceptor (client-acceptor client)
                         :headers-in headers-in
                         :method method
                         :uri url-string
                         :content-stream input-stream
                         :server-protocol protocol)))))
        (and (connection-eof-p connection) :eof))))


(defvar r nil)


(defmethod on-message ((module (eql :web-irc))
                       (msg web:message) (client web:client) (proxy proxy))
  (let ((hunchentoot:*hunchentoot-default-external-format*
         (apply #'flexi-streams:make-external-format
                (connection-outgoing-external-format (connection client))))
        (hunchentoot:*acceptor* (client-acceptor client))
        (hunchentoot:*reply* (client-reply client))
        (hunchentoot:*request* (message-request msg)))
    (unless r (setq r (message-request msg)))
    (push-back (send-q client)
      (flexi-streams:with-output-to-sequence
          (hunchentoot::*hunchentoot-stream*)
        (hunchentoot:process-request hunchentoot:*request*)))))

(defmethod send-message ((client web:client) sequence)
  (send-conn-line client sequence)
  (close-connection client))

(defgeneric generate-page (type &rest args))

(defmethod generate-page ((type (eql :menu)) &key proxy)
  (cl-who:with-html-output-to-string (stream nil :prologue t)
    (:html
     (:head (:title "めにゅ～"))
     (dolist (server (proxy-servers proxy))
       (when (typep server 'irc:server)
         (dolist (channel (server-channels server))
           (with-slots (name) channel
             (dolist (module (proxy-modules proxy))
               (when (typep module 'module:log)
                 (with-slots (loggers) module
                   (dolist (logger loggers)
                     (when (typep logger 'channel-logger)
                       (when (log-channel-p logger name)
                         (let* ((internal
                                 (subseq (internal-name name server proxy) 1))
                                (link
                                 (mkstr "/channel/" (subseq name 1) "?"
                                        "server=" (server-name server))))
                           (cl-who:htm
                            (:a :href link
                                (:b (cl-who:str internal)) :br))))))))))))))))



(defun generate-channel-page (proxy channel-name)
  (cl-who:with-html-output-to-string (stream nil)
    (dolist (module (proxy-modules proxy))
      (when (typep module 'module:log)
        (with-slots (loggers) module
          (dolist (logger loggers)
            (when (typep logger 'channel-logger)
              (let-when (dir (log-channel-p logger channel-name))
                (let ((filename
                       (make-log-filename logger (get-universal-time))))
                  (let ((logs (read-log logger dir filename)))
                    (dolist (log (nreverse logs))
                      (cl-who:htm (:p (cl-who:str log))))))))))))))


(defmethod generate-page ((type (eql :channel)) &key proxy channel-name)
  (cl-who:with-html-output-to-string (stream nil :prologue t)
    (:html
     (:head (:title (cl-who:str channel-name)))
     (:form :method "post" (:input :type "text" :name "PRIVMSG"))
     (cl-who:str (generate-channel-page proxy channel-name)))))


(defun channel-page (request)
  (let ((uri (hunchentoot:request-uri request))
        (method (hunchentoot:request-method request)))
    (cl-ppcre:register-groups-bind
        (channel-name server-name) ("/channel/([^/]+)\\?server=([^/]+)" uri)
      (when (and channel-name server-name)
        (let-when (server (find-server *proxy* server-name))
          (let-when (channel (find-if (lambda (c)
                                        (string= channel-name (channel-name c)
                                                 :start2 1))
                                      (server-channels server)))
            (with-slots (name) channel
              (when (eql method :post)
                (let ((alist (hunchentoot:post-parameters request)))
                  (let ((privmsg (cdr (assoc "PRIVMSG" alist :test #'string=))))
                    (push-back (send-q server)
                      (make-instance 'irc:privmsg-message
                       :arguments (list name privmsg)))
                    (setf (hunchentoot:return-code*)
                          hunchentoot:+http-see-other+)
                    (setf (hunchentoot:header-out "Location") uri))))
              (generate-page :channel
               :proxy *proxy*
               :channel-name (internal-name name server *proxy*)))))))))


(defun main-page (request)
  (declare (request))
  (generate-page :menu :proxy *proxy*))

(defvar *request-handlers* nil)

(defun dispatch-request (request)
  (dolist (handler *request-handlers*)
    (awhen (funcall handler request)
      (return it))))

(setq *request-handlers*
      (list 'channel-page 'main-page
            'hunchentoot::list-request-dispatcher))

#+nil
(push :web-irc (proxy-modules *proxy*))