(in-package :coconuts.http)


(defclass web:request (message)
  ((headers-in :initarg :headers
               :accessor request-headers-in))
   (method :initarg :method
           :accessor request-method)
   (uri :initarg :uri
        :accessor request-uri)
   (protocol :initarg :protocol
             :accessor protocol)
   (remote-addr :initarg :remote-addr
                :accessor remote-addr)
   (remote-port :initarg :remote-port
                :accessor remote-port)
   (content-stream :initarg :content-stream
                   :accessor content-stream)
   (cookies :initform nil
            :accessor cookies)
   (get-parameters :initform nil
                   :accessor get-parameters)
   (post-parameters :initform nil
                    :accessor post-parameters)
   (script-name :initform nil
                :accessor script-name)
   (query-string :initform nil
                 :accessor query-string)
   (session :initform nil
            :accessor session)
   (aux-data :initform nil
             :accessor aux-data)
   (raw-post-data :initform nil))

(export-symbol :web :reply)
(defclass web:reply (message)
  ((content-type :initform nil
                 :accessor content-type)
   (content-length :initform nil
                   :accessor content-length)
   (headers-out :initform nil
                :accessor headers-out)
   (return-code :initform +http-ok+
                :accessor return-code)
   (external-format :initform nil
                    :accessor reply-external-format)
   (cookies :initform nil
            :accessor cookies)))

(defmethod read-message ((client web:client) &key hang)
  (declare (ignore hang))
  (with-slots (connection) client
    (if (listen (connection-stream connection))
        (let ((chunga:*accept-bogus-eols* t)
              (input-stream (flexi-streams:make-flexi-stream
                             (connection-stream connection)
                             :element-type 'flexi-streams:octet)))
          (multiple-value-bind (headers method url-string protocol)
                               (hunchentoot::get-request-data input-stream)
            (make-instance 'web:request
                           :remote-addr (connection-host connection)
                           :remote-port (connection-port connection)
                           :headers headers
                           :method method
                           :uri url-string
                           :content-stream input-stream
                           :protocol protocol)))
        (and (connection-eof-p connection) :eof))))

(defmethod on-message ((module (eql :distribute))
                       (req web:request) (client web:client) (proxy proxy))
  (let-when (reply (dispatch-request proxy client req))
    (push-back (send-q client) reply)))

