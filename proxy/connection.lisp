(in-package :coast)

;; socketのラッパー
(defclass connection ()
  ((port :initform nil
         :initarg :port
         :accessor connection-port)
   (host :initform nil
         :initarg :host
         :accessor connection-host)
   (socket :initform nil
           :initarg :socket
           :accessor connection-socket)
   (stream :initform nil
           :initarg :stream
           :accessor connection-stream)
   (incoming-external-format
    :initform (list :utf8 :eol-style :crlf)
    :initarg :incoming-external-format
    :accessor connection-incoming-external-format)
   (outgoing-external-format
    :initform (list :utf8 :eol-style :crlf)
    :initarg :outgoing-external-format
    :accessor connection-outgoing-external-format)))

(define-condition connection-broken (error)
  ((connection :initarg :connection
               :reader connection)))

(define-condition encoding-error (error)
  ((sequence :initarg :sequence
             :reader encoding-error-sequence)))

(defgeneric connection-read-line (connection)
  (:documentation "一行分をエンコードして読む"))

(defgeneric connection-write-line (connection sequence)
  (:documentation"sequenceをデコードして送る"))

(defgeneric conection-listen (connection))

(defgeneric connection-close (connection)
  (:documentation "connectionを閉じる"))

(defgeneric connection-open-p (connection)
  (:documentation "connectionが開いているか"))

(defgeneric connection-eof-p (connection)
  (:documentation "connectionにeofが来ているか"))

(defgeneric connection-listen (connection))


(defun read-until-end-of-line (stream eol-style &optional (eof :eof))
  (let* ((index 0)
         (limit (case eol-style
                  (:cr #(13))
                  (:lf #(10))
                  (t   #(13 10))))
         (length (length limit)))
    (loop for byte = (read-byte stream nil eof)
          collect byte into list
      if (eql byte eof)
        ;; 読んでる最中にeof => 強制終了
        do (return eof)
      else if (= byte (elt limit index))
        do (incf index)
      else
        do (setf index 0)

      if (= index length)
        return (concatenate 'vector list))))

(defun decode (octets format)
  (handler-case
      (flexi-streams:octets-to-string octets :external-format format)
    (error () (error 'encoding-error :sequence octets))))

(defun encode (string format)
  (flexi-streams:string-to-octets string :external-format format))

(defmethod connection-listen ((conn connection))
  (listen (connection-stream conn)))

(defmethod connection-read-line ((conn connection))
  (with-accessors ((stream connection-stream)
                   (format connection-incoming-external-format)) conn
        ; 改行コード取得。
    (let ((style (getf (cdr format) :eol-style)))
      (when-let ((octets (read-until-end-of-line stream style :eof)))
        (when (not (eql octets :eof))
          (chomp (decode octets format)))))))

(defmethod connection-write-line ((conn connection) (string string))
  (handler-case
      (with-accessors ((stream connection-stream)
                       (format connection-outgoing-external-format)) conn
        (let ((octets (encode (format nil "~A~%" string) format)))
          (write-sequence octets stream))
        (force-output stream)
        string)
    (sb-int:simple-stream-error ()
      (error 'connection-broken :connection conn))))

(defmethod connection-close ((conn connection))
  (connection-close (connection-socket conn)))

(defmethod connection-close ((conn usocket:usocket))
  (usocket:socket-close conn))

#+sbcl
(defmethod connection-close ((conn sb-bsd-sockets:socket))
  (sb-bsd-sockets:socket-close conn))

(defmethod connection-close ((conn acl-compat.socket::chunked-stream))
  (acl-compat.socket::close conn))

(defmethod connection-close ((conn acl-compat.socket::server-socket))
  (acl-compat.socket::close conn))


(defmethod connection-open-p ((conn connection))
  (aand (connection-socket conn) (connection-open-p it)))

(defmethod connection-open-p ((conn acl-compat.socket::chunked-stream))
  (let ((plist (acl-compat.socket::stream-plist conn)))
    (when-let ((socket (getf plist :socket)))
      (connection-open-p socket))))

(defmethod connection-open-p ((conn usocket:usocket))
  (connection-open-p (usocket:socket conn)))

#+sbcl
(defmethod connection-open-p ((conn sb-bsd-sockets:socket))
  (sb-bsd-sockets:socket-open-p conn))


(defmethod connection-eof-p ((conn connection))
  (aif (connection-stream conn)
       (connection-eof-p it)
       :eof))

(defmethod connection-eof-p ((conn acl-compat.socket::chunked-stream))
  (aif (connection-stream conn)
       (connection-open-p it)
       :eof))

#+sbcl
(defmethod connection-eof-p ((conn sb-sys:fd-stream))
  (sb-impl::fd-stream-listen conn))


(defun connect (host port)
  (let ((soc (usocket:socket-connect host port
               :element-type 'flexi-streams:octet)))
    (make-instance 'connection
                   :host host :port port
                   :socket (usocket:socket soc)
                   :stream (usocket:socket-stream soc))))

(defun try-connection (hosts ports encoding &optional (err-p t))
  (dolist (host hosts)
    (dolist (port ports)
      (when-let ((conn (ignore-errors (connect host port))))
        (setf (connection-incoming-external-format conn) encoding)
        (setf (connection-outgoing-external-format conn) encoding)
        (return-from try-connection conn))))
  (when err-p
    (error "Failed to connect to ~A ~A." hosts ports)))
