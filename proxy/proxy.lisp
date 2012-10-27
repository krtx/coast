(in-package :coast)

(defclass agent ()
  ((connection :initform nil
               :initarg :connection
               :accessor agent-connection)
   (send-queue :initform nil
               :initarg :send-queue
               :accessor agent-send-queue)))

(defclass server-agent (agent) ())
(defclass client-agent (agent) ())


(defstruct proxy
  (name "coast")
  modules
  server-agents
  client-agents
  networks
  irc-list
  irc-loggers
  common-internal-nickname
  ; logを取るときにmsgを内部化するために必要
  (name-table (make-hash-table :test #'equal))
  (client-encoding (list :utf-8 :eol-style :crlf)))


(defun add-network (proxy network-name irc server-agent)
  (push (list network-name irc server-agent)
        (proxy-networks proxy)))

(defun delete-network (proxy name-or-object)
  (setf (proxy-networks proxy)
        (delete-if (lambda (entry)
                     (member name-or-object entry :test #'equal))
                   (proxy-networks proxy))))

(defun network-item (proxy thing &optional type)
  (funcall (case type
             (:name #'first)
             (:irc #'second)
             (:server-agent #'third)
             (t #'identity))
           (find-if (lambda (entry)
                      (member thing entry :test #'equal))
                    (proxy-networks proxy))))

(defun network-name (proxy thing)
  (network-item proxy thing :name))

(defun network-name-list (proxy)
  (mapcar #'car (proxy-networks proxy)))

(defun network-pair-item (proxy object)
  (awhen (cdr (network-item proxy object))
    (destructuring-bind (irc server-agent) it
      (if (eq object irc)
          server-agent
          irc))))

(defun proxy-default-network (proxy)
  (car (proxy-networks proxy)))


(cl-interpol:enable-interpol-syntax)

(defun insert-network-name (network-name string)
  (if (cl-ppcre:scan #?/(:\*\.jp)/ string)
      (cl-ppcre:register-groups-bind (stem suf)
          (#?/(.*)(:\*\.jp)/ string :sharedp t)
        #?"${stem}@${network-name}${suf}")
      #?"${string}@${network-name}"))

(defun remove-network-name (network-name string)
  (let ((regex (format nil "@~A" network-name)))
    (cl-ppcre:regex-replace regex string "")))


(defun add-name (name-table network-name external-name internal-name)
  (let ((entry (cons external-name internal-name)))
    (push entry (gethash network-name name-table))))

(defun internal-name (name-table network-name external-name)
  (let ((alist (gethash network-name name-table)))
    (cdr (assoc external-name alist :test #'equal))))

(defun external-name (name-table network-name internal-name)
  (let ((alist (gethash network-name name-table)))
    (car (rassoc internal-name alist :test #'equal))))

(defun used-as-internal-name-p (name-table string)
  "どこかのnetworkでinternal-nameとして使用されているか?"
  ; internal-nameとして使用される <=> external-nameが存在
  (some (lambda (network-name)
          (external-name name-table network-name string))
        (alexandria:hash-table-keys name-table)))

(defun add-external-name (name-table network-name external-name
                          &optional type-or-string)
  "外向きの名前と内向きの名前の項目を追加。
内向きの名前は一意でないといけない。
type = :insert     -> agentのネットワーク名を挿入して追加
type = :non-insert -> agentのネットワーク名を挿入せずそのまま追加
type = 文字列      -> typeをinternalにする
それ以外           -> すでにほかのagentで登録されていたらネットワーク名
                      を挿入、されていなかったらそのまま追加
"
  (when (not (internal-name name-table network-name external-name))
    (let ((internal-name
           (cond ((eql type-or-string :insert)
                  (insert-network-name network-name external-name))
                 ((eql type-or-string :non-insert) external-name)
                 ((stringp type-or-string) type-or-string)
                 (t
                  (if (used-as-internal-name-p name-table external-name)
                      (insert-network-name network-name external-name)
                      external-name)))))
      (prog1 internal-name
        (add-name name-table network-name external-name internal-name)
        (format t "Added: ~A <-> ~A~%" external-name internal-name)))))

(defgeneric extract-network-name (proxy message))
(defgeneric valid-message-p (message))
(defgeneric process-agent-read (proxy agent))
(defgeneric process-agent-send (proxy agent))
(defgeneric construct-internal-message (proxy irc message agent))
(defgeneric construct-external-message (proxy irc message agent))
(defgeneric construct-client-to-client-message
    (proxy irc message client1 client2))

(defgeneric on-connection (proxy client-agent))
(defgeneric on-login (proxy client-agent))
(defgeneric on-quit (proxy client-agent))

;; for debug
(defmethod construct-internal-message (proxy irc message agent)
  (print "reconstruct: invalid input")
  (mapcar #'print (list proxy irc message agent)))

(defmethod construct-external-message (proxy irc message agent)
  (print "reconstruct: invalid input")
  (mapcar #'print (list proxy irc message agent)))


(defgeneric proxy-log (proxy agent logger message when))

(defmethod proxy-log (proxy agent logger message when))

(defun setup-proxy (proxy networks)
  (dolist (arg networks)
    (destructuring-bind (&key network
                              nickname
                              (username nickname)
                              (realname nickname)
                              (encoding (list :utf8 :eol-style :crlf))
                              (class 'coast.irc::irc)
                              hosts
                              ports
                              password
                              channels) arg
      (add-external-name (proxy-name-table proxy) network nickname
                         (proxy-common-internal-nickname proxy))
      (let* ((irc (make-instance class
                                 :nickname nickname
                                 :username username
                                 :realname realname))
             ; socketを開いて接続
             (connection (try-connection hosts ports encoding))
             ; 接続した瞬間にserverに送るメッセージ
             (messages (coast.irc::generate-initial-messages
                        irc
                        channels
                        password))
             (server-agent (make-instance 'irc-server-agent
                                          :connection connection
                                          :send-queue messages)))
        (add-network proxy network irc server-agent)
        (push irc (proxy-irc-list proxy))
        (push server-agent (proxy-server-agents proxy))))))

(defun process-proxy (proxy)
  (labels ((process-agent (agent)
             (with-accessors ((connection agent-connection)) agent
               ;; 残っているメッセージがあるなら3つまで送る。
               (loop repeat 3
                     while (and (agent-send-queue agent)
                                (connection-open-p connection)) do
                 (process-agent-send proxy agent))
                   ; send時のmoduleを実行
               ;; メッセージが来ていたら全部読む。
               (loop while (and (connection-open-p connection)
                                (connection-listen connection)) do
                 (process-agent-read proxy agent))
               ;; 接続が死んでる場合。
               (when (or (connection-eof-p connection)
                         (not (connection-open-p connection)))
                 (connection-close connection)
                 (if (subtypep (type-of agent) 'server-agent)
                     (delete-network proxy agent)
                     (progn
                       (on-quit proxy agent)
                       (pull agent (proxy-client-agents proxy))))))))
    (mapc #'process-agent (proxy-server-agents proxy))
    (mapc #'process-agent (proxy-client-agents proxy))))


(defclass acceptor ()
  ((socket :initform nil
           :initarg :socket
           :accessor acceptor-socket)
   (agent-class :initform nil
                 :initarg :agent-class
                 :accessor acceptor-agent-class)))

(defmethod print-object ((acceptor acceptor) stream)
  (print-unreadable-object (acceptor stream :type t :identity t)
    (with-slots (socket client-class) acceptor
      (format stream "port ~d for ~A~%"
              (acl-compat.socket::port socket)
              (acceptor-agent-class acceptor)))))

(defun make-server-socket (port)
  (acl-compat.socket:make-socket :local-port port
                                 :local-host nil
                                 :connect :passive
                                 :format :bivalent
                                 :reuse-address t))

(defun accept-connection (socket &key wait)
  (when-let ((socket-stream
              (acl-compat.socket:accept-connection socket :wait wait)))
    (let ((stream (slot-value socket-stream 'gray-stream::lisp-stream))
          (ipaddr (acl-compat.socket:remote-host socket-stream)))
      (make-instance 'connection
                     :host (acl-compat.socket:ipaddr-to-dotted ipaddr)
                     :port (acl-compat.socket:remote-port socket-stream)
                     :stream stream
                     :socket socket-stream))))

(defun process-new-connection (proxy acceptor)
  (with-accessors ((agent-class acceptor-agent-class)
                   (socket acceptor-socket)) acceptor
    (when-let ((conn (accept-connection socket :wait 0)))
      (format t "accept connection from ~A~%" (connection-host conn))
      (setf (connection-incoming-external-format conn)
            (proxy-client-encoding proxy))
      (setf (connection-outgoing-external-format conn)
            (proxy-client-encoding proxy))
      (let ((agent (make-instance agent-class :connection conn)))
        (handler-case (on-connection proxy agent)
          (error ()
            (format t "error while processing connection.~%")
            (connection-close conn)))
        (when (connection-open-p conn)
          (push agent (proxy-client-agents proxy)))))))

(defun proxy-main-loop (proxy acceptor)
  (loop
    (sleep 0.1)
    (handler-case (process-proxy proxy)
      (encoding-error (e)
        (print (encoding-error-sequence e))))
     (process-new-connection proxy acceptor)))


(defvar *proxy* nil)
(defvar *acceptor* nil)

(defun run (&key name networks common-internal-nickname irc-loggers)
  (when *acceptor*
    (acl-compat.socket::close (acceptor-socket *acceptor*)))
  (setq *acceptor* (make-instance 'acceptor
                                  :socket (make-server-socket 15535)
                                  :agent-class 'irc-client-agent))
  (when *proxy*
    (mapc (lambda (agent)
            (connection-close (agent-connection agent)))
          (proxy-server-agents *proxy*))
    (mapc (lambda (agent)
            (connection-close (agent-connection agent)))
          (proxy-client-agents *proxy*)))
  (setq *proxy* (make-proxy
                 :name name
                 :common-internal-nickname common-internal-nickname
                 :irc-loggers irc-loggers))
  (setup-proxy *proxy* networks)
  (proxy-main-loop *proxy* *acceptor*))

;;sample

#+nil
(run
 :name "coast"
 :common-internal-nickname "koji"
 :networks '((:network "sample"
              :nickname "nick"
              :encoding (:utf8 :eol-style :crlf)
              :hosts ("localhost")
              :ports (16665)
              :channels ("#channel")))
 :irc-loggers (list (make-instance 'coast.irc::channel-logger)))

