(in-package :coast)

(defclass irc-agent () ())

(defclass irc-server-agent (server-agent irc-agent) ())

(defclass irc-client-agent (client-agent irc-agent coast.irc::user-mixin)
  ((logged-in-p :initform nil
                :accessor irc-client-agent-logged-in-p)))

(defun make-welcome-messages (proxy irc-client-agent)
  "リモートクライアントがログインしたときのメッセージ"
  (let ((proxy-name (proxy-name proxy))
        (nickname (coast.irc::nickname irc-client-agent)))
    (loop for (cmd text)
       in '((:rpl_welcome "Welcome to the Internet Relay Network")
            (:rpl_yourhost "Your host is coast, running version coast.irc::0")
            (:rpl_motdstart "- coast Message of the Day -")
            (:rpl_motd "- - C O C O N U T S - :::version coast.irc::0:::")
            (:rpl_motd "- Copyright (c) 2010 koji. All rights reserved.")
            (:rpl_motd "- This is free software.")
            (:rpl_endofmotd "End of MOTD command."))
       collect (make-instance (coast.irc::find-message-class cmd)
                              :source proxy-name
                              :arguments (list nickname text)))))

(defun make-rpl_namreply-arguments (irc channel)
  (let* ((capabilities (coast.irc::irc-capabilities irc))
         (prefix (cadr (assoc "PREFIX" capabilities :test #'equal))))
    (cl-ppcre:register-groups-bind (values prefixes)
                                   ("\\((.*)\\)(.*)" prefix)
      (when (not values)
        (setq values   ""))
      (when (not prefixes)
        (setq prefixes ""))
      (loop for user in (coast.irc::channel-users irc channel)
         collect
           (let ((modes (coast.irc::get-user-modes user channel)))
             (when (< 1 (length modes))
               (setf modes (list "o")))
             (aif (and modes (position (char (car modes) 0) values))
                  (mkstr (char prefixes it) (coast.irc::nickname user))
                  (coast.irc::nickname user)))))))

(defun make-channel-join-messages (proxy irc)
  "ローカルクライアントが入っているチャンネルの情報を返す"
  (with-accessors ((nickname coast.irc::nickname)
                   (username coast.irc::username)
                   (hostname coast.irc::hostname))
      (coast.irc::irc-user irc)
    (loop for channel in (coast.irc::irc-channels irc)
          for name = (coast.irc::channel-name channel)
          for topic = (coast.irc::channel-topic channel)
          for visib = (coast.irc::channel-visibility channel)
      collect (make-instance 'coast.irc::join-message
                             :source nickname
                             :user username
                             :host hostname
                             :arguments (list name))
      when topic
        collect (make-instance 'coast.irc::rpl_topic-message
                               :source (proxy-name proxy)
                               :arguments (list nickname name topic))
      collect (let* ((nicks (make-rpl_namreply-arguments irc channel))
                     (trail (format nil "~{~@{~a~^ ~}~}" nicks))
                     (args (list nickname visib name trail)))
                (make-instance 'coast.irc::rpl_namreply-message
                               :source (proxy-name proxy)
                               :arguments args))
     collect (let ((args (list nickname name "End of NAMES list")))
               (make-instance 'coast.irc::rpl_endofnames-message
                              :source (proxy-name proxy)
                              :arguments args)))))


(defun add-entries-to-name-table (proxy server-agent message)
  "server-agentから来たmessageに従ってname-tableにエントリを追加"
  (with-accessors ((source coast.irc::message-source)
                   (command coast.irc::message-command)
                   (arguments coast.irc::message-arguments)) message
    (when-let* ((irc (network-pair-item proxy server-agent))
                (in-irc-network-name (network-name proxy server-agent)))
      (case command
        (:join
         (add-external-name (proxy-name-table proxy)
                            in-irc-network-name
                            source)
         (let ((channel (car arguments)))
           (add-external-name (proxy-name-table proxy)
                              in-irc-network-name
                              channel
                              :insert)))
        (:nick
         (let ((new-nick (car arguments)))
           (add-external-name (proxy-name-table proxy)
                              in-irc-network-name
                              new-nick)))
        (:rpl_namreply
         (let ((nicks (fourth arguments)))
           (dolist (nick% (cl-ppcre:split " " nicks))
             (let ((nick (coast.irc::cut-nickname-prefix irc nick%)))
               (add-external-name (proxy-name-table proxy)
                                  in-irc-network-name
                                  nick)))))))))



(defmethod extract-network-name (proxy (msg coast.irc::message))
  ;; ニックネームやチャンネル名が登録されているはずなので
  ;; 登録されてるサーバを探す
  (let ((nick (coast.irc::message-source msg))
        (channel (car (coast.irc::message-arguments msg)))
        (name-table (proxy-name-table proxy)))
    (dolist (network-name (network-name-list proxy))
      (when (or (external-name name-table network-name nick)
                (external-name name-table network-name channel))
        (return-from extract-network-name network-name)))))

(defmethod extract-network-name (proxy (msg coast.irc::join-message))
  (let ((internal-channel-name (car (coast.irc::message-arguments
                                     msg))))
    (loop for network-name in (network-name-list proxy)
      when (remove-network-name network-name internal-channel-name)
        return network-name)))

(defmethod valid-message-p ((msg coast.irc::message))
  (with-accessors ((command coast.irc::message-command)
                   (arguments coast.irc::message-arguments)) msg
    (case command
      ((:privmsg :notice)
       (when (and (car arguments) (cadr arguments))
         msg))
      (:join
       (when (car arguments)
         msg))
      (t msg))))

(defgeneric process-irc-message (proxy agent message))

(defmethod process-irc-message ((proxy proxy)
                                (agent irc-server-agent)
                                (message coast.irc::message))
  (let ((irc (network-pair-item proxy agent)))
    (coast.irc::update-irc irc message)
    (dolist (client-agent (proxy-client-agents proxy))
      (when-let ((msg2 (construct-internal-message proxy
                                                   irc
                                                   message
                                                   client-agent)))
        (push-back (agent-send-queue client-agent) msg2)))))

(defmethod process-irc-message ((proxy proxy)
                                (agent irc-server-agent)
                                (message coast.irc::ping-message))
  (let ((host (car (coast.irc::message-arguments message))))
    (push-back (agent-send-queue agent)
               (make-instance 'coast.irc::pong-message
                              :arguments (list host host)))))

(defmethod process-irc-message ((proxy proxy)
                                (agent irc-client-agent)
                                (message coast.irc::message))
  (with-accessors ((command coast.irc::message-command)
                   (arguments coast.irc::message-arguments)) message
    (case command
      ((:mode :part :privmsg :notice :whois :who :join)
       (when (valid-message-p message)
         ; messageからどのネットワークに向けたものかを判定
         (bind (((nil irc server-agent)
                 (or (network-item
                      proxy
                      (extract-network-name proxy message))
                     (proxy-default-network proxy))))
           (when-let ((msg2 (construct-external-message
                             proxy
                             irc
                             message
                             agent)))
             (push-back (agent-send-queue server-agent) msg2)
             (dolist (client-agent (proxy-client-agents proxy))
               (when (not (eq client-agent agent))
                 (when-let ((msg3 (construct-client-to-client-message
                                   proxy
                                   irc
                                   message
                                   agent
                                   client-agent)))
                   (push-back (agent-send-queue client-agent) msg3))))))))
      (:nick
       (when (not (irc-client-agent-logged-in-p agent))
         (setf (coast.irc::nickname agent) (first arguments))
         (on-login proxy agent)))
      (:user
       (when (not (irc-client-agent-logged-in-p agent))
         (setf (coast.irc::username agent) (first arguments))
         (setf (coast.irc::realname agent) (fourth arguments))
         (on-login proxy agent))))))



(defmethod proxy-log ((proxy proxy)
                      (agent irc-server-agent)
                      (logger coast.irc::channel-logger)
                      (message coast.irc::message)
                      when)
  (let ((network-name (network-name proxy agent))
        (irc (network-pair-item proxy agent))
        (cmd (coast.irc::message-command message))
        (msg2 (coast.irc::copy-message message)))
    (case cmd
      ((:nick :quite)
       (when (eql when :read)
         (let* ((nick (coast.irc::message-source msg2))
                (user (coast.irc::get-user irc nick)))
           (dolist (channel (coast.irc::user-channels user))
             (let ((name (internal-name
                          (proxy-name-table proxy)
                          network-name
                          (coast.irc::channel-name channel))))
               (coast.irc::channel-log irc logger msg2 name when))))))
      ((:privmsg :notice :mode :quit :part :topic :join)
       (when-let ((channel-name
                   #1=(car (coast.irc::message-arguments msg2))))
         (setf #1# (internal-name (proxy-name-table proxy)
                                  network-name
                                  channel-name))
         (coast.irc::channel-log irc
                                    logger
                                    msg2
                                    channel-name
                                    when))))))

(defmethod process-agent-read ((proxy proxy) (agent irc-agent))
  (with-accessors ((connection agent-connection)) agent
      ; 文字列を読み取ってmessage構築
    (let* ((string (connection-read-line connection))
           (message (coast.irc::construct-message string)))
      (format t "Reading: [~A]~%"
              (coast.irc::make-raw-message message))
      (add-entries-to-name-table proxy agent message)
      (dolist (logger (proxy-irc-loggers proxy))
        (proxy-log proxy agent logger message :read))
      (process-irc-message proxy agent message))))

(defmethod process-agent-send ((proxy proxy) (agent irc-agent))
  "send-queueに貯まっているメッセージをひとつ送る"
  (let* ((message (pop (agent-send-queue agent)))
         (string (coast.irc::make-raw-message message)))
    (format t "Sending: [~A]~%" string)
    (connection-write-line (agent-connection agent) string)
    (dolist (logger (proxy-irc-loggers proxy))
      (proxy-log proxy agent logger message :send))))

(defmethod on-connection ((proxy proxy) (client-agent irc-client-agent))
  "接続時の処理をする"
  ; pingを送る
  (push-back (agent-send-queue client-agent)
             (make-instance 'coast.irc::ping-message
                            :arguments (list (proxy-name proxy))))
  (setf (coast.irc::hostname client-agent)
        (acl-compat.socket::ipaddr-to-hostname
         (acl-compat.socket::remote-host
          (connection-socket (agent-connection client-agent))))))

(defmethod on-login ((proxy proxy) (client-agent irc-client-agent))
  "ログイン時の処理をする"
  (with-accessors ((nickname coast.irc::nickname client-agent)
                   (username coast.irc::username client-agent)
                   (realname coast.irc::realname client-agent)
                   (hostname coast.irc::hostname client-agent))
      client-agent
    (when (and nickname username realname)
      ; ログイン可能
      (nconcf (agent-send-queue client-agent)
              (make-welcome-messages proxy client-agent))
      ; もし必要があればnicknameを変更
      (with-accessors ((internal-nickname
                        proxy-common-internal-nickname)) proxy
        (when (string/= nickname internal-nickname)
          (push-back (agent-send-queue client-agent)
                     (make-instance 'coast.irc::nick-message
                                    :source nickname
                                    :user username
                                    :host hostname
                                    :arguments (list internal-nickname)))))
      (dolist (irc (proxy-irc-list proxy))
        (nconcf (agent-send-queue client-agent)
                (mapcar (lambda (message)
                          (construct-internal-message proxy
                                                      irc
                                                      message
                                                      client-agent))
                        (make-channel-join-messages proxy irc))))
      (setf (irc-client-agent-logged-in-p client-agent) t))))

(defmethod on-quit ((proxy proxy) (client-agent irc-client-agent))
  (format t "Connection to ~A closed~%"
          (connection-host (agent-connection client-agent))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-externalization-rule (rule vars)
    (case (first rule)
      (:replace
       (destructuring-bind (proxy irc msg agent) vars
         (case (second rule)
           (:source
            #1=`(awhen (external-name (proxy-name-table ,proxy)
                                      (network-name ,proxy ,irc)
                                      (coast.irc::message-source ,msg))
                  (setf (coast.irc::message-source ,msg) it)))
           (:user
            #2=`(when (equal (coast.irc::message-user ,msg)
                             (coast.irc::username ,agent))
                  (setf (coast.irc::message-user ,msg)
                        (coast.irc::username
                         (coast.irc::irc-user ,irc)))))
           (:host
            #3=`(when (equal (coast.irc::message-host ,msg)
                             (coast.irc::hostname ,agent))
                  (setf (coast.irc::message-host ,msg)
                        (coast.irc::hostname
                         (coast.irc::irc-user ,irc)))))
           (:prefixes `(progn ,#1# ,#2# ,#3#))
           (t (destructuring-bind (place &key listp) (cdr rule)
                (if listp
                    `(awhen ,place ;ちゃんと値があるか確認
                       (setf ,place
                             (mapcar
                              (lambda (name)
                                (external-name (proxy-name-table ,proxy)
                                               (network-name ,proxy ,irc)
                                               name))
                              it)))
                    `(setf ,place
                           (external-name (proxy-name-table ,proxy)
                                          (network-name ,proxy ,irc)
                                          ,place))))))))
      ((:username :hostname :realname)
       (destructuring-bind (proxy irc msg agent) vars
         (declare (ignore proxy msg))
         (let ((place (cadr rule)))
           `(when (equal ,place
                         (,(intern (string (first rule))
                                   :coast.irc)
                           ,agent))
              (setf ,place (,(intern (string (first rule))
                                     :coast.irc)
                             (coast.irc::irc-user ,irc)))))))
      (:progn `(progn ,@(cdr rule))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-externalization (name args &rest clauses)
    (setq args (mapcar #'alexandria:ensure-list args))
    (let* ((vars (mapcar (lambda (x) (if (consp x) (car x) x)) args))
           (var-gensyms (mapcar #'gensym (mapcar #'string vars))))
      `(progn
         ,@(mapcar
            (lambda (clause)
              (ecase (car clause)
                (on
                 (destructuring-bind (class &rest rules) (cdr clause)
                   (let* ((classes (mapcar (lambda (x)
                                             (if (eq (second x) '*)
                                                 class
                                                 (or (second x) t)))
                                           args))
                          (msg (elt var-gensyms (position class classes)))
                          (new (gensym "NEW")))
                     `(defmethod ,name
                          ,(mapcar #'list var-gensyms classes)
                        (let ((,new
                               (make-instance
                                (type-of ,msg)
                                :source
                                 (coast.irc::message-source ,msg)
                                :user (coast.irc::message-user ,msg)
                                :host (coast.irc::message-host ,msg)
                                :command
                                 (coast.irc::message-command ,msg)
                                :arguments
                                 (copy-list
                                  (coast.irc::message-arguments ,msg)))))
                          ((lambda ,vars
                             (declare (ignorable ,@vars))
                             ,@(mapcar
                                (lambda (rule)
                                  (expand-externalization-rule rule vars))
                                rules))
                           ,@(mapcar (lambda (s) (if (eq s msg) new s))
                                     var-gensyms))
                          ,new)))))))
            clauses)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-internalization-rule (rule vars)
    (case (first rule)
      (:replace
       (destructuring-bind (proxy irc msg agent) vars
         (case (second rule)
           (:source
            #1=`(awhen (internal-name (proxy-name-table ,proxy)
                                      (network-name ,proxy ,irc)
                                      (coast.irc::message-source msg))
                  (setf (coast.irc::message-source ,msg) it)))
           (:user
            #2=`(when (equal (coast.irc::message-user ,msg)
                             (coast.irc::username
                              (coast.irc::irc-user ,irc)))
                  (setf (coast.irc::message-user ,msg)
                        (coast.irc::username ,agent))))
           (:host
            #3=`(when (equal (coast.irc::message-user ,msg)
                             (coast.irc::hostname
                              (coast.irc::irc-user ,irc)))
                  (setf (coast.irc::message-host ,msg)
                        (coast.irc::hostname ,agent))))
           (:prefixes `(progn ,#1# ,#2# ,#3#))
           (t (destructuring-bind (place &key listp) (cdr rule)
                (if listp
                    `(awhen ,place
                       (setf ,place
                             (mapcar
                              (lambda (name)
                                (internal-name (proxy-name-table ,proxy)
                                               (network-name ,proxy ,irc)
                                               name))
                              it)))
                    `(setf ,place
                           (internal-name (proxy-name-table ,proxy)
                                          (network-name ,proxy ,irc)
                                          ,place))))))))
      ((:username :hostname :realname)
       (destructuring-bind (proxy irc msg agent) vars
         (declare (ignore proxy msg))
         (let ((place (cadr rule)))
           `(when (equal ,place
                         (,(intern (string (first rule))
                                   :coast.irc)
                           (coast.irc::irc-user ,irc)))
              (setf ,place (,(intern (string (first rule))
                                     :coast.irc)
                             ,agent))))))
      (:progn `(progn ,@(cdr rule))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-internalization (name args &rest clauses)
    (setq args (mapcar #'alexandria:ensure-list args))
    (let* ((vars (mapcar (lambda (x) (if (consp x) (car x) x)) args))
           (var-gensyms (mapcar #'gensym (mapcar #'string vars))))
      `(progn
         ,@(mapcar
            (lambda (clause)
              (ecase (car clause)
                (on
                 (destructuring-bind (class &rest rules) (cdr clause)
                   (let* ((classes (mapcar (lambda (x)
                                             (if (eq (second x) '*)
                                                 class
                                                 (or (second x) t)))
                                           args))
                          (msg (elt var-gensyms (position class classes)))
                          (new (gensym "NEW")))
                     `(defmethod ,name
                          ,(mapcar #'list var-gensyms classes)
                        (let ((,new
                               (make-instance
                                (type-of ,msg)
                                :source
                                 (coast.irc::message-source ,msg)
                                :user (coast.irc::message-user ,msg)
                                :host (coast.irc::message-host ,msg)
                                :command
                                 (coast.irc::message-command ,msg)
                                :arguments
                                 (copy-list
                                  (coast.irc::message-arguments ,msg)))))
                          ((lambda ,vars
                             (declare (ignorable ,@vars))
                             ,@(mapcar
                                (lambda (rule)
                                  (expand-internalization-rule rule vars))
                                rules))
                           ,@(mapcar (lambda (s) (if (eq s msg) new s))
                                     var-gensyms))
                          ,new)))))))
            clauses)))))

(define-internalization construct-internal-message
    ((proxy proxy) (irc coast.irc::irc) (msg *) (agent irc-client-agent))

  (on coast.irc::message
    (:replace :prefixes)
    (:replace (car (coast.irc::message-arguments msg))))

  (on coast.irc::ping-message
    (:progn (setf (coast.irc::message-arguments msg)
                  (list (proxy-name proxy) (proxy-name proxy)))))

  (on coast.irc::quit-message
    (:replace :prefixes))

  (on coast.irc::mode-message
    (:replace :prefixes)
    (:replace (car (coast.irc::message-arguments msg)))
    (:replace (cddr (coast.irc::message-arguments msg)) :listp t)))

(define-internalization construct-internal-message
    ((proxy proxy) (irc coast.irc::irc) (msg *) (agent irc-client-agent))

  (on coast.irc::rpl-message
    (:replace (first (coast.irc::message-arguments msg)))
    (:replace (second (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_luserclient-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_luserchannels-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_luserme-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_localusers-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_globalusers-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_statsdline-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_motdstart-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_motd-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_endofmotd-message
    (:replace (first (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_whoreply-message
    (:replace (first (coast.irc::message-arguments msg)))
    (:replace (second (coast.irc::message-arguments msg)))
    (:replace (sixth (coast.irc::message-arguments msg)))
    (:username (third (coast.irc::message-arguments msg)))
    (:hostname (fourth (coast.irc::message-arguments msg))))

  (on coast.irc::rpl_whoischannels-message
    (:replace (first (coast.irc::message-arguments msg)))
    (:replace (second (coast.irc::message-arguments msg)))
    (:progn
      (with-accessors ((arguments coast.irc::message-arguments)) msg
        (setf (third arguments)
              (format nil "~{~@{~a~^ ~}~}"
                (mapcar (lambda (channel-name)
                          (multiple-value-bind (channel mode)
                              (coast.irc::cut-nickname-prefix
                               irc
                               channel-name)
                            (mkstr (or mode "")
                                   (internal-name (proxy-name-table proxy)
                                                  (network-name proxy irc)
                                                  channel))))
                        (cl-ppcre:split " " (third arguments))))))))

  (on coast.irc::rpl_namreply-message
    (:replace (first (coast.irc::message-arguments msg)))
    (:replace (third (coast.irc::message-arguments msg)))
    (:progn
      (with-accessors ((arguments coast.irc::message-arguments)) msg
        (setf (fourth arguments)
              (format nil "~{~@{~a~^ ~}~}"
                (mapcar (lambda (nickname)
                          (multiple-value-bind (nick mode)
                              (coast.irc::cut-nickname-prefix
                               irc
                               nickname)
                            (mkstr (or mode "")
                                   (internal-name (proxy-name-table proxy)
                                                  (network-name proxy irc)
                                                  nick))))
                        (cl-ppcre:split " " (fourth arguments)))))))))

(define-externalization construct-external-message
    ((proxy proxy) (irc coast.irc::irc) (msg *) (agent irc-client-agent))
  (on coast.irc::message
    (:replace (car (coast.irc::message-arguments msg))))

  (on coast.irc::join-message
    (:progn
      (with-accessors ((arguments coast.irc::message-arguments)) msg
        (let ((internal-channel-name (car arguments)))
          (dolist (network-name (network-name-list proxy))
            (multiple-value-bind (external-channel-name win)
                (remove-network-name network-name internal-channel-name)
              (when win
                (setf (car arguments) external-channel-name)
                (return))))))))

  (on coast.irc::mode-message
    (:replace (car (coast.irc::message-arguments msg)))
    (:replace (cddr (coast.irc::message-arguments msg)) :listp t))

  (on coast.irc::pong-message
    (:progn
      (setf (coast.irc::message-arguments msg)
            (let ((host (connection-host (agent-connection agent))))
              (list host host)))))

  (on coast.irc::whois-message
    (:replace (coast.irc::message-arguments msg) :listp t)))

;(define-reconstructor reconstruct :remote-client->remote-client
;    (proxy msg (client1 remote-client) (client2 remote-client))
;  (on coast.irc::message
;    (:replace :prefixes)))
