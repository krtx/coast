(in-package :coast.irc)

; IRC status
(defclass irc ()
  ((user :initform nil
         :accessor irc-user)
   (channels :initform nil
             :accessor irc-channels)
   (visible-users :initform nil
                  :accessor irc-visible-users)
   (capabilities :initform *default-isupport-values*
                 :accessor irc-capabilities)))

(defclass user-mixin ()
  ((nickname :initform nil
             :initarg :nickname
             :accessor nickname)
   (username :initform nil
             :initarg :username
             :accessor username)
   (hostname :initform nil
             :initarg :hostname
             :accessor hostname)
   (realname :initform nil
             :initarg :realname
             :accessor realname)))

; quitのログ取るためにchannelsスロットは必須。
(defclass user (user-mixin)
  ((modes :initarg :modes
          :initform (make-hash-table :test #'equal)
          :accessor user-modes)
   (channels :initarg :channels
             :initform nil
             :accessor user-channels)))

(defclass channel ()
  ((name :initarg :name
         :accessor channel-name)
   (topic :initarg :topic
          :accessor channel-topic
          :initform nil)
   (modes :initarg :modes
          :accessor channel-modes
          :initform nil)
   (visibility :initarg :visibility
               :accessor channel-visibility
               :initform nil)))

(defmethod initialize-instance :after ((irc irc) &key nickname
                                                      username
                                                      realname)
  (when (and nickname username realname)
    (let ((user (make-instance 'user
                               :nickname nickname
                               :username username
                               :realname realname)))
      (setf (irc-user irc) user)
      (push user (irc-visible-users irc)))))

(defmethod print-object ((object channel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (channel-name object))))

(defmethod print-object ((object user) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (nickname object))))


;;; server-capabilitiesを扱うもの。
(defun channel-name-p (irc string)
  (let ((caps (irc-capabilities irc)))
    (let ((chantypes (cadr (assoc "CHANTYPES" caps :test #'equal))))
      (and (position (char string 0) chantypes) t))))

(defun cut-nickname-prefix (irc nickname)
  (let ((caps (irc-capabilities irc)))
    (let ((prefix (cadr (assoc "PREFIX" caps :test #'equal))))
      (cl-ppcre:register-groups-bind (values prefixes)
                                     ("\\((.*)\\)(.*)" prefix)
        (dotimes (i (length prefixes))
          (when (char= (char prefixes i) (char nickname 0))
            (return-from cut-nickname-prefix
              (values (subseq nickname 1)
                      (subseq prefixes i (1+ i))
                      (subseq values i (1+ i))))))
        nickname))))

(defun has-nickname-prefix-p (irc string)
  (let ((caps (irc-capabilities irc)))
    (let ((prefix (cadr (assoc "PREFIX" caps :test #'equal))))
      (cl-ppcre:register-groups-bind (values prefixes)
                                     ("\\((.*)\\)(.*)" prefix)
        (declare (ignore values))
        (and (position (char string 0) prefixes) t)))))


(defgeneric get-channel (irc channel))
(defgeneric add-channel (irc channel))
(defgeneric delete-channel (irc channel))
(defgeneric channel-users (irc channel))

(defgeneric get-user (irc nick))
(defgeneric add-user (irc nick))
(defgeneric delete-user (place nick))
(defgeneric add-user-mode (user channel mode))
(defgeneric get-user-modes (user channel))
(defgeneric delete-user-mode (user channel mode))
(defgeneric delete-user-modes (user channel))

(defmethod get-channel ((irc irc) (name string))
  (find-if (lambda (channel)
             (equal (channel-name channel) name))
           (irc-channels irc)))

(defmethod get-channel ((irc irc) (channel channel))
  (find channel (irc-channels irc)))

(defmethod get-channel ((user user) (name string))
  (find-if (lambda (channel)
             (equal (channel-name channel) name))
           (user-channels user)))

(defmethod get-channel ((user user) (channel channel))
  (find channel (user-channels user)))


(defmethod add-channel ((irc irc) (channel channel))
  (pushnew channel (irc-channels irc))
  channel)

(defmethod add-channel ((user user) (channel channel))
  (pushnew channel (user-channels user))
  channel)


(defmethod delete-channel ((user user) (channel channel))
  (pull channel (user-channels user)))

(defmethod delete-channel ((irc irc) (channel channel))
  (pull channel (irc-channels irc))
  (dolist (user (irc-visible-users irc))
    (delete-channel user channel)))


(defun ensure-channel-exists (irc name)
  (or (get-channel irc name)
      (add-channel irc (make-instance 'channel :name name))))


(defmethod channel-users ((irc irc) (channel channel))
  (remove-if-not (lambda (user) (get-channel user channel))
                 (irc-visible-users irc)))

(defmethod channel-users ((irc irc) (channel string))
  (awhen (get-channel irc channel)
    (channel-users irc it)))

(defun add-channel-mode (channel mode)
  (pushnew mode (channel-modes channel) :test #'equal))

(defun delete-channel-mode (channel mode)
  (pull mode (channel-modes channel) :test #'equal))


(defmethod get-user ((irc irc) (nick string))
  (find-if (lambda (user)
             (equal (nickname user) nick))
           (irc-visible-users irc)))

(defmethod get-user ((irc irc) (user user))
  (find user (irc-visible-users irc)))

(defmethod add-user ((irc irc) (user user))
  (pushnew user (irc-visible-users irc))
  user)

(defun ensure-user-exists (irc nick)
  (or (get-user irc nick)
      (add-user irc (make-instance 'user :nickname nick))))

;; userをこの世界から消す。
(defmethod delete-user ((irc irc) (user user))
  (pull user (irc-visible-users irc)))

(defmethod delete-user ((irc irc) (nick string))
  (awhen (get-user irc nick)
    (delete-user irc it)))

(defmethod get-user-modes ((user user) (channel string))
  (gethash channel (user-modes user)))

(defmethod get-user-modes ((user user) (channel channel))
  (get-user-modes user (channel-name channel)))

(defmethod add-user-mode ((user user) (channel string) (mode string))
  (pushnew mode (gethash channel (user-modes user)) :test #'equal))

(defmethod add-user-mode ((user user) (channel channel) (mode string))
  (add-user-mode user (channel-name channel) mode))

(defmethod delete-user-mode ((user user) (channel string) (mode string))
  (pull mode (gethash channel (user-modes user)) :test #'equal))

(defmethod delete-user-mode ((user user) (channel channel) (mode string))
  (delete-user-mode user (channel-name channel) mode))

(defmethod delete-user-modes ((user user) (channel string))
  (remhash channel (user-modes user)))

(defmethod delete-user-modes ((user user) (channel channel))
  (remhash (channel-name channel) (user-modes user)))


(defgeneric update-irc (irc msg)
  (:documentation "msgの内容からircの情報を更新する"))

(defmethod update-irc (irc msg))

(defmethod update-irc ((irc irc) (msg nick-message))
  (let ((old-nick (message-source msg))
        (new-nick (car (message-arguments msg))))
    (awhen (get-user irc old-nick)
      (setf (nickname it) new-nick))))

(defmethod update-irc ((irc irc) (msg topic-message))
  (destructuring-bind (channel topic) (message-arguments msg)
    (awhen (get-channel irc channel)
      (setf (channel-topic it) topic))))


;; #channel +ooo hoge huga var
(defmethod update-irc ((irc irc) (msg mode-message))
  (destructuring-bind (channel-name mode-changes &rest nicks)
      (message-arguments msg)
    (when-let ((channel (get-channel irc channel-name)))
      (destructuring-bind (+/- &rest modes)
          (cl-ppcre:split "" mode-changes)
        (let ((op (if (string= +/- "+")
                      #'add-user-mode
                      #'delete-user-mode)))
          (loop for mode in modes
                for nick in nicks
             do (let ((user (ensure-user-exists irc nick)))
                  (add-channel user channel)
                  (funcall op user channel mode))))))))

(defmethod update-irc ((irc irc) (msg join-message))
  (with-slots (source user host arguments) msg
    (let ((channel-name (car arguments)))
      (let ((channel (ensure-channel-exists irc channel-name)))
        (let ((u (ensure-user-exists irc source)))
          (add-channel u channel)
          (when user
            (setf (username u) user))
          (when host
            (setf (hostname u) host)))))))

(defmethod update-irc ((irc irc) (msg part-message))
  (with-slots (source arguments) msg
    (let ((channel-name (car arguments)))
      (let ((channel (get-channel irc channel-name))
            (user (get-user irc source)))
        (cond ((eq user (irc-user irc))
               (delete-channel irc channel)
               (dolist (user (irc-visible-users irc))
                 (when (null (user-channels user))
                   (unless (eq user (irc-user irc))
                     (delete-user irc user)))))
              (t
               (delete-channel user channel)
               (when (null (user-channels user))
                 (delete-user irc user))))))))

(defmethod update-irc ((irc irc) (msg quit-message))
  (let ((nick (message-source msg)))
    (delete-user irc nick)))


;;; rpl系message
;;332
(defmethod update-irc ((irc irc) (msg rpl_topic-message))
  (let ((name (second (message-arguments msg)))
        (topic (third (message-arguments msg))))
    (let ((channel (ensure-channel-exists irc name)))
      (setf (channel-topic channel) topic))))

;;333は無視。

;;353
(defmethod update-irc ((irc irc) (msg rpl_namreply-message))
  (destructuring-bind (my-nick visibility name nicknames)
                      (message-arguments msg)
    (declare (ignore my-nick))
    (let ((channel (ensure-channel-exists irc name))
          (nicks (cl-ppcre:split " " nicknames)))
      (setf (channel-visibility channel) visibility)
      (dolist (nick nicks)
        (multiple-value-bind (nick prefix value)
                             (cut-nickname-prefix irc nick)
          (declare (ignore prefix))
          (let ((user (ensure-user-exists irc nick)))
            (add-channel user channel)
            (when value
              (add-user-mode user channel value))))))))

;311
(defmethod update-irc ((irc irc) (msg rpl_whoisuser-message))
  (destructuring-bind (my-nick nick user host star real)
                      (message-arguments msg)
    (declare (ignore my-nick star))
    ;ユーザーがチャンネルにいない場合は何もしない。
    (awhen (get-user irc nick)
      (setf (username it) (remove-prefix user #\~))
      (setf (realname it) real)
      (setf (hostname it) host))))

;; 319
;; :host 319 my-nick nick :#channel1 #channel2
(defmethod update-irc ((irc irc) (msg rpl_whoischannels-message))
  (destructuring-bind (my-nick nick names) (message-arguments msg)
    (declare (ignore my-nick))
    (let ((channel-name-list (cl-ppcre:split " " names)))
      (dolist (channel-name channel-name-list)
        (multiple-value-bind (chname prefix value)
                             (cut-nickname-prefix irc channel-name)
          (declare (ignore prefix))
          (when-let ((channel (and value (get-channel irc chname))))
            (let ((user (ensure-user-exists irc nick)))
              (add-channel user channel)
              (add-user-mode user channel value))))))))

;322
(defmethod update-irc ((irc irc) (msg rpl_list-message))
  (let ((name (second (message-arguments msg)))
        (topic (fourth (message-arguments msg))))
    (let ((channel (ensure-channel-exists irc name)))
      (when (stringp topic)
        (setf (channel-topic channel) topic)))))

;324
;; :host 324 nick #channel +sn
(defmethod update-irc ((irc irc) (msg rpl_channelmodeis-message))
  (let ((name (second (message-arguments msg)))
        (mode-changes (third (message-arguments msg))))
    (destructuring-bind (+/- &rest modes)
                        (cl-ppcre:split "" mode-changes)
      (let ((channel (ensure-channel-exists irc name))
            (op (if (string= +/- "+")
                    #'add-channel-mode
                    #'delete-channel-mode)))
        (dolist (mode modes)
          (funcall op channel mode))))))

(defun generate-initial-messages (irc &optional channels password (num 8))
  "IRCサーバに接続したときに流すメッセージを返す"
  (with-accessors ((nick nickname)
                   (user username)
                   (real realname)) (irc-user irc)
    (delete nil
            (list* (when password
                     (make-instance 'message
                                    :command :pass
                                    :arguments (list password)))
                   (make-instance 'nick-message
                                  :arguments (list nick))
                   (make-instance 'user-message
                                  :arguments (list user
                                                   (mkstr num)
                                                   "*"
                                                   real))
                   (mapcar (lambda (channel)
                             (make-instance 'join-message
                                            :arguments (list channel)))
                           channels)))))
