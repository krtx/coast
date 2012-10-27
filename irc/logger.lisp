(in-package :coast.irc)

(cl-interpol:enable-interpol-syntax)

(defclass logger ()
  ((header :initform "%H:%M:%S"
           :initarg :header
           :accessor logger-header)))

(defclass channel-logger (logger)
  ((filename :initform "%Y.%m.%d.txt"
             :initarg :filename
             :accessor channel-logger-filename)
   (file-mode :initform 700
              :initarg :mode
              :accessor channel-logger-file-mode)
   (base-directory :initform "./"
                   :initarg :base-directory
                   :accessor channel-logger-base-directory)
   (directory-mode :initform 700
                   :initarg :directory-mode
                   :accessor channel-logger-directory-mode)
   (channels :initform (list '(:channel ".*")) ; (dirname chname-regex)
             :initarg :channels
             :accessor channel-logger-channels)
   (external-format :initform :utf-8
                    :initarg :external-format
                    :accessor channel-logger-external-format)))


;;; utility
(defun make-directory (path mode)
  (sb-ext:run-program "/bin/mkdir"
    (list "-p" (namestring path) "-m" (mkstr mode))))

(defun ensure-directory-exists (dir mode)
  (unless (directory-exists-p dir)
    (make-directory dir mode)))

(defun insert-time (string &optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
                       (decode-universal-time universal-time)
    (let ((time (list (format nil "~4,'0d" year)
                      (format nil "~2,'0d" month)
                      (format nil "~2,'0d" date)
                      (format nil "~2,'0d" hour)
                      (format nil "~2,'0d" minute)
                      (format nil "~2,'0d" second))))
      (cl-ppcre:regex-replace-all 
       "(%Y)|(%m)|(%d)|(%H)|(%M)|(%S)"
       string
       (lambda (match &rest regs)
         (declare (ignore match))
         (elt time (position-if-not #'null regs)))
       :simple-calls t))))

(defun make-log-header (logger msg)
  (insert-time (logger-header logger) (message-received-time msg)))

(defun make-log-body (irc msg when)
  (with-slots (source user host command arguments) msg
    (case command
      (:quit
       (when (eql when :read)
         (destructuring-bind (quit-msg) arguments
           #?"! ${source}(${quit-msg})")))
      (:part
       (when (eql when :read)
         (destructuring-bind (channel &optional (part-msg "")) arguments
           (declare (ignore channel))
           #?"! ${source}(${part-msg})")))
      (:topic
       (when (eql when :read)
         (destructuring-bind (channel topic) arguments
           #?"TOPIC of channel ${channel} by ${source}: ${topic}")))
      (:nick
       (when (eql when :read)
         (let ((old-nick (or source ""))
               (new-nick (car arguments)))
           #?"* ${old-nick} -> ${new-nick}")))
      (:mode
       (when (eql when :read)
         (destructuring-bind (channel modes &rest nicks) arguments
           #?"Mode by ${source}: ${channel} ${modes} @{nicks}")))
      (:join
       (when (eql when :read)
         (let ((channel (car arguments)))
           #?"+ ${source}(${source}!${user}@${host}) to ${channel}")))
      (:privmsg
       (destructuring-bind (channel privmsg) arguments
         (case when
           (:send
            ;ircがprivmsgを送ったとき
            (let ((nick (nickname (irc-user irc))))
              #?">${channel}:${nick}< ${privmsg}"))
           (:read
            ;誰からのprimsgがきた
            (if (string= source (nickname (irc-user irc)))
                #?">${channel}:${source}< ${privmsg}" ;自分のprivmsgだった
                #?"<${channel}:${source}> ${privmsg}")))))
      (:notice
       (destructuring-bind (channel notice) arguments
         (case when
           (:send
            ;ircがnoticeを送ったとき
            (let ((nick (nickname (irc-user irc))))
              #?"({channel}:${nick}) ${notice}"))
           (:read
            ;誰からのnoticeがきた
            (if (string= source (nickname (irc-user irc)))
                #?"(${channel}:${source}) ${notice}" ;自分のniticeだった
                #?"(${channel}:${source}) ${notice}"))))))))


(defun make-log-directory-pathname (base-dir channel-name)
  ;危なそうな文字はエスケープ。
  (let* ((regex "([^-\\w@#%!+&.\\x80-\\xff])")
         (dirname (cl-ppcre:regex-replace-all
                   regex
                   channel-name
                   (lambda (match reg)
                     (declare (ignore match))
                     (format nil "=~2,'0x" (char-code (char reg 0))))
                   :simple-calls t))
         (dir (make-pathname :directory (list :relative dirname))))
    (merge-pathnames dir base-dir)))

(defun log-channel-p (channel-logger channel-name)
  "channel-nameはchannel-loggerでログをとることになっているか？
なっているならログを保存するディレクトリを返す"
  (with-slots (base-directory channels) channel-logger
    (loop for (dirname ch-regex) in channels
       when (cl-ppcre:scan ch-regex channel-name)
         return (if (eql dirname :channel)
                    (make-log-directory-pathname base-directory channel-name)
                    (make-log-directory-pathname base-directory dirname)))))

(defun make-channel-log-file-pathname (channel-logger channel-name time)
  "ログを保存するファイルのpathnameをつくる"
  (let ((filename  (insert-time (channel-logger-filename channel-logger) time))
        (directory (log-channel-p channel-logger channel-name))
        (directory-mode (channel-logger-directory-mode channel-logger)))
    (when directory
      (ensure-directory-exists directory directory-mode)
      (merge-pathnames directory (pathname-as-file filename)))))

(defun channel-log (irc channel-logger msg channel-name when)
  (when-let ((body (make-log-body irc msg when))
             (header (make-log-header channel-logger msg)))
    (with-slots (received-time) msg
      (with-slots (external-format) channel-logger
        (when-let ((target (make-channel-log-file-pathname
                            channel-logger
                            channel-name
                            received-time)))
          (with-open-file (out target
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create
                               :external-format external-format)
            (format out "~A ~A~%" header body)
            target))))))
