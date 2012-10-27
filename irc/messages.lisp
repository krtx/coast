(in-package :coast.irc)

(defclass message ()
  ((source :initform nil
           :initarg :source
           :accessor message-source)
   (user :initform nil
         :initarg :user
         :accessor message-user)
   (host :initarg :host
         :initform nil
         :accessor message-host)
   (command :initform nil
            :initarg :command
            :accessor message-command)
   (arguments :initform nil
              :initarg :arguments
              :accessor message-arguments)
   (received-time :initform (get-universal-time)
                  :initarg :received-time
                  :accessor message-received-time)
   (raw-string :initform nil
               :initarg :raw-string
               :accessor message-raw-string)))

(defclass ctcp-mixin ()
  ((ctcp-command
    :initarg :ctcp-command
    :accessor ctcp-message-ctcp-command)))

(defclass rpl-message (message) ())

(defclass ctcp-message (message ctcp-mixin) ())

(defgeneric find-message-class (command))
(defgeneric find-ctcp-message-class (command))

(defgeneric copy-message (message))
(defgeneric make-raw-message (message)
  (:documentation "IRCクライアントに送るための生のメッセージを生成。"))

;; *ctcpf*があればカット
(defmethod initialize-instance :after ((msg ctcp-message) &key)
  (with-slots (arguments) msg
    (when-let ((ctcp-arg (cadr arguments)))
      (when (char= (char ctcp-arg 0) *ctcpf*)
        (setf ctcp-arg (subseq ctcp-arg 1)))
      (let ((len (length ctcp-arg)))
        (when (char= (char ctcp-arg (1- len)) *ctcpf*)
          (setf ctcp-arg (subseq ctcp-arg 0 (1- len)))))
      (setf (cadr arguments) ctcp-arg))))


;; 該当しないものはnil。
(defmethod find-message-class (command))
(defmethod find-ctcp-message-class (command))

(defmethod find-message-class ((command string))
  (find-message-class (command-as-keyword command)))

(defmethod find-ctcp-message-class ((command string))
  (find-ctcp-message-class (as-keyword command)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-message-symbol (name &optional (prefix ""))
    (intern (mkstr prefix (symbol-name name)
                   "-" (symbol-name '#:message))
            :coast.irc)))

(defmacro define-irc-message-class (name)
  (let ((sym (intern-message-symbol name)))
    `(progn
       (export-symbol :coast.irc ,(string sym))
       (defclass ,sym (message) ()
         (:default-initargs :command ,name))
       (defmethod find-message-class ((command (eql ,name)))
         (find-class ',sym)))))

(defmacro define-rpl-message-class (name)
  (let ((sym (intern-message-symbol name)))
    `(progn
       (export-symbol :coast.irc ,(string sym))
       (defclass ,sym (rpl-message) ()
         (:default-initargs :command ,name))
       (defmethod find-message-class ((command (eql ,name)))
         (find-class ',sym)))))

(defmacro define-ctcp-message-class (name)
  (let ((sym (intern-message-symbol name "CTCP-")))
    `(progn
       (export-symbol :coast.irc ,(string sym))
       (defclass ,sym (ctcp-message) ()
         (:default-initargs :command ,name))
       (defmethod find-ctcp-message-class ((command (eql ,name)))
         (find-class ',sym)))))


(defmacro define-irc-message-classes (&rest names)
  `(progn ,@(mapcar (lambda (x) `(define-irc-message-class ,x))
                    names)))

(defmacro define-rpl-message-classes (&rest names)
  `(progn ,@(mapcar (lambda (x) `(define-rpl-message-class ,x))
                    names)))

(defmacro define-ctcp-message-classes (&rest names)
  `(progn ,@(mapcar (lambda (x) `(define-ctcp-message-class ,x))
                    names)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-irc-message-classes
      :privmsg :notice :kick :topic :error :mode :ping :nick
      :join :part :quit :kill :pong :invite :user :who :whois)
  #.`(define-rpl-message-classes ,@(mapcar #'cadr *reply-names*))
  (define-ctcp-message-classes
      :action :clientinfo :finger :ping
      :source :time :userinfo :version))

(defmethod copy-message ((msg message))
  (make-instance (type-of msg)
                 :source    (copy-seq (message-source msg))
                 :user      (copy-seq (message-user   msg))
                 :host      (copy-seq (message-host   msg))
                 :command   (message-command msg)
                 :arguments (mapcar #'copy-seq (message-arguments msg))
                 :received-time (message-received-time msg)))

(defmethod copy-message ((msg ctcp-message))
  (let ((copy (call-next-method)))
    (setf (ctcp-message-ctcp-command copy)
          (ctcp-message-ctcp-command msg))
    copy))

(defun raw-message (source user host command arguments)
  (let ((*print-circle* nil))
    (mkstr (if source (format nil ":~A" source) "")
           (if user (format nil "!~A" user)   "")
           (if host (format nil "@~A" host)   "")
           (if (or source user host) " " "")
           (format nil "~A~{ ~A~} " command (butlast arguments))
           (format nil ":~A" (lastcar arguments)))))

(defun ctcp-argumentation (ctcp-cmd arguments)
  (destructuring-bind (nick &rest rest) arguments
    (let ((arg (mkstr (format nil "~A" *ctcpf*)
                      (format nil "~A~{ ~A~}" ctcp-cmd rest)
                      (format nil "~A" *ctcpf*))))
      (list nick arg))))

(defmethod make-raw-message ((msg message))
  (with-slots (source user host command arguments) msg
    (raw-message source user host command arguments)))

(defmethod make-raw-message ((msg rpl-message))
  (with-slots (source user host command arguments) msg
    (let ((num (find-reply-number command)))
      (raw-message source user host num arguments))))

(defmethod make-raw-message ((msg ctcp-message))
  (with-slots (source user host command ctcp-command arguments) msg
    (let ((args (ctcp-argumentation ctcp-command arguments)))
      (raw-message source user host command args))))

(defun make-message (source user host cmd args &key ctcp)
  "プログラム内で扱うためのmessageクラスを各パラメーターから構成。"
  (let ((class (or (find-ctcp-message-class ctcp)
                   (find-message-class cmd)
                   'message)))
    (let ((msg (make-instance class
                :source source :user user :host host
                :command cmd :arguments args)))
      (when ctcp (setf (ctcp-message-ctcp-command msg) ctcp))
      msg)))

(defun construct-message (raw)
  (let ((stree (parse-and-compose raw)))
    (let* ((src  (tree-ref stree :prefix :source))
           (user (tree-ref stree :prefix :user))
           (host (tree-ref stree :prefix :host))
           (cmd  (tree-ref stree :command))
           (args (tree-ref stree :params))
           (ctcp (ctcp-message-command (lastcar args))))
      (let ((msg (make-message src user host cmd args :ctcp ctcp)))
        (with-slots (user command raw-string) msg
          (setf user (remove-prefix user #\~))
          (setf command (or (command-as-keyword cmd) cmd))
          (setf raw-string raw))
        msg))))
