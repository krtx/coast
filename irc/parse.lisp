(in-package :coast.irc)


(defun split-string (delims string &key (remove-empty-subseqs t) count)
  (split-sequence:split-sequence-if (lambda (x) (member x delims)) string
   :remove-empty-subseqs remove-empty-subseqs :count count))

(defun trail (tokens)
  (let ((trail (format nil "~{~A~^ ~}" tokens)))
    (if (and (string/= trail "")
             (char= (char trail 0) #\:))
        (subseq trail 1) trail)))


(defun tokenize (str)
  (let ((line (car (split-string '(#\Return #\Linefeed) str :count 1))))
    (let ((tokens (split-string '(#\Space) line :remove-empty-subseqs nil)))
      (loop for i from 0
            for token in tokens
        when (string/= token "")
          if (and (< 0 i)
                  (char= (char token 0) #\:))
            return (append1 acc (trail (subseq tokens i)))
          else
            collect token into acc
        finally (return acc)))))


;;; トークンのリストを必要なだけ読んで、
;;; パース結果と次の残りのトークンのリストを返す

(defun parse-prefix (tokens)
  (let ((token (car tokens)))
    (if (char= (char token 0) #\:)  ;; prefixか？
        (values (list :prefix
                      (mapcar #'list
                              '(:source :user :host)
                              (split-string '(#\! #\@ #\:) token)))
                (cdr tokens))
        (values nil tokens))))

(defun parse-command (tokens)
  (aif (car tokens)
       (values (list :command it) (cdr tokens))
       (values nil tokens)))

(defun parse-params (tokens)
  (if (null tokens)
      (values nil nil)
      (values (list :params tokens) nil)))

(defun parse-and-compose (raw)
  (labels ((rec (tokens funcs &optional acc)
             (if (or (null tokens) (null funcs))
                 (nreverse (delete nil acc))
                 (multiple-value-bind (elem rest)
                     (funcall (car funcs) tokens)
                   (rec rest (cdr funcs) (cons elem acc))))))
    (rec (tokenize raw)
         '(parse-prefix parse-command parse-params))))


(defun tree-ref (stree &rest keys)
  (if (null keys)
      stree
      (apply #'tree-ref
             (cadr (assoc (car keys) stree))
             (cdr keys))))

(defun ctcp-command-p (str command)
  (let ((len (min (1- (length str))
                  (length (symbol-name command)))))
    (when (and (char= *ctcpf* (elt str 0))
               (string= str (symbol-name command)
                        :start1 1 :end1 (1+ len)
                        :start2 0 :end2 len))
      command)))


(defun numeric-reply-p (string)
  (every #'digit-char-p string))

(defun ctcp-message-command (string)
  (when (and (stringp string)
             (plusp (length string))
             (eql (char string 0) *ctcpf*))
    (case (char string 1)
      (#\A (ctcp-command-p string :action))
      (#\C (ctcp-command-p string :clientinfo))
      (#\F (ctcp-command-p string :finger))
      (#\P (ctcp-command-p string :ping))
      (#\S (ctcp-command-p string :source))
      (#\T (ctcp-command-p string :time))
      (#\U (ctcp-command-p string :userinfo))
      (#\V (ctcp-command-p string :version))
      (t nil))))

(defun find-reply-name (reply-number &key (reply-names *reply-names*))
  (cadr (assoc reply-number reply-names)))

(defun find-reply-number (reply-name &key (reply-names *reply-names*))
  (car (rassoc reply-name reply-names :key #'car)))

(defun command-as-keyword (string)
  (if (numeric-reply-p string)
      (find-reply-name (parse-integer string))
      (make-keyword (string-upcase string))))

