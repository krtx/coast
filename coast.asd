(defpackage :coast-system (:use :cl :asdf))
(in-package :coast-system)

(defsystem :coast.irc
  :depends-on (:flexi-streams  :cl-interpol
               :split-sequence :usocket
               :cl-fad         :metabang-bind
               :cl-ppcre       :anaphora
               :alexandria     :acl-compat)
  :serial t
  :components ((:file "utility")
               (:module :irc
                :serial t
                :components ((:file "packages")
                             (:file "variable")
                             (:file "parse")
                             (:file "messages")
                             (:file "irc")
                             (:file "logger")))))


(defsystem :coast
  :depends-on (:coast.irc)
  :serial t
  :components ((:module :proxy
                :serial t
                :components ((:file "packages")
                             (:file "connection")
                             (:file "proxy")
                             (:file "proxy-irc")))))
