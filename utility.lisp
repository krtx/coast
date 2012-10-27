(cl:defpackage :coast.utility
  (:use :cl :cl-fad)
  (:import-from :alexandria
                :if-let :when-let :when-let*
                :appendf :nconcf :flatten :lastcar
                :compose :flatten :make-keyword)
  (:import-from :anaphora :it :aif :aand :awhen)
  (:import-from :metabang-bind :bind)
  (:export :if-let :when-let :when-let*
           :appendf :nconcf :flatten :lastcar
           :compose :flatten
           :it :aif :aand :awhen
           :bind
           :push-back))
(cl:in-package :coast.utility)

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (list obj)
  (append list (list obj)))

(defun as-keyword (str)
  (intern str :keyword))

(define-modify-macro push-back (&rest args)
  (lambda (old-value &rest args)
    (nconc old-value args)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym))) syms)
     ,@body))


(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan (lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun chomp (string)
  (let ((len (length string)))
    (if (char= (char string (1- len)) #\Newline)
        (subseq string 0 (1- len)) string)))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))


(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))


(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))


(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
      `(let ,(mapcar #'list (list ,@gs) (list ,@os))
         ,(progn ,@body)))))


(defun remove-prefix (string char)
  (if (and (stringp string)
           (< 0 (length string))
           (char= (elt string 0) char))
      (subseq string 1)
      string))

(defmacro export-symbol (package &rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export (intern (mkstr ,@args) ,package) ,package)))

(do-all-symbols (sym)
  (ignore-errors (export sym)))