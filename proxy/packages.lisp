(cl:defpackage :coast
  (:use :cl :cl-fad :coast.utility)
  (:import-from :alexandria
                :if-let :when-let :when-let*
                :appendf :nconcf :flatten :lastcar
                :compose :flatten)
  (:import-from :anaphora :it :aif :aand :awhen)
  (:import-from :metabang-bind :bind)
  (:export :if-let :when-let :when-let*
           :appendf :nconcf :flatten :lastcar
           :compose :flatten :chomp
           :it :aif :aand :awhen
           :bind
           :push-back))
