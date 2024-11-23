(in-package #:cl-user)
(defpackage #:cl-nn
  (:use :cl)
  (:export
   :sigmoid
   :dSigmoid
   :newdeltanetwork
   :deltalearn
   :deltarecall))
