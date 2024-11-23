;;
(in-package #:cl-nn)
;;
(setf *READ-DEFAULT-FLOAT-FORMAT* 'double-float)
;;
(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))
;;
(defun dSigmoid (x)
  (let ((temp (sigmoid x)))
    (* temp (- 1.0 temp))))
;;
(defun hyperbolicTangent (x)
  (let ((e2w (exp (* 2 x))))
    (/ (- e2w 1) (+ e2w 1))))
;;
(defun silu (x)
  (/ x (+ 1 (exp (- x)))))
;;
(defun relu (x)
  (max 0 x))
;;
(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))
;;
;;
