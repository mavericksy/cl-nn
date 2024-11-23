;;
;;
(in-package #:cl-nn-tests)
;;
;;
(defun approx (a b &optional (sway 0.01))
  (let ((err sway))
    (and (<= (- a b) err)
         (<= (- b a) err))))
;;
(defun approxl (l ll)
  (every #'(lambda (z) (equal z 't))
         (mapcar #'(lambda (a b) (approx a b))
                 l ll)))
;;
;;
;;
(def-suite text-approx)
(in-suite text-approx)
;;
(test test-approx
  ""
  (is-true (approx 1 1))
  (is-false (approx 1 0))
  ;;
  (is-true (approxl '(1) '(1)))
  (is-true (approxl '(0.1) '(0.1)))
  ;;
  (is-true (approxl '(1 2) '(1 2)))
  (is-true (approxl '(2 1) '(2 1)))
  ;;
  (is-false (approxl '(4 2) '(1 3)))
  (is-false (approxl '(1 2) '(1 3)))
  ;;
  (is-true (approxl '(1 2) '(1 2 3)))
  )
;;
;; (run! 'test-approx)
;;
;;
(def-suite activation-functions)
;;
(in-suite activation-functions)
;;
(test sigmoid
  "Test sigmoid activation function"
  ;;
  (is (approx 0 (sigmoid -5))
      "sig(-5) ~ 0")
  (is (approx 0.5 (sigmoid 0))
      "sig(0) ~ 0.5")
  (is (approx 1 (sigmoid 5))
      "sig(5) ~ 1"))
;;
(test dSigmoid
  "Test the derivative of the sigmoid"
  ;;
  (is (approx 0 (dsigmoid -5))
      "dsig(-5) ~ 0")
  (is (approx 0.25 (dsigmoid 0))
      "dsig(0) = 0.25")
  (is (approx 0 (dsigmoid 5))
      "dsig(5) ~ 0"))
;;
;;
(def-suite delta-rule)
;;
;;
(in-suite delta-rule)
;;
;;
(test test-delta-one
  ""
  (let* ((temp (newdeltanetwork '(2 3 2))))
    (dotimes (i 1000000)
      (setf temp
            (deltalearn temp '(((1 0) (0 1))
                               ((0 1) (1 0))
                               ((0 0) (1 1))
                               ((1 1) (0 0))))))
    ;;
    (is (approxl '(0 1) (deltarecall temp '(1 0))))
    (is (approxl '(1 0) (deltarecall temp '(0 1))))
    (is (approxl '(1 1) (deltarecall temp '(0 0))))
    (is (approxl '(0 0) (deltarecall temp '(1 1))))))
;;
;; (run! 'delta-rule)
;;
;;
(test test-delta-two
  ""
  (let ((temp (newdeltanetwork '(4 5 5 4))))
    (dotimes (i 500000)
      (setf temp (deltalearn temp '(((1 0 0 0) (0 1 0 0))
                                    ((0 1 0 0) (0 0 1 0))
                                    ((0 0 1 0) (0 0 0 1))
                                    ((0 0 0 1) (1 0 0 0))))))
    ;;
    (is (approxl '(0 1 0 0) (deltarecall temp '(1 0 0 0))))
    (is (approxl '(0 0 1 0) (deltarecall temp '(0 1 0 0))))
    (is (approxl '(0 0 0 1) (deltarecall temp '(0 0 1 0))))
    (is (approxl '(1 0 0 0) (deltarecall temp '(0 0 0 1))))))
;;
;; (run! 'test-delta-two)
;;
;;
(run-all-tests)
