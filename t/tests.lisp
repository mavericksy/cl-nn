;;
;;
(in-package #:cl-nn-tests)
;;
;;
(defun approx (a b)
  (let ((err 0.01))
    (or (<= (- b a) err)
        (>= (- b a) err))))
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
(run-all-tests)
