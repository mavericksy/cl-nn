;;
(in-package #:cl-nn)
;;
;;
(defun hopfield-init (training-data)
  ""
  (let* ((num-inputs (length (car training-data)))
         (num-training-examples (length training-data))
         (training-list (make-array (list num-training-examples num-inputs)))
         (inputCells (make-array num-inputs))
         (tempStorage (make-array num-inputs))
         (fieldWeights (make-array (list num-inputs num-inputs)))
         (temp nil))
    ;;
    ;; Copy training data
    ;;
    (dotimes (j num-training-examples)
      (dotimes (i num-inputs)
        (setf (aref training-list j i)
              (nth i (nth j training-data)))))
    ;;
    (defun adjustInput (val)
      (if (< val 0.1)
          -1.0
          +1.0))
    ;;
    ;;
    (dotimes (i num-inputs)
      (dotimes (n num-training-examples)
        (setf (aref training-list n i) (adjustinput (aref training-list n i)))))
    ;;
    (dotimes (i num-inputs)
      (dotimes (j num-inputs)
        (setf (aref fieldweights i j) 0)))
    ;;
    ;;
    (dotimes (j-1 (- num-inputs 1))
      (let ((j (+ j-1 1)))
        (dotimes (i j)
          (dotimes (s num-training-examples)
            (setf temp (truncate (+ (* (adjustinput (aref training-list s i))
                                       (adjustinput (aref training-list s j)))
                                    (aref fieldweights i j))))
            (setf (aref fieldweights i j) temp)
            (setf (aref fieldweights j i) temp)))))
    ;;
    (dotimes (i num-inputs)
      (setf (aref tempstorage i) 0)
      (dotimes (j i)
        (setf (aref tempstorage i) (+ (aref tempstorage i) (aref fieldweights i j)))))
    ;;
    (list num-inputs num-training-examples training-list inputcells tempstorage fieldweights)
    ;;
    ))
;;
;;
;;
(defun hopfieldnetrecall (hopfieldNetwork numOfIterations)
  ""
  (let ((num-inputs (first hopfieldNetwork))
        (num-training-examples (second hopfieldnetwork))
        (training-list (third hopfieldnetwork))
        (inputcells (fourth hopfieldnetwork))
        (tempstorage (fifth hopfieldnetwork))
        (hopfieldweights (sixth hopfieldnetwork)))
    ;;
    ;; Calculate change in energy from old input values and the autocorrelation weight matrix
    ;;
    (defun deltaEnergy (k y)
      (let ((temp 0.0))
        (dotimes (j num-inputs)
          (setf temp (+ temp (* (aref hopfieldweights k j)
                                (aref y j)))))
        (- (* 2.0 temp)
           (aref tempstorage k))))
    ;;
    ;;
    (dotimes (ii numofiterations)
      (dotimes (i num-inputs)
        (setf (aref inputcells i)
              (if (> (deltaEnergy i inputcells) 0)
                  1
                  0))))))
;;
;;
(defparameter tData '((1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 0 0)
                      (0 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 1 0)
                      (0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 1 1 0 1 1)))
;;
;;
(defparameter hopfield (hopfield-init tData))
(print hopfield)
;;
(print (hopfieldnetrecall hopfield 5))
;;
