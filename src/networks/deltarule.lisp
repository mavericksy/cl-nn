(in-package #:cl-nn)
;;
;; Delta rule network
;;
(defparameter *default-eida-list* '(0.5 0.4 0.3 0.2 0.08 0.07))
(defparameter *delta-default-input-noise-value* 0.08)
(defparameter *delta-rule-debug-flag* nil)
(setf *random-state* (make-random-state t))
;;
;;
(defun NewDeltaNetwork (sizeList &optional (alpha 0.2) (beta 0.8))
  "
    Create a NN according to the sizeList.
    ie '(120 5 7) will create a three layer NN
    with 120 inputs, 5 hidden and 7 output layers

    alpha: coefficient for new weight change
    beta: coefficient for adding in last weight change
"
  (let* ((numLayers (length sizeList))
         (weights-list nil)
         (delta-weight-list nil)
         (old-delta-weight-list nil)    ; momentum terms
         (activation-list (mapcar
                           #'(lambda (s) (make-array s
                                                     :element-type 'float))
                           sizeList))
         (sum-of-products-list (mapcar
                                #'(lambda (s) (make-array s
                                                          :element-type 'float))
                                (cdr sizeList)))
         (back-delta-list (mapcar
                           #'(lambda (s) (make-array s
                                                     :element-type 'float))
                           (cdr sizeList)))
         (eida-list *default-eida-list*))
    ;;
    ;;
    ;; create arrays comisant to connections between slabs/layers
    ;;
    (dotimes (i (- numLayers 1))
      (setf weights-list
            (cons (list (nth i sizeList) (nth (+ i 1) sizeList))
                  weights-list))
      (setf delta-weight-list
            (cons (list (nth i sizeList) (nth (+ i 1) sizeList))
                  delta-weight-list))
      (setf old-delta-weight-list
            (cons (list (nth i sizeList) (nth (+ i 1) sizeList))
                  old-delta-weight-list)))
    (setf weights-list
          (mapcar #'(lambda (s) (make-array s :element-type 'float))
                  (reverse weights-list)))
    (setf delta-weight-list
          (mapcar #'(lambda (s) (make-array s :element-type 'float))
                  (reverse delta-weight-list)))
    (setf old-delta-weight-list
          (mapcar #'(lambda (s) (make-array s :element-type 'float
                                              :initial-element 0.0))
                  (reverse old-delta-weight-list)))
    ;;
    ;; Preset arrays to random activation and weights
    ;;
    (mapc #'(lambda (x)
              (let ((num (array-dimension x 0)))
                (dotimes (n num)
                  (setf (aref x n) (random 0.1)))))
          activation-list)
    ;;
    (mapc #'(lambda (x)
              (let ((numi (array-dimension x 0))
                    (numj (array-dimension x 1)))
                (dotimes (j numj)
                  (dotimes (i numi)
                    ;; TODO investigate whether needing to change weights for other transfer functions
                    (setf (aref x i j) (- 0.5 (random 1.0))))))) ; random weight between -0.5 and 0.5
          weights-list)
    ;;
    ;;
    (list numlayers sizelist activation-list sum-of-products-list
          weights-list delta-weight-list back-delta-list old-delta-weight-list
          alpha beta)))
;;
(defun DeltaLearn (netList trainList &optional (tfunction #'sigmoid) (autoPlot 'no) (input-format nil))
  ""
  (let* ((nLayers               (first netList))
         (sizeList              (second netList))
         (activationList        (third netList))
         (sum-of-products-list  (fourth netList))
         (weights-list          (fifth netList))
         (delta-weights-list    (sixth netList))
         (back-delta-list       (seventh netList))
         (old-delta-weight-list (eighth netList))
         (alpha                 (ninth netList))
         (beta                  (tenth netList))
         ;;
         (inputs nil)
         (targetOutputs nil)
         (iDimension nil)
         (jDimension nil)
         (iActivationVector nil)
         (jActivationVector nil)
         (n nil)
         (weightArr nil)
         (sum-of-products-arr nil)
         (iDeltaVector nil)
         (jDeltaVector nil)
         (deltaWeightArr nil)
         (oldDeltaWeightArr nil)
         (sum nil)
         (iSumOfProductsArr nil)
         (err nil)
         (outputErr 0.0)
         (delta nil)
         (eida nil)
         (inputNoise 0.0))
    ;;
    ;; Zero out the deltas
    ;;
    (dotimes (n (- nLayers 1))
      (let* ((dw (nth n back-delta-list))
             (len1 (array-dimension dw 0)))
        (dotimes (i len1)
          (setf (aref dw i) 0.0))))
    ;;
    ;; Zero out the delta-weights
    ;;
    (dotimes (n (- nLayers 1))
      (let* ((dw (nth n delta-weights-list))
             (len1 (array-dimension dw 0))
             (len2 (array-dimension dw 1)))
        (dotimes (i len1)
          (dotimes (j len2)
            (setf (aref dw i j) 0.0)))))
    ;;
    ;;
    (setf inputNoise *delta-default-input-noise-value*)
    ;;
    (dolist (tl trainList)
      ;;
      (setf inputs (car tl))
      (setf targetOutputs (cadr tl))
      ;;
      (when *delta-rule-debug-flag*
        (print (list "Current targets: " targetoutputs)))
      ;;
      (setf idimension (car sizelist)
            iactivationvector (car activationList))
      ;;
      ;; Copy training inputs to input slab
      ;;
      (dotimes (i idimension)
        (setf (aref iactivationvector i)
              (+ (nth i inputs) (frandom (- inputnoise) inputnoise))))
      ;;
      ;;Propogate activation through all the slabs
      ;;
      (dotimes (n-1 (- nlayers 1))
        (setf n (+ n-1 1))
        (setf jdimension (nth n sizeList))
        (setf jactivationvector (nth n activationlist))
        (setf weightarr (nth n-1 weights-list))
        (setf sum-of-products-arr (nth n-1 sum-of-products-list))
        (dotimes (j jdimension)         ; process each neuron
          (setf sum 0.0)
          (dotimes (i idimension) ; get activation from each neuron in previous slab
            (setf sum (+ sum (* (aref weightarr i j)
                                (aref iactivationvector i)))))
          (setf (aref sum-of-products-arr j) sum)
          (setf (aref jactivationvector j) (funcall tfunction sum)))
        (setf idimension jdimension)
        (setf iactivationvector jactivationvector))
      ;;
      ;; Activation spread through the network and sum of products calculated.
      ;; Modify the weights using back-propogation
      ;; Start by calculating the error signal for each neuron in the output layer
      ;;
      (setf jdimension (nth (- nlayers 1) sizelist))
      (setf jactivationvector (nth (- nlayers 1) activationlist))
      (setf jdeltavector (nth (- nlayers 2) back-delta-list))
      (setf sum-of-products-arr (nth (- nlayers 2) sum-of-products-list))
      (setf outputerr 0)
      ;;
      (dotimes (j jdimension)
        (setf delta (- (nth j targetoutputs) (aref jactivationvector j)))
        (setf outputerr (+ outputerr (abs delta)))
        (setf (aref jdeltavector j)
              (+ (aref jdeltavector j)
                 (* delta (dsigmoid (aref sum-of-products-arr j))))))
      ;;
      ;; Calculate the backpropogated error signal for all hidden slabs
      ;;
      (dotimes (nn (- nlayers 2))
        (setf n (- nlayers 3 nn))
        (setf idimension (nth (+ n 1) sizelist))
        (setf isumofproductsarr (nth n sum-of-products-list))
        (setf ideltavector (nth n back-delta-list))
        (dotimes (i idimension)
          (setf (aref ideltavector i) 0.0))
        (setf weightarr (nth (+ n 1) weights-list))
        (dotimes (i idimension)
          (setf err 0.0)
          (dotimes (j jdimension)
            (setf err (+ err (* (aref jdeltavector j) (aref weightarr i j)))))
          (setf (aref ideltavector i)
                (+ (aref ideltavector i)
                   (* err (dsigmoid (aref isumofproductsarr i))))))
        (setf jdimension idimension)
        (setf jdeltavector ideltavector))
      ;;
      ;; Update delta weights in the network
      ;;
      (setf idimension (car sizelist))
      (dotimes (n (- nlayers 1))
        (setf iactivationvector (nth n activationlist))
        (setf jdimension (nth (+ n 1) sizelist))
        (setf jdeltavector (nth n back-delta-list))
        (setf deltaweightarr (nth n delta-weights-list))
        (setf weightarr (nth n weights-list))
        (setf eida (nth n *default-eida-list*))
        (dotimes (j jdimension)
          (dotimes (i idimension)
            (setf delta (* eida (aref jdeltavector j) (aref iactivationvector i)))
            (setf (aref deltaweightarr i j)
                  (+ (aref deltaweightarr i j) delta)))) ; store delta to update weights
        (setf idimension jdimension))

      ;;
      ;; Update weights in the network
      ;;
      (setf idimension (car sizelist))
      (dotimes (n (- nlayers 1))
        (setf iactivationvector (nth n activationlist))
        (setf jdimension (nth (+ n 1) sizelist))
        (setf jdeltavector (nth n back-delta-list))
        (setf deltaweightarr (nth n delta-weights-list))
        (setf olddeltaweightarr (nth n old-delta-weight-list))
        (setf weightarr (nth n weights-list))
        (dotimes (j jdimension)
          (dotimes (i idimension)
            (setf (aref weightarr i j)
                  (+ (aref weightarr i j)
                     (* alpha (aref deltaweightarr i j))
                     (* beta (aref olddeltaweightarr i j))))
            (setf (aref olddeltaweightarr i j)
                  (aref deltaweightarr i j))))
        (setf idimension jdimension)))
    ;;
    ;;
    netlist))
;;
(defun deltaRecall (netList inputs &optional (tfunction #'sigmoid))
  (let* ((nLayers (first netList))
         (sizeList (second netList))
         (activationList (third netList))
         (weightList (fifth netList))
         (iDim (car sizelist))          ;size of input slab
         (jDim nil)
         (iActivationVector (car activationlist)) ;input activations
         (jActivationVector nil)
         (n nil)
         (weightArray nil)
         (returnList nil)
         (sum nil))
    ;;
    (dotimes (i iDim)                   ; copy training inputs to input slab
      (setf (aref iactivationvector i)
            (nth i inputs)))
    ;;
    (dotimes (n-1 (- nlayers 1))        ; update layer j to i
      (setf n (+ n-1 1))
      (setf jdim (nth n sizeList))
      (setf jactivationvector (nth n activationlist))
      (setf weightarray (nth n-1 weightlist))
      (dotimes (j jdim)                 ; process each neuron
        (setf sum 0.0)
        (dotimes (i idim)               ; get activation from each neuron in previous slab
          (setf sum (+ sum (* (aref weightarray i j)
                              (aref iactivationvector i)))))
        (if *delta-rule-debug-flag*
            (print (list "sum=" sum)))
        (setf (aref jactivationvector j)
              (funcall tfunction sum)))
      (setf idim jdim)                  ; get next slab ready
      (setf iactivationvector jactivationvector))
    ;;
    (dotimes (j jdim)
      (setf returnlist (append returnlist (list (aref jactivationvector j)))))
    returnlist))
;;
