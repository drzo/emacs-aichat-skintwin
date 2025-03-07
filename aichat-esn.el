;; aichat-esn.el --- Echo State Networks for aichat  -*- lexical-binding: t; -*-

(require 'aichat-util)

(defgroup aichat-esn nil
  "Echo State Networks for aichat."
  :group 'aichat
  :prefix "aichat-esn-")

;; ESN Parameters
(defcustom aichat-esn-reservoir-size 100
  "Size of the reservoir network."
  :type 'integer
  :group 'aichat-esn)

(defcustom aichat-esn-connectivity 0.1
  "Fraction of reservoir connections (0.0 to 1.0)."
  :type 'float
  :group 'aichat-esn)

(defcustom aichat-esn-spectral-radius 0.95
  "Spectral radius for reservoir weight scaling."
  :type 'float
  :group 'aichat-esn)

;; Reservoir State
(cl-defstruct (aichat-esn-state (:constructor aichat-esn-state--create))
  "Reservoir state structure."
  input-weights    ; Input-to-reservoir weights
  reservoir       ; Reservoir weights
  output-weights  ; Reservoir-to-output weights
  state)          ; Current reservoir state

(defun aichat-esn-make-state ()
  "Create initial reservoir state."
  (let* ((size aichat-esn-reservoir-size)
         ;; Random sparse reservoir weights
         (reservoir (cl-loop for i below size collect
                            (cl-loop for j below size
                                   when (< (random 1.0) aichat-esn-connectivity)
                                   collect (cons j (- (random 2.0) 1.0)))))
         ;; Scale weights to desired spectral radius
         (scaled-reservoir (aichat-esn-scale-weights reservoir)))
    (aichat-esn-state--create
     :input-weights (make-vector size 0.0)
     :reservoir scaled-reservoir
     :output-weights (make-vector size 0.0)
     :state (make-vector size 0.0))))

(defun aichat-esn-scale-weights (weights)
  "Scale WEIGHTS to achieve desired spectral radius."
  ;; TODO: Implement proper spectral radius scaling
  ;; For now just normalize by size
  (let ((scale (/ aichat-esn-spectral-radius
                  (sqrt (length weights)))))
    (mapcar (lambda (row)
              (mapcar (lambda (w) (* scale (cdr w))) row))
            weights)))

(defun aichat-esn-update (state input)
  "Update reservoir STATE with INPUT."
  (let* ((old-state (aichat-esn-state-state state))
         (size (length old-state))
         (new-state (make-vector size 0.0)))
    ;; Update reservoir state
    (dotimes (i size)
      (let ((sum 0.0))
        ;; Input contribution
        (setq sum (+ sum (* (aref (aichat-esn-state-input-weights state) i)
                           input)))
        ;; Reservoir contribution
        (dolist (w (nth i (aichat-esn-state-reservoir state)))
          (setq sum (+ sum (* (cdr w) (aref old-state (car w))))))
        ;; Apply tanh activation
        (aset new-state i (tanh sum))))
    ;; Update state
    (setf (aichat-esn-state-state state) new-state)
    ;; Return output
    (let ((output 0.0))
      (dotimes (i size)
        (setq output (+ output
                       (* (aref new-state i)
                          (aref (aichat-esn-state-output-weights state) i)))))
      output)))

;; Training
(defun aichat-esn-train (state inputs outputs &optional learning-rate)
  "Train reservoir on INPUTS and OUTPUTS sequences."
  (let ((rate (or learning-rate 0.1)))
    (cl-loop for input in inputs
             for target in outputs
             for output = (aichat-esn-update state input)
             for error = (- target output)
             do (let ((reservoir-state (aichat-esn-state-state state)))
                  ;; Update output weights using simple gradient descent
                  (dotimes (i (length reservoir-state))
                    (let ((delta (* rate error (aref reservoir-state i))))
                      (aset (aichat-esn-state-output-weights state) i
                            (+ (aref (aichat-esn-state-output-weights state) i)
                               delta))))))))

;; Integration with OpenCog
(defun aichat-esn-predict-pattern (pattern)
  "Use ESN to predict next element in PATTERN."
  (let ((state (aichat-esn-make-state)))
    ;; Train on pattern
    (aichat-esn-train state
                      (butlast pattern)
                      (cdr pattern))
    ;; Predict next value
    (aichat-esn-update state (car (last pattern)))))

(provide 'aichat-esn)