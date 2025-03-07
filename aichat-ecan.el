;; aichat-ecan.el --- Economic Attention Allocation Networks -*- lexical-binding: t; -*-

(require 'aichat-symbolic)
(require 'aichat-util)

(defgroup aichat-ecan nil
  "Economic Attention Allocation Networks for aichat."
  :group 'aichat
  :prefix "aichat-ecan-")

;; ECAN Parameters
(defcustom aichat-ecan-decay-rate 0.1
  "Rate at which attention values decay over time."
  :type 'float
  :group 'aichat-ecan)

(defcustom aichat-ecan-spread-threshold 0.2
  "Minimum attention value for spreading activation."
  :type 'float
  :group 'aichat-ecan)

(defcustom aichat-ecan-max-spread-steps 3
  "Maximum number of steps for spreading activation."
  :type 'integer
  :group 'aichat-ecan)

;; Attention Value Structure
(cl-defstruct (aichat-ecan-av (:constructor aichat-ecan-av--create))
  "Attention value structure."
  sti   ; Short-term importance (0.0 to 1.0)
  lti   ; Long-term importance (0.0 to 1.0)
  vlti) ; Very long-term importance (boolean)

(defun aichat-ecan-make-av (&optional sti lti vlti)
  "Create attention value with STI, LTI and VLTI."
  (aichat-ecan-av--create
   :sti (or sti 0.0)
   :lti (or lti 0.0)
   :vlti (or vlti nil)))

;; Attention Bank
(defvar aichat-ecan-bank (make-hash-table :test 'equal)
  "Storage for attention values.")

(defun aichat-ecan-get-av (atom)
  "Get attention value for ATOM."
  (or (gethash (aichat-symbolic-atom-name atom) aichat-ecan-bank)
      (aichat-ecan-make-av)))

(defun aichat-ecan-set-av (atom av)
  "Set attention value AV for ATOM."
  (puthash (aichat-symbolic-atom-name atom) av aichat-ecan-bank))

;; Attention Dynamics
(defun aichat-ecan-decay (av)
  "Apply decay to attention value AV."
  (let ((new-sti (* (aichat-ecan-av-sti av)
                    (- 1.0 aichat-ecan-decay-rate)))
        (new-lti (if (aichat-ecan-av-vlti av)
                    (aichat-ecan-av-lti av)
                  (* (aichat-ecan-av-lti av)
                     (- 1.0 (* 0.5 aichat-ecan-decay-rate))))))
    (aichat-ecan-make-av new-sti new-lti (aichat-ecan-av-vlti av))))

(defun aichat-ecan-stimulate (atom amount)
  "Stimulate ATOM with AMOUNT."
  (let* ((av (aichat-ecan-get-av atom))
         (new-sti (min 1.0 (+ (aichat-ecan-av-sti av) amount))))
    (aichat-ecan-set-av atom
                        (aichat-ecan-make-av new-sti
                                            (aichat-ecan-av-lti av)
                                            (aichat-ecan-av-vlti av)))))

(defun aichat-ecan-spread-importance (kb)
  "Spread importance through knowledge base KB."
  (let ((atoms-to-spread '()))
    ;; Collect atoms above threshold
    (maphash (lambda (name atom)
               (let ((av (aichat-ecan-get-av atom)))
                 (when (> (aichat-ecan-av-sti av)
                         aichat-ecan-spread-threshold)
                   (push atom atoms-to-spread))))
             kb)
    
    ;; Spread activation
    (dotimes (_ aichat-ecan-max-spread-steps)
      (dolist (atom atoms-to-spread)
        (let* ((av (aichat-ecan-get-av atom))
               (spread-amount (/ (aichat-ecan-av-sti av)
                               (length (aichat-symbolic-atom-links atom)))))
          ;; Spread to connected atoms
          (dolist (target (aichat-symbolic-atom-links atom))
            (aichat-ecan-stimulate target spread-amount)))))))

;; Forgetting
(defun aichat-ecan-forget-atom (atom)
  "Remove ATOM if importance is too low."
  (let ((av (aichat-ecan-get-av atom)))
    (when (and (< (aichat-ecan-av-sti av) 0.1)
               (< (aichat-ecan-av-lti av) 0.1)
               (not (aichat-ecan-av-vlti av)))
      (remhash (aichat-symbolic-atom-name atom) aichat-ecan-bank))))

;; Hebbian Learning
(defun aichat-ecan-hebbian-learn (atom1 atom2)
  "Strengthen connection between ATOM1 and ATOM2."
  (let ((av1 (aichat-ecan-get-av atom1))
        (av2 (aichat-ecan-get-av atom2)))
    ;; Increase LTI based on correlation
    (let ((correlation (* (aichat-ecan-av-sti av1)
                         (aichat-ecan-av-sti av2))))
      (aichat-ecan-set-av
       atom1 (aichat-ecan-make-av
              (aichat-ecan-av-sti av1)
              (min 1.0 (+ (aichat-ecan-av-lti av1)
                         (* 0.1 correlation)))
              (aichat-ecan-av-vlti av1)))
      (aichat-ecan-set-av
       atom2 (aichat-ecan-make-av
              (aichat-ecan-av-sti av2)
              (min 1.0 (+ (aichat-ecan-av-lti av2)
                         (* 0.1 correlation)))
              (aichat-ecan-av-vlti av2))))))

;; Integration with OpenCog
(defun aichat-ecan-update (kb)
  "Update attention values in knowledge base KB."
  ;; Decay all atoms
  (maphash (lambda (name atom)
             (aichat-ecan-set-av atom
                                (aichat-ecan-decay
                                 (aichat-ecan-get-av atom))))
           kb)
  
  ;; Spread importance
  (aichat-ecan-spread-importance kb)
  
  ;; Forget low importance atoms
  (maphash (lambda (name atom)
             (aichat-ecan-forget-atom atom))
           kb)
  
  ;; Return updated KB
  kb)

(provide 'aichat-ecan)