;;; aichat.el --- AI Chat in Emacs  -*- lexical-binding: t; -*-

(require 'aichat-util)

(require 'aichat-symbolic)
(require 'aichat-opencog)
(require 'aichat-moses)
(require 'aichat-pln)
(require 'aichat-esn)
(require 'aichat-ecan)

;; Load org integration if available
(when (locate-library "aichat-opencog-org")
  (require 'aichat-opencog-org))

(defgroup aichat nil
  "AI Chat in Emacs."
  :group 'tools
  :prefix "aichat-")

(defcustom aichat-default-provider 'openai
  "Default AI chat provider."
  :type '(choice (const :tag "OpenAI" openai)
                (const :tag "Bing AI" bingai))
  :group 'aichat)

(defcustom aichat-use-symbolic nil
  "Whether to use symbolic expression handling."
  :type 'boolean
  :group 'aichat)

(defcustom aichat-cognitive-architecture 'opencog
  "Cognitive architecture to use."
  :type '(choice (const :tag "OpenCog" opencog)
                (const :tag "MOSES" moses)
                (const :tag "ESN" esn)
                (const :tag "None" nil))
  :group 'aichat)

(defcustom aichat-learning-enabled t
  "Whether to enable program learning from interactions."
  :type 'boolean
  :group 'aichat)

(defvar aichat-interaction-history nil
  "History of chat interactions for learning.")

(defun aichat-chat ()
  "Start a chat session with the default provider."
  (interactive)
  (let ((provider-module (intern (format "aichat-%s" aichat-default-provider))))
    (require provider-module)
    (let ((chat-buffer (funcall (intern (format "%s-chat" provider-module)))))
      (when aichat-learning-enabled
        (aichat-setup-learning chat-buffer))
      chat-buffer)))

(defun aichat-setup-learning (buffer)
  "Setup learning hooks for BUFFER."
  (with-current-buffer buffer
    (add-hook 'after-change-functions #'aichat-learn-from-change nil t)))

(defun aichat-learn-from-change (beg end len)
  "Learn from changes between BEG and END."
  (when (and aichat-learning-enabled
             (> end beg))
    (let ((new-text (buffer-substring-no-properties beg end)))
      (push new-text aichat-interaction-history)
      ;; Apply cognitive architecture-specific learning
      (cond
       ;; MOSES pattern learning
       ((eq aichat-cognitive-architecture 'moses)
        (when (> (length aichat-interaction-history) 10)
          (aichat-moses-learn-pattern aichat-interaction-history)))
       ;; ESN pattern prediction
       ((eq aichat-cognitive-architecture 'esn)
        (aichat-esn-predict-pattern aichat-interaction-history))
       ;; OpenCog processing
       ((eq aichat-cognitive-architecture 'opencog)
        (aichat-opencog-process-message new-text))))))

(defun aichat-toggle-learning ()
  "Toggle program learning from interactions."
  (interactive)
  (setq aichat-learning-enabled (not aichat-learning-enabled))
  (message "AI Learning %s" (if aichat-learning-enabled "enabled" "disabled")))

(defun aichat-clear-history ()
  "Clear interaction history."
  (interactive)
  (setq aichat-interaction-history nil)
  (message "Interaction history cleared"))

(defun aichat-visualize-cognitive-state ()
  "Visualize the current state of the cognitive architecture."
  (interactive)
  (cond
   ((eq aichat-cognitive-architecture 'opencog)
    (cond
     ;; If org integration is available, use it
     ((fboundp 'aichat-opencog-org-visualize-attention-flow)
      (call-interactively #'aichat-opencog-org-visualize-attention-flow))
     ;; Otherwise use basic text output
     (t
      (let ((buffer (get-buffer-create "*AICHAT-Cognitive-State*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "# OpenCog Cognitive State\n\n")
          (insert (aichat-opencog-atoms-to-org))
          (goto-char (point-min)))
        (display-buffer buffer)))))
   
   ((eq aichat-cognitive-architecture 'moses)
    (let ((buffer (get-buffer-create "*AICHAT-Cognitive-State*"))
          (programs aichat-moses-population))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "# MOSES Program Evolution\n\n")
        (when programs
          (dotimes (i (min 5 (length programs)))
            (insert "## Program " (number-to-string (1+ i)) "\n\n")
            (insert "```lisp\n")
            (insert (aichat-moses-program-to-string (nth i programs)))
            (insert "\n```\n\n")))
        (goto-char (point-min)))
      (display-buffer buffer)))
   
   ((eq aichat-cognitive-architecture 'esn)
    (message "ESN visualization not implemented yet"))
   
   (t (message "No cognitive architecture active"))))

(provide 'aichat)