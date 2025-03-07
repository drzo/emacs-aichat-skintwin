;; aichat-openai.el --- OpenAI integration for aichat  -*- lexical-binding: t; -*-

(require 'aichat-util)
(require 'markdown-mode)

(defgroup aichat-openai nil
  "OpenAI integration settings."
  :group 'aichat
  :prefix "aichat-openai-")

(defcustom aichat-openai-api-key #'aichat-openai--default-api-key
  "OpenAI API key or function to get it."
  :type '(choice string function)
  :group 'aichat-openai)

(defcustom aichat-openai-chat-directory 
  (expand-file-name "aichat/openai" user-emacs-directory)
  "Directory to store chat files."
  :type 'string
  :group 'aichat-openai)

(defun aichat-openai--default-api-key ()
  "Get API key from auth-source."
  (auth-source-pick-first-password :host "platform.openai.com"))

(defun aichat-openai-chat ()
  "Start a chat session with OpenAI."
  (interactive)
  (let* ((name (completing-read "Chat name: " 
                               (when (file-exists-p aichat-openai-chat-directory)
                                 (directory-files aichat-openai-chat-directory nil
                                                "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))
         (filename (expand-file-name (if (string-suffix-p ".aichat" name) 
                                       name
                                     (concat name ".aichat"))
                                   aichat-openai-chat-directory)))
    (make-directory aichat-openai-chat-directory t)
    (find-file filename)
    (unless (file-exists-p filename)
      (insert "# System\n\nYou are a helpful assistant.\n\n# User\n\n"))))

(provide 'aichat-openai)