;; aichat-util.el --- Utility functions for aichat  -*- lexical-binding: t; -*-

(require 'json)
(require 'url)

(defgroup aichat nil
  "AI Chat in Emacs."
  :group 'tools
  :prefix "aichat-")

(defcustom aichat-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'aichat)

(defun aichat-debug (format-string &rest args)
  "Debug message helper.
Prints FORMAT-STRING with ARGS if debug is enabled."
  (when aichat-debug
    (let ((buf (get-buffer-create "*AICHAT-DEBUG*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (apply #'format format-string args))
        (insert "\n")))))

(defun aichat-json-serialize (object)
  "Serialize OBJECT to JSON string."
  (json-encode object))

(defun aichat-json-parse (string)
  "Parse STRING as JSON and return object."
  (json-read-from-string string))

(defun aichat-json-access (object path)
  "Access nested value in JSON OBJECT using PATH.
PATH format: {key1}[index]{key2}"
  (let ((value object))
    (with-temp-buffer
      (insert path)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "{\\([^}]+\\)}")
          (setq value (cdr (assoc (match-string 1) value)))
          (goto-char (match-end 0)))
         ((looking-at "\\[\\([0-9]+\\)\\]")
          (setq value (aref value (string-to-number (match-string 1))))
          (goto-char (match-end 0)))
         (t (forward-char 1)))))
    value))

(provide 'aichat-util)