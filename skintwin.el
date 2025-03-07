;;; skintwin.el --- OpenCog-based dermatological model -*- lexical-binding: t; -*-

;; Author: AI Chat
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (aichat "1.0.0") (markdown-mode "2.5"))
;; Keywords: ai, biology, medicine, opencog
;; URL: https://github.com/aichat/skintwin

;;; Commentary:

;; SkinTwin is a multiscale dermatological model powered by OpenCog's
;; cognitive architecture components. It integrates various modules to
;; represent skin biology, environmental factors, and clinical outcomes
;; through a comprehensive knowledge graph and reasoning system.

;;; Code:

;; Core dependencies
(require 'aichat-symbolic)
(require 'aichat-opencog)
(require 'aichat-ecan)
(require 'aichat-moses)
(require 'aichat-pln)
(require 'aichat-opencog-org)

;; Load SkinTwin components
(require 'skintwin-integration)

;; Optional components - load if available
(when (locate-library "skintwin-esn")
  (require 'skintwin-esn))

(when (locate-library "skintwin-visualization")
  (require 'skintwin-visualization))

;; Database integration
(require 'skintwin-db)

;; API functions
(require 'skintwin-api)

;; Load the interactive mode
(when (locate-library "skintwin-mode")
  (require 'skintwin-mode))

(defgroup skintwin nil
  "OpenCog-based dermatological model."
  :group 'applications
  :prefix "skintwin-")

(defcustom skintwin-auto-initialize t
  "Whether to automatically initialize the SkinTwin system when loading."
  :type 'boolean
  :group 'skintwin)

;;;###autoload
(defun skintwin ()
  "Initialize and start the SkinTwin system."
  (interactive)
  (if (featurep 'skintwin-mode)
      ;; If skintwin-mode is available, use it
      (skintwin-mode 1)
    ;; Otherwise, initialize directly
    (skintwin-initialize)
    (skintwin-dashboard)))

;; Automatically initialize if requested
(when skintwin-auto-initialize
  (eval-after-load 'skintwin
    '(unless (and (boundp 'skintwin-mode) skintwin-mode)
       (message "Initializing SkinTwin system...")
       (skintwin-initialize))))

;; Quick access functions
(defalias 'skintwin-dashboard 'skintwin-dashboard
  "Display the SkinTwin dashboard.")

(defalias 'skintwin-analyze 'skintwin-analyze-patient
  "Analyze a patient in the SkinTwin system.")

(defalias 'skintwin-query 'skintwin-query-treatments
  "Query treatments for a condition in the SkinTwin system.")

(defalias 'skintwin-visualize 'skintwin-visualize-graph
  "Visualize the SkinTwin knowledge graph.")

(defalias 'skintwin-predict 'skintwin-predict-progression
  "Predict disease progression in the SkinTwin system.")

(provide 'skintwin)
;;; skintwin.el ends here