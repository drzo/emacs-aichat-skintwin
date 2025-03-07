;;; skintwin-mode.el --- Interactive mode for SkinTwin -*- lexical-binding: t; -*-

(require 'skintwin-integration)
(require 'org)

(defgroup skintwin-mode nil
  "Interactive mode for SkinTwin OpenCog-based dermatological model."
  :group 'skintwin
  :prefix "skintwin-mode-")

;; Configuration options
(defcustom skintwin-mode-auto-initialize t
  "Whether to automatically initialize SkinTwin when enabling the mode."
  :type 'boolean
  :group 'skintwin-mode)

(defcustom skintwin-mode-dashboard-at-startup t
  "Whether to automatically display the dashboard when enabling the mode."
  :type 'boolean
  :group 'skintwin-mode)

(defcustom skintwin-mode-auto-enable-opencog-org t
  "Whether to automatically enable aichat-opencog-org-mode when enabling skintwin-mode."
  :type 'boolean
  :group 'skintwin-mode)

(defcustom skintwin-mode-welcome-message t
  "Whether to display a welcome message when enabling the mode."
  :type 'boolean
  :group 'skintwin-mode)

;; Key bindings
(defvar skintwin-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Core commands
    (define-key map (kbd "C-c s i") 'skintwin-initialize)
    (define-key map (kbd "C-c s d") 'skintwin-dashboard)
    
    ;; Query and analysis
    (define-key map (kbd "C-c s q") 'skintwin-query-treatments)
    (define-key map (kbd "C-c s a") 'skintwin-analyze-patient)
    (define-key map (kbd "C-c s p") 'skintwin-predict-progression)
    
    ;; Visualization
    (define-key map (kbd "C-c s v") 'skintwin-visualize-graph)
    (define-key map (kbd "C-c s V") 'skintwin-visualization-dashboard)
    
    ;; Knowledge management
    (define-key map (kbd "C-c s k") 'skintwin-manage-knowledge)
    (define-key map (kbd "C-c s r") 'skintwin-run-reasoning)
    
    ;; Patient management
    (define-key map (kbd "C-c s P") 'skintwin-patient-dashboard)
    
    ;; Help
    (define-key map (kbd "C-c s h") 'skintwin-mode-help)
    
    map)
  "Keymap for SkinTwin minor mode.")

;; Mode definition
(define-minor-mode skintwin-mode
  "Minor mode for SkinTwin OpenCog-based dermatological model.

\\{skintwin-mode-map}"
  :init-value nil
  :lighter " SkinTwin"
  :keymap skintwin-mode-map
  (if skintwin-mode
      (skintwin-mode-initialize)
    (skintwin-mode-teardown)))

;; Initialization functions
(defun skintwin-mode-initialize ()
  "Initialize SkinTwin mode."
  ;; Enable opencog-org-mode if available and requested
  (when (and skintwin-mode-auto-enable-opencog-org
             (fboundp 'aichat-opencog-org-mode))
    (aichat-opencog-org-mode 1))
  
  ;; Initialize SkinTwin system if requested
  (when skintwin-mode-auto-initialize
    (skintwin-initialize))
  
  ;; Show dashboard if requested
  (when skintwin-mode-dashboard-at-startup
    (skintwin-dashboard))
  
  ;; Show welcome message if requested
  (when skintwin-mode-welcome-message
    (skintwin-mode-show-welcome-message)))

(defun skintwin-mode-teardown ()
  "Clean up when disabling SkinTwin mode."
  ;; Disable opencog-org-mode if it was enabled by us
  (when (and skintwin-mode-auto-enable-opencog-org
             (fboundp 'aichat-opencog-org-mode))
    (aichat-opencog-org-mode -1))
  
  (message "SkinTwin mode disabled."))

(defun skintwin-mode-show-welcome-message ()
  "Display a welcome message when enabling SkinTwin mode."
  (let ((buf (get-buffer-create "*SkinTwin Welcome*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Welcome to SkinTwin\n\n")
      (insert "SkinTwin is an OpenCog-based dermatological model that integrates multiple cognitive\n")
      (insert "architecture components to represent skin biology, treatments, and outcomes.\n\n")
      
      (insert "## Getting Started\n\n")
      (insert "- Press `C-c s d` to open the SkinTwin dashboard\n")
      (insert "- Press `C-c s h` for help and documentation\n")
      (insert "- Press `C-c s q` to query treatments for a skin condition\n")
      (insert "- Press `C-c s a` to analyze a patient\n\n")
      
      (insert "SkinTwin is now initialized and ready to use.\n")
      (insert "Happy exploring!\n")
      
      (markdown-mode)
      (goto-char (point-min)))
    
    (display-buffer buf)))

;; Helper functions
(defun skintwin-run-reasoning ()
  "Run PLN reasoning on the knowledge base."
  (interactive)
  (message "Running PLN reasoning...")
  (aichat-pln-reason aichat-opencog-kb)
  (message "PLN reasoning completed. Knowledge base updated."))

(defun skintwin-manage-knowledge ()
  "Manage the SkinTwin knowledge base."
  (interactive)
  (let* ((actions '(("Import from Org file" . skintwin-import-org-file)
                   ("Export to Org file" . skintwin-export-to-org)
                   ("View knowledge graph" . skintwin-visualize-graph)
                   ("Manage attention values" . skintwin-manage-attention)
                   ("Run PLN reasoning" . skintwin-run-reasoning)))
         (action (completing-read "Select action: " (mapcar #'car actions) nil t)))
    (funcall (cdr (assoc action actions)))))

(defun skintwin-import-org-file ()
  "Import knowledge from an Org file."
  (interactive)
  (let ((file (read-file-name "Select Org file to import: " nil nil t)))
    (when (and file (file-exists-p file))
      (message "Importing knowledge from %s..." (file-name-nondirectory file))
      (skintwin-org-to-kb file)
      (message "Import completed from %s" (file-name-nondirectory file)))))

(defun skintwin-export-to-org ()
  "Export knowledge to an Org file."
  (interactive)
  (let ((file (read-file-name "Export to file: " nil nil nil "skintwin-export.org")))
    (when file
      (message "Exporting knowledge to %s..." (file-name-nondirectory file))
      (aichat-opencog-org-kb-to-buffer nil (find-file-noselect file))
      (message "Export completed to %s" (file-name-nondirectory file)))))

(defun skintwin-manage-attention ()
  "Manage attention values in the knowledge base."
  (interactive)
  (let* ((actions '(("View attention heatmap" . skintwin-visualization-attention-heatmap)
                   ("Visualize attention flow" . skintwin-visualization-attention-flow)
                   ("Stimulate concept" . skintwin-stimulate-concept)
                   ("Reset attention values" . skintwin-reset-attention)))
         (action (completing-read "Select action: " (mapcar #'car actions) nil t)))
    (funcall (cdr (assoc action actions)))))

(defun skintwin-stimulate-concept ()
  "Stimulate a concept to increase its attention value."
  (interactive)
  (let* ((concepts '())
         (concept-name nil)
         (amount nil))
    
    ;; Collect concept names
    (maphash (lambda (k v)
               (when (eq (aichat-symbolic-atom-type v) 'concept)
                 (push k concepts)))
             aichat-opencog-kb)
    
    ;; Ask for concept and amount
    (setq concept-name (completing-read "Select concept: " concepts nil t))
    (setq amount (read-number "Stimulation amount (0.0-1.0): " 0.8))
    
    ;; Stimulate the concept
    (let ((concept (aichat-opencog-kb-get concept-name)))
      (when concept
        (aichat-ecan-stimulate concept amount)
        (message "Stimulated '%s' with %.2f" concept-name amount)))))

(defun skintwin-reset-attention ()
  "Reset all attention values in the knowledge base."
  (interactive)
  (when (yes-or-no-p "Reset all attention values? ")
    (maphash (lambda (k v)
               (when (aichat-ecan-get-av v)
                 (aichat-ecan-set-av v (aichat-ecan-make-av 0.1 0.1 nil))))
             aichat-opencog-kb)
    (message "All attention values have been reset.")))

(defun skintwin-patient-dashboard ()
  "Display the patient management dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*SkinTwin-Patient-Dashboard*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: SkinTwin Patient Management\n\n")
      
      ;; Actions
      (insert "* Patient Management Actions\n\n")
      (insert "Press the following keys for patient management actions:\n\n")
      (insert "| Key | Action | Description |\n")
      (insert "|-----|--------|-------------|\n")
      (insert "| a | Add Patient | Add a new patient to the system |\n")
      (insert "| c | Add Condition | Associate a condition with a patient |\n")
      (insert "| s | View Patient | Search for and view patient details |\n")
      (insert "| r | Recommend Treatment | Generate treatment recommendations |\n")
      (insert "| p | Predict Progression | Predict disease progression for a patient |\n")
      
      ;; Patient list
      (insert "\n* Patient Registry\n\n")
      
      ;; Find all patients
      (let ((patients '()))
        (maphash (lambda (k v)
                   (when (eq (aichat-symbolic-atom-type v) 'concept)
                     ;; Check if it might be a patient (not in any category)
                     (let* ((inheritance-pattern 
                            (aichat-symbolic-atom 'inheritance nil nil nil (list v)))
                            (matches (aichat-opencog-match-pattern 
                                     inheritance-pattern aichat-opencog-kb))
                            (is-in-category nil))
                       
                       (dolist (match matches)
                         (let* ((links (aichat-symbolic-atom-links match))
                                (parent (car links))
                                (parent-name (aichat-symbolic-atom-name parent)))
                           (when (member parent-name 
                                        '("skin_layer" "cell_type" "skin_condition"))
                             (setq is-in-category t))))
                       
                       ;; If not in a category, it might be a patient
                       (unless is-in-category
                         (let* ((has-condition (aichat-symbolic-predicate "has_condition"))
                                (condition-pattern (aichat-symbolic-evaluation 
                                                  has-condition (list v nil) nil))
                                (conditions (aichat-opencog-match-pattern 
                                            condition-pattern aichat-opencog-kb)))
                           
                           ;; If it has conditions, it's definitely a patient
                           (when conditions
                             (push (cons k conditions) patients)))))))
                 aichat-opencog-kb)
        
        ;; Display patients
        (if patients
            (progn
              (insert "| Patient ID | Conditions | Actions |\n")
              (insert "|------------|------------|--------|\n")
              
              (dolist (patient patients)
                (let* ((patient-id (car patient))
                       (conditions (cdr patient))
                       (condition-names 
                        (mapcar (lambda (cond-rel)
                                 (let* ((links (aichat-symbolic-atom-links cond-rel))
                                        (condition (cadr links)))
                                   (aichat-symbolic-atom-name condition)))
                                conditions)))
                  
                  (insert (format "| %s | %s | [View] [Analyze] |\n" 
                                 patient-id 
                                 (mapconcat #'identity condition-names ", "))))))
          
          (insert "No patients found in the system. Use 'a' to add a new patient.\n")))
      
      ;; Set up keybindings for the dashboard
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "a") (lambda () 
                               (interactive) 
                               (call-interactively #'skintwin-add-patient)))
      (local-set-key (kbd "c") (lambda () 
                               (interactive) 
                               (call-interactively #'skintwin-add-patient-condition)))
      (local-set-key (kbd "s") (lambda () 
                               (interactive)
                               (let* ((patients '())
                                      (patient-id nil))
                                 
                                 ;; Collect patient IDs
                                 (maphash (lambda (k v)
                                            (when (eq (aichat-symbolic-atom-type v) 'concept)
                                              (let* ((has-condition (aichat-symbolic-predicate "has_condition"))
                                                     (condition-pattern (aichat-symbolic-evaluation 
                                                                       has-condition (list v nil) nil))
                                                     (conditions (aichat-opencog-match-pattern 
                                                                 condition-pattern aichat-opencog-kb)))
                                                
                                                ;; If it has conditions, it's a patient
                                                (when conditions
                                                  (push k patients)))))
                                          aichat-opencog-kb)
                                 
                                 (setq patient-id (completing-read "Select patient: " patients nil t))
                                 (when patient-id
                                   (skintwin-analyze-patient patient-id)))))
      (local-set-key (kbd "r") (lambda () 
                               (interactive)
                               (if (featurep 'skintwin-esn)
                                   (call-interactively #'skintwin-esn-treatment-recommendation)
                                 (message "ESN module not loaded. Cannot generate recommendations."))))
      (local-set-key (kbd "p") (lambda () 
                               (interactive) 
                               (call-interactively #'skintwin-predict-progression))))
    
    (display-buffer buffer)))

(defun skintwin-mode-help ()
  "Display help information for SkinTwin mode."
  (interactive)
  (let ((buffer (get-buffer-create "*SkinTwin Help*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# SkinTwin Help\n\n")
      
      (insert "## Overview\n\n")
      (insert "SkinTwin is a multiscale dermatological model based on OpenCog's cognitive architecture components.\n")
      (insert "It integrates various modules to represent skin biology, environmental factors, and clinical outcomes.\n\n")
      
      (insert "## Key Components\n\n")
      (insert "- **DermatoGraph (AtomSpace)**: Knowledge representation system\n")
      (insert "- **SensoryFocus (ECAN)**: Attention allocation system\n")
      (insert "- **DermatoLogic (PLN)**: Probabilistic reasoning system\n")
      (insert "- **EpidermiLearn (MOSES)**: Pattern mining and model building\n")
      (insert "- **RuleDerm (URE)**: Rule-based reasoning system\n")
      (insert "- **ESN Prediction**: Temporal pattern recognition for disease progression\n\n")
      
      (insert "## Key Bindings\n\n")
      (insert "| Keybinding | Command | Description |\n")
      (insert "|------------|---------|-------------|\n")
      (insert "| C-c s i | skintwin-initialize | Initialize the SkinTwin system |\n")
      (insert "| C-c s d | skintwin-dashboard | Display the main dashboard |\n")
      (insert "| C-c s q | skintwin-query-treatments | Query treatments for a condition |\n")
      (insert "| C-c s a | skintwin-analyze-patient | Analyze a patient |\n")
      (insert "| C-c s p | skintwin-predict-progression | Predict disease progression |\n")
      (insert "| C-c s v | skintwin-visualize-graph | Visualize the knowledge graph |\n")
      (insert "| C-c s V | skintwin-visualization-dashboard | Open visualization dashboard |\n")
      (insert "| C-c s k | skintwin-manage-knowledge | Manage the knowledge base |\n")
      (insert "| C-c s r | skintwin-run-reasoning | Run PLN reasoning |\n")
      (insert "| C-c s P | skintwin-patient-dashboard | Patient management dashboard |\n")
      (insert "| C-c s h | skintwin-mode-help | Display this help |\n\n")
      
      (insert "## Getting Started\n\n")
      (insert "1. Initialize the system with `C-c s i`\n")
      (insert "2. Open the dashboard with `C-c s d`\n")
      (insert "3. Add a patient with `C-c s P` then press `a`\n")
      (insert "4. Add a condition to the patient using `c`\n")
      (insert "5. Analyze the patient with `C-c s a`\n")
      (insert "6. Query treatments for a condition with `C-c s q`\n\n")
      
      (insert "## Working with Org Files\n\n")
      (insert "SkinTwin integrates with Org mode to allow structured representation of knowledge.\n")
      (insert "You can:\n\n")
      (insert "- Convert Org headings to knowledge base entries with `C-c o k`\n")
      (insert "- Convert an entire Org buffer to the knowledge base with `C-c o b`\n")
      (insert "- Import knowledge from an Org file with `C-c s k` then selecting 'Import from Org file'\n")
      (insert "- Export the knowledge base to an Org file with `C-c s k` then selecting 'Export to Org file'\n\n")
      
      (insert "## Further Documentation\n\n")
      (insert "For more detailed documentation on specific components:\n\n")
      (insert "- DermatoGraph: See `skintwin.org` for the knowledge structure\n")
      (insert "- SensoryFocus: See `aichat-ecan.el` for attention allocation mechanisms\n")
      (insert "- DermatoLogic: See `aichat-pln.el` for reasoning capabilities\n")
      (insert "- EpidermiLearn: See `aichat-moses.el` for pattern mining features\n")
      (insert "- ESN Prediction: See `skintwin-esn.el` for temporal modeling\n")
      
      (markdown-mode)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(defun skintwin-guess-buffer-type ()
  "Guess the type of the current buffer and enable appropriate features."
  (interactive)
  (cond
   ;; Org file with SkinTwin content
   ((and (eq major-mode 'org-mode)
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "SkinTwin\\|skin biology\\|dermatological model" nil t)))
    (skintwin-mode 1)
    (when (fboundp 'aichat-opencog-org-mode)
      (aichat-opencog-org-mode 1))
    (message "Enabled SkinTwin and OpenCog Org modes for this buffer"))
   
   ;; Elisp file with SkinTwin content
   ((and (eq major-mode 'emacs-lisp-mode)
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "skintwin\\|aichat-opencog" nil t)))
    (skintwin-mode 1)
    (message "Enabled SkinTwin mode for this Elisp buffer"))
   
   ;; Not a SkinTwin buffer
   (t
    (message "This doesn't appear to be a SkinTwin-related buffer"))))

;; Auto-enable for SkinTwin files
(defun skintwin-mode-auto-enable ()
  "Automatically enable skintwin-mode for appropriate files."
  (when (and (buffer-file-name)
             (string-match-p "skintwin\\|skin.*\\.org" (buffer-file-name)))
    (skintwin-mode 1)))

;; Add to hooks
(add-hook 'org-mode-hook 'skintwin-mode-auto-enable)
(add-hook 'emacs-lisp-mode-hook 'skintwin-mode-auto-enable)

(provide 'skintwin-mode)
;;; skintwin-mode.el ends here