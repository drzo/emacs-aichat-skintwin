;;; aichat-integration.el --- Integration between cognitive components -*- lexical-binding: t; -*-

(require 'aichat-util)
(require 'aichat-symbolic)
(require 'aichat-opencog)
(require 'aichat-ecan)
(require 'aichat-moses)
(require 'aichat-pln)
(require 'aichat-esn)

(defgroup aichat-integration nil
  "Integration between cognitive architecture components in aichat."
  :group 'aichat
  :prefix "aichat-integration-")

;; Configuration Options
(defcustom aichat-integration-auto-stimulate-focus t
  "Whether to automatically stimulate atoms in focus."
  :type 'boolean
  :group 'aichat-integration)

(defcustom aichat-integration-moses-learn-threshold 5
  "Number of interactions before triggering MOSES pattern learning."
  :type 'integer
  :group 'aichat-integration)

(defcustom aichat-integration-enable-esn-prediction t
  "Whether to use ESN for sequence prediction."
  :type 'boolean
  :group 'aichat-integration)

(defcustom aichat-integration-pln-reasoning-steps 2
  "Number of PLN reasoning steps to perform during integration."
  :type 'integer
  :group 'aichat-integration)

;; Core Integration Functions

(defun aichat-integration-process-input (input &optional context)
  "Process INPUT using all cognitive components with optional CONTEXT."
  (let ((kb aichat-opencog-kb)
        (esn-state (when aichat-integration-enable-esn-prediction
                     (aichat-esn-make-state)))
        (context (or context aichat-opencog-attention-focus))
        (result-atoms '()))
    
    ;; 1. Extract symbolic representation from input
    (let ((concepts (aichat-integration-extract-concepts input)))
      ;; Add to KB and set initial attention
      (dolist (concept concepts)
        (aichat-opencog-kb-add concept)
        (aichat-ecan-stimulate concept 0.6)
        (push concept result-atoms)))
    
    ;; 2. Update attention with ECAN
    (aichat-ecan-update kb)
    
    ;; 3. Apply context-based stimulation
    (when (and context aichat-integration-auto-stimulate-focus)
      (dolist (atom context)
        (aichat-ecan-stimulate atom 0.4)))
    
    ;; 4. Apply PLN reasoning
    (dotimes (_ aichat-integration-pln-reasoning-steps)
      (let ((inferences (aichat-pln-forward-chain kb 1)))
        (dolist (inf inferences)
          (aichat-opencog-kb-add inf)
          (push inf result-atoms))))
    
    ;; 5. Update ESN state if enabled
    (when aichat-integration-enable-esn-prediction
      (let ((numeric-input (aichat-integration-text-to-numeric input)))
        (aichat-esn-update esn-state numeric-input)))
    
    ;; 6. Trigger MOSES learning if enough data
    (when (> (length aichat-interaction-history) 
             aichat-integration-moses-learn-threshold)
      (let ((pattern (aichat-moses-learn-pattern
                     (seq-take aichat-interaction-history 
                               aichat-integration-moses-learn-threshold))))
        (when pattern
          ;; Convert pattern to symbolic representation and add to KB
          (let ((pattern-atom (aichat-symbolic-predicate 
                              (format "pattern-%s" (random 1000)))))
            (aichat-opencog-kb-add pattern-atom)
            (aichat-ecan-stimulate pattern-atom 0.7)
            (push pattern-atom result-atoms)))))
    
    ;; 7. Update attention focus for next time
    (aichat-opencog-set-focus result-atoms)
    
    ;; Return the relevant results
    (list :atoms result-atoms
          :esn-state (when esn-state (aichat-esn-state-state esn-state))
          :attention-map (aichat-integration-get-attention-map)
          :kb kb)))

(defun aichat-integration-extract-concepts (text)
  "Extract concepts from TEXT as symbolic atoms."
  (let ((concepts '())
        (words (split-string text "\\s-+" t)))
    ;; Simple extraction - just create concepts from words
    (dolist (word words)
      ;; Skip small words and punctuation
      (when (and (> (length word) 3)
                 (string-match-p "^[a-zA-Z]" word))
        (let* ((clean-word (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "" word)))
               (concept (aichat-symbolic-concept clean-word)))
          (push concept concepts))))
    
    ;; Also try to extract simple relationships (A is B)
    (when (string-match "\\([a-zA-Z]+\\)\\s-+is\\s-+\\([a-zA-Z]+\\)" text)
      (let* ((subject (match-string 1 text))
             (object (match-string 2 text))
             (subj-concept (aichat-symbolic-concept (downcase subject)))
             (obj-concept (aichat-symbolic-concept (downcase object)))
             (inheritance (aichat-symbolic-inheritance obj-concept subj-concept)))
        (push subj-concept concepts)
        (push obj-concept concepts)
        (push inheritance concepts)))
    
    (nreverse concepts)))

(defun aichat-integration-text-to-numeric (text)
  "Convert TEXT to a numeric value for ESN processing."
  ;; Simple hash-based approach for demo purposes
  (let ((hash-val 0.0))
    (dolist (char (string-to-list text))
      (setq hash-val (+ hash-val (/ (float (% (char-to-string char) 64)) 100.0))))
    (min 1.0 hash-val)))

(defun aichat-integration-get-attention-map ()
  "Get a map of atom names to attention values."
  (let ((attention-map (make-hash-table :test 'equal)))
    (maphash (lambda (name atom)
               (let ((av (aichat-ecan-get-av atom)))
                 (puthash name (aichat-ecan-av-sti av) attention-map)))
             aichat-opencog-kb)
    attention-map))

;; Visualization Functions

(defun aichat-integration-visualize ()
  "Create a visualization of the integrated cognitive system."
  (interactive)
  (let ((buffer (get-buffer-create "*AIChat-Cognitive-Visualization*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# Integrated Cognitive Architecture Visualization\n\n")
      
      ;; 1. Show top concepts by attention
      (insert "## Attention Distribution\n\n")
      (insert "| Concept | Attention | Visualization |\n")
      (insert "|---------|-----------|---------------|\n")
      (let ((atoms '()))
        (maphash (lambda (_k v) (push v atoms)) aichat-opencog-kb)
        (setq atoms 
              (sort atoms 
                    (lambda (a b) 
                      (> (aichat-ecan-av-sti (aichat-ecan-get-av a))
                         (aichat-ecan-av-sti (aichat-ecan-get-av b))))))
        
        (cl-loop for atom in atoms
                 for i from 0 below 10
                 do (let* ((av (aichat-ecan-get-av atom))
                          (sti (aichat-ecan-av-sti av))
                          (name (aichat-symbolic-atom-name atom))
                          (type (aichat-symbolic-atom-type atom))
                          (bar-chars (min 20 (ceiling (* sti 20)))))
                      (insert (format "| %s (%s) | %.3f | %s |\n" 
                                     name 
                                     type
                                     sti
                                     (make-string bar-chars ?█))))))
      
      ;; 2. Show MOSES patterns if available
      (when aichat-moses-population
        (insert "\n## MOSES Discovered Patterns\n\n")
        (let ((top-programs (seq-take aichat-moses-population 3)))
          (dolist (program top-programs)
            (insert "```lisp\n")
            (insert (aichat-moses-program-to-string program))
            (insert "\n```\n\n"))))
      
      ;; 3. Show ESN state if available
      (when aichat-integration-enable-esn-prediction
        (insert "\n## ESN Reservoir State\n\n")
        (let* ((state (aichat-esn-make-state))
               (state-vec (aichat-esn-state-state state))
               (sample-size (min 10 (length state-vec))))
          (insert "| Neuron | Activation |\n")
          (insert "|--------|------------|\n")
          (dotimes (i sample-size)
            (let ((val (aref state-vec i))
                  (bar-chars (min 20 (ceiling (* (+ 0.5 (/ (abs (aref state-vec i)) 2)) 20)))))
              (insert (format "| %d | %s %.3f |\n" 
                             i
                             (make-string bar-chars ?█)
                             val))))))
      
      ;; 4. Show PLN inferences
      (insert "\n## Recent PLN Inferences\n\n")
      (let ((inferences '()))
        ;; Find inheritance links with high STI (recently inferred)
        (maphash (lambda (_k v)
                   (when (eq (aichat-symbolic-atom-type v) 'inheritance)
                     (let ((av (aichat-ecan-get-av v)))
                       (when (> (aichat-ecan-av-sti av) 0.6)
                         (push v inferences)))))
                 aichat-opencog-kb)
        
        (if inferences
            (progn
              (insert "| From | Relation | To | Confidence |\n")
              (insert "|------|----------|----|-----------|\n")
              (dolist (inf inferences)
                (let* ((links (aichat-symbolic-atom-links inf))
                       (parent (aichat-symbolic-atom-name (car links)))
                       (child (aichat-symbolic-atom-name (cadr links)))
                       (tv (aichat-symbolic-atom-tv inf))
                       (confidence (aichat-pln-tv-confidence tv)))
                  (insert (format "| %s | → | %s | %.3f |\n" 
                                 child parent confidence)))))
          (insert "No recent inferences found.\n")))
      
      ;; 5. Component Integration Diagram
      (insert "\n## Component Integration\n\n")
      (insert "```\n")
      (insert "+----------------+      +----------------+\n")
      (insert "| Text Input     |----->| ECAN Attention |\n")
      (insert "+----------------+      +----------------+\n")
      (insert "        |                      |\n")
      (insert "        v                      v\n")
      (insert "+----------------+      +----------------+\n")
      (insert "| Symbolic Rep.  |<---->| PLN Reasoning  |\n")
      (insert "+----------------+      +----------------+\n")
      (insert "        |                      ^\n")
      (insert "        v                      |\n")
      (insert "+----------------+      +----------------+\n")
      (insert "| Pattern Mining |----->| ESN Prediction |\n")
      (insert "| (MOSES)        |      |                |\n")
      (insert "+----------------+      +----------------+\n")
      (insert "```\n")
      
      ;; Add component status
      (insert "\n## Component Status\n\n")
      (insert (format "- ECAN: %s\n" 
                     (if (fboundp 'aichat-ecan-update) 
                         "Active ✓" "Inactive ✗")))
      (insert (format "- PLN: %s\n" 
                     (if (fboundp 'aichat-pln-reason) 
                         "Active ✓" "Inactive ✗")))
      (insert (format "- MOSES: %s\n" 
                     (if (and (boundp 'aichat-moses-population)
                             aichat-moses-population) 
                         "Active ✓" "Inactive ✗")))
      (insert (format "- ESN: %s\n" 
                     (if aichat-integration-enable-esn-prediction
                         "Active ✓" "Inactive ✗")))
      
      (markdown-mode)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

;; Interactive Commands

(defun aichat-integration-process-region (start end)
  "Process the region from START to END using the integrated cognitive system."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (result (aichat-integration-process-input text))
         (atoms (plist-get result :atoms))
         (attention-map (plist-get result :attention-map))
         (buffer (get-buffer-create "*AIChat-Cognitive-Processing*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# Cognitive Processing Result\n\n")
      
      ;; Show the input
      (insert "## Input Text\n\n")
      (insert (format "> %s\n\n" text))
      
      ;; Show extracted concepts
      (insert "## Extracted Concepts\n\n")
      (if atoms
          (progn
            (insert "| Concept | Type | Attention |\n")
            (insert "|---------|------|----------|\n")
            (dolist (atom atoms)
              (let* ((name (aichat-symbolic-atom-name atom))
                     (type (aichat-symbolic-atom-type atom))
                     (sti (gethash name attention-map 0.0)))
                (insert (format "| %s | %s | %.3f |\n" 
                              (or name "(unnamed)") 
                              type 
                              sti)))))
        (insert "No concepts extracted.\n"))
      
      ;; Show attention focus
      (insert "\n## Attention Focus\n\n")
      (if aichat-opencog-attention-focus
          (progn
            (insert "| Concept | Type | Attention |\n")
            (insert "|---------|------|----------|\n")
            (dolist (atom aichat-opencog-attention-focus)
              (let* ((name (aichat-symbolic-atom-name atom))
                     (type (aichat-symbolic-atom-type atom))
                     (sti (gethash name attention-map 0.0)))
                (insert (format "| %s | %s | %.3f |\n" 
                              (or name "(unnamed)") 
                              type 
                              sti)))))
        (insert "No atoms in focus.\n"))
      
      ;; Show related concepts
      (insert "\n## Related Concepts\n\n")
      (if atoms
          (let ((related '()))
            ;; Find relationships involving the extracted concepts
            (dolist (atom atoms)
              (let* ((name (aichat-symbolic-atom-name atom))
                     (pattern (aichat-symbolic-atom 'inheritance atom nil nil))
                     (matches (aichat-opencog-match-pattern pattern aichat-opencog-kb)))
                (dolist (match matches)
                  (push match related))))
            
            (if related
                (progn
                  (insert "| Concept A | Relation | Concept B |\n")
                  (insert "|----------|----------|----------|\n")
                  (dolist (rel related)
                    (let* ((links (aichat-symbolic-atom-links rel))
                           (parent (aichat-symbolic-atom-name (car links)))
                           (child (aichat-symbolic-atom-name (cadr links))))
                      (insert (format "| %s | → | %s |\n" 
                                    child parent)))))
              (insert "No related concepts found.\n")))
        (insert "No atoms to find relations for.\n"))
      
      (markdown-mode)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(defun aichat-integration-explain-cognitive-architecture ()
  "Show an explanation of the cognitive architecture."
  (interactive)
  (let ((buffer (get-buffer-create "*AIChat-Cognitive-Architecture*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# OpenCog-Inspired Cognitive Architecture in Emacs\n\n")
      
      (insert "## Overview\n\n")
      (insert "This system implements a simplified version of the OpenCog cognitive architecture ")
      (insert "within Emacs, combining multiple AI components to create an integrated system.\n\n")
      
      (insert "## Components\n\n")
      
      (insert "### ECAN (Economic Attention Allocation Network)\n\n")
      (insert "- Manages attention allocation through economic principles\n")
      (insert "- Uses Short-Term Importance (STI), Long-Term Importance (LTI), and Very Long-Term Importance (VLTI)\n")
      (insert "- Implements attention decay, spreading, and forgetting mechanisms\n\n")
      
      (insert "### PLN (Probabilistic Logic Networks)\n\n")
      (insert "- Performs uncertain reasoning with truth values (strength, confidence)\n")
      (insert "- Implements inference rules like deduction, induction, and abduction\n")
      (insert "- Combines multiple sources of evidence through revision\n\n")
      
      (insert "### MOSES (Meta-Optimizing Semantic Evolutionary Search)\n\n")
      (insert "- Evolves programs to model patterns in data\n")
      (insert "- Uses program representation, variation, and fitness evaluation\n")
      (insert "- Includes mechanisms for program simplification\n\n")
      
      (insert "### ESN (Echo State Networks)\n\n")
      (insert "- Implements reservoir computing for temporal pattern recognition\n")
      (insert "- Maintains state for sequence prediction\n")
      (insert "- Complements symbolic reasoning with subsymbolic processing\n\n")
      
      (insert "## Integration\n\n")
      (insert "The integration layer connects these components into a unified system:\n\n")
      (insert "1. Text input is processed to extract symbolic representations\n")
      (insert "2. ECAN manages attention on these symbols\n")
      (insert "3. PLN performs reasoning to derive new knowledge\n")
      (insert "4. MOSES identifies patterns in interaction history\n")
      (insert "5. ESN predicts temporal patterns\n\n")
      
      (insert "## Visualization\n\n")
      (insert "The system provides various visualization tools:\n\n")
      (insert "- `aichat-integration-visualize` - Overview of the cognitive system\n")
      (insert "- `aichat-integration-process-region` - Process and analyze text\n")
      (insert "- `aichat-opencog-org-kb-to-buffer` - View knowledge base in Org format\n")
      (insert "- `aichat-opencog-org-visualize-attention-flow` - Visualize attention dynamics\n\n")
      
      (insert "## Usage\n\n")
      (insert "To process text with the cognitive architecture:\n\n")
      (insert "1. Select a region of text\n")
      (insert "2. Run `M-x aichat-integration-process-region`\n")
      (insert "3. View the results in the processing buffer\n\n")
      
      (insert "To visualize the cognitive system:\n\n")
      (insert "1. Run `M-x aichat-integration-visualize`\n")
      (insert "2. Explore the different components and their current state\n\n")
      
      (insert "To use Org-mode integration:\n\n")
      (insert "1. Open an Org file (e.g., `aichat-demo.org`)\n")
      (insert "2. Enable the minor mode with `M-x aichat-opencog-org-mode`\n")
      (insert "3. Use the key bindings (C-c o) to interact with the system\n")
      
      (markdown-mode)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

;; Define key mappings for the integration functionality
(defvar aichat-integration-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") 'aichat-integration-visualize)
    (define-key map (kbd "p") 'aichat-integration-process-region)
    (define-key map (kbd "e") 'aichat-integration-explain-cognitive-architecture)
    map)
  "Keymap for aichat-integration commands.")

(defvar aichat-integration-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") aichat-integration-map)
    map)
  "Prefix keymap for aichat-integration.")

;; Setting up global key prefix C-c C to access integration features
(define-key global-map (kbd "C-c C") aichat-integration-prefix-map)

(provide 'aichat-integration)