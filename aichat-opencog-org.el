;;; aichat-opencog-org.el --- Enhanced Org-mode integration for OpenCog -*- lexical-binding: t; -*-

(require 'org)
(require 'aichat-opencog)
(require 'aichat-symbolic)
(require 'aichat-util)
(require 'aichat-ecan)

(defgroup aichat-opencog-org nil
  "Org-mode integration for OpenCog."
  :group 'aichat-opencog
  :prefix "aichat-opencog-org-")

;; Customization options
(defcustom aichat-opencog-org-auto-sync nil
  "Whether to automatically sync Org content with the knowledge base."
  :type 'boolean
  :group 'aichat-opencog-org)

(defcustom aichat-opencog-org-visualize-attention t
  "Whether to visualize attention values in Org representation."
  :type 'boolean
  :group 'aichat-opencog-org)

;; Visualization helpers
(defun aichat-opencog-org--attention-star (value)
  "Convert attention VALUE to star representation."
  (let ((stars (min 5 (ceiling (* value 5)))))
    (make-string stars ?★)))

(defun aichat-opencog-org--attention-color (value)
  "Return face property for attention VALUE."
  (cond
   ((>= value 0.8) '(:foreground "red" :weight bold))
   ((>= value 0.6) '(:foreground "orange" :weight bold))
   ((>= value 0.4) '(:foreground "yellow"))
   ((>= value 0.2) '(:foreground "green"))
   (t '(:foreground "gray"))))

;; Enhanced conversion functions

(defun aichat-opencog-org-kb-to-buffer (&optional kb buffer)
  "Convert knowledge base KB to an Org buffer BUFFER.
Includes visualization of attention values and relations."
  (interactive)
  (let ((kb (or kb aichat-opencog-kb))
        (buffer (or buffer (get-buffer-create "*OpenCog-Org*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: OpenCog Knowledge Base\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
      (insert "#+PROPERTY: ATTENTION_VALUES t\n\n")
      
      ;; Create a table of contents
      (insert "* Knowledge Base Overview\n")
      (insert "  :PROPERTIES:\n")
      (insert "  :VISIBILITY: folded\n")
      (insert "  :END:\n")
      (insert "** Concept Count: " (number-to-string (hash-table-count kb)) "\n")
      (insert "** Top Attention Concepts\n")
      
      ;; Get top attention concepts
      (let ((atoms '()))
        (maphash (lambda (k v) (push v atoms)) kb)
        (setq atoms 
              (sort atoms 
                    (lambda (a b) 
                      (> (aichat-ecan-av-sti (aichat-ecan-get-av a))
                         (aichat-ecan-av-sti (aichat-ecan-get-av b))))))
        
        ;; Show top 5 concepts
        (cl-loop for atom in atoms
                 for i from 0 below 5
                 do (let* ((av (aichat-ecan-get-av atom))
                          (sti (aichat-ecan-av-sti av))
                          (name (aichat-symbolic-atom-name atom)))
                      (insert (format "   - %s %s %s\n" 
                                     name
                                     (aichat-opencog-org--attention-star sti)
                                     (propertize (format "(%.2f)" sti)
                                                'face (aichat-opencog-org--attention-color sti)))))))
      
      ;; Insert actual knowledge content grouped by type
      (insert "\n* Concepts\n")
      (insert "  :PROPERTIES:\n")
      (insert "  :VISIBILITY: folded\n")
      (insert "  :END:\n")
      
      ;; Group atoms by type
      (let ((concepts '())
            (inheritances '())
            (evaluations '()))
        (maphash (lambda (_k v)
                   (case (aichat-symbolic-atom-type v)
                     ('concept (push v concepts))
                     ('inheritance (push v inheritances))
                     ('evaluation (push v evaluations))))
                 kb)

        ;; Sort by attention value
        (setq concepts (sort concepts
                            (lambda (a b)
                              (> (aichat-ecan-av-sti (aichat-ecan-get-av a))
                                 (aichat-ecan-av-sti (aichat-ecan-get-av b))))))
        
        ;; Output concepts
        (dolist (concept concepts)
          (let* ((name (aichat-symbolic-atom-name concept))
                (av (aichat-ecan-get-av concept))
                (sti (aichat-ecan-av-sti av))
                (lti (aichat-ecan-av-lti av))
                (vlti (aichat-ecan-av-vlti av)))
            (insert (format "** %s %s\n" 
                           name
                           (if aichat-opencog-org-visualize-attention
                               (propertize (aichat-opencog-org--attention-star sti)
                                          'face (aichat-opencog-org--attention-color sti))
                             "")))
            (insert "   :PROPERTIES:\n")
            (insert (format "   :STI: %.3f\n" sti))
            (insert (format "   :LTI: %.3f\n" lti))
            (insert (format "   :VLTI: %s\n" (if vlti "t" "nil")))
            (insert "   :END:\n")
            
            ;; Find relationships involving this concept
            (let ((related-inheritances 
                   (cl-remove-if-not
                    (lambda (inh)
                      (let ((links (aichat-symbolic-atom-links inh)))
                        (or (equal (aichat-symbolic-atom-name (car links)) name)
                            (equal (aichat-symbolic-atom-name (cadr links)) name))))
                    inheritances))
                  (related-evaluations
                   (cl-remove-if-not
                    (lambda (eval)
                      (let ((links (aichat-symbolic-atom-links eval)))
                        (or (equal (aichat-symbolic-atom-name (car links)) name)
                            (and (listp (cdr links))
                                 (cl-some (lambda (l) 
                                           (equal (aichat-symbolic-atom-name l) name))
                                         (cdr links))))))
                    evaluations)))
              
              ;; If there are relationships, show them
              (when related-inheritances
                (insert "*** Inheritance Relationships\n")
                (dolist (inh related-inheritances)
                  (let* ((links (aichat-symbolic-atom-links inh))
                         (parent (aichat-symbolic-atom-name (car links)))
                         (child (aichat-symbolic-atom-name (cadr links)))
                         (av-inh (aichat-ecan-get-av inh))
                         (sti-inh (aichat-ecan-av-sti av-inh)))
                    (if (equal name parent)
                        (insert (format "    - Parent of: %s %s\n" 
                                       child 
                                       (if aichat-opencog-org-visualize-attention
                                           (propertize (format "(%.2f)" sti-inh)
                                                      'face (aichat-opencog-org--attention-color sti-inh))
                                         "")))
                      (insert (format "    - Child of: %s %s\n" 
                                     parent
                                     (if aichat-opencog-org-visualize-attention
                                         (propertize (format "(%.2f)" sti-inh)
                                                    'face (aichat-opencog-org--attention-color sti-inh))
                                       "")))))))
              
              ;; Show evaluations
              (when related-evaluations
                (insert "*** Evaluations\n")
                (dolist (eval related-evaluations)
                  (let* ((links (aichat-symbolic-atom-links eval))
                         (pred (aichat-symbolic-atom-name (car links)))
                         (args (cdr links))
                         (av-eval (aichat-ecan-get-av eval))
                         (sti-eval (aichat-ecan-av-sti av-eval)))
                    (insert (format "    - %s(%s) %s\n" 
                                   pred 
                                   (mapconcat (lambda (a) (aichat-symbolic-atom-name a)) 
                                              args ", ")
                                   (if aichat-opencog-org-visualize-attention
                                       (propertize (format "(%.2f)" sti-eval)
                                                  'face (aichat-opencog-org--attention-color sti-eval))
                                     "")))))))))
      
      ;; Finalize buffer
      (org-update-statistics-cookies t)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(defun aichat-opencog-org-heading-to-kb ()
  "Convert current Org heading to knowledge base atoms."
  (interactive)
  (save-excursion
    (let* ((heading (org-get-heading t t t t))
           (props (org-entry-properties))
           (content (org-get-entry))
           (heading-level (org-current-level))
           ;; Create concept for heading
           (concept (aichat-symbolic-concept heading)))
      
      ;; Add to KB
      (aichat-opencog-kb-add concept)
      
      ;; Set attention values if present in properties
      (let ((sti (cdr (assoc "STI" props)))
            (lti (cdr (assoc "LTI" props)))
            (vlti (cdr (assoc "VLTI" props)))
            (av (aichat-ecan-make-av)))
        (when sti (setf (aichat-ecan-av-sti av) (string-to-number sti)))
        (when lti (setf (aichat-ecan-av-lti av) (string-to-number lti)))
        (when vlti (setf (aichat-ecan-av-vlti av) (string= vlti "t")))
        (aichat-ecan-set-av concept av))
      
      ;; Process child headings if any - create inheritance relationships
      (let ((end-of-subtree (save-excursion (org-end-of-subtree t))))
        (forward-line)
        (while (and (< (point) end-of-subtree)
                    (re-search-forward org-heading-regexp end-of-subtree t))
          (let* ((child-level (org-current-level))
                 (child-heading (org-get-heading t t t t)))
            (when (= child-level (1+ heading-level))
              ;; This is a direct child - create inheritance relationship
              (let ((child-concept (aichat-symbolic-concept child-heading))
                    (inheritance (aichat-symbolic-inheritance 
                                  concept child-concept)))
                (aichat-opencog-kb-add child-concept)
                (aichat-opencog-kb-add inheritance))))))

      ;; Process content for possible relationships
      (when content
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          ;; Look for patterns like "predicate(arg1, arg2)"
          (while (re-search-forward "\\([a-zA-Z0-9_-]+\\)(\\([^)]+\\))" nil t)
            (let* ((pred-name (match-string 1))
                   (args-str (match-string 2))
                   (args (split-string args-str ",\\s-*")))
              ;; Create predicate and arguments
              (let ((pred (aichat-symbolic-predicate pred-name))
                    (arg-atoms (mapcar #'aichat-symbolic-concept args)))
                (aichat-opencog-kb-add pred)
                (dolist (arg arg-atoms)
                  (aichat-opencog-kb-add arg))
                ;; Create evaluation link
                (let ((eval (aichat-symbolic-evaluation pred arg-atoms)))
                  (aichat-opencog-kb-add eval)))))))
      
      (message "Added %s to knowledge base" heading))))

(defun aichat-opencog-org-buffer-to-kb ()
  "Convert entire Org buffer to knowledge base atoms."
  (interactive)
  (org-map-entries #'aichat-opencog-org-heading-to-kb))

;; Interactive functions for using the system

(defun aichat-opencog-org-toggle-auto-sync ()
  "Toggle automatic synchronization of Org content with KB."
  (interactive)
  (setq aichat-opencog-org-auto-sync (not aichat-opencog-org-auto-sync))
  (message "Auto-sync is %s" (if aichat-opencog-org-auto-sync "enabled" "disabled"))
  (if aichat-opencog-org-auto-sync
      (add-hook 'org-after-todo-state-change-hook #'aichat-opencog-org-heading-to-kb)
    (remove-hook 'org-after-todo-state-change-hook #'aichat-opencog-org-heading-to-kb)))

(defun aichat-opencog-org-query-related-concepts (concept-name)
  "Find concepts related to CONCEPT-NAME and display them."
  (interactive "sConcept name: ")
  (let* ((concept (aichat-opencog-kb-get concept-name))
         (buffer (get-buffer-create "*OpenCog-Query*")))
    (if (null concept)
        (message "Concept %s not found in knowledge base" concept-name)
      ;; Stimulate the concept to increase attention
      (aichat-ecan-stimulate concept 0.5)
      
      ;; Create a pattern for matching
      (let* ((pattern (aichat-symbolic-atom 'inheritance concept nil nil))
             (matches (aichat-opencog-match-pattern pattern aichat-opencog-kb)))
        
        (with-current-buffer buffer
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Concepts Related to %s\n\n" concept-name))
          
          (if matches
              (progn
                (insert (format "* Found %d related concepts\n" (length matches)))
                (dolist (match matches)
                  (let* ((links (aichat-symbolic-atom-links match))
                         (parent (car links))
                         (child (cadr links))
                         (parent-name (aichat-symbolic-atom-name parent))
                         (child-name (aichat-symbolic-atom-name child))
                         (av (aichat-ecan-get-av match))
                         (sti (aichat-ecan-av-sti av)))
                    
                    ;; Determine relationship direction
                    (if (equal concept-name parent-name)
                        (insert (format "** Child: %s %s\n" 
                                      child-name
                                      (if aichat-opencog-org-visualize-attention
                                          (propertize (aichat-opencog-org--attention-star sti)
                                                     'face (aichat-opencog-org--attention-color sti))
                                        "")))
                      (insert (format "** Parent: %s %s\n" 
                                     parent-name
                                     (if aichat-opencog-org-visualize-attention
                                         (propertize (aichat-opencog-org--attention-star sti)
                                                    'face (aichat-opencog-org--attention-color sti))
                                       "")))))))
            (insert "* No related concepts found\n")))
        
        (display-buffer buffer)))))

(defun aichat-opencog-org-reason-about-concept (concept-name)
  "Apply reasoning to CONCEPT-NAME and display inferences."
  (interactive "sConcept name: ")
  (let* ((concept (aichat-opencog-kb-get concept-name))
         (buffer (get-buffer-create "*OpenCog-Reasoning*")))
    (if (null concept)
        (message "Concept %s not found in knowledge base" concept-name)
      ;; Apply reasoning
      (require 'aichat-pln)
      (let ((inferences '()))
        
        ;; Get first-order related concepts
        (let* ((pattern (aichat-symbolic-atom 'inheritance concept nil nil))
               (direct-matches (aichat-opencog-match-pattern pattern aichat-opencog-kb)))
          
          ;; For each directly related concept, try to infer new relationships
          (dolist (match direct-matches)
            (let* ((links (aichat-symbolic-atom-links match))
                   (parent (car links))
                   (child (cadr links))
                   ;; Find second-order relationships
                   (next-pattern (aichat-symbolic-atom 'inheritance 
                                                      (if (equal concept parent) child parent)
                                                      nil nil))
                   (next-matches (aichat-opencog-match-pattern next-pattern aichat-opencog-kb)))
              
              ;; For each second-order relationship, apply deduction rule
              (dolist (next-match next-matches)
                (let* ((next-links (aichat-symbolic-atom-links next-match))
                       (next-parent (car next-links))
                       (next-child (cadr next-links))
                       ;; Determine if we can form a chain
                       (chain-source (if (equal concept parent) parent next-child))
                       (chain-target (if (equal concept parent) next-child parent)))
                  
                  (when (or (equal child next-parent) 
                            (equal child next-child))
                    ;; We can form a deduction - apply PLN rules
                    (let* ((new-tv (aichat-pln-deduction 
                                   (aichat-symbolic-atom-tv match)
                                   (aichat-symbolic-atom-tv next-match)))
                           (new-atom (aichat-symbolic-inheritance 
                                     chain-source chain-target new-tv)))
                      (push new-atom inferences))))))))
        
        (with-current-buffer buffer
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Reasoning about %s\n\n" concept-name))
          
          (if inferences
              (progn
                (insert (format "* Inferred %d new relationships\n" (length inferences)))
                (dolist (inference inferences)
                  (let* ((links (aichat-symbolic-atom-links inference))
                         (parent (car links))
                         (child (cadr links))
                         (parent-name (aichat-symbolic-atom-name parent))
                         (child-name (aichat-symbolic-atom-name child))
                         (tv (aichat-symbolic-atom-tv inference))
                         (mean (aichat-pln-tv-mean tv))
                         (confidence (aichat-pln-tv-confidence tv)))
                    
                    (insert (format "** %s → %s\n" parent-name child-name))
                    (insert "   :PROPERTIES:\n")
                    (insert (format "   :STRENGTH: %.3f\n" mean))
                    (insert (format "   :CONFIDENCE: %.3f\n" confidence))
                    (insert "   :END:\n"))))
            (insert "* No new inferences found\n")))
        
        (display-buffer buffer)))))

(defun aichat-opencog-org-visualize-attention-flow ()
  "Visualize attention flow in the knowledge base."
  (interactive)
  (let ((buffer (get-buffer-create "*OpenCog-Attention*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Attention Flow Visualization\n\n")
      
      ;; Update attention with ECAN dynamics
      (aichat-ecan-update aichat-opencog-kb)
      
      ;; Get top attention atoms
      (let ((atoms '()))
        (maphash (lambda (k v) (push v atoms)) aichat-opencog-kb)
        (setq atoms 
              (sort atoms 
                    (lambda (a b) 
                      (> (aichat-ecan-av-sti (aichat-ecan-get-av a))
                         (aichat-ecan-av-sti (aichat-ecan-get-av b))))))
        
        ;; Show attention distribution
        (insert "* Current Attention Distribution\n")
        (cl-loop for atom in atoms
                 for i from 0 below 20
                 do (let* ((av (aichat-ecan-get-av atom))
                          (sti (aichat-ecan-av-sti av))
                          (name (aichat-symbolic-atom-name atom))
                          (type (aichat-symbolic-atom-type atom)))
                      (insert (format "** %s (%s) %s\n" 
                                     name
                                     type
                                     (propertize (aichat-opencog-org--attention-star sti)
                                                'face (aichat-opencog-org--attention-color sti))))
                      (insert "   :PROPERTIES:\n")
                      (insert (format "   :STI: %.3f\n" sti))
                      (insert (format "   :LTI: %.3f\n" (aichat-ecan-av-lti av)))
                      (insert (format "   :VLTI: %s\n" (if (aichat-ecan-av-vlti av) "t" "nil")))
                      (insert "   :END:\n"))))
      
      ;; Simulate attention flow
      (insert "\n* Simulated Attention Flow\n")
      (insert "Simulating attention spreading for 3 steps:\n\n")
      
      (dotimes (i 3)
        (insert (format "** Step %d\n" (1+ i)))
        (aichat-ecan-spread-importance aichat-opencog-kb)
        (aichat-ecan-update aichat-opencog-kb)
        
        ;; Show top 5 concepts after this update
        (let ((top-atoms '()))
          (maphash (lambda (k v) 
                    (when (eq (aichat-symbolic-atom-type v) 'concept)
                      (push v top-atoms))) 
                   aichat-opencog-kb)
          (setq top-atoms 
                (sort top-atoms 
                      (lambda (a b) 
                        (> (aichat-ecan-av-sti (aichat-ecan-get-av a))
                           (aichat-ecan-av-sti (aichat-ecan-get-av b))))))
          
          ;; Show top 5 concepts
          (cl-loop for atom in top-atoms
                   for j from 0 below 5
                   do (let* ((av (aichat-ecan-get-av atom))
                            (sti (aichat-ecan-av-sti av))
                            (name (aichat-symbolic-atom-name atom)))
                        (insert (format "   - %s %s %s\n" 
                                       name
                                       (aichat-opencog-org--attention-star sti)
                                       (propertize (format "(%.2f)" sti)
                                                  'face (aichat-opencog-org--attention-color sti)))))))))
    
    (display-buffer buffer)))

;; Define a minor mode for org-opencog integration
(define-minor-mode aichat-opencog-org-mode
  "Toggle OpenCog Org integration mode.
With a prefix argument ARG, enable OpenCog Org mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil."
  :init-value nil
  :lighter " OpenCog"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c o v") 'aichat-opencog-org-visualize-attention-flow)
            (define-key map (kbd "C-c o q") 'aichat-opencog-org-query-related-concepts)
            (define-key map (kbd "C-c o r") 'aichat-opencog-org-reason-about-concept)
            (define-key map (kbd "C-c o k") 'aichat-opencog-org-heading-to-kb)
            (define-key map (kbd "C-c o b") 'aichat-opencog-org-buffer-to-kb)
            (define-key map (kbd "C-c o o") 'aichat-opencog-org-kb-to-buffer)
            (define-key map (kbd "C-c o a") 'aichat-opencog-org-toggle-auto-sync)
            map)
  (if aichat-opencog-org-mode
      (progn
        (message "OpenCog Org mode enabled")
        (when aichat-opencog-org-auto-sync
          (add-hook 'org-after-todo-state-change-hook 
                    #'aichat-opencog-org-heading-to-kb nil t)))
    (remove-hook 'org-after-todo-state-change-hook 
                 #'aichat-opencog-org-heading-to-kb t)))

(provide 'aichat-opencog-org)