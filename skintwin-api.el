;;; skintwin-api.el --- API functions for SkinTwin -*- lexical-binding: t; -*-

(require 'skintwin-db)
(require 'aichat-symbolic)
(require 'aichat-ecan)
(require 'aichat-opencog)
(require 'aichat-pln)

(defgroup skintwin-api nil
  "API functions for the SkinTwin dermatological model."
  :group 'skintwin
  :prefix "skintwin-api-")

;; User-facing API functions

;;;###autoload
(defun skintwin-query-treatments (condition-name)
  "Query treatments for CONDITION-NAME.
Returns a list of treatment recommendations sorted by efficacy."
  (interactive "sEnter condition name: ")
  (let* ((condition (aichat-opencog-kb-get condition-name))
         (buffer (get-buffer-create "*SkinTwin-Results*")))
    
    (if (null condition)
        (progn
          (message "Condition '%s' not found. Syncing with database..." condition-name)
          (skintwin-db-sync-skin-conditions)
          (skintwin-db-sync-treatments)
          (setq condition (aichat-opencog-kb-get condition-name)))
      
      ;; Stimulate the condition to focus attention
      (aichat-ecan-stimulate condition 0.9))
    
    (if (null condition)
        (message "Condition '%s' not found after syncing with database." condition-name)
      
      ;; Look for treatment relationships
      (let* ((treats-pred (aichat-symbolic-predicate "treats"))
             (potentially-treats-pred (aichat-symbolic-predicate "potentially_treats"))
             (may-treat-pred (aichat-symbolic-predicate "may_treat"))
             (direct-treatments (aichat-opencog-match-pattern
                                (aichat-symbolic-evaluation 
                                 treats-pred (list nil condition) nil)
                                aichat-opencog-kb))
             (potential-treatments (aichat-opencog-match-pattern
                                   (aichat-symbolic-evaluation 
                                    potentially-treats-pred (list nil condition) nil)
                                   aichat-opencog-kb))
             (possible-treatments (aichat-opencog-match-pattern
                                  (aichat-symbolic-evaluation 
                                   may-treat-pred (list nil condition) nil)
                                  aichat-opencog-kb)))
        
        (with-current-buffer buffer
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Treatments for %s\n\n" condition-name))
          
          ;; Display direct treatments
          (insert "* Established Treatments\n")
          (if direct-treatments
              (progn
                ;; Sort treatments by efficacy
                (setq direct-treatments
                      (sort direct-treatments
                            (lambda (a b)
                              (> (aichat-pln-tv-mean (aichat-symbolic-atom-tv a))
                                 (aichat-pln-tv-mean (aichat-symbolic-atom-tv b))))))
                
                (dolist (treat direct-treatments)
                  (let* ((links (aichat-symbolic-atom-links treat))
                         (treatment (car (cdr (car links))))
                         (treatment-name (aichat-symbolic-atom-name treatment))
                         (tv (aichat-symbolic-atom-tv treat))
                         (efficacy (aichat-pln-tv-mean tv))
                         (confidence (aichat-pln-tv-confidence tv)))
                    (insert (format "** %s\n" treatment-name))
                    (insert "   :PROPERTIES:\n")
                    (insert (format "   :EFFICACY: %.2f\n" efficacy))
                    (insert (format "   :CONFIDENCE: %.2f\n" confidence))
                    (insert "   :END:\n")
                    
                    ;; Find mechanism of action
                    (let* ((mechanism-pred (aichat-symbolic-predicate "has_mechanism"))
                           (mechanism-pattern (aichat-symbolic-evaluation
                                             mechanism-pred (list treatment nil) nil))
                           (mechanisms (aichat-opencog-match-pattern
                                       mechanism-pattern aichat-opencog-kb)))
                      (when mechanisms
                        (insert "   + Mechanisms of action:\n")
                        (dolist (mech mechanisms)
                          (let* ((mech-links (aichat-symbolic-atom-links mech))
                                 (mech-name (aichat-symbolic-atom-name (cadr mech-links))))
                            (insert (format "     - %s\n" mech-name)))))))))
            (insert "   No established treatments found.\n"))
          
          ;; Display potential treatments
          (insert "\n* Potential Treatments\n")
          (if (or potential-treatments possible-treatments)
              (progn
                ;; Sort by efficacy
                (setq potential-treatments
                      (sort (append potential-treatments possible-treatments)
                            (lambda (a b)
                              (> (aichat-pln-tv-mean (aichat-symbolic-atom-tv a))
                                 (aichat-pln-tv-mean (aichat-symbolic-atom-tv b))))))
                
                (dolist (treat potential-treatments)
                  (let* ((links (aichat-symbolic-atom-links treat))
                         (treatment (car (cdr (car links))))
                         (treatment-name (aichat-symbolic-atom-name treatment))
                         (tv (aichat-symbolic-atom-tv treat))
                         (efficacy (aichat-pln-tv-mean tv))
                         (confidence (aichat-pln-tv-confidence tv)))
                    (insert (format "** %s\n" treatment-name))
                    (insert "   :PROPERTIES:\n")
                    (insert (format "   :PREDICTED_EFFICACY: %.2f\n" efficacy))
                    (insert (format "   :CONFIDENCE: %.2f\n" confidence))
                    (insert "   :END:\n")
                    
                    ;; Find mechanism of action
                    (let* ((mechanism-pred (aichat-symbolic-predicate "has_mechanism"))
                           (mechanism-pattern (aichat-symbolic-evaluation
                                             mechanism-pred (list treatment nil) nil))
                           (mechanisms (aichat-opencog-match-pattern
                                       mechanism-pattern aichat-opencog-kb)))
                      (when mechanisms
                        (insert "   + Mechanisms of action:\n")
                        (dolist (mech mechanisms)
                          (let* ((mech-links (aichat-symbolic-atom-links mech))
                                 (mech-name (aichat-symbolic-atom-name (cadr mech-links))))
                            (insert (format "     - %s\n" mech-name)))))))))
            (insert "   No potential treatments found.\n"))
          
          ;; Run PLN reasoning to generate additional insights
          (insert "\n* Reasoning About Treatments\n")
          (let* ((inferences (aichat-pln-forward-chain aichat-opencog-kb 1))
                 (relevant-inferences (cl-remove-if-not
                                      (lambda (inf)
                                        (and (eq (aichat-symbolic-atom-type inf) 'evaluation)
                                             (let ((links (aichat-symbolic-atom-links inf)))
                                               (and (>= (length links) 2)
                                                    (cadr links)
                                                    (aichat-symbolic-atom-links (cadr links))
                                                    (equal (aichat-symbolic-atom-name 
                                                           (cadr (aichat-symbolic-atom-links (cadr links))))
                                                           condition-name)))))
                                      inferences)))
            (if relevant-inferences
                (dolist (inf relevant-inferences)
                  (let* ((links (aichat-symbolic-atom-links inf))
                         (pred (car links))
                         (args (cadr links))
                         (pred-name (aichat-symbolic-atom-name pred))
                         (treatment-name (aichat-symbolic-atom-name (car (aichat-symbolic-atom-links args))))
                         (tv (aichat-symbolic-atom-tv inf))
                         (confidence (aichat-pln-tv-confidence tv)))
                    (insert (format "** Inferred: %s %s\n" treatment-name pred-name))
                    (insert (format "   Confidence: %.2f\n" confidence))))
              (insert "   No additional inferences available.\n")))))
      
      (display-buffer buffer))))

;;;###autoload
(defun skintwin-analyze-patient (patient-id)
  "Analyze patient with PATIENT-ID."
  (interactive "sPatient ID: ")
  (let* ((patient (aichat-opencog-kb-get patient-id))
         (buffer (get-buffer-create "*SkinTwin-Patient-Analysis*")))
    
    (if (null patient)
        (progn
          (message "Patient '%s' not found. Syncing with database..." patient-id)
          (skintwin-db-sync-all)
          (setq patient (aichat-opencog-kb-get patient-id)))
      
      ;; Stimulate the patient to focus attention
      (aichat-ecan-stimulate patient 0.9))
    
    (if (null patient)
        (message "Patient '%s' not found after syncing with database." patient-id)
      
      ;; Get patient data
      (let* ((has-condition-pred (aichat-symbolic-predicate "has_condition"))
             (has-condition-pattern (aichat-symbolic-evaluation 
                                    has-condition-pred (list patient nil) nil))
             (conditions (aichat-opencog-match-pattern
                         has-condition-pattern aichat-opencog-kb))
             (properties '()))
        
        ;; Gather all patient properties
        (maphash (lambda (k v)
                   (when (and (eq (aichat-symbolic-atom-type v) 'evaluation)
                            (let ((links (aichat-symbolic-atom-links v)))
                              (and (>= (length links) 2)
                                   (equal (aichat-symbolic-atom-name (car links))
                                          (aichat-symbolic-atom-name patient)))))
                     (push v properties)))
                 aichat-opencog-kb)
        
        (with-current-buffer buffer
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Patient Analysis: %s\n\n" patient-id))
          
          ;; Display patient information
          (insert "* Patient Information\n")
          (dolist (prop properties)
            (let* ((links (aichat-symbolic-atom-links prop))
                   (pred (car links))
                   (pred-name (aichat-symbolic-atom-name pred))
                   (value (cadr links)))
              (unless (string= pred-name "has_condition")
                (insert (format "** %s: %s\n" 
                               (capitalize (replace-regexp-in-string "_" " " pred-name))
                               (if (aichat-symbolic-atom-p value)
                                   (aichat-symbolic-atom-name value)
                                 value))))))
          
          ;; Display conditions
          (insert "\n* Conditions\n")
          (if conditions
              (dolist (cond-rel conditions)
                (let* ((links (aichat-symbolic-atom-links cond-rel))
                       (condition (cadr links))
                       (condition-name (aichat-symbolic-atom-name condition))
                       (tv (aichat-symbolic-atom-tv cond-rel))
                       (severity (aichat-pln-tv-mean tv))
                       (confidence (aichat-pln-tv-confidence tv)))
                  (insert (format "** %s\n" condition-name))
                  (insert "   :PROPERTIES:\n")
                  (insert (format "   :SEVERITY: %.2f\n" severity))
                  (insert (format "   :CONFIDENCE: %.2f\n" confidence))
                  (insert "   :END:\n")
                  
                  ;; Find affected areas
                  (let* ((affects-pred (aichat-symbolic-predicate "affects"))
                         (affects-pattern (aichat-symbolic-evaluation
                                         affects-pred (list condition nil) nil))
                         (affected-areas (aichat-opencog-match-pattern
                                         affects-pattern aichat-opencog-kb)))
                    (when affected-areas
                      (insert "   + Affected areas:\n")
                      (dolist (area affected-areas)
                        (let* ((area-links (aichat-symbolic-atom-links area))
                               (area-concept (cadr area-links))
                               (area-name (aichat-symbolic-atom-name area-concept)))
                          (insert (format "     - %s\n" area-name))))))
                  
                  ;; Find recommended treatments
                  (let* ((treats-pred (aichat-symbolic-predicate "treats"))
                         (treats-pattern (aichat-symbolic-evaluation
                                         treats-pred (list nil condition) nil))
                         (treatments (aichat-opencog-match-pattern
                                     treats-pattern aichat-opencog-kb)))
                    (if treatments
                        (progn
                          (insert "   + Recommended treatments:\n")
                          (dolist (treat treatments)
                            (let* ((treat-links (aichat-symbolic-atom-links treat))
                                   (treatment (car (cdr (car treat-links))))
                                   (treatment-name (aichat-symbolic-atom-name treatment))
                                   (treat-tv (aichat-symbolic-atom-tv treat))
                                   (efficacy (aichat-pln-tv-mean treat-tv)))
                              (insert (format "     - %s (efficacy: %.2f)\n" 
                                            treatment-name efficacy)))))
                      (insert "   + No specific treatments found.\n")))))
            (insert "   No conditions recorded for this patient.\n"))
          
          ;; Run reasoning for personalized insights
          (insert "\n* Personalized Insights\n")
          (let* ((age-prop (cl-find-if 
                           (lambda (p) 
                             (string= (aichat-symbolic-atom-name (car (aichat-symbolic-atom-links p)))
                                     "age")) 
                           properties))
                 (age (and age-prop (cadr (aichat-symbolic-atom-links age-prop))))
                 (gender-prop (cl-find-if 
                              (lambda (p) 
                                (string= (aichat-symbolic-atom-name (car (aichat-symbolic-atom-links p)))
                                        "gender")) 
                              properties))
                 (gender (and gender-prop (aichat-symbolic-atom-name (cadr (aichat-symbolic-atom-links gender-prop))))))
            
            (when (or age gender)
              (insert "** Demographics-based Considerations\n")
              (when age
                (insert (format "   + Age: %s\n" age))
                (cond
                 ((and (numberp age) (< age 12))
                  (insert "     - Pediatric considerations apply\n")
                  (insert "     - May need adjusted dosing for treatments\n")
                  (insert "     - Consider involving pediatric specialists\n"))
                 ((and (numberp age) (> age 65))
                  (insert "     - Geriatric considerations apply\n")
                  (insert "     - Monitor for increased skin fragility\n")
                  (insert "     - Watch for medication interactions\n"))))
              
              (when gender
                (insert (format "   + Gender: %s\n" gender))
                (cond
                 ((string= (downcase gender) "female")
                  (insert "     - Consider potential hormonal influences\n")
                  (if (member "acne" (mapcar (lambda (c) 
                                              (aichat-symbolic-atom-name 
                                               (cadr (aichat-symbolic-atom-links c))))
                                            conditions))
                      (insert "     - For acne, consider evaluation for PCOS if indicated\n")))
                 ((string= (downcase gender) "male")
                  (insert "     - May have thicker skin requiring adjusted treatment approaches\n")))))
            
            ;; Comorbidity analysis
            (when (> (length conditions) 1)
              (insert "** Comorbidity Analysis\n")
              (insert "   + Multiple skin conditions detected\n")
              (insert "   + Consider interactions between treatment approaches\n")
              (insert "   + Monitor for overlap in affected areas\n")))
          
          ;; Generate treatment plan
          (insert "\n* Suggested Treatment Approach\n")
          (if conditions
              (progn
                (dolist (cond-rel conditions)
                  (let* ((links (aichat-symbolic-atom-links cond-rel))
                         (condition (cadr links))
                         (condition-name (aichat-symbolic-atom-name condition))
                         (tv (aichat-symbolic-atom-tv cond-rel))
                         (severity (aichat-pln-tv-mean tv)))
                    (insert (format "** For %s (Severity: %.2f)\n" condition-name severity))
                    
                    ;; Find recommended treatments
                    (let* ((treats-pred (aichat-symbolic-predicate "treats"))
                           (treats-pattern (aichat-symbolic-evaluation
                                           treats-pred (list nil condition) nil))
                           (treatments (aichat-opencog-match-pattern
                                       treats-pattern aichat-opencog-kb)))
                      (if treatments
                          (progn
                            ;; Sort treatments by efficacy
                            (setq treatments 
                                  (sort treatments
                                        (lambda (a b)
                                          (> (aichat-pln-tv-mean (aichat-symbolic-atom-tv a))
                                             (aichat-pln-tv-mean (aichat-symbolic-atom-tv b))))))
                            
                            (dolist (treat treatments)
                              (let* ((treat-links (aichat-symbolic-atom-links treat))
                                     (treatment (car (cdr (car treat-links))))
                                     (treatment-name (aichat-symbolic-atom-name treatment))
                                     (treat-tv (aichat-symbolic-atom-tv treat))
                                     (efficacy (aichat-pln-tv-mean treat-tv)))
                                (insert (format "   + %s\n" treatment-name))
                                (insert (format "     - Efficacy: %.2f\n" efficacy))
                                
                                ;; Adjust recommendation based on severity
                                (cond
                                 ((< severity 0.3)
                                  (insert "     - Recommended approach: Mild/conservative treatment\n"))
                                 ((< severity 0.7)
                                  (insert "     - Recommended approach: Moderate treatment intensity\n"))
                                 (t
                                  (insert "     - Recommended approach: Aggressive treatment recommended\n"))))))
                        (insert "   + No specific treatments found in database.\n")))))
                
                ;; General recommendations
                (insert "\n** General Recommendations\n")
                (insert "   + Monitor response to treatment\n")
                (insert "   + Follow up in 2-4 weeks to assess progress\n")
                (insert "   + Consider lifestyle modifications such as:\n")
                (insert "     - Gentle skin care routine\n")
                (insert "     - Avoiding known irritants\n")
                (insert "     - Maintaining adequate hydration\n"))
            (insert "   No conditions recorded for this patient.\n")))))
      
      (display-buffer buffer))))

;;;###autoload
(defun skintwin-predict-progression (condition-name treatment-name steps)
  "Predict progression of CONDITION-NAME with TREATMENT-NAME over STEPS."
  (interactive (list 
                (read-string "Condition name: ")
                (read-string "Treatment name: ")
                (read-number "Number of time steps: " 10)))
  (require 'skintwin-esn)
  (let* ((condition (aichat-opencog-kb-get condition-name))
         (treatment (aichat-opencog-kb-get treatment-name))
         (buffer (get-buffer-create "*SkinTwin-Progression*")))
    
    (if (or (null condition) (null treatment))
        (progn
          (message "Condition or treatment not found. Syncing with database...")
          (skintwin-db-sync-all)
          (setq condition (aichat-opencog-kb-get condition-name))
          (setq treatment (aichat-opencog-kb-get treatment-name))))
    
    (if (or (null condition) (null treatment))
        (message "Condition or treatment not found after syncing with database.")
      
      ;; Get treatment efficacy
      (let* ((treats-pred (aichat-symbolic-predicate "treats"))
             (treats-pattern (aichat-symbolic-evaluation 
                             treats-pred (list treatment condition) nil))
             (treats-rel (car (aichat-opencog-match-pattern
                              treats-pattern aichat-opencog-kb))))
        
        (with-current-buffer buffer
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Predicted Progression: %s with %s\n\n" 
                         condition-name treatment-name))
          
          (if treats-rel
              (let* ((tv (aichat-symbolic-atom-tv treats-rel))
                     (efficacy (aichat-pln-tv-mean tv))
                     (confidence (aichat-pln-tv-confidence tv))
                     (model (skintwin-esn-create condition treatment))
                     (progression (skintwin-esn-predict-progression model steps)))
                
                ;; Display overview
                (insert "* Treatment Overview\n")
                (insert (format "** Condition: %s\n" condition-name))
                (insert (format "** Treatment: %s\n" treatment-name))
                (insert (format "** Efficacy: %.2f\n" efficacy))
                (insert (format "** Confidence: %.2f\n\n" confidence))
                
                ;; Display progression data
                (insert "* Progression Data\n")
                (insert "| Time | Severity | Visualization |\n")
                (insert "|------|----------|---------------|\n")
                
                (let ((time 0))
                  (dolist (severity progression)
                    (let* ((bar-length (ceiling (* 20 severity)))
                           (bar (make-string bar-length ?#)))
                      (insert (format "| %4d | %8.2f | %s |\n" 
                                     time severity bar)))
                    (setq time (1+ time))))
                
                ;; Create ASCII chart
                (insert "\n* Visual Representation\n\n")
                (insert "```\nSeverity\n^\n")
                (dotimes (i 10)
                  (let ((level (- 1.0 (* i 0.1))))
                    (insert (format "%3.1f |" level))
                    (dotimes (j (length progression))
                      (let ((severity (nth j progression)))
                        (if (>= severity (- level 0.05))
                            (insert "*")
                          (insert " "))))
                    (insert "\n")))
                (insert "     +")
                (dotimes (i (length progression))
                  (insert "-"))
                (insert "> Time\n```\n\n")
                
                ;; Analysis and interpretation
                (insert "* Clinical Interpretation\n\n")
                (let* ((initial-severity (car progression))
                       (final-severity (car (last progression)))
                       (improvement-pct (* 100 (/ (- initial-severity final-severity)
                                                 initial-severity))))
                  (insert (format "** Expected Improvement: %.1f%%\n\n" improvement-pct))
                  
                  (cond
                   ((> improvement-pct 70)
                    (insert "- **Excellent response** expected from this treatment\n")
                    (insert "- Consider maintenance therapy after initial course\n")
                    (insert "- Monitor for potential relapse\n"))
                   ((> improvement-pct 40)
                    (insert "- **Good response** expected from this treatment\n")
                    (insert "- May need extended course for optimal results\n")
                    (insert "- Consider combination therapy for enhanced efficacy\n"))
                   ((> improvement-pct 20)
                    (insert "- **Moderate response** expected from this treatment\n")
                    (insert "- Consider alternative or adjunctive treatments\n")
                    (insert "- Close monitoring recommended\n"))
                   (t
                    (insert "- **Limited response** expected from this treatment\n")
                    (insert "- Alternative treatment approaches should be considered\n")
                    (insert "- Referral to specialist may be warranted\n")))))
            
            ;; No direct treatment relationship found
            (insert "* No Direct Treatment Data Available\n\n")
            (insert "Treatment efficacy data not available for this specific combination.\n")
            (insert "Consider:\n")
            (insert "- Consulting clinical guidelines\n")
            (insert "- Reviewing literature for similar cases\n")
            (insert "- Using alternative treatments with known efficacy\n"))))
      
      (display-buffer buffer))))

;;;###autoload
(defun skintwin-visualize-graph (focus-concept depth)
  "Visualize the knowledge graph around FOCUS-CONCEPT with DEPTH."
  (interactive (list
                (read-string "Focus concept: ")
                (read-number "Depth (1-3): " 2)))
  (let* ((concept (aichat-opencog-kb-get focus-concept))
         (buffer (get-buffer-create "*SkinTwin-Graph*")))
    
    (if (null concept)
        (progn
          (message "Concept '%s' not found. Syncing with database..." focus-concept)
          (skintwin-db-sync-all)
          (setq concept (aichat-opencog-kb-get focus-concept))))
    
    (if (null concept)
        (message "Concept '%s' not found after syncing with database." focus-concept)
      
      ;; Stimulate the concept to focus attention
      (aichat-ecan-stimulate concept 0.9)
      
      ;; Collect nodes and edges
      (let ((nodes (list concept))
            (edges '())
            (visited (make-hash-table :test 'equal)))
        
        ;; Helper to add nodes and edges
        (cl-labels ((explore (node current-depth)
                            (when (and node (> current-depth 0))
                              (puthash (aichat-symbolic-atom-name node) t visited)
                              
                              ;; Find all related atoms
                              (maphash (lambda (k v)
                                         (when (eq (aichat-symbolic-atom-type v) 'evaluation)
                                           (let* ((links (aichat-symbolic-atom-links v))
                                                 (pred (car links))
                                                 (args (cdr links)))
                                             (when (and (>= (length args) 1)
                                                      (or (equal (aichat-symbolic-atom-name (car args))
                                                               (aichat-symbolic-atom-name node))
                                                          (and (cdr args)
                                                               (equal (aichat-symbolic-atom-name (cadr args))
                                                                     (aichat-symbolic-atom-name node)))))
                                               (let* ((pred-name (aichat-symbolic-atom-name pred))
                                                     (other-node (if (equal (aichat-symbolic-atom-name (car args))
                                                                          (aichat-symbolic-atom-name node))
                                                                    (cadr args)
                                                                  (car args))))
                                                 (when (and other-node 
                                                          (not (gethash (aichat-symbolic-atom-name other-node) visited)))
                                                   (push other-node nodes)
                                                   (push (list node pred-name other-node) edges)
                                                   (explore other-node (1- current-depth))))))))
                                       aichat-opencog-kb)
                              
                              ;; Find inheritance relationships
                              (maphash (lambda (k v)
                                         (when (eq (aichat-symbolic-atom-type v) 'inheritance)
                                           (let* ((links (aichat-symbolic-atom-links v))
                                                 (parent (car links))
                                                 (child (cadr links)))
                                             (when (or (equal (aichat-symbolic-atom-name parent)
                                                           (aichat-symbolic-atom-name node))
                                                      (equal (aichat-symbolic-atom-name child)
                                                           (aichat-symbolic-atom-name node)))
                                               (let ((other-node (if (equal (aichat-symbolic-atom-name parent)
                                                                         (aichat-symbolic-atom-name node))
                                                                  child
                                                                parent)))
                                                 (when (and other-node 
                                                          (not (gethash (aichat-symbolic-atom-name other-node) visited)))
                                                   (push other-node nodes)
                                                   (if (equal (aichat-symbolic-atom-name parent)
                                                            (aichat-symbolic-atom-name node))
                                                       (push (list node "type_of" other-node) edges)
                                                     (push (list other-node "type_of" node) edges))
                                                   (explore other-node (1- current-depth))))))))
                                       aichat-opencog-kb))))
          
          ;; Start exploration from focus concept
          (explore concept depth))
        
        (with-current-buffer buffer
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Knowledge Graph: %s\n\n" focus-concept))
          
          ;; Display nodes with attention values
          (insert "* Nodes\n")
          (insert "| Concept | Type | Attention |\n")
          (insert "|---------|------|----------|\n")
          (dolist (node nodes)
            (let* ((name (aichat-symbolic-atom-name node))
                   (type (aichat-symbolic-atom-type node))
                   (av (aichat-ecan-get-av node))
                   (sti (aichat-ecan-av-sti av)))
              (insert (format "| %s | %s | %.2f |\n" name type sti))))
          
          ;; Display edges
          (insert "\n* Relationships\n")
          (insert "| From | Relation | To |\n")
          (insert "|------|----------|----|\n")
          (dolist (edge edges)
            (let* ((from (aichat-symbolic-atom-name (car edge)))
                   (rel (cadr edge))
                   (to (aichat-symbolic-atom-name (caddr edge))))
              (insert (format "| %s | %s | %s |\n" from rel to))))
          
          ;; Graph visualization (ASCII)
          (insert "\n* Graph Visualization\n\n")
          (insert "```\n")
          
          ;; Simple ASCII visualization
          (let* ((center focus-concept)
                 (direct-relations (cl-remove-if-not
                                   (lambda (e)
                                     (or (string= (aichat-symbolic-atom-name (car e)) center)
                                         (string= (aichat-symbolic-atom-name (caddr e)) center)))
                                   edges)))
            
            ;; Center node
            (insert (format "        [%s]\n" center))
            (insert "          |\n")
            
            ;; Direct relations
            (dolist (rel direct-relations)
              (let* ((from (aichat-symbolic-atom-name (car rel)))
                     (relation (cadr rel))
                     (to (aichat-symbolic-atom-name (caddr rel)))
                     (direction (if (string= from center) "→" "←"))
                     (other-node (if (string= from center) to from)))
                (insert (format "          |-%s-[%s]--[%s]\n" 
                               relation direction other-node)))))
          
          (insert "```\n\n")
          
          ;; Run and display ECAN attention spreading
          (insert "* Attention Dynamics\n\n")
          (aichat-ecan-spread-importance aichat-opencog-kb)
          
          ;; Show top attention concepts after spreading
          (let ((sorted-nodes (sort (copy-sequence nodes)
                                   (lambda (a b)
                                     (> (aichat-ecan-av-sti (aichat-ecan-get-av a))
                                        (aichat-ecan-av-sti (aichat-ecan-get-av b)))))))
            (insert "| Concept | Initial Attention | After Spreading |\n")
            (insert "|---------|------------------|------------------|\n")
            (dolist (node sorted-nodes)
              (let* ((name (aichat-symbolic-atom-name node))
                     (av-before (aichat-ecan-get-av node))
                     (sti-before (aichat-ecan-av-sti av-before)))
                
                ;; Spread importance
                (aichat-ecan-spread-importance aichat-opencog-kb)
                (let* ((av-after (aichat-ecan-get-av node))
                       (sti-after (aichat-ecan-av-sti av-after)))
                  (insert (format "| %s | %.2f | %.2f |\n" 
                                 name sti-before sti-after))))))))
      
      (display-buffer buffer))))

;;;###autoload
(defun skintwin-dashboard ()
  "Display the SkinTwin dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*SkinTwin-Dashboard*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: SkinTwin Dashboard\n\n")
      
      ;; Overview section
      (insert "* SkinTwin: OpenCog-Based Dermatological Model\n\n")
      (insert "SkinTwin is a multiscale dermatological model powered by OpenCog's cognitive architecture components.\n")
      (insert "It integrates various modules to represent skin biology, environmental factors, and clinical outcomes\n")
      (insert "through a comprehensive knowledge graph and reasoning system.\n\n")
      
      ;; Component summary
      (insert "** Core Components\n\n")
      (insert "| Component | OpenCog Equivalent | Description |\n")
      (insert "|-----------|-------------------|-------------|\n")
      (insert "| DermatoGraph | AtomSpace | Knowledge representation system |\n")
      (insert "| SensoryFocus | ECAN | Attention allocation system |\n")
      (insert "| DermatoLogic | PLN | Probabilistic reasoning system |\n")
      (insert "| EpidermiLearn | MOSES | Pattern mining and model building |\n")
      (insert "| RuleDerm | URE | Rule-based reasoning system |\n")
      
      ;; Key features
      (insert "\n** Key Features\n\n")
      (insert "- Multi-scale representation of skin biology\n")
      (insert "- Integration of molecular, cellular, and tissue-level data\n")
      (insert "- Predictive analytics for treatment outcomes\n")
      (insert "- Learning from clinical and experimental data\n")
      (insert "- Dynamic attention allocation to relevant information\n\n")
      
      ;; Database connection status
      (insert "* Database Connection\n\n")
      (if (and skintwin-db-supabase-url skintwin-db-supabase-key)
          (insert "✅ Connected to Supabase\n")
        (insert "❌ Not connected to Supabase. Run `skintwin-db-initialize` to connect.\n"))
      
      (insert (format "- Last synchronization: %s\n\n" 
                     (if skintwin-db--last-sync-time
                         (format-time-string "%Y-%m-%d %H:%M:%S" skintwin-db--last-sync-time)
                       "Never")))
      
      ;; Actions
      (insert "* Actions\n\n")
      (insert "| Command | Description | Keybinding |\n")
      (insert "|---------|-------------|------------|\n")
      (insert "| skintwin-query-treatments | Query treatments for a condition | C-c s q |\n")
      (insert "| skintwin-analyze-patient | Analyze a patient | C-c s a |\n")
      (insert "| skintwin-predict-progression | Predict disease progression | C-c s p |\n")
      (insert "| skintwin-visualize-graph | Visualize knowledge graph | C-c s v |\n")
      (insert "| skintwin-db-sync-all | Synchronize with database | C-c s d s |\n")
      
      ;; Knowledge Base stats
      (insert "\n* Knowledge Base Statistics\n\n")
      (let ((atom-count 0)
            (concept-count 0)
            (inheritance-count 0)
            (evaluation-count 0))
        
        (maphash (lambda (k v)
                   (cl-incf atom-count)
                   (case (aichat-symbolic-atom-type v)
                     ('concept (cl-incf concept-count))
                     ('inheritance (cl-incf inheritance-count))
                     ('evaluation (cl-incf evaluation-count))))
                 aichat-opencog-kb)
        
        (insert (format "- Total atoms: %d\n" atom-count))
        (insert (format "- Concepts: %d\n" concept-count))
        (insert (format "- Inheritance relationships: %d\n" inheritance-count))
        (insert (format "- Evaluation links: %d\n\n" evaluation-count)))
      
      ;; Getting started
      (insert "* Getting Started\n\n")
      (insert "1. Initialize the database connection with `M-x skintwin-db-initialize`\n")
      (insert "2. Synchronize data from the database with `M-x skintwin-db-sync-all`\n")
      (insert "3. Query treatments for a condition with `M-x skintwin-query-treatments`\n")
      (insert "4. Analyze a patient with `M-x skintwin-analyze-patient`\n")
      (insert "5. Predict disease progression with `M-x skintwin-predict-progression`\n")
      (insert "6. Visualize the knowledge graph with `M-x skintwin-visualize-graph`\n"))
    
    (display-buffer buffer)))

(provide 'skintwin-api)
;;; skintwin-api.el ends here