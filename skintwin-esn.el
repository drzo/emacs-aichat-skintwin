;;; skintwin-esn.el --- Echo State Network enhancements for SkinTwin -*- lexical-binding: t; -*-

(require 'aichat-esn)
(require 'aichat-symbolic)
(require 'aichat-opencog)
(require 'aichat-util)

(defgroup skintwin-esn nil
  "Echo State Network enhancements for SkinTwin."
  :group 'skintwin
  :prefix "skintwin-esn-")

;; Configuration options
(defcustom skintwin-esn-reservoir-size 150
  "Size of the reservoir for skin condition progression prediction."
  :type 'integer
  :group 'skintwin-esn)

(defcustom skintwin-esn-connectivity 0.15
  "Connectivity fraction for the ESN reservoir (0.0 to 1.0)."
  :type 'float
  :group 'skintwin-esn)

(defcustom skintwin-esn-spectral-radius 0.85
  "Spectral radius for ESN reservoir weight scaling."
  :type 'float
  :group 'skintwin-esn)

(defcustom skintwin-esn-input-scaling 1.0
  "Scaling factor for input signals to the ESN."
  :type 'float
  :group 'skintwin-esn)

(defcustom skintwin-esn-output-feedback 0.0
  "Strength of output feedback into the reservoir (0.0 to 1.0)."
  :type 'float
  :group 'skintwin-esn)

(defcustom skintwin-esn-training-length 50
  "Number of time steps to use when training the reservoir."
  :type 'integer
  :group 'skintwin-esn)

;; Enhanced Reservoir State
(cl-defstruct (skintwin-esn-state (:include aichat-esn-state))
  "Enhanced reservoir state structure for SkinTwin."
  condition        ; Target skin condition
  treatment        ; Applied treatment
  initial-severity ; Initial condition severity
  progression      ; History of condition severity
  feedback-weights ; Weights for output feedback
  training-error)  ; Error on training data

;; Core Functions
(defun skintwin-esn-create (condition treatment &optional size)
  "Create enhanced ESN for CONDITION with TREATMENT and optional SIZE."
  (let* ((size (or size skintwin-esn-reservoir-size))
         ;; Override global settings with our custom parameters
         (aichat-esn-reservoir-size size)
         (aichat-esn-connectivity skintwin-esn-connectivity)
         (aichat-esn-spectral-radius skintwin-esn-spectral-radius)
         ;; Create base ESN state
         (base-state (aichat-esn-make-state))
         ;; Create our enhanced state
         (state (make-skintwin-esn-state
                 :input-weights (skintwin-esn-state-input-weights base-state)
                 :reservoir (skintwin-esn-state-reservoir base-state)
                 :output-weights (skintwin-esn-state-output-weights base-state)
                 :state (skintwin-esn-state-state base-state)
                 :condition condition
                 :treatment treatment
                 :initial-severity 0.5
                 :progression '()
                 :feedback-weights (make-vector size 0.0)
                 :training-error 1.0)))
    
    ;; Return the enhanced state
    state))

(defun skintwin-esn-update-with-feedback (state input output)
  "Update ESN STATE with INPUT and previous OUTPUT with feedback."
  (let* ((old-state (skintwin-esn-state-state state))
         (size (length old-state))
         (new-state (make-vector size 0.0))
         (feedback-weights (skintwin-esn-state-feedback-weights state)))
    
    ;; Update reservoir state
    (dotimes (i size)
      (let ((sum 0.0))
        ;; Input contribution
        (setq sum (+ sum (* skintwin-esn-input-scaling
                           (aref (skintwin-esn-state-input-weights state) i)
                           input)))
        
        ;; Reservoir contribution
        (dolist (w (nth i (skintwin-esn-state-reservoir state)))
          (setq sum (+ sum (* (cdr w) (aref old-state (car w))))))
        
        ;; Output feedback contribution
        (when (and output (> skintwin-esn-output-feedback 0.0))
          (setq sum (+ sum (* skintwin-esn-output-feedback
                             (aref feedback-weights i)
                             output))))
        
        ;; Apply tanh activation
        (aset new-state i (tanh sum))))
    
    ;; Update state
    (setf (skintwin-esn-state-state state) new-state)
    
    ;; Calculate output
    (let ((output 0.0))
      (dotimes (i size)
        (setq output (+ output
                       (* (aref new-state i)
                          (aref (skintwin-esn-state-output-weights state) i)))))
      output)))

(defun skintwin-esn-train-for-condition (state data &optional learning-rate)
  "Train ESN on condition progression DATA with optional LEARNING-RATE."
  (let ((rate (or learning-rate 0.05))
        (inputs (mapcar #'car data))
        (outputs (mapcar #'cdr data)))
    
    ;; Initialize the reservoir with the first few inputs
    (dotimes (_ 5)
      (when inputs
        (aichat-esn-update state (car inputs))))
    
    ;; Train output weights with the full dataset
    (cl-loop for input in inputs
             for target in outputs
             for output = (aichat-esn-update state input)
             for error = (- target output)
             for total-error = 0.0
             do (let ((reservoir-state (aichat-esn-state-state state)))
                  ;; Update output weights using simple gradient descent
                  (dotimes (i (length reservoir-state))
                    (let ((delta (* rate error (aref reservoir-state i))))
                      (aset (aichat-esn-state-output-weights state) i
                            (+ (aref (aichat-esn-state-output-weights state) i)
                               delta))))
                  ;; Learn feedback weights too
                  (when (> skintwin-esn-output-feedback 0.0)
                    (dotimes (i (length reservoir-state))
                      (let ((delta (* rate error 0.1)))
                        (aset (skintwin-esn-state-feedback-weights state) i
                              (+ (aref (skintwin-esn-state-feedback-weights state) i)
                                 (* delta output))))))
                  
                  ;; Track training error
                  (setq total-error (+ total-error (abs error))))
             finally (setf (skintwin-esn-state-training-error state) 
                           (/ total-error (length inputs))))))

(defun skintwin-esn-predict-progression (state steps &optional input-sequence)
  "Predict condition progression using STATE for STEPS with optional INPUT-SEQUENCE."
  (let* ((condition (skintwin-esn-state-condition state))
         (treatment (skintwin-esn-state-treatment state))
         (initial-severity (skintwin-esn-state-initial-severity state))
         (progression (list initial-severity))
         (current-value initial-severity)
         (input-seq (or input-sequence 
                        (make-list steps (cons 'treatment-efficacy 0.7)))))
    
    ;; Generate the progression time series
    (let ((prev-output nil))
      (dotimes (i steps)
        (let* ((current-input (if (< i (length input-seq))
                                 (car (nth i input-seq))
                               (car (car (last input-seq)))))
               (improvement (skintwin-esn-update-with-feedback 
                            state current-input prev-output))
               ;; New value decreases by the improvement, but never becomes negative
               (new-value (max 0.0 (- current-value (* improvement 0.1)))))
          
          ;; Add to progression and update current value
          (push new-value progression)
          (setq current-value new-value)
          (setq prev-output improvement))))
    
    ;; Update the state with the predicted progression
    (setf (skintwin-esn-state-progression state) (nreverse progression))
    
    ;; Return the progression
    (skintwin-esn-state-progression state)))

;; Data preparation functions
(defun skintwin-esn-prepare-training-data (condition-name treatment-name)
  "Prepare ESN training data for CONDITION-NAME with TREATMENT-NAME."
  (let* ((condition (aichat-opencog-kb-get condition-name))
         (treatment (aichat-opencog-kb-get treatment-name))
         (treatment-data '()))
    
    (if (and condition treatment)
        (progn
          ;; Check for treatment efficacy data
          (let* ((treats-pred (aichat-symbolic-predicate "treats"))
                 (treats-rel (aichat-opencog-match-pattern
                             (aichat-symbolic-evaluation 
                              treats-pred (list treatment condition) nil)
                             aichat-opencog-kb)))
            
            (if treats-rel
                (let* ((tv (aichat-symbolic-atom-tv (car treats-rel)))
                       (efficacy (aichat-pln-tv-mean tv))
                       (confidence (aichat-pln-tv-confidence tv)))
                  
                  ;; Generate synthetic progression data based on efficacy
                  (let ((initial-severity 0.9)
                        (decay-rate (+ 0.05 (* 0.2 efficacy))))
                    
                    ;; Create time series data
                    (dotimes (i skintwin-esn-training-length)
                      (let* ((time (/ (float i) 10))
                             ;; Decay function: severity decreases exponentially with time
                             (severity (* initial-severity 
                                        (exp (- (* decay-rate time)))))
                             ;; Input is treatment efficacy
                             (input efficacy))
                        
                        ;; Add to training data: (input . expected-output)
                        (push (cons input (- 1.0 severity)) treatment-data)))))
              
              ;; No direct relationship found, use defaults
              (let ((default-efficacy 0.5)
                    (default-decay 0.1))
                
                ;; Create default progression
                (dotimes (i skintwin-esn-training-length)
                  (let* ((time (/ (float i) 10))
                         (severity (* 0.9 (exp (- (* default-decay time)))))
                         (input default-efficacy))
                    
                    ;; Add to training data
                    (push (cons input (- 1.0 severity)) treatment-data))))))
          
          ;; Return reversed data to get chronological order
          (nreverse treatment-data))
      
      ;; Return empty list if condition or treatment not found
      '())))

(defun skintwin-esn-train-models ()
  "Train ESN models for all condition-treatment pairs in the knowledge base."
  (interactive)
  (let ((models '())
        (conditions '())
        (treatments '()))
    
    ;; Find all skin conditions
    (maphash (lambda (k v)
               (when (and (eq (aichat-symbolic-atom-type v) 'inheritance)
                        (let ((links (aichat-symbolic-atom-links v)))
                          (and (>= (length links) 2)
                               (equal (aichat-symbolic-atom-name (car links))
                                     "skin_condition"))))
                 (push (aichat-symbolic-atom-name (cadr (aichat-symbolic-atom-links v)))
                       conditions)))
             aichat-opencog-kb)
    
    ;; Find all treatments
    (let ((treats-pred (aichat-symbolic-predicate "treats")))
      (maphash (lambda (k v)
                 (when (and (eq (aichat-symbolic-atom-type v) 'evaluation)
                          (let ((links (aichat-symbolic-atom-links v)))
                            (and (>= (length links) 1)
                                 (eq (car links) treats-pred))))
                   (push (aichat-symbolic-atom-name 
                         (cadr (aichat-symbolic-atom-links v)))
                         treatments)))
               aichat-opencog-kb))
    
    ;; Remove duplicates
    (setq conditions (delete-dups conditions))
    (setq treatments (delete-dups treatments))
    
    ;; Train models for each condition-treatment pair
    (dolist (condition-name conditions)
      (dolist (treatment-name treatments)
        (message "Training ESN model for %s with %s..." condition-name treatment-name)
        (let* ((condition (aichat-opencog-kb-get condition-name))
               (treatment (aichat-opencog-kb-get treatment-name))
               (training-data (skintwin-esn-prepare-training-data 
                              condition-name treatment-name)))
          
          (when (and condition treatment (> (length training-data) 0))
            ;; Create and train the model
            (let ((model (skintwin-esn-create condition treatment)))
              (skintwin-esn-train-for-condition model training-data)
              
              ;; Store the trained model
              (push (cons (cons condition-name treatment-name) model) models))))))
    
    ;; Return the trained models
    (message "Trained %d ESN models for condition-treatment pairs" (length models))
    models))

;; Visualization functions
(defun skintwin-esn-visualize-progression (model &optional steps)
  "Visualize the progression prediction from MODEL for STEPS."
  (interactive)
  (let* ((steps (or steps 10))
         (condition-name (aichat-symbolic-atom-name 
                         (skintwin-esn-state-condition model)))
         (treatment-name (aichat-symbolic-atom-name 
                         (skintwin-esn-state-treatment model)))
         (progression (skintwin-esn-predict-progression model steps))
         (buffer (get-buffer-create "*SkinTwin-ESN-Progression*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: ESN Progression Prediction: %s with %s\n\n" 
                     condition-name treatment-name))
      
      ;; Summary information
      (insert "* Model Information\n\n")
      (insert (format "- Condition: %s\n" condition-name))
      (insert (format "- Treatment: %s\n" treatment-name))
      (insert (format "- Initial Severity: %.2f\n" 
                     (skintwin-esn-state-initial-severity model)))
      (insert (format "- Training Error: %.4f\n" 
                     (skintwin-esn-state-training-error model)))
      (insert (format "- Reservoir Size: %d neurons\n" skintwin-esn-reservoir-size))
      (insert (format "- Final Predicted Severity: %.2f\n" 
                     (car (last progression))))
      
      ;; Display the progression data
      (insert "\n* Progression Data\n\n")
      (insert "| Time Step | Severity | Visual |\n")
      (insert "|-----------|----------|--------|\n")
      
      (let ((step 0))
        (dolist (severity progression)
          (let ((bar-length (floor (* severity 20))))
            (insert (format "| %d | %.2f | %s |\n" 
                           step severity (make-string bar-length ?█))))
          (setq step (1+ step))))
      
      ;; Add a plot representation (text-based)
      (insert "\n* Visual Representation\n\n")
      (insert "```\nSeverity\n^\n|")
      
      ;; Y-axis (severity)
      (dotimes (i 10)
        (insert "\n|")
        (let ((severity-level (- 1.0 (* i 0.1))))
          ;; Add a horizontal line every 0.2
          (if (= (mod i 2) 0)
              (insert (format " %.1f -" severity-level))
            (insert "     |"))
          
          ;; Plot points
          (let ((step 0))
            (dolist (severity progression)
              (when (>= severity (- severity-level 0.05))
                (insert (make-string (- step (length (buffer-substring-no-properties 
                                                     (line-beginning-position)
                                                     (point))))
                                    ? ))
                (insert "*"))
              (setq step (+ step 2))))))
      
      ;; X-axis (time)
      (insert "\n|_")
      (dotimes (i (length progression))
        (insert "__"))
      (insert "___> Time\n```\n")
      
      ;; Interpretation section
      (insert "\n* Clinical Interpretation\n\n")
      (let* ((initial-severity (car progression))
             (final-severity (car (last progression)))
             (percent-improvement (* 100 (/ (- initial-severity final-severity) 
                                           initial-severity))))
        
        (cond
         ((>= percent-improvement 70)
          (insert "The Echo State Network predicts **significant improvement** with this treatment. ")
          (insert "The condition severity is expected to decrease by approximately ")
          (insert (format "%.1f%% over the prediction period.\n\n" percent-improvement))
          (insert "Recommendation: **Continue with current treatment plan**"))
         
         ((>= percent-improvement 30)
          (insert "The Echo State Network predicts **moderate improvement** with this treatment. ")
          (insert "The condition severity is expected to decrease by approximately ")
          (insert (format "%.1f%% over the prediction period.\n\n" percent-improvement))
          (insert "Recommendation: **Consider supplementary treatments** to enhance outcomes"))
         
         (t
          (insert "The Echo State Network predicts **limited improvement** with this treatment. ")
          (insert "The condition severity is expected to decrease by only ")
          (insert (format "%.1f%% over the prediction period.\n\n" percent-improvement))
          (insert "Recommendation: **Consider alternative treatment approaches**"))))))
    
    (display-buffer buffer)))

(defun skintwin-esn-compare-treatments (condition-name treatment-names &optional steps)
  "Compare multiple TREATMENT-NAMES for CONDITION-NAME over STEPS."
  (interactive 
   (list (read-string "Condition name: ")
         (split-string (read-string "Treatment names (comma-separated): ") "," t "\\s-+")
         (read-number "Number of time steps: " 10)))
  
  (let* ((steps (or steps 10))
         (condition (aichat-opencog-kb-get condition-name))
         (treatments (mapcar #'aichat-opencog-kb-get treatment-names))
         (buffer (get-buffer-create "*SkinTwin-Treatment-Comparison*"))
         (models '())
         (progressions '()))
    
    (if (null condition)
        (message "Condition '%s' not found in knowledge base" condition-name)
      
      ;; Create and train models for each treatment
      (dolist (treatment treatments)
        (when treatment
          (let* ((treatment-name (aichat-symbolic-atom-name treatment))
                 (training-data (skintwin-esn-prepare-training-data 
                                condition-name treatment-name)))
            
            (when (> (length training-data) 0)
              ;; Create and train the model
              (let ((model (skintwin-esn-create condition treatment)))
                (skintwin-esn-train-for-condition model training-data)
                (push model models)
                
                ;; Generate progression
                (let ((progression (skintwin-esn-predict-progression model steps)))
                  (push (cons treatment-name progression) progressions)))))))
      
      ;; Display comparison
      (with-current-buffer buffer
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Treatment Comparison for %s\n\n" condition-name))
        
        ;; Display comparison table
        (insert "* Effectiveness Comparison\n\n")
        (insert "| Treatment | Initial | Final | Improvement % | Recommendation |\n")
        (insert "|-----------+---------+-------+---------------+----------------|\n")
        
        (dolist (prog-data progressions)
          (let* ((treatment-name (car prog-data))
                 (progression (cdr prog-data))
                 (initial-severity (car progression))
                 (final-severity (car (last progression)))
                 (percent-improvement (* 100 (/ (- initial-severity final-severity) 
                                               initial-severity))))
            
            (insert (format "| %s | %.2f | %.2f | %.1f%% | %s |\n"
                           treatment-name
                           initial-severity
                           final-severity
                           percent-improvement
                           (cond
                            ((>= percent-improvement 70) "Recommended")
                            ((>= percent-improvement 30) "Consider")
                            (t "Not recommended"))))))
        
        ;; Visual comparison
        (insert "\n* Visual Comparison\n\n")
        (insert "```\nSeverity\n^\n|")
        
        ;; Y-axis (severity)
        (dotimes (i 10)
          (insert "\n|")
          (let ((severity-level (- 1.0 (* i 0.1))))
            ;; Add a horizontal line every 0.2
            (if (= (mod i 2) 0)
                (insert (format " %.1f -" severity-level))
              (insert "     |"))
            
            ;; Plot points for each treatment with different symbols
            (let ((step 0)
                  (symbols '("*" "+" "x" "#" "@" "$" "%" "&")))
              (dotimes (j (length progressions))
                (let* ((prog-data (nth j progressions))
                       (progression (cdr prog-data))
                       (symbol (nth (mod j (length symbols)) symbols)))
                  
                  (dotimes (k (length progression))
                    (let ((severity (nth k progression)))
                      (when (>= severity (- severity-level 0.05))
                        (insert (make-string (- (+ step k) 
                                               (length (buffer-substring-no-properties 
                                                       (line-beginning-position)
                                                       (point))))
                                            ? ))
                        (insert symbol)))))
                (setq step (+ step (+ 2 (length progression)))))))))
        
        ;; Legend
        (insert "\n|\n| Legend:")
        (let ((symbols '("*" "+" "x" "#" "@" "$" "%" "&")))
          (dotimes (j (length progressions))
            (when (< j (length symbols))
              (let* ((prog-data (nth j progressions))
                     (treatment-name (car prog-data))
                     (symbol (nth j symbols)))
                (insert (format " %s=%s" symbol treatment-name))))))
        (insert "\n|_")
        (dotimes (i 20)
          (insert "_"))
        (insert "___> Time\n```\n")
        
        ;; Clinical interpretation
        (insert "\n* Clinical Interpretation\n\n")
        
        (let* ((sorted-progs (sort (copy-sequence progressions)
                                  (lambda (a b)
                                    (> (- (car (car a)) (car (last (cdr a))))
                                       (- (car (car b)) (car (last (cdr b))))))))
               (best-treatment (car (car sorted-progs))))
          
          (insert (format "Based on ESN prediction models, **%s** appears to be the most effective treatment " 
                         best-treatment))
          (insert (format "for %s. " condition-name))
          (insert "The following factors were considered in this analysis:\n\n")
          (insert "1. Rate of condition improvement over time\n")
          (insert "2. Final predicted severity\n")
          (insert "3. Overall reduction in symptom severity\n\n")
          
          (insert "## Treatment Recommendations\n\n")
          (dolist (prog-data progressions)
            (let* ((treatment-name (car prog-data))
                   (progression (cdr prog-data))
                   (initial-severity (car progression))
                   (final-severity (car (last progression)))
                   (percent-improvement (* 100 (/ (- initial-severity final-severity) 
                                                 initial-severity))))
              
              (insert (format "- **%s**: " treatment-name))
              (cond
               ((>= percent-improvement 70)
                (insert "Highly recommended first-line treatment\n"))
               ((>= percent-improvement 30)
                (insert "Consider as a supplementary or second-line treatment\n"))
               (t
                (insert "Not recommended based on predicted efficacy\n")))))))
      
      (display-buffer buffer))))

(defun skintwin-esn-treatment-recommendation (patient-id condition-name)
  "Generate ESN-based treatment recommendations for PATIENT-ID with CONDITION-NAME."
  (interactive "sPatient ID: \nsCondition name: ")
  (let* ((patient (aichat-opencog-kb-get patient-id))
         (condition (aichat-opencog-kb-get condition-name))
         (buffer (get-buffer-create "*SkinTwin-Treatment-Recommendation*")))
    
    (if (or (null patient) (null condition))
        (message "Patient or condition not found in knowledge base")
      
      ;; Find all potential treatments
      (let* ((treatments '())
             (treats-pred (aichat-symbolic-predicate "treats"))
             (potentially-treats-pred (aichat-symbolic-predicate "potentially_treats"))
             (direct-treatments (aichat-opencog-match-pattern
                                (aichat-symbolic-evaluation 
                                 treats-pred (list nil condition) nil)
                                aichat-opencog-kb))
             (potential-treatments (aichat-opencog-match-pattern
                                   (aichat-symbolic-evaluation 
                                    potentially-treats-pred (list nil condition) nil)
                                   aichat-opencog-kb)))
        
        ;; Collect treatments
        (dolist (treat-rel direct-treatments)
          (let* ((links (aichat-symbolic-atom-links treat-rel))
                 (treatment (car (cdr (car links)))))
            (push (aichat-symbolic-atom-name treatment) treatments)))
        
        (dolist (treat-rel potential-treatments)
          (let* ((links (aichat-symbolic-atom-links treat-rel))
                 (treatment (car (cdr (car links)))))
            (push (aichat-symbolic-atom-name treatment) treatments)))
        
        ;; Remove duplicates
        (setq treatments (delete-dups treatments))
        
        ;; Compare treatments if we have multiple options
        (if (> (length treatments) 0)
            (skintwin-esn-compare-treatments condition-name treatments 15)
          (message "No treatments found for condition %s" condition-name))))))

(provide 'skintwin-esn)
;;; skintwin-esn.el ends here