;;; skintwin-visualization.el --- Visualization tools for SkinTwin -*- lexical-binding: t; -*-

(require 'aichat-symbolic)
(require 'aichat-opencog)
(require 'aichat-ecan)
(require 'org)

(defgroup skintwin-visualization nil
  "Visualization tools for SkinTwin."
  :group 'skintwin
  :prefix "skintwin-visualization-")

;; Configuration options
(defcustom skintwin-visualization-attention-levels 5
  "Number of attention levels to display in visualizations."
  :type 'integer
  :group 'skintwin-visualization)

(defcustom skintwin-visualization-graph-size 80
  "Size (width) of graphs in characters."
  :type 'integer
  :group 'skintwin-visualization)

(defcustom skintwin-visualization-color-gradients t
  "Whether to use color gradients in visualizations."
  :type 'boolean
  :group 'skintwin-visualization)

;; Core visualization functions
(defun skintwin-visualization-attention-heatmap ()
  "Generate an attention heatmap for concepts in the knowledge base."
  (interactive)
  (let ((buffer (get-buffer-create "*SkinTwin-Attention-Heatmap*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: SkinTwin Attention Heatmap\n\n")
      
      ;; Collect all atoms with attention values
      (let ((attention-values '()))
        (maphash (lambda (k v)
                   (when (aichat-ecan-get-av v)
                     (push (cons v (aichat-ecan-av-sti (aichat-ecan-get-av v))) 
                           attention-values)))
                 aichat-opencog-kb)
        
        ;; Sort by attention value
        (setq attention-values 
              (sort attention-values
                    (lambda (a b) (> (cdr a) (cdr b)))))
        
        ;; Group by categories
        (let ((categories (make-hash-table :test 'equal))
              (category-totals (make-hash-table :test 'equal)))
          
          ;; Group atoms by category using pattern matching
          (dolist (pair attention-values)
            (let* ((atom (car pair))
                   (sti (cdr pair))
                   (atom-type (aichat-symbolic-atom-type atom))
                   (name (aichat-symbolic-atom-name atom))
                   (category "Other"))
              
              ;; Determine category based on inheritance relationships
              (let* ((inheritance-pattern 
                     (aichat-symbolic-atom 'inheritance nil nil nil (list atom)))
                     (matches (aichat-opencog-match-pattern 
                              inheritance-pattern aichat-opencog-kb)))
                
                (when matches
                  (let* ((match (car matches))
                         (links (aichat-symbolic-atom-links match))
                         (parent (car links)))
                    (setq category (aichat-symbolic-atom-name parent)))))
              
              ;; Add to category
              (let ((items (gethash category categories '())))
                (push (cons name sti) items)
                (puthash category items categories))
              
              ;; Update category total
              (let ((total (gethash category category-totals 0.0)))
                (puthash category (+ total sti) category-totals))))
          
          ;; Display heatmap by category
          (insert "* Attention Distribution by Category\n\n")
          (insert "| Category | Attention | Concepts | Heatmap |\n")
          (insert "|----------|-----------|----------|--------|\n")
          
          ;; Sort categories by total attention
          (let ((sorted-categories '()))
            (maphash (lambda (k v) 
                       (push (cons k v) sorted-categories))
                     category-totals)
            (setq sorted-categories 
                  (sort sorted-categories
                        (lambda (a b) (> (cdr a) (cdr b)))))
            
            ;; Display each category
            (dolist (cat-pair sorted-categories)
              (let* ((category (car cat-pair))
                     (total-attn (cdr cat-pair))
                     (items (gethash category categories '()))
                     (num-items (length items))
                     (bar-length (floor (* (/ total-attn 
                                            (max 1.0 (cdr (car sorted-categories))))
                                         skintwin-visualization-graph-size))))
                
                (insert (format "| %s | %.3f | %d | %s |\n" 
                               category total-attn num-items 
                               (make-string bar-length ?█))))))
          
          ;; Display heatmap by atom
          (insert "\n* Top Concepts by Attention\n\n")
          (insert "| Concept | Category | Attention | Heatmap |\n")
          (insert "|---------|----------|-----------|--------|\n")
          
          ;; Take top N atoms
          (let ((top-atoms (seq-take attention-values 30)))
            (dolist (pair top-atoms)
              (let* ((atom (car pair))
                     (sti (cdr pair))
                     (name (aichat-symbolic-atom-name atom))
                     (category "Other")
                     (bar-length (floor (* (/ sti 
                                            (max 1.0 (cdr (car attention-values))))
                                         skintwin-visualization-graph-size))))
                
                ;; Determine category
                (let* ((inheritance-pattern 
                       (aichat-symbolic-atom 'inheritance nil nil nil (list atom)))
                       (matches (aichat-opencog-match-pattern 
                                inheritance-pattern aichat-opencog-kb)))
                  
                  (when matches
                    (let* ((match (car matches))
                           (links (aichat-symbolic-atom-links match))
                           (parent (car links)))
                      (setq category (aichat-symbolic-atom-name parent)))))
                
                (insert (format "| %s | %s | %.3f | %s |\n" 
                               name category sti
                               (make-string bar-length ?█))))))
          
          ;; Add a 2D attention heatmap
          (insert "\n* 2D Attention Heatmap\n\n")
          (insert "```\n")
          
          ;; Display intensity levels
          (dotimes (i 10)
            (let ((intensity-char (nth (- 9 i) '(?\s ?. ?, ?- ?+ ?= ?# ?@ ?% ?&))))
              (insert (format "Level %d: %c  " i intensity-char))))
          
          (insert "\n\n")
          
          ;; Get top categories and atoms
          (let* ((top-categories 
                 (seq-take 
                  (sort (let ((cats '()))
                          (maphash (lambda (k v) (push k cats)) categories)
                          cats)
                        (lambda (a b) 
                          (> (gethash a category-totals 0)
                             (gethash b category-totals 0))))
                  10))
                 (max-name-length (apply #'max (mapcar #'length top-categories)))
                 (grid-size (min 20 (length top-categories))))
            
            ;; Create a grid
            (dotimes (i grid-size)
              (let ((category (nth i top-categories)))
                ;; Print category name
                (insert (format "%-*s | " max-name-length category))
                
                ;; Print cells
                (let ((items (gethash category categories '())))
                  (dotimes (j (min grid-size (length items)))
                    (let* ((item-pair (nth j items))
                           (item-name (car item-pair))
                           (item-sti (cdr item-pair))
                           (max-sti (cdr (car attention-values)))
                           (intensity-level (min 9 (floor (* 10 (/ item-sti max-sti)))))
                           (intensity-char (nth intensity-level 
                                             '(?\s ?. ?, ?- ?+ ?= ?# ?@ ?% ?&))))
                      (insert (format "%c " intensity-char))))
                  (insert "\n")))))
          (insert "```\n")
          
          ;; Add a hierarchical view
          (insert "\n* Hierarchical View of Attention\n\n")
          
          ;; Function to display hierarchy with attention
          (let ((displayed-atoms (make-hash-table :test 'equal)))
            (cl-labels ((display-hierarchy (concept level)
                                        (when (and concept 
                                                  (not (gethash (aichat-symbolic-atom-name concept) 
                                                               displayed-atoms)))
                                          ;; Mark as displayed to prevent cycles
                                          (puthash (aichat-symbolic-atom-name concept) t displayed-atoms)
                                          
                                          ;; Display this concept
                                          (let* ((indent (make-string (* level 2) ? ))
                                                (av (aichat-ecan-get-av concept))
                                                (name (aichat-symbolic-atom-name concept))
                                                (sti (if av (aichat-ecan-av-sti av) 0.0))
                                                (max-sti (if attention-values
                                                            (cdr (car attention-values))
                                                          1.0))
                                                (bar-length (floor (* (/ sti max-sti) 10))))
                                            
                                            (insert (format "%s- %s [%.2f] %s\n" 
                                                          indent name sti
                                                          (make-string bar-length ?█)))
                                            
                                            ;; Find and display children
                                            (let* ((pattern (aichat-symbolic-atom 'inheritance 
                                                                                concept nil nil))
                                                  (matches (aichat-opencog-match-pattern 
                                                          pattern aichat-opencog-kb)))
                                              (dolist (match matches)
                                                (let* ((links (aichat-symbolic-atom-links match))
                                                      (child (cadr links)))
                                                  (display-hierarchy child (1+ level)))))))))
              
              ;; Start with the highest level concepts
              (let ((top-concepts '("skin_layer" "cell_type" "skin_condition")))
                (dolist (concept-name top-concepts)
                  (let ((concept (aichat-opencog-kb-get concept-name)))
                    (when concept
                      (insert (format "** %s Tree\n" concept-name))
                      (display-hierarchy concept 1)))))))))))
    
    (display-buffer buffer)))

(defun skintwin-visualization-network-graph (&optional focus-concept depth)
  "Generate a text-based network graph, optionally focusing on FOCUS-CONCEPT with DEPTH."
  (interactive (list (completing-read "Focus concept (leave empty for full graph): "
                                     (let ((concepts '()))
                                       (maphash (lambda (k v)
                                                 (when (eq (aichat-symbolic-atom-type v) 'concept)
                                                   (push k concepts)))
                                               aichat-opencog-kb)
                                       concepts)
                                     nil t)
                     (read-number "Exploration depth: " 2)))
  
  (let* ((buffer (get-buffer-create "*SkinTwin-Network-Graph*"))
         (focus (if (and focus-concept (not (string-empty-p focus-concept)))
                   (aichat-opencog-kb-get focus-concept)
                 nil))
         (depth (or depth 2)))
    
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (if focus
          (insert (format "#+TITLE: Network Graph for %s\n\n" focus-concept))
        (insert "#+TITLE: SkinTwin Knowledge Network Graph\n\n"))
      
      ;; Collect nodes and edges for the graph
      (let ((nodes (make-hash-table :test 'equal))
            (edges '()))
        
        ;; Function to explore the graph from a node to a certain depth
        (cl-labels ((explore-node (node current-depth)
                                (when (and node (<= current-depth depth))
                                  ;; Add this node
                                  (puthash (aichat-symbolic-atom-name node) node nodes)
                                  
                                  ;; Explore inheritance relationships
                                  (let* ((inheritance-pattern 
                                         (aichat-symbolic-atom 'inheritance node nil nil))
                                         (matches (aichat-opencog-match-pattern 
                                                  inheritance-pattern aichat-opencog-kb)))
                                    
                                    (dolist (match matches)
                                      (let* ((links (aichat-symbolic-atom-links match))
                                             (target (cadr links)))
                                        
                                        ;; Add edge
                                        (push (list (aichat-symbolic-atom-name node)
                                                   (aichat-symbolic-atom-name target)
                                                   "inherits")
                                              edges)
                                        
                                        ;; Explore deeper
                                        (explore-node target (1+ current-depth)))))
                                  
                                  ;; Explore evaluation relationships
                                  (maphash 
                                   (lambda (k v)
                                     (when (eq (aichat-symbolic-atom-type v) 'evaluation)
                                       (let ((links (aichat-symbolic-atom-links v)))
                                         (when (and (>= (length links) 2)
                                                   (or (equal (aichat-symbolic-atom-name (car links))
                                                             (aichat-symbolic-atom-name node))
                                                       (equal (aichat-symbolic-atom-name (cadr links))
                                                             (aichat-symbolic-atom-name node))))
                                           
                                           ;; Add the predicate node
                                           (let ((pred (car links)))
                                             (puthash (aichat-symbolic-atom-name pred) pred nodes))
                                           
                                           ;; Add the other node
                                           (let* ((other (if (equal (aichat-symbolic-atom-name (car links))
                                                                  (aichat-symbolic-atom-name node))
                                                           (cadr links)
                                                         (car links))))
                                             
                                             (puthash (aichat-symbolic-atom-name other) other nodes)
                                             
                                             ;; Add edge
                                             (push (list (aichat-symbolic-atom-name node)
                                                       (aichat-symbolic-atom-name other)
                                                       (aichat-symbolic-atom-name (car links)))
                                                   edges)
                                             
                                             ;; Explore deeper
                                             (explore-node other (1+ current-depth)))))))
                                   aichat-opencog-kb)))))
          
          ;; Start exploration from focus node or all top-level nodes
          (if focus
              (explore-node focus 0)
            (let ((top-nodes '()))
              (maphash (lambda (k v)
                         (when (eq (aichat-symbolic-atom-type v) 'concept)
                           (push v top-nodes)))
                       aichat-opencog-kb)
              
              ;; Start from top 10 nodes
              (let ((top-10 (seq-take top-nodes 10)))
                (dolist (node top-10)
                  (explore-node node 0))))))
        
        ;; Display graph statistics
        (insert (format "* Network Graph (%d nodes, %d edges)\n\n" 
                       (hash-table-count nodes) (length edges)))
        
        ;; Create a mapping of nodes to indices for the matrix
        (let* ((node-names '())
               (node-indices (make-hash-table :test 'equal))
               (index 0))
          
          ;; Collect node names
          (maphash (lambda (k _v) (push k node-names)) nodes)
          (setq node-names (sort node-names #'string<))
          
          ;; Assign indices
          (dolist (name node-names)
            (puthash name index node-indices)
            (setq index (1+ index)))
          
          ;; Create adjacency matrix visualization
          (when (< (length node-names) 30) ; Only for reasonably sized graphs
            (insert "** Adjacency Matrix\n\n")
            (insert "```\n")
            
            ;; Column headers (abbreviated)
            (insert "     ")
            (dotimes (i (length node-names))
              (let ((name (nth i node-names)))
                (insert (substring name 0 (min 1 (length name))))))
            (insert "\n")
            
            ;; Matrix rows
            (dotimes (i (length node-names))
              (let ((row-name (nth i node-names)))
                ;; Row header (abbreviated)
                (insert (format "%-4s " (substring row-name 0 (min 3 (length row-name)))))
                
                ;; Matrix cells
                (dotimes (j (length node-names))
                  (let ((col-name (nth j node-names))
                        (has-edge nil))
                    
                    ;; Check if there's an edge
                    (dolist (edge edges)
                      (when (and (equal (nth 0 edge) row-name)
                                (equal (nth 1 edge) col-name))
                        (setq has-edge t)))
                    
                    (insert (if has-edge "●" "○"))))
                (insert "\n")))
            (insert "```\n"))
          
          ;; Create a simplified text-based graph visualization
          (insert "** Graph Visualization\n\n")
          (insert "```\n")
          
          ;; Display nodes with attention values
          (let ((max-name-length (apply #'max (mapcar #'length node-names))))
            (insert (format "Nodes (%d):\n" (length node-names)))
            (let ((nodes-with-attention '()))
              
              ;; Get attention values
              (dolist (name node-names)
                (let* ((node (gethash name nodes))
                       (av (aichat-ecan-get-av node))
                       (sti (if av (aichat-ecan-av-sti av) 0.0)))
                  (push (cons name sti) nodes-with-attention)))
              
              ;; Sort by attention
              (setq nodes-with-attention 
                    (sort nodes-with-attention
                          (lambda (a b) (> (cdr a) (cdr b)))))
              
              ;; Display in columns
              (let ((columns 3)
                    (i 0))
                (dolist (pair nodes-with-attention)
                  (let* ((name (car pair))
                         (sti (cdr pair))
                         (bar-length (min 10 (floor (* sti 20)))))
                    (insert (format "%-*s [%.2f] %s%s" 
                                   max-name-length name sti
                                   (make-string bar-length ?█)
                                   (if (= (mod i (1- columns)) 0) "\n" " | ")))
                    (setq i (1+ i)))))))
          
          ;; Display edges
          (insert (format "\n\nEdges (%d):\n" (length edges)))
          (dolist (edge edges)
            (insert (format "%s --%s--> %s\n" 
                           (nth 0 edge) (nth 2 edge) (nth 1 edge))))
          
          (insert "```\n")
          
          ;; Create a hierarchical view if focused on a single concept
          (when focus
            (insert "** Hierarchical View\n\n")
            
            ;; Function to display hierarchy
            (let ((displayed-atoms (make-hash-table :test 'equal)))
              (cl-labels ((display-hierarchy (node level direction)
                                          ;; Direction: 'up for parents, 'down for children
                                          (when (and node 
                                                    (not (gethash (format "%s-%s" 
                                                                        (aichat-symbolic-atom-name node)
                                                                        direction)
                                                                displayed-atoms)))
                                            ;; Mark as displayed to prevent cycles
                                            (puthash (format "%s-%s" 
                                                            (aichat-symbolic-atom-name node)
                                                            direction)
                                                    t displayed-atoms)
                                            
                                            ;; Display this node
                                            (let* ((indent (make-string (* level 2) ? ))
                                                  (av (aichat-ecan-get-av node))
                                                  (name (aichat-symbolic-atom-name node))
                                                  (sti (if av (aichat-ecan-av-sti av) 0.0)))
                                              
                                              (insert (format "%s- %s [%.2f]\n" 
                                                            indent name sti))
                                              
                                              ;; Navigate up or down the hierarchy
                                              (cond
                                               ((eq direction 'up)
                                                ;; Find parents
                                                (let* ((inheritance-pattern 
                                                       (aichat-symbolic-atom 'inheritance nil nil nil (list node)))
                                                       (matches (aichat-opencog-match-pattern 
                                                                inheritance-pattern aichat-opencog-kb)))
                                                  
                                                  (dolist (match matches)
                                                    (let* ((links (aichat-symbolic-atom-links match))
                                                           (parent (car links)))
                                                      (display-hierarchy parent (1+ level) 'up)))))
                                               
                                               ((eq direction 'down)
                                                ;; Find children
                                                (let* ((inheritance-pattern 
                                                       (aichat-symbolic-atom 'inheritance node nil nil))
                                                       (matches (aichat-opencog-match-pattern 
                                                                inheritance-pattern aichat-opencog-kb)))
                                                  
                                                  (dolist (match matches)
                                                    (let* ((links (aichat-symbolic-atom-links match))
                                                           (child (cadr links)))
                                                      (display-hierarchy child (1+ level) 'down))))))))))
                
                ;; Display hierarchy in both directions
                (insert "*** Parent Hierarchy\n")
                (display-hierarchy focus 0 'up)
                (insert "\n*** Child Hierarchy\n")
                (display-hierarchy focus 0 'down))))))
      
      ;; Display the buffer
      (display-buffer buffer))))

(defun skintwin-visualization-esn-state (condition-name treatment-name)
  "Visualize the Echo State Network state for CONDITION-NAME treated with TREATMENT-NAME."
  (interactive "sCondition name: \nsTreatment name: ")
  
  ;; Check if skintwin-esn is available
  (if (featurep 'skintwin-esn)
      (if (fboundp 'skintwin-esn-train-models)
          (let* ((condition (aichat-opencog-kb-get condition-name))
                 (treatment (aichat-opencog-kb-get treatment-name))
                 (buffer (get-buffer-create "*SkinTwin-ESN-State*")))
            
            (if (and condition treatment)
                (progn
                  ;; Create and train a model
                  (with-current-buffer buffer
                    (erase-buffer)
                    (org-mode)
                    (insert (format "#+TITLE: ESN State: %s with %s\n\n" 
                                   condition-name treatment-name))
                    
                    (insert "* Echo State Network Neuron Activations\n\n")
                    (insert "Initializing ESN for visualization...\n"))
                  
                  (display-buffer buffer)
                  
                  ;; Create ESN model
                  (require 'skintwin-esn)
                  (let* ((training-data (skintwin-esn-prepare-training-data 
                                        condition-name treatment-name))
                         (model (skintwin-esn-create condition treatment)))
                    
                    ;; Train the model
                    (skintwin-esn-train-for-condition model training-data)
                    
                    ;; Generate sequence to visualize state
                    (let ((progression (skintwin-esn-predict-progression model 5)))
                      
                      ;; Now visualize the state
                      (with-current-buffer buffer
                        (erase-buffer)
                        (org-mode)
                        (insert (format "#+TITLE: ESN State: %s with %s\n\n" 
                                       condition-name treatment-name))
                        
                        (insert "* Echo State Network Neuron Activations\n\n")
                        
                        ;; Show reservoir state
                        (let* ((state-vec (skintwin-esn-state-state model))
                               (size (length state-vec))
                               (columns 20)
                               (rows (+ (/ size columns) 
                                       (if (= 0 (mod size columns)) 0 1))))
                          
                          (insert "** Reservoir State\n\n")
                          (insert "```\n")
                          
                          ;; Show neuron state matrix
                          (dotimes (row rows)
                            (dotimes (col columns)
                              (let ((index (+ (* row columns) col)))
                                (if (< index size)
                                    (let* ((val (aref state-vec index))
                                          (intensity-level (+ 5 (floor (* val 5)))))
                                      (insert (nth intensity-level 
                                                 '(" " "." "," "-" "+" "=" "#" "@" "%" "&"))))
                                  (insert " "))))
                            (insert "\n"))
                          
                          (insert "\nLegend: Less active " )
                          (dotimes (i 10)
                            (insert (nth i '(" " "." "," "-" "+" "=" "#" "@" "%" "&"))))
                          (insert " More active\n```\n")
                          
                          ;; Show histogram of activation values
                          (insert "\n** Activation Distribution\n\n")
                          (let ((bins (make-vector 10 0)))
                            
                            ;; Count neurons in each activation bin
                            (dotimes (i size)
                              (let* ((val (aref state-vec i))
                                    (bin (min 9 (floor (* (+ 0.5 (/ (+ val 1.0) 2.0)) 10)))))
                                (aset bins bin (1+ (aref bins bin)))))
                            
                            ;; Display histogram
                            (insert "| Activation | Count | Distribution |\n")
                            (insert "|------------|-------|-------------|\n")
                            
                            (dotimes (bin 10)
                              (let* ((lower (- 1.0 (* (- 9 bin) 0.2)))
                                    (upper (- 1.0 (* (- 10 bin) 0.2)))
                                    (count (aref bins bin))
                                    (bar-length (min 50 
                                                   (* 50 (/ (float count) 
                                                          (float (apply #'max (append bins nil))))))))
                                
                                (insert (format "| %.1f to %.1f | %d | %s |\n" 
                                               lower upper count
                                               (make-string bar-length ?█))))))
                          
                          ;; Show temporal pattern in activations
                          (insert "\n** Temporal ESN Activation Pattern\n\n")
                          (insert "```\n")
                          (insert "Neuron activations over time (sample of 20 neurons):\n\n")
                          
                          ;; Show time steps
                          (insert "Time:  0  1  2  3  4  5\n")
                          (insert "      -----------------\n")
                          
                          ;; Show 20 random neurons
                          (let ((sample-neurons (seq-sample 
                                                (number-sequence 0 (1- size)) 20)))
                            
                            (dotimes (i 20)
                              (let ((neuron (nth i sample-neurons)))
                                (insert (format "%4d | " neuron))
                                
                                ;; Store initial state
                                (let ((states '())
                                      (current-state (copy-sequence state-vec)))
                                  (push current-state states)
                                  
                                  ;; Generate 5 more states
                                  (dotimes (t 5)
                                    (skintwin-esn-update-with-feedback 
                                     model 0.7 (nth t progression))
                                    (push (copy-sequence (skintwin-esn-state-state model)) 
                                          states))
                                  
                                  ;; Display states (reversed to get chronological order)
                                  (dolist (s (nreverse states))
                                    (let* ((val (aref s neuron))
                                          (intensity-level (+ 5 (floor (* val 5)))))
                                      (insert (format "%s  " (nth intensity-level 
                                                             '(" " "." "," "-" "+" "=" "#" "@" "%" "&")))))))
                                (insert "\n"))))
                          
                          (insert "```\n")
                          
                          ;; Add interpretation
                          (insert "\n* ESN Analysis\n\n")
                          (insert "The Echo State Network represents a dynamical system that models ")
                          (insert "the progression of skin conditions over time. Key observations:\n\n")
                          
                          (insert "1. **Activation patterns** show the internal state of the reservoir\n")
                          (insert "2. **Temporal evolution** demonstrates how the network processes time series data\n")
                          (insert "3. **Distribution of activations** indicates the network's response characteristics\n\n")
                          
                          (insert "## Prediction Quality\n\n")
                          (insert (format "Training Error: %.4f\n\n" 
                                         (skintwin-esn-state-training-error model)))
                          
                          (if (< (skintwin-esn-state-training-error model) 0.1)
                              (insert "The network appears to be well-trained for this condition-treatment pair, ")
                            (insert "The network shows moderate training error for this condition-treatment pair, "))
                          
                          (insert "suggesting that its predictions should be reasonably reliable for clinical guidance.\n")))))
                  
                (message "Condition or treatment not found in knowledge base")))
        (message "Function skintwin-esn-train-models not available"))
    (message "skintwin-esn module not loaded; visualization not available")))

(defun skintwin-visualization-attention-flow ()
  "Visualize the attention flow process in the SkinTwin system."
  (interactive)
  (let ((buffer (get-buffer-create "*SkinTwin-Attention-Flow*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: SkinTwin Attention Flow Visualization\n\n")
      
      ;; Capture initial attention state
      (let ((initial-attention '()))
        (maphash (lambda (k v)
                   (when (aichat-ecan-get-av v)
                     (push (cons v (aichat-ecan-av-sti (aichat-ecan-get-av v))) 
                           initial-attention)))
                 aichat-opencog-kb)
        
        ;; Display initial state
        (insert "* Initial Attention State\n\n")
        (insert "| Concept | Attention | Bar |\n")
        (insert "|---------|-----------|-----|\n")
        
        ;; Sort by attention value
        (setq initial-attention 
              (sort initial-attention
                    (lambda (a b) (> (cdr a) (cdr b)))))
        
        ;; Display top 10
        (let ((top-atoms (seq-take initial-attention 10)))
          (dolist (pair top-atoms)
            (let* ((atom (car pair))
                   (sti (cdr pair))
                   (name (aichat-symbolic-atom-name atom))
                   (bar-length (floor (* sti 20))))
              (insert (format "| %s | %.3f | %s |\n" 
                             name sti (make-string bar-length ?█))))))
        
        ;; Now run several attention updates and capture the changes
        (insert "\n* Attention Flow Simulation\n\n")
        (insert "Simulating attention spreading for 3 steps:\n\n")
        
        (dotimes (step 3)
          (insert (format "** Step %d\n\n" (1+ step)))
          
          ;; Apply ECAN update
          (aichat-ecan-spread-importance aichat-opencog-kb)
          (aichat-ecan-update aichat-opencog-kb)
          
          ;; Capture new attention state
          (let ((new-attention '()))
            (maphash (lambda (k v)
                       (when (aichat-ecan-get-av v)
                         (push (cons v (aichat-ecan-av-sti (aichat-ecan-get-av v))) 
                               new-attention)))
                     aichat-opencog-kb)
            
            ;; Sort by attention value
            (setq new-attention 
                  (sort new-attention
                        (lambda (a b) (> (cdr a) (cdr b)))))
            
            ;; Compare with initial state to show flow
            (insert "| Concept | Before | After | Change | Flow |\n")
            (insert "|---------|--------|-------|--------|------|\n")
            
            ;; Display top 10 from new state
            (let ((top-atoms (seq-take new-attention 10)))
              (dolist (pair top-atoms)
                (let* ((atom (car pair))
                       (new-sti (cdr pair))
                       (name (aichat-symbolic-atom-name atom))
                       ;; Find original attention if available
                       (initial-pair (assoc atom initial-attention))
                       (old-sti (if initial-pair (cdr initial-pair) 0.0))
                       (change (- new-sti old-sti))
                       (bar-length (floor (* 20 (/ change 0.5))))
                       (flow-indicator (if (> change 0) 
                                         (make-string (abs bar-length) ?▲)
                                       (make-string (abs bar-length) ?▼))))
                  (insert (format "| %s | %.3f | %.3f | %+.3f | %s |\n" 
                                 name old-sti new-sti change flow-indicator)))))
            
            ;; Update initial state for next comparison
            (setq initial-attention new-attention)))
        
        ;; Add an animated representation of attention flow
        (insert "\n* Animated Attention Flow\n\n")
        (insert "```\n")
        
        ;; Create a simple animation frame showing attention spreading
        (let* ((concepts (list "skin_condition" "psoriasis" "eczema" "keratinocyte" "acne" 
                              "inflammation" "treatment" "corticosteroid" "retinoid"))
               (positions (make-hash-table :test 'equal))
               (width 40)
               (height 15))
          
          ;; Assign random positions to concepts
          (dolist (concept concepts)
            (puthash concept 
                     (cons (random width) (random height))
                     positions))
          
          ;; Draw frames
          (dotimes (frame 5)
            (insert (format "Frame %d:\n" frame))
            
            ;; Create a grid
            (let ((grid (make-vector (* width height) nil)))
              
              ;; Place concepts on grid
              (dolist (concept concepts)
                (let* ((pos (gethash concept positions))
                       (x (car pos))
                       (y (cdr pos))
                       (index (+ (* y width) x))
                       (atom (aichat-opencog-kb-get concept))
                       (av (and atom (aichat-ecan-get-av atom)))
                       (sti (if av (aichat-ecan-av-sti av) 0.0))
                       (intensity-level (min 9 (floor (* sti 10)))))
                  
                  ;; Place on grid
                  (aset grid index (cons concept intensity-level))))
              
              ;; Draw edges between related concepts for animation
              (dolist (concept1 concepts)
                (dolist (concept2 concepts)
                  (when (and (not (equal concept1 concept2))
                           (< (random 100) 30)) ; 30% chance of edge for animation
                    (let* ((pos1 (gethash concept1 positions))
                           (pos2 (gethash concept2 positions))
                           (x1 (car pos1))
                           (y1 (cdr pos1))
                           (x2 (car pos2))
                           (y2 (cdr pos2)))
                      
                      ;; Draw a simple line for animation
                      (cl-loop for t from 0.0 to 1.0 by 0.1 do
                              (let* ((x (+ (* (- 1 t) x1) (* t x2)))
                                    (y (+ (* (- 1 t) y1) (* t y2)))
                                    (index (+ (* (floor y) width) (floor x))))
                                (when (and (>= index 0) (< index (* width height)))
                                  (unless (aref grid index)
                                    (aset grid index (cons nil (mod (+ frame (floor (* t 10))) 10))))))))))
              
              ;; Display the grid
              (dotimes (y height)
                (dotimes (x width)
                  (let* ((index (+ (* y width) x))
                         (cell (aref grid index)))
                    (if cell
                        (let ((concept (car cell))
                              (intensity (cdr cell)))
                          (if concept
                              ;; Show first character of concept
                              (insert (substring concept 0 1))
                            ;; Show flow character
                            (insert (nth intensity '(" " "." "," "+" "*" "#" "@" "%" "&" "!")))))
                      ;; Empty cell
                      (insert " "))))
                (insert "\n"))
              
              (insert "\n"))))
        
        (insert "```\n"))
      
      ;; Show the buffer
      (display-buffer buffer))))

(defun skintwin-visualization-dashboard ()
  "Display a visualization dashboard for the SkinTwin system."
  (interactive)
  (let ((buffer (get-buffer-create "*SkinTwin-Visualization-Dashboard*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: SkinTwin Visualization Dashboard\n\n")
      
      ;; Dashboard controls
      (insert "* Visualization Controls\n\n")
      (insert "Press the following keys for different visualizations:\n\n")
      (insert "| Key | Visualization | Description |\n")
      (insert "|-----|--------------|-------------|\n")
      (insert "| a | Attention Heatmap | View attention distribution across concepts |\n")
      (insert "| n | Network Graph | View knowledge network structure |\n")
      (insert "| f | Attention Flow | Visualize attention spreading process |\n")
      (insert "| e | ESN State | Examine Echo State Network activation |\n")
      (insert "| r | Reasoning Visualization | View PLN reasoning process |\n")
      
      ;; System Status Summary
      (insert "\n* System Status Summary\n\n")
      
      ;; KB statistics
      (let ((concept-count 0)
            (predicate-count 0)
            (link-count 0)
            (total-attention 0.0)
            (atoms-with-attention 0))
        
        (maphash (lambda (k v)
                   (case (aichat-symbolic-atom-type v)
                     ('concept (setq concept-count (1+ concept-count)))
                     ('predicate (setq predicate-count (1+ predicate-count)))
                     (otherwise (setq link-count (1+ link-count))))
                   
                   ;; Track attention statistics
                   (let ((av (aichat-ecan-get-av v)))
                     (when av
                       (setq total-attention (+ total-attention (aichat-ecan-av-sti av)))
                       (setq atoms-with-attention (1+ atoms-with-attention)))))
                 aichat-opencog-kb)
        
        (insert "** Knowledge Base\n")
        (insert (format "- Concepts: %d\n" concept-count))
        (insert (format "- Predicates: %d\n" predicate-count))
        (insert (format "- Relationships: %d\n" link-count))
        (insert (format "- Total Knowledge Size: %d atoms\n\n" 
                       (+ concept-count predicate-count link-count)))
        
        (insert "** Attention System\n")
        (insert (format "- Atoms with Attention: %d\n" atoms-with-attention))
        (insert (format "- Average Attention: %.3f\n" 
                       (if (> atoms-with-attention 0)
                           (/ total-attention atoms-with-attention)
                         0.0)))
        (insert (format "- Attention Spread Threshold: %.2f\n" aichat-ecan-spread-threshold))
        (insert (format "- Attention Decay Rate: %.2f\n\n" aichat-ecan-decay-rate)))
      
      ;; Quick visualizations
      (insert "* Quick Visualizations\n\n")
      
      ;; Attention bar chart
      (insert "** Top Concepts by Attention\n\n")
      (insert "```\n")
      (let ((attention-values '()))
        (maphash (lambda (k v)
                   (when (aichat-ecan-get-av v)
                     (push (cons v (aichat-ecan-av-sti (aichat-ecan-get-av v))) 
                           attention-values)))
                 aichat-opencog-kb)
        
        (setq attention-values 
              (sort attention-values
                    (lambda (a b) (> (cdr a) (cdr b)))))
        
        (let ((top-values (seq-take attention-values 10))
              (max-name-length 0))
          
          ;; Find max name length for formatting
          (dolist (pair top-values)
            (let* ((atom (car pair))
                   (name (aichat-symbolic-atom-name atom)))
              (setq max-name-length (max max-name-length (length name)))))
          
          ;; Display bar chart
          (dolist (pair top-values)
            (let* ((atom (car pair))
                   (sti (cdr pair))
                   (name (aichat-symbolic-atom-name atom))
                   (bar-length (floor (* sti 50))))
              (insert (format "%-*s |%s %.3f\n" 
                             max-name-length name
                             (make-string bar-length ?█) sti))))))
      (insert "```\n")
      
      ;; Network minimap
      (insert "\n** Knowledge Network Minimap\n\n")
      (insert "```\n")
      
      ;; Create a small network visualization
      (let* ((key-concepts '("skin_condition" "cell_type" "skin_layer"))
             (related-concepts '())
             (edges '())
             (mini-width 20)
             (mini-height 10)
             (concept-positions (make-hash-table :test 'equal)))
        
        ;; Get related concepts
        (dolist (key key-concepts)
          (let ((concept (aichat-opencog-kb-get key)))
            (when concept
              (push concept related-concepts)
              
              ;; Find direct children
              (let* ((inheritance-pattern (aichat-symbolic-atom 'inheritance concept nil nil))
                     (matches (aichat-opencog-match-pattern 
                              inheritance-pattern aichat-opencog-kb)))
                
                (dolist (match matches)
                  (let* ((links (aichat-symbolic-atom-links match))
                         (child (cadr links)))
                    (push child related-concepts)
                    (push (list (aichat-symbolic-atom-name concept)
                               (aichat-symbolic-atom-name child))
                          edges)))))))
        
        ;; Assign positions
        (let ((x 0)
              (y 0))
          (dolist (concept related-concepts)
            (puthash (aichat-symbolic-atom-name concept) 
                     (cons (mod x mini-width) (mod y mini-height))
                     concept-positions)
            (setq x (1+ x))
            (when (>= x mini-width)
              (setq x 0)
              (setq y (1+ y)))))
        
        ;; Create grid
        (let ((grid (make-vector (* mini-width mini-height) nil)))
          
          ;; Place concepts
          (dolist (concept related-concepts)
            (let* ((name (aichat-symbolic-atom-name concept))
                   (pos (gethash name concept-positions))
                   (x (car pos))
                   (y (cdr pos))
                   (index (+ (* y mini-width) x)))
              (aset grid index name)))
          
          ;; Draw the grid
          (dotimes (y mini-height)
            (dotimes (x mini-width)
              (let* ((index (+ (* y mini-width) x))
                     (name (aref grid index)))
                (if name
                    (insert (substring name 0 1))
                  (insert " "))))
            (insert "\n"))
          
          ;; Add legend
          (insert "\nLegend:\n")
          (dolist (concept related-concepts)
            (let ((name (aichat-symbolic-atom-name concept)))
              (insert (format "%s = %s\n" (substring name 0 1) name))))))
      
      (insert "```\n")
      
      ;; Set up keybindings for the dashboard
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "a") 'skintwin-visualization-attention-heatmap)
      (local-set-key (kbd "n") 'skintwin-visualization-network-graph)
      (local-set-key (kbd "f") 'skintwin-visualization-attention-flow)
      (local-set-key (kbd "e") (lambda () 
                               (interactive)
                               (let ((condition (completing-read 
                                               "Condition: " 
                                               '("psoriasis" "eczema" "acne")))
                                     (treatment (completing-read 
                                                "Treatment: " 
                                                '("corticosteroid" "retinoid" "antibiotic"))))
                                 (skintwin-visualization-esn-state condition treatment)))))
    
    ;; Display the buffer
    (display-buffer buffer)))

(provide 'skintwin-visualization)
;;; skintwin-visualization.el ends here