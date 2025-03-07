;;; skintwin-db.el --- Database integration for SkinTwin -*- lexical-binding: t; -*-

(require 'aichat-symbolic)
(require 'aichat-ecan)
(require 'aichat-opencog)
(require 'aichat-util)
(require 'json)
(require 'url)

(defgroup skintwin-db nil
  "Database integration for the SkinTwin dermatological model."
  :group 'skintwin
  :prefix "skintwin-db-")

;; Configuration options
(defcustom skintwin-db-supabase-url nil
  "URL of the Supabase project."
  :type 'string
  :group 'skintwin-db)

(defcustom skintwin-db-supabase-key nil
  "Anon key for the Supabase project."
  :type 'string
  :group 'skintwin-db)

(defcustom skintwin-db-auto-sync-interval 60
  "Interval in seconds for automatically synchronizing with the database."
  :type 'integer
  :group 'skintwin-db)

(defcustom skintwin-db-enable-auto-sync nil
  "Whether to automatically synchronize with the database."
  :type 'boolean
  :group 'skintwin-db)

;; Internal variables
(defvar skintwin-db--sync-timer nil
  "Timer for automatic database synchronization.")

(defvar skintwin-db--last-sync-time nil
  "Timestamp of the last database synchronization.")

(defvar skintwin-db--sync-in-progress nil
  "Flag indicating whether a synchronization is in progress.")

;; Utility functions

(defun skintwin-db-load-env ()
  "Load environment variables for Supabase configuration."
  (let ((env-file (expand-file-name ".env" default-directory)))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
          (let ((key (match-string 1))
                (value (match-string 2)))
            (cond
             ((string= key "VITE_SUPABASE_URL")
              (setq skintwin-db-supabase-url value))
             ((string= key "VITE_SUPABASE_ANON_KEY")
              (setq skintwin-db-supabase-key value)))))))))

(defun skintwin-db-initialize ()
  "Initialize the database connection and configuration."
  (interactive)
  (skintwin-db-load-env)
  (unless (and skintwin-db-supabase-url skintwin-db-supabase-key)
    (message "Supabase configuration not found. Please set skintwin-db-supabase-url and skintwin-db-supabase-key."))
  (when (and skintwin-db-enable-auto-sync (null skintwin-db--sync-timer))
    (setq skintwin-db--sync-timer
          (run-with-timer 0 skintwin-db-auto-sync-interval 'skintwin-db-sync-all))))

(defun skintwin-db-shutdown ()
  "Shutdown the database connection and clean up resources."
  (interactive)
  (when skintwin-db--sync-timer
    (cancel-timer skintwin-db--sync-timer)
    (setq skintwin-db--sync-timer nil)))

;; API request functions

(defun skintwin-db-request (method endpoint &optional data)
  "Make a request to the Supabase API.
METHOD is the HTTP method to use.
ENDPOINT is the API endpoint to request.
DATA is an optional alist to send as JSON."
  (unless (and skintwin-db-supabase-url skintwin-db-supabase-key)
    (error "Supabase configuration not set. Call skintwin-db-initialize first."))
  
  (let* ((url (concat skintwin-db-supabase-url endpoint))
         (url-request-method method)
         (url-request-extra-headers
          `(("apikey" . ,skintwin-db-supabase-key)
            ("Authorization" . ,(concat "Bearer " skintwin-db-supabase-key))
            ("Content-Type" . "application/json")))
         (url-request-data (when data (aichat-json-serialize data)))
         (buffer (url-retrieve-synchronously url t)))
    
    (if buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (let ((json-array-type 'list)
                (json-object-type 'hash-table)
                (json-key-type 'string))
            (condition-case nil
                (json-read)
              (error
                (message "Failed to parse JSON response")
                nil))))
      (message "Failed to connect to Supabase")
      nil)))

(defun skintwin-db-get (table &optional query)
  "Get data from TABLE with optional QUERY parameters."
  (let ((endpoint (concat "/rest/v1/" table))
        (args '()))
    
    (when query
      (let ((params (mapconcat (lambda (pair)
                                 (format "%s=%s"
                                         (url-hexify-string (car pair))
                                         (url-hexify-string (cdr pair))))
                               query "&")))
        (setq endpoint (concat endpoint "?" params))))
    
    (skintwin-db-request "GET" endpoint)))

(defun skintwin-db-post (table data)
  "Insert DATA into TABLE."
  (let ((endpoint (concat "/rest/v1/" table)))
    (skintwin-db-request "POST" endpoint data)))

(defun skintwin-db-patch (table id data)
  "Update item with ID in TABLE with DATA."
  (let ((endpoint (concat "/rest/v1/" table "?id=eq." (url-hexify-string id))))
    (skintwin-db-request "PATCH" endpoint data)))

(defun skintwin-db-delete (table id)
  "Delete item with ID from TABLE."
  (let ((endpoint (concat "/rest/v1/" table "?id=eq." (url-hexify-string id))))
    (skintwin-db-request "DELETE" endpoint)))

;; Data mapping functions

(defun skintwin-db-map-skin-layer-to-atom (layer)
  "Convert a skin layer database record to an atom."
  (let* ((id (gethash "id" layer))
         (name (gethash "name" layer))
         (description (gethash "description" layer))
         (order-index (gethash "order_index" layer))
         ;; Create the atom
         (layer-atom (aichat-symbolic-concept name)))
    
    ;; Add properties
    (when description
      (let ((has-description (aichat-symbolic-predicate "has_description")))
        (aichat-opencog-kb-add has-description)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-description
                                (list layer-atom description)))))
    
    ;; Add order index
    (let ((has-order (aichat-symbolic-predicate "has_order")))
      (aichat-opencog-kb-add has-order)
      (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                              has-order
                              (list layer-atom order-index))))
    
    ;; Add skin_layer inheritance
    (let ((skin-layer (aichat-symbolic-concept "skin_layer")))
      (aichat-opencog-kb-add skin-layer)
      (aichat-opencog-kb-add (aichat-symbolic-inheritance skin-layer layer-atom)))
    
    ;; Return the atom
    layer-atom))

(defun skintwin-db-map-cell-type-to-atom (cell-type)
  "Convert a cell type database record to an atom."
  (let* ((id (gethash "id" cell-type))
         (name (gethash "name" cell-type))
         (description (gethash "description" cell-type))
         (function (gethash "function" cell-type))
         (skin-layer-id (gethash "skin_layer_id" cell-type))
         ;; Create the atom
         (cell-type-atom (aichat-symbolic-concept name)))
    
    ;; Add properties
    (when description
      (let ((has-description (aichat-symbolic-predicate "has_description")))
        (aichat-opencog-kb-add has-description)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-description
                                (list cell-type-atom description)))))
    
    ;; Add function
    (when function
      (let ((has-function (aichat-symbolic-predicate "has_function")))
        (aichat-opencog-kb-add has-function)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-function
                                (list cell-type-atom function)))))
    
    ;; Add cell_type inheritance
    (let ((cell-type-concept (aichat-symbolic-concept "cell_type")))
      (aichat-opencog-kb-add cell-type-concept)
      (aichat-opencog-kb-add (aichat-symbolic-inheritance cell-type-concept cell-type-atom)))
    
    ;; Add relationship to skin layer if present
    (when skin-layer-id
      (let* ((skin-layers (skintwin-db-get "skin_layers" `(("id" . ,skin-layer-id))))
             (skin-layer (and skin-layers (> (length skin-layers) 0) (elt skin-layers 0)))
             (skin-layer-name (and skin-layer (gethash "name" skin-layer)))
             (skin-layer-atom (and skin-layer-name (aichat-symbolic-concept skin-layer-name))))
        
        (when skin-layer-atom
          (let ((has-location (aichat-symbolic-predicate "has_location")))
            (aichat-opencog-kb-add has-location)
            (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                    has-location
                                    (list cell-type-atom skin-layer-atom)))))))
    
    ;; Return the atom
    cell-type-atom))

(defun skintwin-db-map-skin-condition-to-atom (condition)
  "Convert a skin condition database record to an atom."
  (let* ((id (gethash "id" condition))
         (name (gethash "name" condition))
         (description (gethash "description" condition))
         (category (gethash "category" condition))
         (symptoms (gethash "symptoms" condition))
         ;; Create the atom
         (condition-atom (aichat-symbolic-concept name)))
    
    ;; Add properties
    (when description
      (let ((has-description (aichat-symbolic-predicate "has_description")))
        (aichat-opencog-kb-add has-description)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-description
                                (list condition-atom description)))))
    
    ;; Add category
    (when category
      (let ((has-category (aichat-symbolic-predicate "has_category")))
        (aichat-opencog-kb-add has-category)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-category
                                (list condition-atom category)))))
    
    ;; Add symptoms
    (when symptoms
      (let ((has-symptom (aichat-symbolic-predicate "has_symptom")))
        (aichat-opencog-kb-add has-symptom)
        (dolist (symptom symptoms)
          (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                  has-symptom
                                  (list condition-atom symptom))))))
    
    ;; Add skin_condition inheritance
    (let ((skin-condition (aichat-symbolic-concept "skin_condition")))
      (aichat-opencog-kb-add skin-condition)
      (aichat-opencog-kb-add (aichat-symbolic-inheritance skin-condition condition-atom)))
    
    ;; Get affected layers
    (let* ((affected-layers (skintwin-db-get "condition_affected_layers" `(("condition_id" . ,id))))
           (layer-ids (mapcar (lambda (rel) (gethash "layer_id" rel)) affected-layers)))
      
      (dolist (layer-id layer-ids)
        (let* ((skin-layers (skintwin-db-get "skin_layers" `(("id" . ,layer-id))))
               (skin-layer (and skin-layers (> (length skin-layers) 0) (elt skin-layers 0)))
               (skin-layer-name (and skin-layer (gethash "name" skin-layer)))
               (skin-layer-atom (and skin-layer-name (aichat-symbolic-concept skin-layer-name))))
          
          (when skin-layer-atom
            (let ((affects (aichat-symbolic-predicate "affects")))
              (aichat-opencog-kb-add affects)
              (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                      affects
                                      (list condition-atom skin-layer-atom))))))))
    
    ;; Return the atom
    condition-atom))

(defun skintwin-db-map-treatment-to-atom (treatment)
  "Convert a treatment database record to an atom."
  (let* ((id (gethash "id" treatment))
         (name (gethash "name" treatment))
         (description (gethash "description" treatment))
         (type (gethash "type" treatment))
         (formulation (gethash "formulation" treatment))
         (side-effects (gethash "side_effects" treatment))
         ;; Create the atom
         (treatment-atom (aichat-symbolic-concept name)))
    
    ;; Add properties
    (when description
      (let ((has-description (aichat-symbolic-predicate "has_description")))
        (aichat-opencog-kb-add has-description)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-description
                                (list treatment-atom description)))))
    
    ;; Add type
    (when type
      (let ((has-type (aichat-symbolic-predicate "has_type")))
        (aichat-opencog-kb-add has-type)
        (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                has-type
                                (list treatment-atom type)))))
    
    ;; Add formulation
    (when formulation
      (let ((has-formulation (aichat-symbolic-predicate "has_formulation")))
        (aichat-opencog-kb-add has-formulation)
        (dolist (form formulation)
          (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                  has-formulation
                                  (list treatment-atom form))))))
    
    ;; Add side effects
    (when side-effects
      (let ((has-side-effect (aichat-symbolic-predicate "has_side_effect")))
        (aichat-opencog-kb-add has-side-effect)
        (dolist (effect side-effects)
          (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                  has-side-effect
                                  (list treatment-atom effect))))))
    
    ;; Add treatment inheritance
    (let ((treatment-concept (aichat-symbolic-concept "treatment")))
      (aichat-opencog-kb-add treatment-concept)
      (aichat-opencog-kb-add (aichat-symbolic-inheritance treatment-concept treatment-atom)))
    
    ;; Get treatment mechanisms
    (let* ((mechanisms (skintwin-db-get "treatment_mechanisms" `(("treatment_id" . ,id))))
           (mechanism-names (mapcar (lambda (mech) (gethash "mechanism" mech)) mechanisms)))
      
      (dolist (mechanism-name mechanism-names)
        (let ((has-mechanism (aichat-symbolic-predicate "has_mechanism")))
          (aichat-opencog-kb-add has-mechanism)
          (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                  has-mechanism
                                  (list treatment-atom mechanism-name))))))
    
    ;; Get treatment efficacy data
    (let* ((efficacies (skintwin-db-get "treatment_efficacy" `(("treatment_id" . ,id))))
           (condition-data (mapcar (lambda (eff) 
                                     (cons (gethash "condition_id" eff)
                                           (gethash "efficacy" eff)))
                                   efficacies)))
      
      (dolist (cond-item condition-data)
        (let* ((condition-id (car cond-item))
               (efficacy (cdr cond-item))
               (skin-conditions (skintwin-db-get "skin_conditions" `(("id" . ,condition-id))))
               (skin-condition (and skin-conditions (> (length skin-conditions) 0) (elt skin-conditions 0)))
               (condition-name (and skin-condition (gethash "name" skin-condition)))
               (condition-atom (and condition-name (aichat-symbolic-concept condition-name))))
          
          (when condition-atom
            (let ((treats (aichat-symbolic-predicate "treats")))
              (aichat-opencog-kb-add treats)
              ;; Create TV with efficacy value as strength
              (let ((tv (aichat-pln-make-tv efficacy 10.0)))
                (aichat-opencog-kb-add (aichat-symbolic-evaluation 
                                        treats
                                        (list treatment-atom condition-atom)
                                        tv))))))))
    
    ;; Return the atom
    treatment-atom))

(defun skintwin-db-map-knowledge-node-to-atom (node)
  "Convert a knowledge node database record to an atom."
  (let* ((id (gethash "id" node))
         (type (gethash "type" node))
         (name (gethash "name" node))
         (value (gethash "value" node))
         ;; Create the atom based on type
         (atom (cond
                ((string= type "concept")
                 (aichat-symbolic-concept name))
                ((string= type "predicate")
                 (aichat-symbolic-predicate name))
                (t
                 (aichat-symbolic-atom (intern type) name nil nil nil)))))
    
    ;; Add value if present
    (when value
      (setf (aichat-symbolic-atom-value atom) value))
    
    ;; Return the atom
    atom))

(defun skintwin-db-map-attention-value (attention-value node-id)
  "Map attention value database record to an atom's attention value."
  (let* ((sti (gethash "sti" attention-value))
         (lti (gethash "lti" attention-value))
         (vlti (gethash "vlti" attention-value))
         ;; Get the knowledge node
         (nodes (skintwin-db-get "knowledge_nodes" `(("id" . ,node-id))))
         (node (and nodes (> (length nodes) 0) (elt nodes 0)))
         (node-type (and node (gethash "type" node)))
         (node-name (and node (gethash "name" node)))
         ;; Find or create the atom
         (atom (when node
                 (or (aichat-opencog-kb-get node-name)
                     (skintwin-db-map-knowledge-node-to-atom node)))))
    
    ;; Set attention value if atom exists
    (when atom
      (aichat-ecan-set-av atom (aichat-ecan-make-av sti lti vlti)))
    
    ;; Return the atom
    atom))

;; Synchronization functions

(defun skintwin-db-sync-skin-layers ()
  "Synchronize skin layers from the database to the knowledge base."
  (interactive)
  (message "Syncing skin layers...")
  (let ((layers (skintwin-db-get "skin_layers")))
    (when layers
      (dolist (layer layers)
        (let ((atom (skintwin-db-map-skin-layer-to-atom layer)))
          (aichat-opencog-kb-add atom))))
    (message "Synced %d skin layers" (length layers))))

(defun skintwin-db-sync-cell-types ()
  "Synchronize cell types from the database to the knowledge base."
  (interactive)
  (message "Syncing cell types...")
  (let ((cell-types (skintwin-db-get "cell_types")))
    (when cell-types
      (dolist (cell-type cell-types)
        (let ((atom (skintwin-db-map-cell-type-to-atom cell-type)))
          (aichat-opencog-kb-add atom))))
    (message "Synced %d cell types" (length cell-types))))

(defun skintwin-db-sync-skin-conditions ()
  "Synchronize skin conditions from the database to the knowledge base."
  (interactive)
  (message "Syncing skin conditions...")
  (let ((conditions (skintwin-db-get "skin_conditions")))
    (when conditions
      (dolist (condition conditions)
        (let ((atom (skintwin-db-map-skin-condition-to-atom condition)))
          (aichat-opencog-kb-add atom))))
    (message "Synced %d skin conditions" (length conditions))))

(defun skintwin-db-sync-treatments ()
  "Synchronize treatments from the database to the knowledge base."
  (interactive)
  (message "Syncing treatments...")
  (let ((treatments (skintwin-db-get "treatments")))
    (when treatments
      (dolist (treatment treatments)
        (let ((atom (skintwin-db-map-treatment-to-atom treatment)))
          (aichat-opencog-kb-add atom))))
    (message "Synced %d treatments" (length treatments))))

(defun skintwin-db-sync-knowledge-nodes ()
  "Synchronize knowledge nodes from the database to the knowledge base."
  (interactive)
  (message "Syncing knowledge nodes...")
  (let ((nodes (skintwin-db-get "knowledge_nodes")))
    (when nodes
      (dolist (node nodes)
        (let ((atom (skintwin-db-map-knowledge-node-to-atom node)))
          (aichat-opencog-kb-add atom))))
    (message "Synced %d knowledge nodes" (length nodes))))

(defun skintwin-db-sync-attention-values ()
  "Synchronize attention values from the database to the knowledge base."
  (interactive)
  (message "Syncing attention values...")
  (let ((attention-values (skintwin-db-get "attention_values")))
    (when attention-values
      (dolist (av attention-values)
        (let ((node-id (gethash "node_id" av)))
          (skintwin-db-map-attention-value av node-id))))
    (message "Synced %d attention values" (length attention-values))))

(defun skintwin-db-sync-all ()
  "Synchronize all data from the database to the knowledge base."
  (interactive)
  (when skintwin-db--sync-in-progress
    (message "Synchronization already in progress, skipping.")
    (cl-return-from skintwin-db-sync-all))
  
  (setq skintwin-db--sync-in-progress t)
  (message "Starting full database synchronization...")
  
  (condition-case err
      (progn
        ;; Sync in order of dependencies
        (skintwin-db-sync-skin-layers)
        (skintwin-db-sync-cell-types)
        (skintwin-db-sync-skin-conditions)
        (skintwin-db-sync-treatments)
        (skintwin-db-sync-knowledge-nodes)
        (skintwin-db-sync-attention-values)
        
        (setq skintwin-db--last-sync-time (current-time))
        (message "Database synchronization completed."))
    (error
     (message "Error during synchronization: %s" (error-message-string err))))
  
  (setq skintwin-db--sync-in-progress nil))

;; Knowledge Base to Database functions

(defun skintwin-db-atom-to-knowledge-node (atom)
  "Convert an atom to a knowledge node database record."
  (let* ((type (symbol-name (aichat-symbolic-atom-type atom)))
         (name (aichat-symbolic-atom-name atom))
         (value (aichat-symbolic-atom-value atom))
         ;; Create the database record
         (data `(("type" . ,type)
                 ("name" . ,name))))
    
    ;; Add value if present
    (when value
      (setq data (append data `(("value" . ,value)))))
    
    ;; Return the data
    data))

(defun skintwin-db-attention-value-to-db (atom)
  "Convert an atom's attention value to a database record."
  (let* ((name (aichat-symbolic-atom-name atom))
         (av (aichat-ecan-get-av atom))
         (sti (aichat-ecan-av-sti av))
         (lti (aichat-ecan-av-lti av))
         (vlti (aichat-ecan-av-vlti av))
         ;; Get the knowledge node ID
         (nodes (skintwin-db-get "knowledge_nodes" `(("name" . ,name))))
         (node (and nodes (> (length nodes) 0) (elt nodes 0)))
         (node-id (and node (gethash "id" node)))
         ;; Create the data
         (data (and node-id `(("node_id" . ,node-id)
                             ("sti" . ,sti)
                             ("lti" . ,lti)
                             ("vlti" . ,vlti)))))
    
    ;; Return the data
    data))

(defun skintwin-db-push-atom (atom)
  "Push an atom to the database as a knowledge node."
  (let* ((name (aichat-symbolic-atom-name atom))
         (data (skintwin-db-atom-to-knowledge-node atom))
         ;; Check if the node already exists
         (nodes (skintwin-db-get "knowledge_nodes" `(("name" . ,name))))
         (node (and nodes (> (length nodes) 0) (elt nodes 0)))
         (node-id (and node (gethash "id" node))))
    
    (if node-id
        ;; Update existing node
        (skintwin-db-patch "knowledge_nodes" node-id data)
      ;; Create new node
      (skintwin-db-post "knowledge_nodes" data))
    
    ;; Push attention value
    (let ((av-data (skintwin-db-attention-value-to-db atom)))
      (when av-data
        ;; Check if attention value already exists
        (let* ((av-records (skintwin-db-get "attention_values" `(("node_id" . ,(cdr (assoc "node_id" av-data))))))
               (av-record (and av-records (> (length av-records) 0) (elt av-records 0)))
               (av-id (and av-record (gethash "id" av-record))))
          
          (if av-id
              ;; Update existing attention value
              (skintwin-db-patch "attention_values" av-id av-data)
            ;; Create new attention value
            (skintwin-db-post "attention_values" av-data)))))))

(defun skintwin-db-push-all-atoms ()
  "Push all atoms in the knowledge base to the database."
  (interactive)
  (message "Pushing all atoms to the database...")
  (let ((count 0))
    (maphash (lambda (k v)
               (when (member (aichat-symbolic-atom-type v) '(concept predicate))
                 (skintwin-db-push-atom v)
                 (cl-incf count)))
             aichat-opencog-kb)
    (message "Pushed %d atoms to the database" count)))

;; Initialize module when loaded
(defun skintwin-db-module-init ()
  "Initialize the database module when loaded."
  (skintwin-db-initialize))

(provide 'skintwin-db)
;;; skintwin-db.el ends here