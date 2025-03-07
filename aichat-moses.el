;; aichat-moses.el --- Meta-Optimizing Semantic Evolutionary Search -*- lexical-binding: t; -*-

(require 'aichat-symbolic)
(require 'aichat-util)

(defgroup aichat-moses nil
  "MOSES program learning for aichat."
  :group 'aichat
  :prefix "aichat-moses-")

;; Knob Representation
(cl-defstruct (aichat-moses-knob (:constructor aichat-moses-knob--create))
  "Representation of a program variation point."
  type      ; Type of knob (e.g., 'numeric, 'boolean, 'function)
  values    ; List of possible values
  current   ; Current value index
  position) ; Position in program

(defun aichat-moses-make-knob (type values &optional current position)
  "Create knob of TYPE with VALUES, optional CURRENT and POSITION."
  (aichat-moses-knob--create
   :type type
   :values values
   :current (or current 0)
   :position position))

(defun aichat-moses-knob-value (knob)
  "Get current value of KNOB."
  (nth (aichat-moses-knob-current knob)
       (aichat-moses-knob-values knob)))

(defun aichat-moses-knob-next (knob)
  "Move KNOB to next value and return it."
  (let ((next (mod (1+ (aichat-moses-knob-current knob))
                   (length (aichat-moses-knob-values knob)))))
    (setf (aichat-moses-knob-current knob) next)
    (aichat-moses-knob-value knob)))

(defun aichat-moses-knob-random (knob)
  "Set KNOB to random value and return it."
  (let ((rand (random (length (aichat-moses-knob-values knob)))))
    (setf (aichat-moses-knob-current knob) rand)
    (aichat-moses-knob-value knob)))

;; Enhanced Program Representation
(cl-defstruct (aichat-moses-deme (:include aichat-moses-program))
  "Representation of a program instance with knobs."
  knobs)    ; List of variation points

(defun aichat-moses-make-deme (expr &optional score complexity knobs)
  "Create deme with expression EXPR and optional SCORE, COMPLEXITY, KNOBS."
  (let ((deme (make-aichat-moses-deme
               :expression expr
               :score (or score 0.0)
               :complexity (or complexity (aichat-moses-complexity expr))
               :knobs (or knobs (aichat-moses-find-knobs expr)))))
    (aichat-moses-update-deme deme)
    deme))

(defun aichat-moses-find-knobs (expr)
  "Find potential variation points in EXPR."
  (let ((knobs '()))
    (cl-labels ((walk (e path)
                      (cond
                       ;; Control flow variation
                       ((and (listp e) (memq (car e) '(if while dolist)))
                        (push (aichat-moses-make-knob
                               'control
                               (list 'if 'while 'dolist)
                               0 path)
                               knobs))
                       ;; Binding variation
                       ((and (listp e) (eq (car e) 'let))
                        (push (aichat-moses-make-knob
                               'binding
                               '((let . 2) (let* . 2) (progn . 2))
                               0 path)
                               knobs))
                       ;; Numeric constant
                       ((numberp e)
                        (push (aichat-moses-make-knob
                              'numeric
                              (list e (+ e 1) (- e 1) (* e 2) (/ e 2))
                              0 path)
                              knobs))
                       ;; Function call
                       ((and (listp e) (not (null e)))
                        (let ((fn (car e)))
                          ;; Function variation
                          (when (assoc fn aichat-moses-functions)
                            (push (aichat-moses-make-knob
                                  'function
                                  (cl-remove-if-not
                                   (lambda (f)
                                     (= (aichat-moses-function-arity (car f))
                                        (aichat-moses-function-arity fn)))
                                   aichat-moses-functions)
                                  0 path)
                                  knobs))
                          ;; Recurse on arguments
                          (cl-loop for arg in (cdr e)
                                  for i from 0
                                  do (walk arg (append path (list i)))))))))
      (walk expr '()))
    (nreverse knobs)))

(defun aichat-moses-update-deme (deme)
  "Update deme's expression based on current knob values."
  (let ((expr (aichat-moses-program-expression deme)))
    (dolist (knob (aichat-moses-deme-knobs deme))
      (setq expr (aichat-moses-apply-knob expr knob)))
    (setf (aichat-moses-program-expression deme) expr))
  deme)

(defun aichat-moses-apply-knob (expr knob)
  "Apply KNOB's current value to EXPR at its position."
  (if (null (aichat-moses-knob-position knob))
      (aichat-moses-knob-value knob)
    (let ((path (aichat-moses-knob-position knob)))
      (aichat-moses-update-at-path expr path (aichat-moses-knob-value knob)))))

(defun aichat-moses-update-at-path (expr path value)
  "Update EXPR at PATH with VALUE."
  (if (null path)
      value
    (let ((pos (car path))
          (rest (cdr path)))
      (if (listp expr)
          (let ((copy (copy-sequence expr)))
            (setf (nth pos copy)
                  (aichat-moses-update-at-path (nth pos expr) rest value))
            copy)
        expr))))

;; Operator Definitions
(defconst aichat-moses-terminals
  '(x                    ; Input variable
    0 1 -1 2            ; Common constants
    t nil               ; Boolean constants
    break continue)     ; Control flow terminals
  "Terminal symbols for program generation.")

(defconst aichat-moses-functions
  '((+ . 2) (- . 2) (* . 2) (/ . 2)   ; Arithmetic
    (mod . 2) (expt . 2)               ; More arithmetic
    (and . 2) (or . 2) (not . 1)       ; Boolean logic
    (< . 2) (<= . 2) (> . 2) (>= . 2)  ; Comparison
    (equal . 2) (if . 3)               ; Basic control flow
    (while . 2) (dolist . 2)           ; Looping constructs
    (progn . 2) (let . 2)              ; Sequencing and binding
    (list . 2) (car . 1) (cdr . 1)     ; List operations
    (cons . 2) (append . 2))
  "Available functions and their arities.")

(defun aichat-moses-function-arity (fn)
  "Get arity of function FN."
  (cdr (assoc fn aichat-moses-functions)))

(defun aichat-moses-safe-eval (expr bindings)
  "Safely evaluate EXPR with BINDINGS."
  (condition-case err
      (let* ((max-lisp-eval-depth 20)   ; Prevent infinite recursion
             (max-specpdl-size 50)      ; Limit special bindings
             (catch-tag (gensym))
             (result
              (catch catch-tag
                (let ((while-count 0))
                  ;; Add loop control
                  (cl-letf (((symbol-function 'while)
                            (lambda (test body)
                              (when (> (cl-incf while-count) 100)
                                (throw catch-tag nil))
                              (while test body))))
                    (eval `(let ,bindings ,expr)))))))
        result)
    (error nil)))

;; Program Representation
(cl-defstruct (aichat-moses-program (:constructor aichat-moses-program--create))
  "Representation of an evolved program."
  expression    ; Symbolic expression
  score        ; Fitness score
  complexity)  ; Program complexity penalty

(defun aichat-moses-make-program (expr &optional score complexity)
  "Create program with expression EXPR, optional SCORE and COMPLEXITY."
  (aichat-moses-program--create
   :expression expr
   :score (or score 0.0)
   :complexity (or complexity (aichat-moses-complexity expr))))

;; Program Complexity
(defun aichat-moses-complexity (expr)
  "Calculate complexity score for expression EXPR."
  (cond ((atom expr) 1)
        ((listp expr)
         (+ 1 (apply #'+ (mapcar #'aichat-moses-complexity expr))))
        (t 1)))

;; Population Management
(defvar aichat-moses-population nil
  "Current population of programs.")

(defun aichat-moses-init-population (size &optional init-fn)
  "Initialize population of SIZE programs using INIT-FN."
  (setq aichat-moses-population
        (cl-loop repeat size
                 collect (funcall (or init-fn #'aichat-moses-random-program)))))

(defun aichat-moses-random-program ()
  "Generate a random initial program."
  (aichat-moses-make-program
   (aichat-moses-random-expr 3)))

(defun aichat-moses-random-expr (depth)
  "Generate random expression up to DEPTH."
  (if (or (<= depth 0) (< (random 100) 30))
      (aichat-moses-random-terminal)
    (aichat-moses-random-function)))

(defun aichat-moses-random-terminal ()
  "Generate random terminal."
  (nth (random (length aichat-moses-terminals))
       aichat-moses-terminals))

(defun aichat-moses-random-function ()
  "Generate random function call."
  (let* ((fn (nth (random (length aichat-moses-functions))
                  aichat-moses-functions))
         (arity (aichat-moses-function-arity fn)))
    (cons fn
          (cl-loop repeat arity
                   collect (aichat-moses-random-expr (1- depth))))))

;; Genetic Operations
(defun aichat-moses-crossover (prog1 prog2)
  "Perform crossover between PROG1 and PROG2."
  (let* ((expr1 (aichat-moses-program-expression prog1))
         (expr2 (aichat-moses-program-expression prog2))
         (point1 (aichat-moses-random-point expr1))
         (point2 (aichat-moses-random-point expr2))
         (new-expr1 (aichat-moses-replace-point expr1 point1
                                               (aichat-moses-get-point expr2 point2)))
         (new-expr2 (aichat-moses-replace-point expr2 point2
                                               (aichat-moses-get-point expr1 point1))))
    (list (aichat-moses-make-program new-expr1)
          (aichat-moses-make-program new-expr2))))

(defun aichat-moses-mutate (prog)
  "Mutate program PROG."
  (let* ((expr (aichat-moses-program-expression prog))
         (point (aichat-moses-random-point expr))
         (new-expr (aichat-moses-replace-point
                   expr point
                   (aichat-moses-random-expr 2))))
    (aichat-moses-make-program new-expr)))

;; Program Evaluation
(defun aichat-moses-evaluate (prog data)
  "Evaluate program PROG on training DATA."
  (let ((expr (aichat-moses-program-expression prog))
        (score 0.0))
    (dolist (case data)
      (let* ((inputs (car case))
             (expected (cdr case))
             (result (aichat-moses-execute expr inputs))
             (case-score (aichat-moses-score-case result expected)))
        (setf score (+ score case-score))))
    (setf (aichat-moses-program-score prog)
          (- (/ score (length data))
             (* 0.01 (aichat-moses-program-complexity prog))))
    prog))

(defun aichat-moses-execute (expr inputs)
  "Execute expression EXPR with INPUTS."
  (let ((bindings
         (cond ((numberp inputs)
                `((x ,inputs)))
               ((listp inputs)
                (cl-loop for val in inputs
                         for i from 0
                         collect `(,(intern (format "x%d" i)) ,val)))
               (t `((x ,inputs))))))
    (aichat-moses-safe-eval expr bindings)))

(defun aichat-moses-score-case (result expected)
  "Score RESULT against EXPECTED value."
  (cond
   ;; Exact match
   ((equal result expected) 1.0)
   ;; Numeric comparison
   ((and (numberp result) (numberp expected))
    (let ((diff (abs (- result expected))))
      (if (< diff 0.0001)
          0.9
        (/ 1.0 (1+ diff)))))
   ;; List comparison
   ((and (listp result) (listp expected))
    (if (equal (length result) (length expected))
        (* 0.5 (cl-loop for r in result
                        for e in expected
                        sum (aichat-moses-score-case r e)))
      0.0))
   ;; No match
   (t 0.0)))

;; Program Analysis
(defun aichat-moses-simplify (expr)
  "Simplify expression EXPR."
  (cond
   ;; Control flow simplification
   ((and (listp expr) (memq (car expr) '(if while dolist)))
    (let ((op (car expr))
          (test (aichat-moses-simplify (cadr expr)))
          (body (aichat-moses-simplify (caddr expr))))
      (cond
       ;; (if t x y) => x
       ((and (eq op 'if) (eq test t))
        (cadr expr))
       ;; (if nil x y) => y
       ((and (eq op 'if) (eq test nil))
        (caddr expr))
       ;; (while nil x) => nil
       ((and (eq op 'while) (eq test nil))
        nil)
       ;; Keep as is
       (t (list op test body)))))
   ;; Arithmetic simplification
   ((and (listp expr) (= (length expr) 3))
    (let ((op (car expr))
          (x (aichat-moses-simplify (cadr expr)))
          (y (aichat-moses-simplify (caddr expr))))
      (cond
       ;; x + 0 = x
       ((and (eq op '+) (equal y 0)) x)
       ;; 0 + x = x
       ((and (eq op '+) (equal x 0)) y)
       ;; x * 1 = x
       ((and (eq op '*) (equal y 1)) x)
       ;; 1 * x = x
       ((and (eq op '*) (equal x 1)) y)
       ;; x * 0 = 0
       ((and (eq op '*) (or (equal x 0) (equal y 0))) 0)
       ;; Keep as is
       (t (list op x y)))))
   ;; No simplification possible
   (t expr)))

;; Program Visualization
(defun aichat-moses-program-to-string (prog)
  "Convert program PROG to readable string."
  (let ((expr (aichat-moses-program-expression prog)))
    (format "%s\nScore: %.3f\nComplexity: %d"
            (aichat-moses-expr-to-string expr)
            (aichat-moses-program-score prog)
            (aichat-moses-program-complexity prog))))

(defun aichat-moses-expr-to-string (expr)
  "Convert expression EXPR to readable string."
  (cond ((null expr) "nil")
        ((atom expr) (format "%s" expr))
        (t (format "(%s %s)"
                  (car expr)
                  (mapconcat #'aichat-moses-expr-to-string
                            (cdr expr) " ")))))

;; Evolution
(defun aichat-moses-evolve (generations data &optional pop-size)
  "Evolve programs for GENERATIONS on training DATA."
  (let ((pop-size (or pop-size 100))
        (best-deme nil))
    ;; Initialize population
    (setq aichat-moses-population
          (cl-loop repeat pop-size
                   collect (aichat-moses-make-deme
                           (aichat-moses-random-expr 3))))
    
    ;; Evolution loop
    (dotimes (gen generations)
      ;; Evaluate all programs
      (dolist (prog aichat-moses-population)
        (aichat-moses-evaluate prog data))
      
      ;; Sort by fitness
      (setq aichat-moses-population
            (sort aichat-moses-population
                  (lambda (a b)
                    (> (aichat-moses-program-score a)
                       (aichat-moses-program-score b)))))

      ;; Update best deme
      (when (or (null best-deme)
                (> (aichat-moses-program-score (car aichat-moses-population))
                   (aichat-moses-program-score best-deme)))
        (setq best-deme (car aichat-moses-population)))
      
      ;; Keep best programs
      (setq aichat-moses-population
            (cl-subseq aichat-moses-population 0 (/ pop-size 2)))
      
      ;; Create new programs through knob variation
      (let ((new-programs '()))
        ;; Knob exploration
        (dotimes (_ (/ pop-size 4))
          (let* ((p1 (nth (random (length aichat-moses-population))
                         aichat-moses-population)))
            ;; Create variation by tweaking knobs
            (let ((variant (copy-aichat-moses-deme p1)))
              ;; Randomly adjust some knobs
              (dolist (knob (aichat-moses-deme-knobs variant))
                (when (< (random 100) 30) ; 30% chance to change each knob
                  (aichat-moses-knob-random knob)))
              ;; Update expression and add to population
              (aichat-moses-update-deme variant)
              (push variant new-programs))))
        
        ;; Mutation
        (dotimes (_ (/ pop-size 4))
          (let* ((p (nth (random (length aichat-moses-population))
                        aichat-moses-population))
                 (mutant (aichat-moses-mutate p)))
            (push mutant new-programs)))
        
        ;; Update population
        (setq aichat-moses-population
              (append aichat-moses-population new-programs))))
    
    ;; Return best program found
    best-deme))

;; Integration with OpenCog
(defun aichat-moses-learn-pattern (examples)
  "Learn program from EXAMPLES using MOSES."
  (let* ((data (mapcar (lambda (ex)
                         (cons (car ex) (cdr ex)))
                       examples))
         (best-program (aichat-moses-evolve 50 data)))
    (aichat-moses-program-expression best-program)))

(provide 'aichat-moses)