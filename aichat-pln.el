;; aichat-pln.el --- Probabilistic Logic Networks for aichat -*- lexical-binding: t; -*-

(require 'aichat-symbolic)
(require 'aichat-util)

(defgroup aichat-pln nil
  "Probabilistic Logic Networks for aichat."
  :group 'aichat
  :prefix "aichat-pln-")

;; Truth Value Operations
(defun aichat-pln-tv-mean (tv)
  "Get mean (strength) from truth value TV."
  (car tv))

(defun aichat-pln-tv-count (tv)
  "Get count from truth value TV."
  (cadr tv))

(defun aichat-pln-tv-confidence (tv)
  "Calculate confidence from truth value TV."
  (/ (aichat-pln-tv-count tv)
     (+ (aichat-pln-tv-count tv) 1.0)))

(defun aichat-pln-make-tv (mean count)
  "Create truth value with MEAN and COUNT."
  (list mean count))

;; Basic PLN Rules
(defun aichat-pln-deduction (ab-tv bc-tv)
  "Apply deduction rule: (A→B, B→C) ⊢ A→C.
AB-TV and BC-TV are truth values of premises."
  (let* ((sab (aichat-pln-tv-mean ab-tv))
         (sbc (aichat-pln-tv-mean bc-tv))
         (nab (aichat-pln-tv-count ab-tv))
         (nbc (aichat-pln-tv-count bc-tv))
         ;; Strength calculation
         (s (* sab sbc))
         ;; Count calculation
         (n (min nab nbc)))
    (aichat-pln-make-tv s n)))

(defun aichat-pln-inversion (ab-tv a-tv b-tv)
  "Apply inversion rule: (A→B, A, B) ⊢ B→A.
AB-TV, A-TV, B-TV are truth values."
  (let* ((sab (aichat-pln-tv-mean ab-tv))
         (sa (aichat-pln-tv-mean a-tv))
         (sb (aichat-pln-tv-mean b-tv))
         (nab (aichat-pln-tv-count ab-tv))
         (na (aichat-pln-tv-count a-tv))
         (nb (aichat-pln-tv-count b-tv))
         ;; Strength calculation using Bayes rule
         (s (if (> sb 0) (/ (* sab sa) sb) 0))
         ;; Count calculation
         (n (min nab (min na nb))))
    (aichat-pln-make-tv s n)))

(defun aichat-pln-revision (tv1 tv2)
  "Apply revision rule to combine truth values TV1 and TV2."
  (let* ((s1 (aichat-pln-tv-mean tv1))
         (s2 (aichat-pln-tv-mean tv2))
         (n1 (aichat-pln-tv-count tv1))
         (n2 (aichat-pln-tv-count tv2))
         ;; Weighted average of strengths
         (s (/ (+ (* s1 n1) (* s2 n2))
              (+ n1 n2)))
         ;; Sum of counts
         (n (+ n1 n2)))
    (aichat-pln-make-tv s n)))

;; Forward Chaining
(defun aichat-pln-forward-chain (kb steps)
  "Apply forward chaining for STEPS iterations on knowledge base KB."
  (let ((new-atoms '()))
    (dotimes (_ steps)
      ;; Try deduction rule
      (maphash (lambda (k1 v1)
                 (when (eq (aichat-symbolic-atom-type v1) 'inheritance)
                   (let* ((a->b v1)
                         (b (cadr (aichat-symbolic-atom-links v1))))
                     (maphash (lambda (k2 v2)
                               (when (and (eq (aichat-symbolic-atom-type v2) 'inheritance)
                                        (equal (car (aichat-symbolic-atom-links v2)) b))
                                 (let* ((b->c v2)
                                       (a (car (aichat-symbolic-atom-links a->b)))
                                       (c (cadr (aichat-symbolic-atom-links b->c)))
                                       (new-tv (aichat-pln-deduction
                                              (aichat-symbolic-atom-tv a->b)
                                              (aichat-symbolic-atom-tv b->c)))
                                       (new-atom (aichat-symbolic-inheritance a c new-tv)))
                                   (push new-atom new-atoms))))
                             kb))))
               kb))
    new-atoms))

;; Backward Chaining
(defun aichat-pln-backward-chain (goal kb depth)
  "Try to prove GOAL using backward chaining on KB up to DEPTH."
  (when (> depth 0)
    (let ((results '()))
      ;; Direct match
      (let ((matches (aichat-opencog-match-pattern goal kb)))
        (when matches
          (push (cons 1.0 (car matches)) results)))
      ;; Try deduction
      (when (eq (aichat-symbolic-atom-type goal) 'inheritance)
        (let ((a (car (aichat-symbolic-atom-links goal)))
              (c (cadr (aichat-symbolic-atom-links goal))))
          (maphash (lambda (k v)
                    (when (and (eq (aichat-symbolic-atom-type v) 'inheritance)
                             (equal (car (aichat-symbolic-atom-links v)) a))
                      (let* ((a->b v)
                            (b (cadr (aichat-symbolic-atom-links v)))
                            (b->c-goal (aichat-symbolic-inheritance b c nil))
                            (b->c-results (aichat-pln-backward-chain b->c-goal kb (1- depth))))
                        (dolist (b->c-result b->c-results)
                          (let* ((confidence (car b->c-result))
                                (b->c (cdr b->c-result))
                                (new-tv (aichat-pln-deduction
                                       (aichat-symbolic-atom-tv a->b)
                                       (aichat-symbolic-atom-tv b->c))))
                            (push (cons (* confidence
                                         (aichat-pln-tv-confidence new-tv))
                                      (aichat-symbolic-inheritance a c new-tv))
                                  results))))))
                  kb)))
      results)))

;; Integration with OpenCog Architecture
(defun aichat-pln-reason (kb)
  "Apply PLN reasoning on knowledge base KB."
  (let ((new-knowledge (aichat-pln-forward-chain kb 3)))
    (dolist (atom new-knowledge)
      (puthash (aichat-symbolic-atom-name atom) atom kb))
    kb))

(provide 'aichat-pln)