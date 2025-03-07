;; aichat-symbolic.el --- Symbolic expression handling for aichat  -*- lexical-binding: t; -*-

(require 'aichat-util)

(defgroup aichat-symbolic nil
  "Symbolic expression handling for aichat."
  :group 'aichat
  :prefix "aichat-symbolic-")

;; Basic Atom Types
(defconst aichat-symbolic-atom-types
  '(concept predicate evaluation list member inheritance)
  "Basic atom types supported.")

(cl-defstruct (aichat-symbolic-atom (:constructor aichat-symbolic-atom--create))
  "Basic atom structure."
  type    ; Atom type (concept, predicate, etc)
  name    ; Identifier/name
  value   ; Value/content
  tv      ; Truth value (confidence, count)
  links)  ; Connected atoms

(defun aichat-symbolic-atom (type name &optional value tv links)
  "Create a new atom of TYPE with NAME.
Optional VALUE, TV (truth value), and LINKS to other atoms."
  (unless (memq type aichat-symbolic-atom-types)
    (error "Invalid atom type: %s" type))
  (aichat-symbolic-atom--create :type type
                               :name name
                               :value value
                               :tv (or tv '(1.0 1))
                               :links (or links nil)))

(defun aichat-symbolic-concept (name &optional tv)
  "Create a ConceptNode with NAME and optional TV."
  (aichat-symbolic-atom 'concept name nil tv nil))

(defun aichat-symbolic-predicate (name &optional tv)
  "Create a PredicateNode with NAME and optional TV."
  (aichat-symbolic-atom 'predicate name nil tv nil))

(defun aichat-symbolic-evaluation (pred args &optional tv)
  "Create an EvaluationLink with PRED and ARGS, optional TV."
  (aichat-symbolic-atom 'evaluation nil nil tv
                       (cons pred args)))

(defun aichat-symbolic-inheritance (parent child &optional tv)
  "Create an InheritanceLink between PARENT and CHILD atoms."
  (aichat-symbolic-atom 'inheritance nil nil tv
                       (list parent child)))

(defun aichat-symbolic-member (element set &optional tv)
  "Create a MemberLink between ELEMENT and SET atoms."
  (aichat-symbolic-atom 'member nil nil tv
                       (list element set)))

(defun aichat-symbolic-list (&rest elements)
  "Create a ListLink containing ELEMENTS."
  (aichat-symbolic-atom 'list nil nil '(1.0 1) elements))

(defun aichat-symbolic-atom-to-string (atom)
  "Convert ATOM to string representation."
  (let ((type (aichat-symbolic-atom-type atom))
        (name (aichat-symbolic-atom-name atom))
        (value (aichat-symbolic-atom-value atom))
        (tv (aichat-symbolic-atom-tv atom))
        (links (aichat-symbolic-atom-links atom)))
    (format "(%s %s%s%s%s)"
            type
            (if name (format "\"%s\" " name) "")
            (if value (format "%s " value) "")
            (if links
                (mapconcat (lambda (x) (concat " " (aichat-symbolic-atom-to-string x)))
                          links "")
              "")
            (format " <%.2f %.0f>" (car tv) (cadr tv)))))

;; Example usage:
;; (let ((john (aichat-symbolic-concept "John"))
;;       (human (aichat-symbolic-concept "human"))
;;       (likes (aichat-symbolic-predicate "likes"))
;;       (pizza (aichat-symbolic-concept "pizza")))
;;   ;; John is human
;;   (aichat-symbolic-inheritance human john)
;;   ;; John likes pizza
;;   (aichat-symbolic-evaluation likes 
;;     (aichat-symbolic-list john pizza)))

(provide 'aichat-symbolic)