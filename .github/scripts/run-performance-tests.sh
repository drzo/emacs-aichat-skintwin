#!/bin/bash
# Performance test script for emacs-aichat-skintwin
# This script is called by the performance-tests.yml workflow

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

echo "=== Performance Test Suite ==="

# Test 1: AtomSpace operations
echo ""
echo "Test 1: AtomSpace Operations (100 iterations)"
cask emacs --batch -L . \
  --eval "(require 'aichat-opencog)" \
  --eval "(benchmark-run 100 \
    (aichat-opencog-add-atom 'ConceptNode \"test-concept\"))" \
  --eval "(message \"AtomSpace benchmark complete\")"

# Test 2: ECAN spreading
echo ""
echo "Test 2: ECAN Spreading (10 iterations)"
cask emacs --batch -L . \
  --eval "(require 'aichat-ecan)" \
  --eval "(progn \
    (require 'aichat-opencog) \
    (dotimes (i 100) \
      (aichat-opencog-add-atom 'ConceptNode (format \"concept-%d\" i))) \
    (message \"Timing ECAN spreading...\") \
    (benchmark-run 10 \
      (aichat-ecan-spread-importance aichat-opencog-kb)))" \
  --eval "(message \"ECAN benchmark complete\")"

# Test 3: PLN reasoning
echo ""
echo "Test 3: PLN Reasoning (50 iterations)"
cask emacs --batch -L . \
  --eval "(require 'aichat-pln)" \
  --eval "(benchmark-run 50 \
    (aichat-pln-deduction \
      (cons 0.8 0.9) \
      (cons 0.7 0.85)))" \
  --eval "(message \"PLN benchmark complete\")"

# Test 4: Memory usage
echo ""
echo "Test 4: Memory Usage (1000 atoms)"
cask emacs --batch -L . \
  --eval "(require 'aichat-opencog)" \
  --eval "(require 'aichat-ecan)" \
  --eval "(require 'aichat-pln)" \
  --eval "(garbage-collect)" \
  --eval "(let ((before (memory-use-counts))) \
    (dotimes (i 1000) \
      (aichat-opencog-add-atom 'ConceptNode (format \"test-%d\" i))) \
    (garbage-collect) \
    (let ((after (memory-use-counts))) \
      (message \"Memory usage - Before: %S, After: %S\" before after)))"

# Test 5: Large knowledge base stress test
echo ""
echo "Test 5: Large KB Stress Test (5000 atoms)"
cask emacs --batch -L . \
  --eval "(require 'aichat-opencog)" \
  --eval "(message \"Creating large knowledge base...\") \
    (benchmark-run 1 \
      (dotimes (i 5000) \
        (aichat-opencog-add-atom 'ConceptNode (format \"concept-%d\" i)) \
        (when (> i 0) \
          (aichat-opencog-add-link 'InheritanceLink \
            (list (format \"concept-%d\" i) \
                  (format \"concept-%d\" (random i)))))))" \
  --eval "(message \"Large KB test complete\")"

# Test 6: Regression detection
echo ""
echo "Test 6: Regression Detection (1000 atoms baseline)"
cask emacs --batch -L . \
  --eval "(require 'aichat-opencog)" \
  --eval "(let ((start (current-time))) \
    (dotimes (i 1000) \
      (aichat-opencog-add-atom 'ConceptNode (format \"test-%d\" i))) \
    (let ((elapsed (float-time (time-subtract (current-time) start)))) \
      (message \"Baseline: 1000 atoms in %.3f seconds\" elapsed) \
      (when (> elapsed 5.0) \
        (message \"WARNING: Performance regression detected!\") \
        (message \"Expected: < 5.0s, Actual: %.3fs\" elapsed))))"

echo ""
echo "=== Performance Tests Complete ==="
