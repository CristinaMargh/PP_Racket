#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implement a function that receives two words (lists of characters) 
; `w1` and `w2` and calculates the longest common prefix of these words, 
; along with the remainder of the two words after removing the common prefix.
; Example:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Use tail recursion.
(define (longest-common-prefix w1 w2)
  (define result1 (longest-common-prefix-helper w1 w2 '()))
  (define result2 (remove-list (length result1) w1))
  (define result3 (remove-list (length result1) w2))

   (cons result1 (cons result2 (cons result3 '())))
  )
; Used to remove `number` elements from the beginning of a list, 
; allowing the removal of the prefix.
(define (remove-list number L2)
   (if (or (null? L2) (equal? 0 number))
      L2
      (remove-list (- number 1) (cdr L2))
   ))
; Tail-recursive helper function
(define (longest-common-prefix-helper w1 w2 helper)
  (cond
    ((or (null? w1) (null? w2)) (reverse helper))
    ((equal? (first w1) (first w2))
     (longest-common-prefix-helper (rest w1) (rest w2) (cons (first w1) helper)))
    (else (reverse helper))))

; TODO 3
; Implement a recursive function that receives a non-empty list 
; of words that start with the same character and calculates the 
; longest common prefix among them.
; Stop the search (iteration) as soon as you are certain that 
; the current common prefix is the final common prefix.
(define (longest-common-prefix-of-list words)
  (if (null? words) words
      (if (null? (cdr words))
          (car words)
          (longest-common-prefix-helper (car words) (longest-common-prefix-of-list (cdr words)) '())
      )))

;; The following two functions are useful for searching a pattern 
;; in a text using the suffix tree.
;; The search idea is as follows:
;; - If the pattern exists in the text, then there is a suffix 
;;   starting with this pattern, so there is a path in the suffix 
;;   tree starting from the root that matches the pattern.
;; - We will look for the branch whose label starts with the first 
;;   letter of the pattern.
;; - If such a branch is not found, the pattern does not appear 
;;   in the text.
;; - If the pattern is entirely contained within the branch's label, 
;;   then it appears in the text.
;; - If the pattern matches the label but is not contained in it 
;;   (e.g., the pattern "nana$" matches the label "na"), then 
;;   continue the search in the branch's subtree.
;; - If the pattern does not match the label (e.g., the pattern 
;;   "numai" does not match the label "na"), then it does not appear 
;;   in the text (otherwise, the label would have been "n", not "na", 
;;   because the label is the longest common prefix of the suffixes 
;;   in its subtree).

; TODO 4
; Implement the function `match-pattern-with-label` that receives 
; a suffix tree and a non-empty pattern and performs a single step 
; from the process described above. It identifies the branch of 
; the tree whose label starts with the first letter of the pattern, 
; then determines how well the pattern matches the label, returning 
; as a result:
; - true, if the pattern is fully contained in the label.
; - a list (label, new pattern, subtree), if the pattern matches the 
;   label but is not fully contained in it 
;   (e.g., ("na", "na$", subtree under the label "na") 
;   for the initial pattern "nana$" and label "na").
; - a list (false, the longest common prefix between the label and 
;   the pattern), if the pattern does not match the label or no label 
;   starting with the desired letter is found.
;   (e.g., (false, "n") for the pattern "numai" and label "na")
;   (e.g., (false, "") for a label not found).
; Note: Although the examples use strings for clarity, remember that 
; we are working with lists of characters.

(define (match-pattern-with-label st pattern)
  (if (equal? (get-ch-branch st (car pattern)) #f)
      (list #f '())
      (if (equal? (longest-common-prefix-helper pattern (car (get-ch-branch st (car pattern))) '()) pattern)
          #t
          (if (equal? (longest-common-prefix-helper pattern (car (get-ch-branch st (car pattern))) '()) (car (get-ch-branch st (car pattern))))
              (list (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern))
                    (cadr (longest-common-prefix pattern (car (get-ch-branch st (car pattern)))))
                    (get-branch-subtree (get-ch-branch st (car pattern))))
              (list #f (longest-common-prefix-helper pattern (car (get-ch-branch st (car pattern))) '()))))))

; TODO 5
; Implement the function `st-has-pattern?` that receives a suffix 
; tree and a pattern and returns true if the pattern appears in 
; the tree, and false otherwise.

(define (st-has-pattern? st pattern)
  (if (equal? (match-pattern-with-label st pattern) #t) #t
      (if (not (car (match-pattern-with-label st pattern))) #f
          (if (equal? (longest-common-prefix-helper (get-branch-label (first-branch st)) pattern '()) pattern) #t
              (st-has-pattern? (get-branch-subtree (get-ch-branch st (car pattern))) 
                               (cadr (longest-common-prefix pattern (car (get-ch-branch st (car pattern))))))))))
