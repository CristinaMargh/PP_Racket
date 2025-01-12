#lang racket
(require "suffix-tree.rkt")
(require "first.rkt")
(require "second.rkt")

(provide (all-defined-out))

;; This stage is dedicated to applications of the suffix tree:
;; - finding a pattern in a text
;; - the longest common substring of two texts
;; - finding a substring of a given length that repeats in the text
;; As per the convention from previous stages, a text is always 
;; represented as a list of characters. 
;; The results of the functions below are also represented as lists of characters.

; TODO 1
; Implement the function `substring?` which receives a text and 
; a non-empty pattern and returns true if the pattern appears 
; in the text, or false otherwise.
; Note: You have already implemented the main logic for this 
; search in stage 1 in the `st-has-pattern?` function, which 
; operates on a suffix tree (ST). Now you have all the tools 
; to implement the corresponding operator for the text type 
; (since in stage 2 you implemented the construction of the suffix 
; tree associated with a text).
(define (substring? text pattern)
  (st-has-pattern? (text->cst text) pattern))

; TODO 2
; Implement the function `longest-common-substring`, which receives 
; two texts and determines their longest common substring using 
; the following algorithm:
; 1. Build the suffix tree (ST1) for the first text.
; 2. For each suffix of the second text (from longest to shortest), 
;    find the longest match with the suffixes of the first text by 
;    following the relevant paths in ST1.
; 3. The final result is the longest match identified in step 2 
;    (in case of ties, keep the first substring found).
; Use a named `let` to iterate through the suffixes.
; Note: For the suffixes of the second text, we do not want the 
; final marker `$` to artificially increase the length of the 
; common substring with this character.
; Hint: Revisit the `match-pattern-with-label` function (stage 1).

(define (get-match st suffix)
  ; A suffix exists in the suffix tree
  (let for-in-suff ((result null) (n (length suffix))) ; Finds the longest match for the given suffix
    (if (zero? n)
        result
        (if (st-has-pattern? st (take suffix n))
            (if (> n (length result)) ; Keep this match
                (for-in-suff (take suffix n) (sub1 n))
                (for-in-suff result (sub1 n))) ; Keep the current result
            (for-in-suff result (sub1 n))))))

; Process all suffixes of text2 and find the longest match in the suffix tree of text1
(define (longest-common-substring text1 text2)
  (let ((st (text->cst text1)) (suff (get-suffixes text2))) ; Construct the suffix tree and suffixes
    ; Initialize variables in the body of the named let
    (let for-suffixes ((st st) (suff suff) (result null))
      (if (null? suff)
          result
          (if (> (length (get-match st (car suff))) (length result))
              (for-suffixes st (cdr suff) (get-match st (car suff)))
              (for-suffixes st (cdr suff) result))))))

; TODO 3
; Implement the function `repeated-substring-of-given-length`, 
; which receives a text and a natural number `len` and traverses 
; the suffix tree of the text to find a substring of length `len` 
; that repeats in the text. If no such substring exists, the function 
; returns false.
; Note: Due to the way the suffix tree is constructed (based on the 
; sorted alphabet), the result will be the first such substring in 
; alphabetical order.
; The idea is as follows: Any path in the compact suffix tree that 
; ends with an internal node (a node with children, not a leaf) 
; represents a substring that repeats, as any such path represents 
; a common prefix for two or more suffixes of the text.
; Use the interface defined in the `suffix-tree` file when 
; manipulating the tree.

; Function to append to the first list just enough elements from 
; the second list to reach the desired length `len`.
(define (append-first-list list1 list2 len)
  (append list1 (take list2 (- len (length list1)))))

(define (helper tree len result)
  ; If the tree is traversed and no match is found
  (if (null? tree) #f
      ; Traverse the branches of the tree using `first-branch` 
      ; and `other-branches`
      (let* ((label (get-branch-label (first-branch tree))))
        (or (if (not (null? (get-branch-subtree (first-branch tree))))
                (if (<= len (+ (length label) (length result)))
                    (append-first-list result label len) ; Add only as much as needed
                    ; Traverse the subtree recursively
                    (helper (get-branch-subtree (first-branch tree)) len (append result label)))
                (helper (other-branches tree) len result))
            (helper (other-branches tree) len result)))))

(define (repeated-substring-of-given-length text len)
  (helper (text->cst text) len null))
