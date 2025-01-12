#lang racket

(provide (all-defined-out))

;; A suffix tree is a tree containing all the suffixes of a text T 
;; (usually a long text requiring multiple operations), as shown in the 
;; figure below (the suffix tree for the text "BANANA", where nodes are 
;; represented by *, and edges are labeled with substrings of T).
;;
;;                            *
;;   __________ ______________|______________
;;  |          |              |              |
;;  $          A           BANANA$           NA
;;  |          |              |              |
;;  *          *              *              *
;;       ______|______                 ______|______
;;      |             |               |             |
;;      $             NA              $            NA$
;;      |             |               |             |        
;;      *             *               *             *
;;                 ___|___
;;                |       |
;;                $      NA$
;;                |       |
;;                *       *
;;
;; Each path in the suffix tree corresponds to a substring of text T, 
;; and each complete path corresponds to a suffix. 
;; To ensure that suffixes that are prefixes of other suffixes are not "lost," 
;; we append the special character `$` at the end of each suffix.
;; In the figure, the leftmost path corresponds to the empty suffix.
;;
;; The above tree is a compact suffix tree, meaning it uses the minimum 
;; number of edges possible (each edge is labeled with the longest common 
;; prefix of the suffixes below it).
;;
;; In contrast, an atomic suffix tree uses the maximum number of edges, 
;; with each edge labeled with a single character. 
;; Example (for the text "BANANA"):
;;
;;                            *
;;   __________ ______________|______________
;;  |          |              |              |
;;  $          A              B              N
;;  |          |              |              |
;;  *          *              *              *
;;         ____|____          |              |
;;        |         |         |              |
;;        $         N         A              A
;;        |         |         |              |        
;;        *         *         *              *
;;                  |         |          ____|____
;;                  |         |         |         |
;;                  A         N         $         N
;;                  |         |         |         |
;;                  *         *         *         *
;;                __|__       |                   |
;;               |     |      |                   |
;;               $     N      A                   A
;;               |     |      |                   |
;;               *     *      *                   *
;;                     |      |                   |
;;                     A      N                   $
;;                     |      |                   |
;;                     *      *                   *
;;                     |      |
;;                     $      A
;;                     |      |
;;                     *      *
;;                            |
;;                            $
;;                            |
;;                            *
;;
;; Regardless of whether we work with compact or atomic representations, 
;; a suffix tree is represented as a list of branches, where each branch 
;; is a pair consisting of the label of the first edge and the subtree 
;; below that edge. For the text "BANANA," the compact suffix tree 
;; representation is:
;;
;; '(("$")
;;   ("a" ("$")
;;        ("na" ("$")
;;              ("na$")))
;;   ("banana$")
;;   ("na" ("$")
;;         ("na$")))
;;
;; We have provided the representation using strings for clarity. 
;; In reality, to take advantage of list-processing functions, 
;; each string will be stored as a list of characters, and the 
;; actual representation becomes:
;;
;; '(((#\$))
;;   ((#\a) ((#\$))
;;          ((#\n #\a) ((#\$))
;;                     ((#\n #\a #\$))))
;;   ((#\b #\a #\n #\a #\n #\a #\$))
;;   ((#\n #\a) ((#\$))
;;              ((#\n #\a #\$))))
;;
;; Below, you will define a library for manipulating the suffix trees 
;; described earlier.

; TODO 1
; Define the following constructors and operators for the Suffix Tree 
; data structure (henceforth abbreviated as ST, from the English name).
; Note: The `Label` type is actually the type `[Char]` (list of characters), 
; but we use the alias `Label` to clarify the meaning of the values of type `Label`.

; The empty suffix tree
; empty-st : -> ST

; The tree is a list of branches, where a branch is a pair of a label and another tree
(define empty-st
  null)

; Operator that checks if an ST is empty
; st-empty? : ST -> Bool
(define (st-empty? st)
  (if (null? st)
      #t
      #f))

; Operator that extracts the first branch of an ST
; first-branch : ST -> (Label, ST)
(define (first-branch st)
  (if (null? st)
      null
      (car st)))

; Operator that extracts the rest of the branches of an ST (excluding the first)
; other-branches : ST -> [(Label, ST)]
; Note: The signature `[(Label, ST)]` is read as "list of pairs 
; between a label and an ST."
(define (other-branches st)
  (if (null? st)
      null
      (cdr st)))

; Operator that extracts the label at the top of a branch
; get-branch-label : (Label, ST) -> Label
(define (get-branch-label edge)
  (car edge))

; Operator that extracts the subtree below the label of a branch
; get-branch-subtree : (Label, ST) -> ST
(define (get-branch-subtree edge)
  (cdr edge))

; Operator that identifies the branch of an ST whose label starts 
; with the character `ch`, if such a branch exists.
; Note: Due to the way trees are constructed, there will never be 
; two branches starting with the same character.
; get-ch-branch : ST x Char -> (Label, ST) - branch, if it exists
;                           -> Bool        - otherwise false
; Important: Use the operators defined above to manipulate the tree 
; so that the implementation of `get-ch-branch` does not need to be 
; changed if we change the representation of suffix trees.
(define (get-ch-branch st ch)
  (if (null? st) #f
      (if (equal? (car (get-branch-label (first-branch st))) ch)
          (first-branch st)
          (get-ch-branch (other-branches st) ch))))
