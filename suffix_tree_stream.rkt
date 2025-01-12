
#lang racket

(provide (all-defined-out))
(require "collection.rkt")
;; In the previous steps we worked with the representation of the
;; suffix tree as a list of branches,
;; where each branch was a pair between the label of the first
;; edge and the subtree below this edge. For the text
;; "banana", the representation of the compact suffix tree
;; (with strings for clarity) was:
;;
;;'(("$")
;; ("a" ("$")
;; ("na" ("$")
;; ("na$")))
;; ("banana$")
;; ("na" ("$")
;; ("na$")))
;;
;; For applications such as finding a template or finding a
;; substring of a given minimum length that repeats in the text
;; it is sufficient to partially explore the
;; suffixes, which means that we would gain if
;; it were built "lazy" - calculating
;; only those branches that are needed and only to
;; the depth that is needed. For example, to find
;; a substring of length 3 that is repeated in the text
;; "banana", only a few edges would need to be detailed, as in
;; the representation below:
;;
;;'(("$") - this path fails
;; ("a" ("$") - this path fails
;; ("na" <promise>)) - the subtree under the label "na"
;; is also a (non-empty) flow
;; <promise>)
;;
;; When we find that the node under the label
;; "na" is an internal node and we discover that the string "ana"
;; (corresponding to the root path) has length 3,
;; the function returns "ana" without building the rest of the tree
;; (a significant time saver for long texts).
;;
;; To achieve this behavior, we will change
;; the representation so that a suffix tree is
;; a stream (not a list) of branches.
;; Each branch remains a pair, not a stream, between
;; the label of the first edge and the subtree below it
;; (which subtree is a stream in turn, etc.).
;; The texts and labels remain lists (not streams) of characters.

;; Next, you will redefine the library for manipulating
;; suffix trees, to account for the change in
;; representation.

; TODO 1
; Redefine the following constructors and operators of the ST data structure
; . Some definitions may remain
; as in step 1.

; empty suffix tree
; empty-st : -> ST
(define empty-st empty-collection)

; operator that checks if an ST is empty
; st-empty? : ST -> Bool
(define (st-empty? st)
  (collection-empty? st))

; operator that extracts the first branch of an ST
; first-branch : ST -> (Label, ST)
(define (first-branch st)
  (if (st-empty? st)
    empty-collection
    (collection-first st)))

; operator that extracts the rest of the branches of an ST (without the first one)
; other-branches : ST -> [(Label, ST)]
; Note: the signature [(Label, ST)] is read here as "flow of
; pairs between a label and an ST"
(define (other-branches st)
  (if (st-empty? st)
    empty-collection
    (collection-rest st)))

; operator that extracts the label from the top of a branch
; get-branch-label : (Label, ST) -> Label
(define (get-branch-label edge)
  (car edge))

; operator that extracts the subtrees under the branch label
; get-branch-subtree : (Label, ST) -> ST
(define (get-branch-subtree edge)
  (cdr edge))

; operator that identifies the branch of an ST whose label
; starts with the character ch, if such a branch exists
; Note: because of the way trees are constructed, there will never be
; two branches that start with the same character.
; get-ch-branch : ST x Char -> (Label, ST) - the branch, if it exists
; -> Bool - otherwise false
; Note: If in step 1 you respected the abstraction barrier,
; the implementation of the get-ch-branch function will not need to be modified.
(define (get-ch-branch st ch)
  (if (st-empty? st) #f
     (if (equal? ​​(car(get-branch-label(first-branch st)))ch)
       (first branch st)
       (get-ch-branch (other-branches st) ch)
       )))
