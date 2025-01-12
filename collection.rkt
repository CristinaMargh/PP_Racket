#lang racket

(provide (all-defined-out))

;; In this file you define your constructors and
;; operators for the Collection type.
;; In the previous steps, collections were actually
;; lists.
;; In the definitions below, you will consider
;; a collection to be implemented as a stream.

; Since stream-cons is not a regular function,
; but is defined as a special syntax, so
; that it does not evaluate its arguments before
; calling it (the behavior we also want for
; collection-cons), we cannot use a definition
; like
; (define collection-cons stream-cons)
; (this kind of definition generates an error).
; Neither
; (define (collection-cons x xs) (stream-cons x xs))
; is a solution, since the functions we define
; in Racket are strict functions, and x and xs will be
; evaluated before entering the function body
; collection-cons and discover that they will be
; the arguments of a stream-cons.
; The way to define collection-cons to reproduce
; exactly the behavior of stream-cons is:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Note: you can change the name of the function, if you
; don't like "collection-cons". It is a function used only
; by you in the file etapa4.rkt, not by the checker.

; TODO
; Continue writing the rest of the definitions
; (which do not require any special syntax).
(define-syntax-rule (collection-first xs) (stream-first xs))
(define-syntax-rule (collection-rest xs) (stream-rest xs))
(define empty-collection empty-stream)
(define-syntax-rule (collection-empty? xs) (stream-empty? xs))
(define-syntax-rule (collection-filter x xs) (stream-filter x xs))
(define-syntax-rule (collection-map x xs) (stream-map x xs))
