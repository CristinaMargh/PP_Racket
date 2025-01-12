#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; We will reuse all functions from stages 1-3 (except for 
;; `longest-common-substring`, which does not benefit from the 
;; stream representation as it traverses the entire tree) 
;; and adapt them to the new representation of an ST (Suffix Tree).
;;
;; Since an ST is constructed starting from a collection of suffixes 
;; and because we want to compute all suffixes only when necessary, 
;; we will modify all functions that processed lists of suffixes to 
;; process streams of suffixes instead.
;;
;; Note: Without this modification of suffix lists into suffix streams, 
;; and assuming we manipulated suffix trees only through the interface 
;; defined in the `suffix-tree` file (thus respecting the abstraction 
;; barrier), we would only need to modify the `suffixes->st` function, 
;; which is essentially a constructor for the ST type.
;; Due to transforming suffix lists into streams, we have far more 
;; implementations to modify.
;; Could we have avoided this? Yes, by using the concept of a collection 
;; of suffixes from the beginning (instead of assuming they would be 
;; processed as lists). Instead of `cons`, `car`, `cdr`, `map`, `filter`, etc., 
;; we would have always used `collection-cons`, `collection-first`, etc. - 
;; these functions being initially defined in a library as their equivalents 
;; on lists and later redefined as `stream-cons`, `stream-first`, etc. 
;; Operators on collections of suffixes would have used only functions 
;; of the `collection-` type.
;;
;; We chose not to proceed this way because it would have caused confusion 
;; at the time (when even list operators were new concepts) and to give you 
;; the opportunity to perform this "re-design" yourself.

; TODO
; Copy the following function implementations from the previous stages 
; and modify them as follows:
; - All functions that work with lists of suffixes will now work with 
;   a new data type `Collection`, whose constructors and operators you 
;   define in the `collection.rkt` file.
; - For all functions, ensure the abstraction barrier is respected 
;   (both for the ST type and the Collection type).
; Note: The fewer functions need modification, the better the initial design.

(define (longest-common-prefix w1 w2)
     (define result1 (common-prefix w1 w2))
     (define result2 (remove-list (length result1) w1))
     (define result3 (remove-list (length result1) w2))
     (cons result1 (cons result2 (cons result3 '()))))

; Used to remove `number` elements from the start of a list, 
; allowing the removal of a prefix.
(define (remove-list number L2)
  (if (or (null? L2) (equal? 0 number))
     L2
     (remove-list (- number 1) (cdr L2))))

(define (common-prefix first second)
  (cond
    ((or (null? first) (null? second)) null)
    ((equal? (car first) (car second))
     (cons (car first) (common-prefix (cdr first) (cdr second))))
    (else null)))

(define (longest-common-prefix-of-collection words)
  (if (collection-empty? words) empty-collection
      (if (collection-empty? (collection-rest words))
          (collection-first words)
          (common-prefix (collection-first words) (longest-common-prefix-of-collection (collection-rest words))))))

(define (match-pattern-with-label st pattern)
  (if (equal? (get-ch-branch st (car pattern)) #f)
      (list #f '())
      (if (equal? (common-prefix pattern (car (get-ch-branch st (car pattern)))) pattern)
          #t
          (if (equal? (common-prefix pattern (car (get-ch-branch st (car pattern)))) (car (get-ch-branch st (car pattern))))
              (list (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern))
                    (cadr (longest-common-prefix pattern (car (get-ch-branch st (car pattern)))))
                    (get-branch-subtree (get-ch-branch st (car pattern))))
              (list #f (common-prefix pattern (car (get-ch-branch st (car pattern)))))))))

(define (st-has-pattern? st pattern)
  (if (equal? (match-pattern-with-label st pattern) #t) #t
      (if (not (car (match-pattern-with-label st pattern))) #f
          (if (equal? (common-prefix (get-branch-label (first-branch st)) pattern) pattern) #t
              (st-has-pattern? (get-branch-subtree (get-ch-branch st (car pattern))) 
                               (cadr (longest-common-prefix pattern (car (get-ch-branch st (car pattern))))))))))

(define (get-suffixes text)
  (if (collection-empty? text)
      text
      (collection-cons text (get-suffixes (collection-rest text)))))

(define (get-ch-words words ch)
  (collection-filter (lambda (word) ; Check if the word is empty
            (and (not (equal? word null)) (equal? (car word) ch))) words))

(define (ast-func suffixes)
  (cons (cons (car (collection-first suffixes)) null) (collection-map (lambda (word) (cdr word)) suffixes)))

(define (cst-func suffixes)
  (cons (longest-common-prefix-of-collection suffixes)
        (collection-map (lambda (word) (drop word (length (longest-common-prefix-of-collection suffixes)))) suffixes)))

; Assume the alphabet parameter is also a stream
; (of course, `suffixes` is also a stream as it is a collection of suffixes)

(define (converted-get-ch get-ch-words) (lambda (x) (lambda (y) (get-ch-words x y))))

(define (suffixes->st labeling-func suffixes alphabet)
  (if (equal? labeling-func ast-func)
      ; AST
      (collection-map (lambda (list)
                        (if (collection-empty? (cdr list)) list
                            (cons (car list) (suffixes->st ast-func (cdr list) alphabet))))
                      (collection-map ast-func 
                                      (collection-filter (lambda (x) (not (collection-empty? x)))
                                                         (collection-map ((converted-get-ch get-ch-words) suffixes) alphabet))))
      ; CST analogously
      (collection-map (lambda (list)
                        (if (collection-empty? (cdr list)) list
                            (cons (car list) (suffixes->st cst-func (cdr list) alphabet))))
                      (collection-map cst-func 
                                      (collection-filter (lambda (x) (not (collection-empty? x)))
                                                         (collection-map ((converted-get-ch get-ch-words) suffixes) alphabet))))))

; Don't forget to convert the alphabet into a stream

(define text->st
  (lambda (words)
    (lambda (function)
      (suffixes->st function (get-suffixes (append words '(#\$)))
                    (foldr (lambda (char result) (collection-cons char result)) empty-collection (remove-duplicates (sort (append words '(#\$)) char<?)))))))

(define (text->ast text)
  ((text->st text) ast-func))

(define (text->cst text)
  ((text->st text) cst-func))

; If the abstraction barrier was respected, this function remains unchanged.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))

; Function to append to the first list enough elements from the second list to reach the desired length `len`.
(define (append-first-list list1 list2 len)
  (append list1 (take list2 (- len (length list1)))))

(define (helper tree len result)
  ; Traverse the tree, no match was found
  (if (st-empty? tree) #f
      ; Traverse branches using `first-branch` and `other-branches`
      (let* ((label (get-branch-label (first-branch tree))))
        (or (if (not (st-empty? (get-branch-subtree (first-branch tree))))
                (if (<= len (+ (length label) (length result)))
                    (append-first-list result label len) ; Add only as much as needed
                    ; Recursively traverse the subtree
                    (helper (get-branch-subtree (first-branch tree)) len (append result label)))
                (helper (other-branches tree) len result))
            (helper (other-branches tree) len result)))))

(define (repeated-substring-of-given-length text len)
  (helper (text->cst text) len null))
