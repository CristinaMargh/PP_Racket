#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))

;; In this stage, we define the algorithm for constructing a suffix tree 
;; (both compact and atomic) based on a given text and alphabet. 
;; It is assumed that the text uses only symbols present in the alphabet.
;; The approach is as follows:
;; 1. Obtain all the suffixes of the text.
;; 2. For each character in the alphabet, determine the suffixes 
;;    starting with that character: these will be grouped into 
;;    branches of the suffix tree (characters not present in the text 
;;    will not generate branches).
;; 3. For each list of suffixes starting with the same character, 
;;    determine the branch label and the new suffixes that will generate
;;    subtrees under that label:
;;    - For an AST (atomic suffix tree), the label is the first character, 
;;      and the new suffixes are the old suffixes without the first character.
;;    - For a CST (compact suffix tree), the label is the longest common 
;;      prefix of the suffixes, and the new suffixes are obtained by 
;;      removing this prefix from the old suffixes.
;; 4. Transform each result from step 3 into a branch:
;;    - The label is already computed.
;;    - The computation of subtrees is done by repeating steps 2-4 
;;      for the new suffixes.

; TODO 1
; Implement a recursive function that receives a text (list of characters) 
; and determines the list of all its suffixes (from the longest to the shortest).
; It is known that the text will end with the special character "$", and all 
; suffixes in the result list must also end with this character 
; (from the entire text followed by "$" to the empty suffix, represented by a single "$").
; ex:
; (get-suffixes '(#\w #\h #\y #\$))
; => '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
; Use stack recursion.
(define (get-suffixes text)
  (if (null? text)
      text
      (cons text (get-suffixes (cdr text)))))

; TODO 2
; Implement a function that receives a list of words 
; and a character `ch`, returning the words from the list 
; that start with the character `ch`.
; Note that some words might be empty.
; Use functional programming (do not use explicit recursion).

(define (first_char word)
  (car word))
(define (get-ch-words words ch)
  (filter (lambda (word) ; check if the word is empty
            (and (not (equal? word '())) (equal? (first_char word) ch))) words))

; TODO 3
; Implement a function that receives a non-empty list of suffixes 
; starting with the same character and computes the pair:
; (AST label for these suffixes, list of new suffixes).
; Recall that for an AST, the label is the first character 
; (but as a list of characters, so it can be processed the same 
; as more complex labels), and the new suffixes are obtained 
; by removing this character from the old suffixes.
; Do not use explicit recursion.

(define (ast-func suffixes)
  (cons (cons(car(car suffixes))'()) (map (lambda (word) (cdr word)) suffixes)))

; TODO 4
; Implement a function that receives a non-empty list of suffixes 
; starting with the same character and computes the pair:
; (CST label for these suffixes, list of new suffixes).
; Recall that for a CST, the label is the longest common prefix, 
; and the new suffixes are obtained by removing this prefix 
; from the old suffixes.
; Do not use explicit recursion.

(define (cst-func suffixes)
  (cons (longest-common-prefix-of-list suffixes)
        (map (lambda (word) (drop word (length (longest-common-prefix-of-list suffixes)))) suffixes)))

; TODO 5
; Implement the function `suffixes->st`, which builds a suffix 
; tree based on a list of suffixes, an alphabet (list of characters 
; that includes all characters from the suffixes), and a labeling 
; function (atomic or compact).
; When the function argument is `ast-func`, we get an AST.
; When the function argument is `cst-func`, we get a CST.
; Note: The function covers steps 2-4 described above.
; The `suffixes->st` function may use explicit recursion, but 
; for the other processes (steps 2 and 3), functional programming 
; must be used.

(define (converted-get-ch get-ch-words) (lambda (x) (lambda (y) (get-ch-words x y))))  

(define (suffixes->st labeling-func suffixes alphabet)
  (if (equal? labeling-func ast-func)
      ; AST
      (map (lambda (list)
        (if (null? (cdr list)) list
            (cons (car list)(suffixes->st ast-func (cdr list) alphabet))))
           ; Use `converted-get-ch` to obtain a list of suffixes starting with the same character
           ; Filter: eliminate empty lists
           ; Map with `labeling-func`, get a list (made of labels and subtrees) for mapping again
           (map ast-func (filter (lambda (x) (not (null? x)))(map ((converted-get-ch get-ch-words) suffixes) alphabet))))
      ; CST, analogously
      (map (lambda (list)
        (if (null? (cdr list)) list
            (cons (car list)(suffixes->st cst-func (cdr list) alphabet))))
        (map cst-func (filter (lambda (x) (not (null? x)))(map ((converted-get-ch get-ch-words) suffixes) alphabet))))
   ))

; TODO 6
; This task involves implementing three functions:
; text->st, text->ast, text->cst, where `text->ast` and `text->cst`
; must be obtained as partial applications of `text->st`.
; To this end, the `text->st` function must be curried.

; a) Implement the `text->st` function, which receives a text 
; (list of characters) and a labeling function, returning 
; the suffix tree corresponding to the text with the specified
; labeling method.
; Steps:
; - Obtain the suffixes of the text, appending the final "$".
; - Obtain the sorted alphabet associated with the text using 
;   library functions `sort`, `remove-duplicates`, and `char<?`
;   (including the "$" character).
; - Call the `suffixes->st` function accordingly.
; Define your own parameters and order for the `text->st` function, 
; but it must be curried in a way that facilitates deriving 
; the `text->ast` and `text->cst` functions below through 
; partial application of `text->st`.
; Note: The checker tests only the `text->ast` and `text->cst` functions.

(define text->st
  (lambda (words)
    (lambda (function)
      (suffixes->st function (get-suffixes (append words '(#\$)))
                        (remove-duplicates (sort (append words '(#\$)) char<?)) ; alphabet
                        ))))
; b) Derive the `text->ast` function from `text->st`, which 
; receives a text (list of characters) and returns the AST 
; corresponding to the text.

(define (text->ast text)
  ((text->st text) ast-func))
; c) Derive the `text->cst` function from `text->st`, which 
; receives a text (list of characters) and returns the CST 
; corresponding to the text.

(define (text->cst text)
  ((text->st text) cst-func))
