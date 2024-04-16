#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

 (define (longest-common-prefix w1 w2)
     (define result1 (common-prefix w1 w2))
     (define result2 (remove-list (length result1) w1))
     (define result3 (remove-list (length result1) w2))
     (cons result1 (cons result2 (cons result3 '())))
  )  

; Folosit pentru a elimina number elemente de la inceputul unei liste, pentru a putea elimina prefixul.
(define (remove-list number L2)
  (if(or (null? L2) (equal? 0 number))
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
          (common-prefix (collection-first words) (longest-common-prefix-of-collection(collection-rest words))))
      ))
     
(define (match-pattern-with-label st pattern)
     
  (if (equal? (get-ch-branch st (car pattern)) #f)
      (list #f '())
      (if (equal? (common-prefix pattern (car(get-ch-branch st (car pattern))) ) pattern)
          #t
          (if (equal? (common-prefix pattern (car(get-ch-branch st (car pattern))) ) (car(get-ch-branch st (car pattern))))
              (list (car(longest-common-prefix (car(get-ch-branch st (car pattern))) pattern)) (cadr(longest-common-prefix pattern (car(get-ch-branch st (car pattern)))))
                    (get-branch-subtree (get-ch-branch st (car pattern))))
              (list #f (common-prefix pattern (car(get-ch-branch st (car pattern))) ))))))
         
(define (st-has-pattern? st pattern)
  (if (equal? (match-pattern-with-label st pattern) #t) #t
  (if(not(car(match-pattern-with-label st pattern))) #f
  (if (equal? (common-prefix (get-branch-label(first-branch st)) pattern ) pattern) #t
          (st-has-pattern? (get-branch-subtree (get-ch-branch st (car pattern))) (cadr(longest-common-prefix pattern (car(get-ch-branch st (car pattern))))))))))


(define (get-suffixes text)
  (if (collection-empty? text)
      text
      (collection-cons text  (get-suffixes (collection-rest text)))))

(define (get-ch-words words ch)
  (collection-filter (lambda (word) ; verificam cazul in care cuvantul e vid
            (and (not (equal? word null)) (equal? (car word) ch))) words))

  (define (ast-func suffixes)
        (cons (cons (car (collection-first suffixes)) null) (collection-map (lambda (word) (cdr word)) suffixes)))
 
(define (cst-func suffixes)
  (cons (longest-common-prefix-of-collection suffixes)
        (collection-map (lambda (word) (drop word (length (longest-common-prefix-of-collection suffixes)))) suffixes)))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)

(define (converted-get-ch get-ch-words) (lambda (x) (lambda (y) (get-ch-words x y))))  
         
         (define (suffixes->st labeling-func suffixes alphabet)
           (if (equal? labeling-func ast-func)
               ;AST
               (collection-map (lambda (list)
                 (if (collection-empty? (cdr list)) list
                     (cons (car list)(suffixes->st ast-func (cdr list) alphabet))))
                    ;Folosesc converted-get-ch ca sa obtin o lista de sufixe care incep cu acelasi caracter
                    ;filter:elimin listele ramase goale
                    ; map cu labeling-func, obtin list(format din eticheta si subarbore) pe care facem iar map
                    (collection-map ast-func (collection-filter (lambda (x) (not (collection-empty? x)))(collection-map ((converted-get-ch get-ch-words) suffixes) alphabet))))
               ;CST analog
               (collection-map (lambda (list)
                 (if (collection-empty? (cdr list)) list 
                     (cons (car list)(suffixes->st cst-func (cdr list) alphabet))))
                 (collection-map cst-func (collection-filter (lambda (x) (not (collection-empty? x)))(collection-map ((converted-get-ch get-ch-words) suffixes) alphabet))))
   )) 

; nu uitați să convertiți alfabetul într-un flux

(define text->st
     (lambda (words)
       (lambda (function)
         (suffixes->st function (get-suffixes (append words '(#\$)))
                           (foldr (lambda (char result)(collection-cons char result)) empty-collection (remove-duplicates (sort (append words '(#\$)) char<?))) ;alfabetul
                        )))) 

 (define (text->ast text)
     ((text->st text) ast-func))

   (define (text->cst text)
  ((text->st text) cst-func))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
 (define (substring? text pattern)
      (st-has-pattern? (text->ast text) pattern))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

(define (append-first-list list1 list2 len)
  (append list1 (take list2 (- len (length list1)))))

(define (helper tree len result)
  ;am parcurs arborele si nu s-a realizat nicio potrivire
    (if (st-empty? tree) #f
        ;vom parcurge branch-urile din tree cu ajutorul functiilor first-branch si other-branches
        (let* ((label (get-branch-label (first-branch tree))))
          (or (if (not(st-empty? (get-branch-subtree (first-branch tree))))
              (if (<= len (+ (length label)(length result)))
                  (append-first-list result label len) ; adaug doar cat am nevoie
                  ;merg recursiv prin subarbore
                  (helper (get-branch-subtree (first-branch tree)) len (append result label)))
              (helper (other-branches tree) len result))
              (helper (other-branches tree) len result)))))
               
(define (repeated-substring-of-given-length text len)                 
  (helper (text->cst text) len null)) 
