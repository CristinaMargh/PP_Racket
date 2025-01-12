#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  (define result1 (longest-common-prefix-helper w1 w2 '()))
  (define result2 (remove-list (length result1) w1))
  (define result3 (remove-list (length result1) w2))

   (cons result1 (cons result2 (cons result3 '())))
  )
; Folosit pentru a elimina number elemente de la inceputul unei liste, pentru a putea elimina prefixul.
 (define (remove-list number L2)
   (if(or (null? L2) (equal? 0 number))
      L2
      (remove-list (- number 1) (cdr L2))
   ))
;recursivitate pe coada ,functie ajutatoare
(define (longest-common-prefix-helper w1 w2 helper)
  (cond
    (( or(null? w1) (null? w2)) (reverse helper))
    ((equal? (first w1) (first w2))
     (longest-common-prefix-helper (rest w1)(rest w2) (cons (first w1) helper)))
    (else (reverse helper))))
    

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (if (null? words) words
      (if (null? (cdr words))
          (car words)
          (longest-common-prefix-helper (car words) (longest-common-prefix-of-list(cdr words)) '())
      )))


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.


(define (match-pattern-with-label st pattern)
     
  (if (equal? (get-ch-branch st (car pattern)) #f)
      (list #f '())
  (if (equal? (longest-common-prefix-helper pattern (car(get-ch-branch st (car pattern))) '()) pattern)
      #t
      (if (equal? (longest-common-prefix-helper pattern (car(get-ch-branch st (car pattern))) '()) (car(get-ch-branch st (car pattern))))
          (list (car(longest-common-prefix (car(get-ch-branch st (car pattern))) pattern)) (cadr(longest-common-prefix pattern (car(get-ch-branch st (car pattern)))))
                (get-branch-subtree (get-ch-branch st (car pattern))))
          (list #f (longest-common-prefix-helper pattern (car(get-ch-branch st (car pattern))) ' ()))))))
  
; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.

(define (st-has-pattern? st pattern)
  (if (equal? (match-pattern-with-label st pattern) #t) #t
  (if(not(car(match-pattern-with-label st pattern))) #f
  (if (equal? (longest-common-prefix-helper (get-branch-label(first-branch st)) pattern '()) pattern) #t
          (st-has-pattern? (get-branch-subtree (get-ch-branch st (car pattern))) (cadr(longest-common-prefix pattern (car(get-ch-branch st (car pattern))))))))))
