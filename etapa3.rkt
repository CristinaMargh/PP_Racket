#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (st-has-pattern? (text->cst text) pattern))

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

(define (get-match st suffix) ; un sufix e in arborele de sufixe
  (let for-in-suff ((rezultat null) (n (length suffix))) ;gaseste cea mai mare potrivire pentru sufixul dat
    (if (zero? n)
        rezultat
        (if (st-has-pattern? st (take suffix n))
            (if (> n (length rezultat)) ;pastrez pe asta
                (for-in-suff (take suffix n) (sub1 n))
                (for-in-suff rezultat (sub1 n))) ;ramane rezultatul de pana acum
            (for-in-suff rezultat (sub1 n))
            ))))
     ;ia toate sufixele din textul 2 si vede care e in arborele din textul1       
(define (longest-common-substring text1 text2)
  (let ((st (text->cst text1)) (suff (get-suffixes text2))); constructia arborelui si a sufixelor
      ;initializarea variabilelor din corpul name-letului
    (let for-suffixes ((st st) (suff suff) (rezultat null))
      (if (null? suff)
          rezultat
          (if (> (length (get-match st (car suff))) (length rezultat))
              (for-suffixes st (cdr suff) (get-match st (car suff)))
              (for-suffixes st (cdr suff) rezultat)
              )))))
          

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false  .
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

;functie care imi adauga la prima lista exact atatea elemente din a2 a astfel incat la final sa obtin lungimea len
(define (append-first-list list1 list2 len)
  (append list1 (take list2 (- len (length list1)))))

(define (helper tree len result)
  ;am parcurs arborele si nu s-a realizat nicio potrivire
    (if (null? tree) #f
        ;vom parcurge branch-urile din tree cu ajutorul functiilor first-branch si other-branches
        (let* ((label (get-branch-label (first-branch tree))))
          (or (if (not(null? (get-branch-subtree (first-branch tree))))
              (if (<= len (+ (length label)(length result)))
                  (append-first-list result label len) ; adaug doar cat am nevoie
                  ;merg recursiv prin subarbore
                  (helper (get-branch-subtree (first-branch tree)) len (append result label)))
              (helper (other-branches tree) len result))
              (helper (other-branches tree) len result)))))
               
(define (repeated-substring-of-given-length text len)                 
  (helper (text->cst text) len null)) 
