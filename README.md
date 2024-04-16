# Racket: Arbori de sufixe
Vom reprezenta un arbore de sufixe ca pe o listă de ramuri, unde fiecare ramură corespunde unui fiu al rădăcinii.
Prin ramură, vom înțelege o pereche între o etichetă și subarborele cu rădăcina în nodul de sub etichetă.
Pentru a folosi funcțiile de lucru pe liste, fiecare etichetă va fi reprezentată nu ca string, ci ca listă de caractere.
Functii principale utilizate:
* first-branch primește un arbore de sufixe (ST) și întoarce prima ramură a acestuia (o pereche etichetă-subarbore)
* other-branches primește un ST și întoarce ST-ul fără prima sa ramură (o listă de ramuri, așa cum era și ST-ul original)
* get-ch-branch primește un ST și un caracter ch și întoarce acea ramură a ST-ului a cărei etichetă începe cu caracterul ch, respectiv false în cazul în care nu există o asemenea ramură
* get-branch-label primește o ramură a unui ST și întoarce eticheta acesteia
* get-branch-subtree primește o ramură a unui ST și întoarce subarborele de sub eticheta acesteia
* longest-common-prefix-of-list primește o listă nevidă de cuvinte care încep cu același caracter și întoarce cel mai lung prefix comun al tuturor cuvintelor din listă
* match-pattern-with-label se folosește pentru a căuta un șablon (un subșir) într-un text, folosind ST-ul asociat textului
* st-has-pattern? primește un ST și un șablon și întoarce true dacă șablonul apare în ST, respectiv false dacă nu apare
* get-suffixes primește un text (o listă de caractere la finalul căreia s-a adăugat caracterul special $) și întoarce toate sufixele textului (în ordine descrescătoare a lungimii)
* ast-func și cst-func reprezintă funcții de etichetare a unui ST
* suffixes->st primește o funcție de etichetare (precum ast-func sau cst-func), o listă de sufixe (toate sufixele unui text) și un alfabet (alfabetul folosit de text) și întoarce:
  AST-ul asociat textului, dacă apelăm suffixes->st cu funcția de etichetare ast-func
  CST-ul asociat textului, dacă apelăm suffixes->st cu funcția de etichetare cst-func
* longest-common-substring primește două texte text1 și text2 și calculează cel mai lung subșir comun al acestora, astfel:
construiește arborele de sufixe ST1 pentru text1
determină toate sufixele S ale textului text2 (fără marcajul de final $, pentru ca acesta să nu fie numărat drept caracter comun celor 2 texte)
pentru fiecare sufix din S (de la cel mai lung la cel mai scurt), caută cea mai lungă potrivire a acestuia cu textul text1 (parcurgând căile relevante în ST1); cea mai lungă asemenea potrivire este rezultatul final
dacă există mai multe cele mai lungi subșiruri comune, funcția îl întoarce pe cel care a fost găsit primul
* repeated-substring-of-given-length primește un text și un număr natural len și caută un subșir de lungime len care se repetă în text
* Pentru etapa 4 folosim streamuri
