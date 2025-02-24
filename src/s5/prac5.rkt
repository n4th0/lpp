#lang racket

(require rackunit)

(require "../s3/lpp.rkt")



(define (aplica-veces f1 f2 n x)
  (if (= n 0) 
    x
    (aplica-veces f1 f2 (- n 1) (f1 (f2 x)))))

#| (aplica-veces (lambda (x) (+ x 1)) (lambda (x) (+ x 2)) 2 10) ; ⇒ 16 |#
#| (aplica-veces (lambda (x) (* x x)) (lambda (x) (+ x 1)) 4 3) ; ⇒ 7072978201 |#

(define (mueve-al-principio-condicion f lista) ; TODO darle una vuelta
  (cond
    ((null? (rest lista)) lista)
    ((f (first lista)) lista)
    (else (cons (first (mueve-al-principio-condicion f (rest lista))) 
                (cons (first lista)
                      (rest (mueve-al-principio-condicion f (rest lista))))))))


#| (mueve-al-principio-condicion number? '(a b c 1 d 1 e)) ; ⇒ (1 a b c d 1 e) |#
#| (mueve-al-principio-condicion number? '(1 a b 1 c)) ; ⇒ (1 a b 1 c) |#
#| #| (mueve-al-principio-condicion number? '(a b c d)) ; ⇒ '(a b c d) |# |#

(define (comprueba f l1 l2)
  (if (or (null? l1 ) (null? l2))
    '()
    (if (f (first l1) (first l2))
      (cons (cons (first l1) (first l2)) (comprueba f (rest l1) (rest l2)))
      (comprueba f (rest l1) (rest l2)))))


#| (comprueba (lambda (x y) |#
#|              (= (string-length (symbol->string x)) y)) |#
#|            '(este es un ejercicio de examen)  |#
#|            '(2 1 2 9 1 6)) |#
#| ; ⇒ ((un . 2) (ejercicio . 9) (examen . 6)) |#
#| (comprueba (lambda (x y) |#
#|               (= (string-length x) (string-length y))) |#
#|              '("aui" "a" "ae" "c" "aeiou") |#
#|              '("hola" "b" "es" "que" "cinco")) |#
#| ; ⇒ (("a" . "b") ("ae" . "es") ("aeiou" . "cinco")) |#


(define (inserta-ordenada-generica elem f? lis)
  (cond 
    ((null? lis) (cons elem lis))
    ((f? elem (first lis)) (cons elem lis))
    (else (cons (first lis) (inserta-ordenada-generica elem f? (rest lis))))))

(define (ordena-generica lista f?)
  (if (null? (rest lista))
    lista
    (inserta-ordenada-generica  (first lista) f? (ordena-generica (rest lista) f?))))

#| (check-equal? (ordena-generica '("Hola" "me" "llamo" "Iñigo" "Montoya") (lambda (x y) (<= (string-length x) (string-length y)))) '("me" "Hola" "llamo" "Iñigo" "Montoya")) |#
#| (check-equal? (ordena-generica '("Hola" "me" "llamo" "Iñigo" "Montoya") (lambda (x y) (string<=? x y)) ) '("Hola" "Iñigo" "Montoya" "llamo" "me")) |#
#| (check-equal? (ordena-generica '((2 . 2) (1 . 1) (3 . 0) (5 . 1)) (lambda (x y) (<= (+ (car x) (cdr x)) (+ (car y) (cdr y))) ) ) '((1 . 1) (3 . 0) (2 . 2) (5 . 1))) |#

(define (carta sim)
  (cons 
    (if (< (char->integer (string-ref (symbol->string sim) 0)) (char->integer #\A)) 
        (- (char->integer (string-ref (symbol->string sim) 0)) (char->integer #\0))
        (cond 
          ((equal? (string-ref (symbol->string sim) 0) #\A) 1)
          ((equal? (string-ref (symbol->string sim) 0) #\J) 10)
          ((equal? (string-ref (symbol->string sim) 0) #\Q) 11)
          ((equal? (string-ref (symbol->string sim) 0) #\K) 12)
          (else -1)
          ))
    (cond 
      ((equal? (string-ref (symbol->string sim) 1) #\♠ ) 'Picas)
      ((equal? (string-ref (symbol->string sim) 1) #\♣ ) 'Tréboles)
      ((equal? (string-ref (symbol->string sim) 1) #\♥ ) 'Corazones)
      ((equal? (string-ref (symbol->string sim) 1) #\♦ ) 'Diamantes)
      (else 'ERROR))))

(define (valor-carta c)
  (car (carta c)))

(define (palo-carta c)
  (cdr (carta c)))

#| (define (ordena-cartas lista) |#
#|   (ordena-generica lista (lambda (x y) (<= (valor-carta x) (valor-carta y))))) |#
#| (ordena-cartas '(Q♠ J♣ 5♣ Q♥ J♦)) ; ⇒ '(5♣ J♣ J♦ Q♠ Q♥) |#

#| (map (lambda (x) |#
#|          (cond  |#
#|             ((symbol? x) (symbol->string x)) |#
#|             ((number? x) (number->string x)) |#
#|             ((boolean? x) (if x "#t" "#f")) |#
#|             (else "desconocido"))) '(1 #t hola #f (1 . 2))) ; '("1" "#t" "hola" "#f" "desconocido") |#

#| (filter (lambda (x)  |#
#|             (equal? (string-ref (symbol->string x) 1) #\a))  |#
#|     '(alicante barcelona madrid almería)) ; '("barcelona" "madrid") |#

#| (foldr (lambda (dato resultado) |#
#|           (string-append dato "*" resultado)) ""  |#
#|           '("Hola" "que" "tal")) ; "Hola*que*tal*" |#

#| (foldr append '() '((1 2) (3 4 5) (6 7) (8))) ; (1 2 3 4 5 6 7 8) |#

#| (foldl (lambda (dato resultado) |#
#|          (string-append |#
#|           (symbol->string (car dato)) |#
#|           (symbol->string (cdr dato)) |#
#|           resultado)) "" '((a . b) (hola . adios) (una . pareja))) ; "unaparejaholaadiosab" |#

#| (foldr (lambda (dato resultado) |#
#|            (cons (+ (car resultado) dato) |#
#|                  (+ (cdr resultado) 1))) '(0 . 0) '(1 1 2 2 3 3)) ; ⇒ (12 6) |#

#| (apply + (map cdr '((1 . 3) (2 . 8) (2 . 4)))) ; 15 |#
#| (apply min (map car (filter (lambda (p) |#
#|                                   (> (car p) (cdr p)))  |#
#|                                   '((3 . 1) (1 . 20) (5 . 2))))) ; 3 |#


; Los siguientes ejercicios utilizan esta definición de lista

(define lista '((2 . 7) (3 . 5) (10 . 4) (5 . 5)))


; Queremos obtener una lista donde cada número es la suma de las
; parejas que son pares

#| (filter (lambda (x)(= (modulo x 2) 0)) |#
#|         (map (lambda (x) (+ (car x) |#
#|                                  (cdr x))) |#
#|                lista)) |#
#| ; ⇒ (8 14 10) |#

; Queremos obtener una lista de parejas invertidas donde la "nueva"
; parte izquierda es mayor que la derecha.
(define (mayor a b)
  (if (< a b)
    a
    b))
(define (menor a b)
  (if (< a b)
    b
    a))

#| (filter (lambda (x) (> (car x) (cdr x))) |#
#|         (map (lambda (x) (cons (cdr x) (car x)))  lista)) |#
#| ; ⇒ ((7 . 2) (5 . 3)) |#

; Queremos obtener una lista cuyos elementos son las partes izquierda
; de aquellas parejas cuya suma sea par.

#| (foldr (lambda (x y) (cons (car x) y)) '() |#
#|         (filter (lambda (x) (even? (+ (car x) (cdr x)))) lista)) |#
#| ; ⇒ (3 10 5) |#


(define (f1 x) (lambda (y z) (string-append y z x)))
(define g1 (f1 "a"))
#| (check-equal? (g1 "clase" "lpp")  "claselppa") |#

#| (define (f2 x) (lambda (y z) (list y x z))) |#
#| (define (g2 x y) ((f2 "lpp") x y)) |#
#| (check-equal? (g2 "hola" "clase") (list "hola" "lpp" "clase")) |#

#| (define (f3 g3) (lambda(z x) (g3 z x))) |#
#| (check-equal? ((f3 cons) 3 4) '(3 . 4)) |#


(define (contar-datos-iguales-fos lista)
  (length (filter (lambda (x) (equal?  (cdr x) (car x))) lista)))

#| (contar-datos-iguales-fos  |#
#|    '((2 . 3) ("hola" . "hola") (\#a . \#a) (true . false)))  |#
#| ; ⇒ 2 |#
#| (contar-datos-iguales-fos  |#
#|    '((2 . "hola") ("hola" . 3) (\#a . true) (\#b . false)))  |#
#| ; ⇒ 0 |#

(define (expande-pareja p)
  (if (= (cdr p) 0)
    '()
    (cons (car p) (expande-pareja (cons (car p) (- (cdr p) 1))))))

(define (expande-lista-fos lista)
  (apply append (map expande-pareja lista)))

#| (expande-lista-fos '((#t . 3) ("LPP" . 2) (b . 4)))  |#
; ⇒ '(#t #t #t "LPP" "LPP" b b b b))

(define (comprueba-simbolos-fos l1 l2)
  (filter (lambda (x) (= (string-length (symbol->string (car x))) (cdr x))) (map (lambda (x y) (cons x y)) l1 l2)))


#| (comprueba-simbolos-fos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) |#
; ⇒ ((un . 2) (ejercicio . 9) (examen . 6))


(define (suma-n-izq n l)
  (map (lambda (x) (cons (+ (car x) n) (cdr x))) l))

#| (suma-n-izq 10 '((1 . 3) (0 . 9) (5 . 8) (4 . 1))) |#
#| ; ⇒ ((11 . 3) (10 . 9) (15 . 8) (14 . 1)) |#

(define (busca-mayor mayor? lista)
  (foldl (lambda (x y) (if (mayor? x y) x y)) (first lista) (rest lista)))

#| (busca-mayor > '(3 4 1 23 4 5 )) |#
#| (busca-mayor (lambda (x y) (> (string-length x) (string-length y))) '("askdjf" "dsfasdfkasdasd"  "dfas" " a")) |#

#| (define (todos-menores? l n) |#
#|   (for-all? (lambda (x) (for-all? (lambda (y) (< y n)) x)) l)) |#

(define (todos-menores? l n)
  (for-all? (lambda (x) (not (exists? (lambda (y) (>= y n)) x))) l))


#| (todos-menores? '((10 30 20) (1 50 30) (30 40 90)) 100) ; ⇒ #t |#
#| (todos-menores? '((10 30 20) (1 50 30) (30 40 90)) 90) ; ⇒ #f |#
#| (todos-menores? '((10 30 20) (1 50 30) (30 40 90)) 55) ; ⇒ #f |#

#| (define (coloca listas e1 e2 e3) |#
#|   (list (cons e1 (first listas)) (cons e2 (second listas)) (cons e3 (third listas)))) |#

(define (reparte-tres l)
  (if (null? l)
    '(() () ())
    (coloca (reparte-tres (rest (rest (rest l)))) (first l) (second l) (third l))))


(define (coloca l . l2)
  (if (null? l2)
    l
    (map (lambda (x y) (cons y x)) l l2)))

#| (coloca '(() () ()) 'a 'b 'c) ; ⇒ '((a) (b) (c)) |#
#| (coloca '((a) (a)) 'b 'b) ; ⇒ '((b a) (b a)) |#
#| (coloca '((a b c d)) 'e) ; ⇒ '((e a b c d)) |#
#| (coloca '()) ; ⇒ '() |#
#| (coloca '((a) (b c) (d e f) (g h i j)) 'k 'l 'm 'n) ; ⇒ '((k a) (l b c) (m d e f) (n g h i j)) |#


(define (reparte-cuatro l)
  (if (null? l)
    '(() () () ())
    (coloca (reparte-cuatro (rest (rest (rest (rest l))))) (first l) (second l) (third l) (fourth l))))

#| (reparte-cuatro '(A♣ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ J♣ Q♣ K♣)) ; ⇒ '((A♣ 5♣ 9♣) (2♣ 6♣ J♣) (3♣ 7♣ Q♣) (4♣ 8♣ K♣)) |#

(define (escoge-en-orden l . n)
    (foldr (lambda (x y) (cons (x l) y) ) '() n))

#| (escoge-en-orden '(1 2 3 4 5)) ; ⇒  '() |#
#| (escoge-en-orden '(1 2 3 4 5) fourth second) ; ⇒ '(4 2) |#
#| (escoge-en-orden '(a b c d) third second fourth first) ; ⇒ '(c b d a) |#
#| (escoge-en-orden '(dos tres un) third first second) ; ⇒ '(un dos tres) |#

(define (reordena-tres-montones l p1 p2 p3)
  (escoge-en-orden (reparte-tres l) p1 p2 p3))

#| (reordena-tres-montones  '(A♣ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ J♣ Q♣ K♣) second first third) |#
; ⇒
; '((2♣ 5♣ 8♣ Q♣) (A♣ 4♣ 7♣ J♣) (3♣ 6♣ 9♣ K♣))

(define (reordena-cuatro-montones l p1 p2 p3 p4)
  (escoge-en-orden (reparte-cuatro l) p1 p2 p3 p4))


#| (reordena-cuatro-montones  '(A♣ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ J♣ Q♣ K♣) fourth second first third) |#
; ⇒
; '((4♣ 8♣ K♣) (2♣ 6♣ J♣) (A♣ 5♣ 9♣) (3♣ 7♣ Q♣))
;
(define (junta-montones montones)
  (apply append montones))

(junta-montones '((4♣ 8♣ K♣) (2♣ 6♣ J♣) (A♣ 5♣ 9♣) (3♣ 7♣ Q♣)))
; ⇒
; (4♣ 8♣ K♣ 2♣ 6♣ J♣ A♣ 5♣ 9♣ 3♣ 7♣ Q♣)

(define (adivina baraja par1 par2 par3)
  (list-ref baraja
            (+ (* (- (car par3) 1) (cdr par2) (cdr par1))
               (* (- (car par2) 1) (cdr par1))
               (- (car par1) 1))))
