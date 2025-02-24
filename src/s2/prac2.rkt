#lang racket
(provide menor)
(provide cifra-caracter)
(provide descifra-caracter)
(provide menor)
(provide carta)

; (define (<name> <args>)
;   <body>)
;
;   (define <name variable> <value> )
;
;    

; ejercicio 1
;

; apartado a
(define (binario-a-decimal b3 b2 b1 b0)
  (+ (* b3 8) 
     (* b2 4) 
     (* b1 2) 
     b0))

#| (binario-a-decimal 1 1 1 1) ; ⇒ 15 |#
#| (binario-a-decimal 0 1 1 0) ; ⇒ 6 |#
#| (binario-a-decimal 0 0 1 0) ; ⇒ 2 |#

(define (binario-a-hexadecimal b3 b2 b1 b0)
  (if (< (binario-a-decimal b3 b2 b1 b0) 10) 
    (integer->char (+ (char->integer #\0) (binario-a-decimal b3 b2 b1 b0)))
    (integer->char (+ (char->integer #\A) (- (binario-a-decimal b3 b2 b1 b0) 10)))))
                    ; numero 55 -> para el #\A = 55 + 10 = 65 -> numero en ascii

#| (binario-a-hexadecimal 0 1 1 0) ; ⇒ #\6 |#
#| (binario-a-hexadecimal 1 0 1 0) ; ⇒ #\A |#
#| (binario-a-hexadecimal 1 1 1 1) ; ⇒ #\F |#

; ejercicio 2
;

(define (encuentra-indice char)
  (if (< (char->integer char) (char->integer #\a)) 
    (- (char->integer char) (char->integer #\A))
    (- (char->integer char) (char->integer #\a))))

#| (encuentra-indice #\a) ; ⇒ 0 |#
#| (encuentra-indice #\b) ; ⇒ 1 |#
#| (encuentra-indice #\m) ; ⇒ 12 |#
#| (encuentra-indice #\z) ; ⇒ 25 |#

(define (encuentra-caracter number)
  (integer->char (+ (char->integer #\a) number)))

#| (encuentra-caracter 0) ; ⇒ #\a |#
#| (encuentra-caracter 1) ; ⇒ #\b |#
#| (encuentra-caracter 12) ; ⇒ #\m |#
#| (encuentra-caracter 25) ; ⇒ #\z |#

(define (entre-az? char)
  (or
   (and (<= (char->integer char) (char->integer #\z))
       (>= (char->integer char) (char->integer #\a)))
   (and (<= (char->integer char) (char->integer #\Z))
       (>= (char->integer char) (char->integer #\A)))))

#| (entre-az? #\a) ; ⇒ #t |#
#| (entre-az? #\m) ; ⇒ #t |#
#| (entre-az? #\z) ; ⇒ #t |#
#| (entre-az? #\`) ; ⇒ #f |#
#| (entre-az? #\{) ; ⇒ #f |#

(define (rota-indice indice desplazamiento)
  (modulo (+ indice desplazamiento) 26))


#| (rota-indice 4 12) ; ⇒ 16) |#
#| (rota-indice 4 24) ; ⇒ 2) |#
#| (rota-indice 4 -5) ; ⇒ 25) |#


(define (cifra-caracter char number)
  (if (entre-az? char) 
    (if (>= (char->integer char) (char->integer #\a))
      (encuentra-caracter (rota-indice (encuentra-indice char) number))
      (integer->char (+ (char->integer (encuentra-caracter (rota-indice (encuentra-indice char) number))) 
                         (- (char->integer #\A) (char->integer #\a)))))
    char))

#| (cifra-caracter #\c 5) ; ⇒ #\h) |#
#| (cifra-caracter #\z -1) ; ⇒ #\y) |#
#| (cifra-caracter #\j 40) ; ⇒ #\x) |#
#| (cifra-caracter #\D 3) ; ⇒ #\G) |#
#| (cifra-caracter #\ñ 3) ; ⇒ #\ñ) |#


(define (descifra-caracter char indice)
  (if (entre-az? char)
    (if (>= (char->integer chpar) (char->integer #\a))
      (encuentra-caracter (rota-indice (encuentra-indice char) (- 0 indice)))
      (integer->char (+ (char->integer (encuentra-caracter (rota-indice (encuentra-indice char) (- 0 indice)))) 
                        (- (char->integer #\A) (char->integer #\a)))))
    char))

#| (descifra-caracter #\d 3) ; ⇒ #\a) |#
#| (descifra-caracter #\y -1) ; ⇒ #\z) |#
#| (descifra-caracter #\x 40) ; ⇒ #\j) |#
#| (descifra-caracter #\G 3) ; ⇒ #\D) |#
#| (descifra-caracter #\tab 3) ; ⇒ #\tab) |#

; ejercicio 3
;


(define (menor-de-tres n1 n2 n3)
  (if (and (< n1 n2) (< n1 n3))
    n1
    (if (and (< n2 n3) (< n2 n3))
      n2
      n3)))

#| (menor-de-tres 2 8 1) ;; ⇒ 1 |#
#| (menor-de-tres 1 8 13) ;; ⇒ 1 |#
#| (menor-de-tres 8 1 13) ;; ⇒ 1 |#

(define (menor x y)
  (if (< x y) x y))

(define (menor-de-tres-v2 n1 n2 n3)
  (if (< n1 n2)
    (menor n1 n3)
    (menor n2 n3)))


#| (menor-de-tres-v2 2 8 1) ;; ⇒ 1 |#
#| (menor-de-tres-v2 1 8 13) ;; ⇒ 1 |#
#| (menor-de-tres-v2 8 1 13) ;; ⇒ 1 |#



#| (define (f x) |#
#|     (cons x 2)) |#
#||#
#| (define (g x y) |#
#|     (cons x y)) |#

#| (g (f (+ 2 1)) (+ 1 1)) |#


;  Normal
#| (g (f (+ 2 1)) (+ 1 1)) |#
;  (cons (f (+ 2 1)) (+ 1 1)) r4
;  (cons (cons (+ 2 1) 2) (+ 1 1)) r4
;  (cons (cons (+ 2 1) 2) 2) r3
;  (cons (cons 3 2) 2) r3
;  (cons (3 . 2) 2) r3
;  ((3 . 2) . 2) r3
;
;  Aplicativo
#| (g (f (+ 2 1)) (+ 1 1)) |#
;  (g (f 3) (+ 1 1)) r3
;  (g (f 3) 2) r3
;  (g (cons 3 2) 2) r4
;  (g (3 . 2) 2) r2
;  (cons (3 . 2) 2) r4
;  ((3 . 2) . 2) r4
;

#| (define (func-1 x) |#
#|     (/ x 0)) |#

#| (define (func-2 x y) |#
#|     (if (= x 0) |#
#|         0 |#
#|         y)) |#

; Orden Normal
; (func-2 0 (func-1 10))
; (if (= 0 0) 0 (func-1 10)) r4
; (if (= 0 0) 0 (/ 10 0)) r4
; (0) r2
; 
; Orden Aplicativo
; (func-2 0 (func-1 10))
; (func-2 0 (/ 10 0)) r4
; (func-2 0 (ind)) r3
; (if (= 0 0) 0 (ind) ) r4
; (0) r2
;


(define (cadenas-mayores l1 l2)
  (list
    (if 
      (>= (string-length (first l1)) (string-length (first l2)))
      (first l1)
      (first l2))
    (if 
      (>= (string-length (second l1)) (string-length (second l2)))
      (second l1)
      (second l2))
    (if 
      (>= (string-length (third l1)) (string-length (third l2)))
      (third l1)
      (third l2))))

#| (cadenas-mayores '("hola" "que" "tal") '("meme" "y" "adios")) ; ⇒ ("hola" "que" "adios") |#
#| (cadenas-mayores '("esto" "es" "lpp") '("hoy" "hay" "clase")) ; ⇒ ("esto" "hay" "clase") |#



; se suponía que había que ponerlo en diferentes funciones, mala mia
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

#| (define tres-de-picas '3♠) |#
#| (define as-de-corazones 'A♥) |#
#| (define jota-de-diamantes 'J♦) |#
#| (carta tres-de-picas) ; ⇒ (3 . Picas) |#
#| (carta as-de-corazones) ; ⇒ (1 . Corazones) |#
#| (carta 'K♣) ; ⇒ (12 . Tréboles) |#
#| (car (carta 'K♣)) |#

(define (jugada-mano c1 c2 c3)
    (cond 
      ((= (car (carta c1)) (car (carta c2)) (car (carta c3))) 
       (string-append "trio de " (number->string (car (carta c2)))))
      ((= (car (carta c1)) (car (carta c2)))
       (string-append "pareja de " (number->string (car (carta c2)))))

      ((= (car (carta c1)) (car (carta c3)))
       (string-append "pareja de " (number->string (car (carta c3)))))

      ((= (car (carta c3)) (car (carta c2)))
       (string-append "pareja de " (number->string (car (carta c2)))))
      (else "nada")))


#| (jugada-mano '3♥ '3♣ '3♥) ; ⇒ "trío de 3" |#
#| (jugada-mano 'K♦ '7♠ 'K♥) ; ⇒ "pareja de 12" |#
#| (jugada-mano '5♣ '4♣ '6♣) ; ⇒ "nada" |#
