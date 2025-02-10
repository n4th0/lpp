#lang racket

(require rackunit)
(require "lpp.rkt")
(require "../s2/prac2.rkt")

(define (minimo lista)
  (if (null? (rest lista))
    (first lista)
    (menor (first lista) (minimo (rest lista)))))

#| (minimo '(2)) ; ⇒ 2 |#
#| (minimo '(1 8 6 4 3)) ; ⇒ 1 |#
#| (minimo '(1 -1 3 -6 4)) ; ⇒ -6 |#

#| (minimo '(1 8 6 4 3)) ; ⇒ 1 |#
; se le pasa la lista (8 6 4 3)
; esa llamada recursiva devuelve 3
; se llama a la funcion menor con los parámetros 1 y 3


(define (concatena lista-char)
  (if (null? lista-char)
    ""
    (string-append (string (first lista-char)) (concatena (rest lista-char)) )))

#| (concatena '()) ; ⇒ "" |#
#| (concatena '(#\H #\o #\l #\a)) ; ⇒ "Hola" |#
#| (concatena '(#\S #\c #\h #\e #\m #\e #\space #\m #\o #\l #\a))   |#
#| ; ⇒ "Scheme mola" |#

(define (cifra-cadena cad desp)
  (if (equal? cad "")
    ""
    (string-append (string (cifra-caracter (string-ref cad 0) desp)) 
                   (cifra-cadena (substring cad 1 (string-length cad)) desp))))

(define (descifra-cadena cad desp)
  (if (equal? cad "")
    ""
    (string-append (string (descifra-caracter (string-ref cad 0) desp)) 
                   (descifra-cadena (substring cad 1 (string-length cad)) desp))))


#| (cifra-cadena "En un lugar de la Mancha, de cuyo nombre no quiero acordarme" 10) ; ⇒ |#
;"Ox ex veqkb no vk Wkxmrk, no meiy xywlbo xy aesoby kmybnkbwo"

#| (descifra-cadena "Ox ex veqkb no vk Wkxmrk, no meiy xywlbo xy aesoby kmybnkbwo" 10) ; ⇒ |#
;"En un lugar de la Mancha, de cuyo nombre no quiero acordarme"


(define (contiene? lista elemento)
  (if (null? (rest lista))
    (equal? (first lista) elemento)
    (if (equal? (first lista) elemento)
      #t
      (contiene? (rest lista) elemento))))

(define (str-contiene? str ch)
  (contiene? (string->list str) ch))

#| (contiene? '(algo 3 #\A) 3) ; ⇒ #t |#
#| (contiene? '(algo 3 #\A) "algo") ; ⇒ #f |#
#| (contiene? '(algo 3 #\A) 'algo) ; ⇒ #t |#
#| (str-contiene? "Hola" #\o) ; ⇒ #t |#
#| (str-contiene? "Esto es una frase" #\space) ; ⇒ #t |#
#| (str-contiene? "Hola" #\h) ; ⇒ #f |#


(define (todos-iguales? lista)
  (if (< (length lista) 2)
  #t
  (and (equal? (first lista) (first (rest lista))) 
       (todos-iguales? (rest lista)))))

#| (todos-iguales? '()) ; ⇒ #t |#
#| (todos-iguales? '(a)) ; ⇒ #t |#
#| (todos-iguales? '(a a)) ; ⇒ #t |#
#| (todos-iguales? '(a a a a a a a)) ; ⇒ #t |#
#| (todos-iguales? '((a b) (a b) (a b))) ; ⇒ #t |#
#| (todos-iguales? '(a a a a a b)) ; ⇒ #f |#


(define (todos-distintos? lista)
  (if (< (length lista) 2)
  #t
  (and (not (contiene? (rest lista) (first lista))) 
       (todos-distintos? (rest lista)))))

#| (todos-distintos? '()) ; ⇒ #t |#
#| (todos-distintos? '(a)) ; ⇒ #t |#
#| (todos-distintos? '(a b c)) ; ⇒ #t |#
#| (todos-distintos? '(a b c a)) ; ⇒ #f |#

(define (solo-dos-iguales? lista)
  (if (< (length lista) 2)
  #f
  (if (contiene? (rest lista) (first lista))
    (todos-distintos? (rest lista))
    (solo-dos-iguales? (rest lista)))))


#| (solo-dos-iguales? '()) ; ⇒ #f |#
#| (solo-dos-iguales? '(a)) ; ⇒ #f |#
#| (solo-dos-iguales? '(a b c a)) ; ⇒ #t |#
#| (solo-dos-iguales? '(a b c b a a)) ; ⇒ #f |#
#| (solo-dos-iguales? '(a b c a a)) ; ⇒ #f |#
#| (solo-dos-iguales? '(a b c a b)) ; ⇒ #f |#

#| (define a "a") |#
#| (define b "b") |#
#| (define c "c") |#
#| (define d "d") |#
#| (define e "e") |#
#| (define f "f") |#
#| (define g "g") |#


#| (list (cons a b) c (list d e)) |#
#| (car (third (list (cons a b) c (list d e)))) |#
#| (cdr (first (list (cons a b) c (list d e)))) |#

#| (define p2 (list (list (cons a (cons b c)) (list d e) f) g)) |#
#| (cddr (first (first p2))) |#
#| (second (second (first p2))) |#


(define (contar-datos-iguales lista)
  (if (null? lista)
    0
    (+ (if (equal? (cdr (first lista)) 
                   (car (first lista)))
          1
          0)
       (contar-datos-iguales (rest lista)))))

#| (contar-datos-iguales '((2 . 3) ("hola" . "hola") (\#a . \#a) (true . false))) ; ⇒ 2 |#
#| (contar-datos-iguales '((2 . "hola") ("hola" . 3) (\#a . true) (\#b . false))) ; ⇒ 0 |#

(define (valor-carta c)
  (car (carta c)))

(define (palo-carta c)
  (cdr (carta c)))

#| (palo-carta 'A♠) ; ⇒ Picas |#
#| (palo-carta '2♣) ; ⇒ Tréboles |#
#| (palo-carta '3♥) ; ⇒ Corazones |#
#| (palo-carta '4♦) ; ⇒ Diamantes |#

(define (veces-palo lista palo)
  (if (null? lista)
    0
    (+ (if (equal? (palo-carta (first lista)) palo) 1 0) 
       (veces-palo (rest lista) palo))))

#| (veces-palo '(5♠ 6♣ 7♥ 8♦ 9♠) 'Picas) ; ⇒ 2 |#
#| (veces-palo '(J♠ Q♣ K♥) 'Diamantes) ; ⇒ 0 |#
#| (veces-palo '(A♣ 2♥ 3♠) 'Corazones) ; ⇒ 1 |#
#| (veces-palo '() 'Tréboles) ; ⇒ 0 |#

(define (color? lista)
  (if (null? (rest lista))
    #t
    (and (equal? (palo-carta (second lista)) (palo-carta (first lista)))   
         (color? (rest lista)))))

#| (color? '(5♣ J♦ J♣ Q♠ Q♥)) ; ⇒ #f |#
#| (color? '(2♦ 5♦ 6♦ J♦ K♦)) ; ⇒ #t |#


(define (escalera? lista)
  (if (null? (rest lista))
    #t
    (and (> (valor-carta (second lista)) (valor-carta (first lista)))   
         (escalera? (rest lista)))))

#| (escalera? '(5♣ 4♦ 3♣)) ; ⇒ #f |#
#| (escalera? '(8♣ 9♦ J♣ Q♦)) ; ⇒ #t |#
#| (escalera? '(8♣ 2♣)) ; ⇒ #f |#
#| (escalera? '(A♣ 2♦ 3♣)) ; ⇒ #t |#


(define (escalera-color? lista)
  (and (escalera? lista) (color? lista)))

#| (escalera-color? '(5♣ 6♦ 7♣ 8♠ 9♥)) ; ⇒ #f |#
#| (escalera-color? '(A♦ 2♦ 3♦ 4♦ 5♦)) ; ⇒ #t |#



(define (suma-der pareja n)
  (cons (car pareja) (+ n (cdr pareja))))

(define (suma-izq pareja n)
  (cons (+ n (car pareja))  (cdr pareja)))

#| (suma-izq (cons 10 20) 3)  ; ⇒ (13 . 20) |#
#| (suma-der (cons 10 20) 5)  ; ⇒ (10 . 25) |#



(define (suma-impares-pares lista)
  (if (null? lista)
    (cons 0 0)
    (if (even? (first lista))
      (suma-der (suma-impares-pares (rest lista)) (first lista))
      (suma-izq (suma-impares-pares (rest lista)) (first lista)))))

#| (suma-impares-pares '(3 2 1 4 8 7 6 5)) ; ⇒ (16 . 20) |#
#| (suma-impares-pares '(3 1 5))           ; ⇒ (9 . 0) |#

(define (cadena-mayor lista)
  (if (null? lista)
    (cons "" 0)
    (if (> (string-length (first lista)) (cdr (cadena-mayor (rest lista))))
      (cons (first lista)  (string-length (first lista)))
      (cadena-mayor (rest lista)))))

#| (cadena-mayor '("vamos" "a" "obtener" "la" "cadena" "mayor")) ; ⇒  ("obtener" . 7) |#
#| (cadena-mayor '("prueba" "con" "maximo" "igual")) ; ⇒ ("maximo" . 6) |#
#| (cadena-mayor '()) ; ⇒ ("" . 0) |#



#| (write "hello world") |#
#| (print "hello world") |#
#| (print "hello world") |#
#| (define (displayconsecutive n start) |#
#|   (displayln start)  |#
#|   (unless (= start n) |#
#|   (displayconsecutive n (+ start 1)))) |#
#| (displayconsecutive 10 0) |#

