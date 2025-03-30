#lang racket

(require rackunit)
(require 2htdp/image)

(require "../s6/lpp.rkt")


; ej1 
#| (define arbol '(15 (4 (2) (3)) (8 (6)) (12 (9) (10) (11)) ) ) |#

#| (check-equal? (dato-arbol (second (hijos-arbol (third (hijos-arbol arbol))))) 10) |#

#| (pinta-arbol arbol) |#


; 1. devuelve 4+2+3 = 9
;
; 2. devuelve 8 + 6 + 12 + 9 + 10 + 11 =  56
;
;
; 1. devuelve '(9 14 42)
; 
; 2.  
;
; (+ 42 15) -> 57
; (+ 14 57) -> 71
; (+ 9 71) ->  80
;

#| (define arbolb '(40 (23 (5 () ()) (32 (29 () ()) ())) (45 () (56 () ())) )) |#
#| (check-equal? (dato-arbolb (hijo-izq-arbolb (hijo-der-arbolb (hijo-izq-arbolb arbolb)))) 29) |#


; ej2

#| (define (to-string-arbol a) |#
#|   (string-append (symbol->string (dato-arbol a)) (to-string-bosque (hijos-arbol a)))) |#


#| (define (to-string-bosque bosque) |#
#|   (if (null? bosque) |#
#|     "" |#
#|     (string-append (to-string-arbol (first bosque)) |#
#|                    (to-string-bosque (rest bosque))))) |#


(define (to-string-arbol-fos arbol)
  (foldl (lambda (x y) (string-append y x)) (symbol->string (dato-arbol arbol)) 
         (map to-string-arbol-fos (hijos-arbol arbol))))


#| (define arbol2 '(a (b (c (d)) (e)) (f))) |#
#| (to-string-arbol-fos arbol2) ; ⇒ "abcdef" |#



#| (define (veces-arbol dato arbol) |#
#|   (if (equal? dato (dato-arbol arbol)) |#
#|     (+ 1 (veces-bosque dato (hijos-arbol arbol) )) |#
#|     (veces-bosque dato (hijos-arbol arbol)))) |#

#| (define (veces-bosque dato bosque) |#
#|   (if (null? bosque) |#
#|     0 |#
#|     (+ (veces-arbol dato (first bosque)) (veces-bosque dato (rest bosque))))) |#


(define (veces-arbol dato arbol)
    (apply + (cons (if (equal? dato (dato-arbol arbol)) 1 0) (map (lambda (x) (veces-arbol dato x)) (hijos-arbol arbol)))))

#| (veces-arbol 'b '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 3 |#
#| (veces-arbol 'g '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 0 |#


#| (define (hojas-cumplen f a) |#
#|   (if (and (hoja-arbol? a) (f (dato-arbol a))) |#
#|     (cons (dato-arbol a) (hojas-cumplen-bos f (hijos-arbol a))) |#
#|     (hojas-cumplen-bos f (hijos-arbol a)))) |#

#| (define (hojas-cumplen-bos f b) |#
#|   (if (null? b) |#
#|     '() |#
#|     (append (hojas-cumplen f (first b)) (hojas-cumplen-bos f (rest b))))) |#


(define (hojas-cumplen f arbol)
  (foldr append (if (and (hoja-arbol? arbol) (f (dato-arbol arbol))) (list (dato-arbol arbol)) '()) 
         (map (lambda (x) (hojas-cumplen f x)) (hijos-arbol arbol))))



#| (define arbol1 '(10 (2) (12 (4) (2)) (10 (5)))) |#
#| (define arbol2 '(10 (2) (12 (4) (2)) (10 (6)))) |#
#| (hojas-cumplen even? arbol1) ; ⇒ '(2 4 2) |#
#| (hojas-cumplen even? arbol2) ; ⇒ '(2 4 2 6) |#

#| (define arbol1 '(10 (2) (12 (4) (2)) (10 (5)))) |#
#| (define arbol2 '(10 (2) (12 (4) (2)) (10 (6)))) |#

#| (define (todas-hojas-cumplen? f a) |#
#|   (if (hoja-arbol? a) |#
#|     (f (dato-arbol a)) |#
#|     (todas-hojas-cumplen-bosque? f (hijos-arbol a)))) |#
#| (define (todas-hojas-cumplen-bosque? f b) |#
#|   (if (null? b) |#
#|     #t |#
#|     (and (todas-hojas-cumplen? f (first b)) (todas-hojas-cumplen-bosque? f (rest b))))) |#


#| (define (todas-hojas-cumplen? f arb) |#
#|   (if (hoja-arbol? arb) |#
#|     (f (dato-arbol arb)) |#
#|     (for-all? (lambda (x) x) (map (lambda (x) (todas-hojas-cumplen? f x)) (hijos-arbol arb))))) |#


#| (todas-hojas-cumplen? even? arbol1) ; ⇒ #f |#
#| (todas-hojas-cumplen? even? arbol2) ; ⇒ #t |#


; ej 4

(define (suma-raices-hijos a)
  (apply + (map dato-arbol (hijos-arbol a))))


(define arbol3 '(20 (2) (8 (4) (2)) (9 (5))))
#| (suma-raices-hijos arbol3) ; ⇒ 19 |#
#| (suma-raices-hijos (second (hijos-arbol arbol3))) ; ⇒ 6 |#

(define (suma-raices b)
  (if (null? b)
    0
    (+ (dato-arbol (first b)) (suma-raices (rest b)))))

#| (define (raices-mayores-arbol? a) |#
#|   (if (hoja-arbol? a) |#
#|     #t |#
#|     (and (> (dato-arbol a) (suma-raices (hijos-arbol a))) (raices-mayores-bosque? (hijos-arbol a))))) |#


#| (define (raices-mayores-bosque? b) |#
#|   (if (null? b) |#
#|     #t |#
#|     (and (raices-mayores-arbol? (first b)) (raices-mayores-bosque? (rest b))))) |#

(define (raices-mayores-arbol? a)
  (if (hoja-arbol? a)
    #t
    (for-all? (lambda (x) x) (cons (> (dato-arbol a) (suma-raices (hijos-arbol a))) (map raices-mayores-arbol? (hijos-arbol a))))))


(define arbol4 '(20 (2) (8 (4) (5)) (9 (5))))

#| (raices-mayores-arbol? arbol3) ; ⇒ #t |#
#| (raices-mayores-arbol? arbol4) ; ⇒ #f |#



(define (comprueba-raices-arbol a)
  (if (hoja-arbol? a)
    (construye-arbol 1 '())
    (construye-arbol (if (> (dato-arbol a) (suma-raices (hijos-arbol a))) 1 0)
                     (comprueba-raices-bosque (hijos-arbol a)))))


(define (comprueba-raices-bosque b)
  (if (null? (rest b))
    (comprueba-raices-arbol (first b))
    (list (comprueba-raices-arbol (first b))  (comprueba-raices-bosque (rest b)))))

#| (comprueba-raices-arbol arbol3) ; ⇒ (1 (1) (1 (1) (1)) (1 (1))) |#
#| (comprueba-raices-arbol arbol4) ; ⇒ (1 (1) (0 (1) (1)) (1 (1))) |#



(define (es-camino? l a)
  (if (null? (rest l))
    (and (hoja-arbol? a) (equal? (first l) (dato-arbol a)))
    (and (equal? (first l) (dato-arbol a)) (es-camino-bosque? (rest l) (hijos-arbol a)))))


(define (es-camino-bosque? l b)
  (if (null? b)
    #f
    (or (es-camino? l (first b)) (es-camino-bosque? l (rest b)))))

#| (define arbol '(a (a (a) (b)) (b (a) (c)) (c))) |#

#| (pinta-arbol arbol) |#

#| (es-camino? '(a b a) arbol) ; ⇒ #t |#
#| (es-camino? '(a b) arbol) ; ⇒ #f |#
#| (es-camino? '(a b a b) arbol) ; ⇒ #f |#

(define arbol '(1 (2 (3 (4) (2)) (5)) (6 (7))))


(define (nodos-nivel n a)
  (if (= n 0)
    (list (dato-arbol a))
    (foldr append '() (map (lambda (x) (nodos-nivel (- n 1) x)) (hijos-arbol a)))))


#| (nodos-nivel 0 arbol) ; ⇒ '(1) |#
#| (nodos-nivel 1 arbol) ; ⇒ '(2 6) |#
#| (nodos-nivel 2 arbol) ; ⇒ '(3 5 7) |#
#| (nodos-nivel 3 arbol) ; ⇒ '(4 2) |#


(define arbolb1 '(20 (13 (2 () ())
                         (18 () ()))
                     (40 (25 () () )
                         (43 () ()))))
(define arbolb2 '(20 (13 (2 () ())
                         (22 () ()))
                     (40 (25 () () )
                         (43 () ()))))



(define (entre? d v1 v2)
  (and (<= d v2) (>= d v1))
  )

; es siempre completo?
;
(define (ordenado-entre? a v1 v2)
  (if (vacio-arbolb? a)
    #t
    (and (entre? (dato-arbolb a) v1 v2)
         (ordenado-entre? (hijo-izq-arbolb a) v1 (dato-arbolb a))
         (ordenado-entre? (hijo-der-arbolb a) (dato-arbolb a) v2))))

#| (ordenado-entre? arbolb1 0 50) ; ⇒ #t |#
#| (ordenado-entre? arbolb2 0 50) ; ⇒ #f |#
#| (ordenado-entre? arbolb1 0 30) ; ⇒ #f |#



(define (ordenado-menor? a d)
  (ordenado-entre? a -10000000 d))

(define (ordenado-mayor? a d)
  (ordenado-entre? a d 10000000))

#| (ordenado-menor? arbolb1 50) ; ⇒ #t |#
#| (ordenado-menor? arbolb1 40) ; ⇒ #f |#
#| (ordenado-menor? arbolb2 50) ; ⇒ #f |#
#| (ordenado-mayor? arbolb1 0)  ; ⇒ #t |#
#| (ordenado-mayor? arbolb1 20) ; ⇒ #f |#
#| (ordenado-mayor? arbolb2 0) ; ⇒ #f |#

(define (ordenado? a) ; idk si se refiere a esto
  (ordenado-entre? a -10000000 10000000))

#| (ordenado? arbolb1) ; ⇒ #t |#
#| (ordenado? arbolb2) ; ⇒ #f |#

(define (camino-arbolb a l)
  (if (or (vacio-arbolb? a) (null? l))
    '()
    (cond 
      ((equal? (first l) (first '(=))) (cons (dato-arbolb a) (camino-arbolb a (rest l))))
      ((equal? (first l) (first '(<)))  (camino-arbolb  (hijo-izq-arbolb a) (rest l)))
      (else (camino-arbolb (hijo-der-arbolb a) (rest l))))))

(define arbolb '(9  (5  (3 (1 () ())
                           (4 () ()))
                        (7 () ()))
                    (15 (13 (10 () ())
                            (14 () ()))
                        (20 ()
                            (23 () ())))))

#| (pinta-arbolb arbolb) |#

#| (camino-arbolb arbolb '(= < < = > =)) ; ⇒ '(9 3 4) |#
#| (camino-arbolb arbolb '(> = < < =)) ; ⇒ '(15 10) |#


(define (inserta-ordenado n a)
  (cond 
    ((vacio-arbolb? a) (construye-arbolb n arbolb-vacio arbolb-vacio))
    ((< n (dato-arbolb a)) (construye-arbolb (dato-arbolb a) (inserta-ordenado n (hijo-izq-arbolb a)) (hijo-der-arbolb a)))
    (else (construye-arbolb (dato-arbolb a) (hijo-izq-arbolb a) (inserta-ordenado n (hijo-der-arbolb a))))))



(define a1 (inserta-ordenado 5 arbolb-vacio)) 
(define a2 (inserta-ordenado 4 a1))
(define a3 (inserta-ordenado 2 a2))
(define a4 (inserta-ordenado 6 a3))
(pinta-arbolb a4)
