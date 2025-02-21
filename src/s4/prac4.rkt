#lang racket

(require rackunit)

(require "../s3/lpp.rkt")

(define (contiene-prefijo prefijo lista)
  (if (null? lista)
    '()
    (cons (es-prefijo? prefijo (first lista)) 
      (contiene-prefijo prefijo (rest lista)))))




(define (es-prefijo? pal1 pal2)
  (cond 
    ((equal? pal1 "") #t)
    ((equal? pal2 "") #f)
    (else (and (equal? (substring pal1 0 1) (substring pal2 0 1)) 
               (es-prefijo? (substring pal1 1 (string-length pal1)) 
                            (substring pal2 1 (string-length pal2)))))))

#| (es-prefijo? "ante" "anterior") ; ⇒ #t |#
#| (contiene-prefijo "ante" '("anterior" "antígona" "antena" "anatema")) ; ⇒ (#t #f #t #f) |#

(define (cadenas-mayores l1 l2)
  (cond 
      ((and (null? l1) (null? l2)) '())
      ((and (not (null? l1)) (null? l2)) (cons (first l1) (cadenas-mayores (rest l1) l2)))
      ((and (null? l1) (not(null? l2))) (cons (first l2) (cadenas-mayores  l1 (rest l2))))
      (else (cons 
      (if (>= (string-length (first l1)) (string-length (first l2)))
      (first l1)
      (first l2)) (cadenas-mayores  (rest l1) (rest l2))))))

#| (cadenas-mayores '("hola" "que" "tal") '("adios"))  |#
#| ; ⇒ ("adios" "que" "tal") |#
#| (cadenas-mayores '("hola" "que" "tal") '("meme" "y" "adios")) |#
#| ; ⇒ ("hola" "que" "adios") |#
#| (cadenas-mayores '("la" "primera" "práctica" "de" "recursión") |#
#|                  '("confiar" "en" "la" "recursión" "facilita" "su" "resolución")) |#
#| ; ⇒ ("confiar" "primera" "práctica" "recursión" "recursión" "su" "resolución") |#


(define (inserta-pos dato pos lista)
  (cond 
    ((null? lista) '())
    ((= pos 0) (cons dato lista))
    (else (cons (first lista) (inserta-pos dato (- pos 1) (rest lista))))))

#| (inserta-pos 'b 2 '(a a a a)) ; ⇒ '(a a b a a) |#
#| (inserta-pos 'b 0 '(a a a a)) ; ⇒ '(b a a a a) |#

(define (inserta-ordenada n lista)
  (cond 
    ((null? lista) (cons n lista))
    ((<= n (first lista)) (cons n lista))
    (else (cons (first lista) (inserta-ordenada n (rest lista))))))

#| (inserta-ordenada 100 '(-8 2 3 11 20)) ; ⇒ (-8 2 3 10 11 20) |#


(define (ordena lista)
  (if (null? (rest lista))
    lista
    (inserta-ordenada (first lista) (ordena (rest lista)) )))

#| (ordena '(2 -1 100 4 -6)) ; ⇒ (-6 -1 2 4 100) |#



(define  (mueve-al-principio lista elemento)
  (cond
    ((equal? (first lista) elemento) lista)
    (else (cons (first (mueve-al-principio (rest lista) elemento)) 
                (cons (first lista)
                      (rest (mueve-al-principio (rest lista) elemento)))))))

#| (mueve-al-principio '(a b e c d e f) 'e) ; ⇒ (e a b c d e f) |#
#| (mueve-al-principio '(a b c d e f g) 'a) ; ⇒ (a b c d e f g) |#


(define (comprueba-simbolos l1 l2)
  (cond 
    ((or (null? l1) (null? l2)) '()) 
    ((= (first l2) 
        (string-length (symbol->string (first l1)))) (cons (cons (first l1) (first l2)) 
                                                           (comprueba-simbolos (rest l1) (rest l2))))
    (else (comprueba-simbolos (rest l1) (rest l2)))))

#| (comprueba-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) |#


(define (expande-pareja p)
  (if (= (cdr p) 0)
    '()
    (cons (car p) (expande-pareja (cons (car p) (- (cdr p) 1))))))

#| (expande-pareja '(hola . 3)) ; ⇒ (hola hola hola) |#
#| (expande-pareja '(#t . 5)) ; ⇒ (#t #t #t #t #t) |#

(define (expande-parejas-2 . lista)

  (cond ((null? lista) '())
        ((= (cdr (first lista)) 0) (apply expande-parejas-2 (rest lista)))
        (else (cons (car (first lista)) (apply expande-parejas-2 (cons (cons (car (first lista)) 
                                                                           (- (cdr (first lista)) 1)) 
                                                                     (rest lista)))))))



(define (expande-lista l)
  (if (null? l)
    '()
    (append (expande-pareja (first l)) (expande-lista (rest l)))))


(define (expande-parejas . l )
  (expande-lista l))

#| (expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4))  |#
; ⇒ (#t #t #t "LPP" "LPP" b b b b)


(define (expande l)

  (cond 
    ((null? l) '())
    ((number? (first l)) (append (expande-pareja (cons (second l) (first l))) (expande (rest (rest l)))))
    (else (cons (first l) (expande (rest l))))))



#| (expande '(4 clase ua 3 lpp aulario))  |#
; ⇒ (clase clase clase clase ua lpp lpp lpp aulario)


#| ((lambda (x) (* x x)) 3) ; ⇒  9 |#
#| ((lambda () (+ 6 4))) ; ⇒ 10 |#
#| ((lambda (x y) (* x (+ 2 y))) (+ 2 3) 4) ; ⇒   30 |#
#| ((lambda (x y) (* x (+ 2 x))) 5) ; ⇒ error |#
#| (define f (lambda (a b) (string-append "***" a b "***"))) |#
#| (define g f) |#
#| (procedure? g) ; ⇒ t |#
#| (g "Hola" "Adios") ; ⇒ ***HolaAdios*** |#


(define suma-3 
  (lambda(x) (+ x 3)))
(define factorial 
  (lambda (x) (if (= x 0)
      1
      (* x (factorial (- x 1))))))



(define (doble x)
   (* 2 x)) ; define bien

(define (foo f g x y)
   (f (g x) y)) ; bien

(define (bar f p x y)
   (if (and (p x) (p y))
       (f x y)
       'error))


#| (foo + 10 doble 15) ; ⇒ error |#
#| (foo doble + 10 15) ; ⇒ error |#
#| (foo + doble 10 15) ; ⇒ 35 |#
#| (foo string-append (lambda (x) (string-append "***" x)) "Hola" "Adios") ; => ***HolaAdios |#
#| (bar doble number? 10 15) ; ⇒ error |#
#| (bar string-append string? "Hola" "Adios") ; ⇒ "HolaAdios" |#
#| (bar + number? "Hola" 5) ; ⇒ 'error |#


#| (cartas 10) |#

(define (coloca listas e1 e2 e3)
  (list (cons e1 (first listas)) (cons e2 (second listas)) (cons e3 (third listas))))

#| (coloca '(() () ()) 'a 'b 'c) ; ⇒ '((a) (b) (c)) |#
#| (coloca '((a) (a) (a)) 'b 'b 'b) ; ⇒ '((b a) (b a) (b a)) |#
#| (coloca '((a) (b c) (d e f)) 'g 'h 'i) ; ⇒ '((g a) (h b c) (i d e f))) |#


(define (reparte-tres l)
  (if (null? l)
    '(() () ())
    (coloca (reparte-tres (rest (rest (rest l)))) (first l) (second l) (third l))))

(define doce-cartas '(A♣ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ J♣ Q♣ K♣))

#| (reparte-tres doce-cartas) ; ⇒ '((A♣ 4♣ 7♣ J♣) (2♣ 5♣ 8♣ Q♣) (3♣ 6♣ 9♣ K♣)) |#


(define (quita-ultimo lista)
  (if (null? (rest (rest lista)))
    (cons (first lista) '())
    (cons (first lista) (quita-ultimo (rest lista)))))


(define (elemento-central lista)
  (if (null? (rest lista))
    (first lista)
    (elemento-central (quita-ultimo (rest lista)))))


#| (elemento-central '(a b c d e f g)) ; ⇒ d |#

#| (quita-ultimo (list 'a 'b 'c 'd)) |#
