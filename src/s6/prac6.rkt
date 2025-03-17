#lang racket

(require rackunit)
(require 2htdp/image)

(require graphics/turtles)

(require "lpp.rkt")


(define (concat-iter lista acum)
  (if (null? lista)
    acum
    (concat-iter (rest lista) (string-append acum (first lista)))))

(define (concat lista)
  (concat-iter lista ""))

#| (concat  '("hola" "y" "adiós")) ; ⇒ "holayadiós" |#
#| (concat-iter '("hola" "y" "adiós") "") ; ⇒ "holayadiós" |#


(define (minimo a b)
  (if (< a b) a b))

(define (maximo a b)
  (if (> a b) a b))

(define (min-max-iter l p)
  (if (null? l) 
    p 
    (min-max-iter (rest l) (cons (minimo (first l) (car p)) (maximo (first l) (cdr p))))))

(define (min-max l)
  (min-max-iter (rest l) (cons (first l) (first l))))

#| (min-max '(2 5 9 12 5 0 4)) ; ⇒ (0 . 12) |#
#| (min-max '(3 2 -8 4 10 0))  ; ⇒ (-8 . 10) |#
#| (min-max-iter '(5 9 12 -2 5 0 4) (cons 2 2)) ; ⇒ (-2 . 12) |#


(define (expande-pareja-iter p l)
  (if (= 0 (cdr p)) l
    (expande-pareja-iter (cons (car p) (- (cdr p) 1)) (cons (car p) l))))

(define (expande-pareja p)
  (expande-pareja-iter p '()))

(define (expande-parejas-iter l . l2)
  (if (null? l2)
    l
    (apply expande-parejas-iter (append l (expande-pareja (first l2))) (rest l2))))

(define (expande-parejas . l)
   (apply expande-parejas-iter '() l))


#| (expande-pareja (cons 'a 4)) ; ⇒ (a a a a) |#
#| (expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4)) |#
#| ; ⇒ (#t #t #t "LPP" "LPP" b b b b) |#

(define (rotar n l)
  (if 
    (= n 0) l 
    (rotar (- n 1) (append (rest l) (list (first l))))))

#| (rotar 4 '(a b c d e f g)) ; ⇒ (e f g a b c d) |#


(define (mi-foldl f ac l)
  (if (null? l) ac (mi-foldl f (f (first l) ac) (rest l))))

#| (mi-foldl string-append "****" '("hola" "que" "tal")) ;⇒ "talquehola****" |#
#| (mi-foldl cons '() '(1 2 3 4)) ; ⇒ (4 3 2 1) |#


(define (binario-a-decimal-it ac l)
  (if (null? l) 
    ac
    (binario-a-decimal-it (+ (first l) (* ac 2)) (rest l))))

(define (binario-a-decimal l)
  (binario-a-decimal-it 0 l))

#| (binario-a-decimal '(1 1 1 1)) ; ⇒ 15 |#
#| (binario-a-decimal '(1 1 0)) ; ⇒ 6 |#
#| (binario-a-decimal '(1 0)) ; ⇒ 2 |#

(define diccionario (make-dic))

(define (pascal-memo f c dic)
  (cond 
    ((key-exists? (cons f c) dic) (get (cons f c) dic))
    ((= c 0) 1)
    ((= f 0) 1)
    ((= c f) 1)
    (else (+ (put (cons (- f 1) c) (pascal-memo (- f 1) c dic) dic)
             (put (cons (- f 1) (- c 1)) (pascal-memo (- f 1) (- c 1) dic) dic)))))

#| (pascal-memo 8 4 diccionario) ; ⇒ 70 |#
#| (pascal-memo 40 20 diccionario) ; ⇒ 137846528820 |#




(define (kotch n tr)
  (cond 
    ((= n 0) (line tr 0 "black"))
    (else (beside ;/align "bottom" 
           (kotch (- n 1) (/ tr 3)) 
                 (rotate 60
                         (kotch (- n 1) (/ tr 3)))
                 (rotate -60
                         (kotch (- n 1) (/ tr 3)))
           (kotch (- n 1) (/ tr 3))))))


(define (copo-nieve n tr)
  (above 
    (beside
      (rotate 60 (kotch n tr))
      (rotate -60 (kotch n tr)))
    (rotate 180 (kotch n tr))))


(define (alfombra-sierpinski t)
  (if  (< t 20)
    (circle t "solid" "red")
    (above 
      (beside 
        (alfombra-sierpinski (/ t 3))
        (alfombra-sierpinski (/ t 3))
        (alfombra-sierpinski (/ t 3)))
      (beside 
        (alfombra-sierpinski (/ t 3))
        (circle (/ t 3) "outline" "green")
        (alfombra-sierpinski (/ t 3)))
      (beside 
        (alfombra-sierpinski (/ t 3))
        (alfombra-sierpinski (/ t 3))
        (alfombra-sierpinski (/ t 3))))))


#| (define diccionario (make-dic)) |#
#||#
#| (define (pascal-memo f c dic) |#
#|   (cond  |#
#|     ((key-exists? (cons f c) dic) (get (cons f c) dic)) |#
#|     ((= c 0) 1) |#
#|     ((= f 0) 1) |#
#|     ((= c f) 1) |#
#|     (else (+ (put (cons (- f 1) c) (pascal-memo (- f 1) c dic) dic) |#
#|              (put (cons (- f 1) (- c 1)) (pascal-memo (- f 1) (- c 1) dic) dic))))) |#

#| (pascal-memo 8 4 diccionario) ; ⇒ 70 |#
#| (pascal-memo 40 20 diccionario) ; ⇒ 137846528820 |#


(define (pitagoras n tr dic shape)
  (if (= n 0)
    (shape tr "outline" "black") ; leaf
    (overlay/xy; above  
      ( overlay/xy; beside/align "bottom";
        (if (key-exists? n dic) 
          (rotate 45 (get n dic))
          (rotate 45 (put n (pitagoras (- n 1) (/ tr (sqrt 2)) dic shape) dic)))
        tr  0
        (if (key-exists? n dic) 
          (rotate -45 (get n dic))
          (rotate -45 (put n (pitagoras (- n 1) (/ tr (sqrt 2)) dic shape) dic))))
       0 (+ tr (/ tr (sqrt 2)))
      (shape tr "outline" "white"))))



(define (hipotenusa x)
  (* x (sqrt 2)))

(define (sierpinski-elem base)
  (isosceles-triangle (hipotenusa (/ base 2)) 90 "outline" "black"))

(define (sierpinski n ancho)
  (if (= 0 n)
      (sierpinski-elem ancho)
     (above  (sierpinski (- n 1) (/ ancho 2))
             (overlay/xy (rotate -45 (sierpinski (- n 1) (/ ancho 2)))
                  ancho  0 (rotate 45 (sierpinski (- n 1) (/ ancho 2)))))))


#| (define (hex s n) |#
#|   (if (= n 0) |#
#|     (regular-polygon s 6 "solid" "red") |#
#|     (above |#
#|       (above/align "left" |#
#|           (above/align "right"  |#
#|                       (beside (hex (* s 2/3) (- n 1)) (hex (* s 2/3) (- n 1))) |#
#|                       (hex (* s 2/3) (- n 1)))  |#
#|           (hex (* s 2/3) (- n 1))) |#

  #|     (beside (hex (* s 2/3) (- n 1)) (hex (* s 2/3) (- n 1)))) |#
  #|   ) |#
  #| ) |#
  #| ;(beside(rotate 60 (triangle s "solid" "red")) (triangle s "solid" "red") (rotate 60 (triangle s "solid" "red"))))) |#




(define (to-list-arbol l) 
  (cons (dato-arbol l) (to-list-bosque (hijos-arbol l))))

(define (to-list-bosque l) 
  (if (null? l)
    '()
    (append (to-list-arbol (first l)) (to-list-bosque (rest l)))))



#| (to-list-arbol '(* (+ (5) (* (2) (3)) (10)) (- (12)))) ; ⇒ (* + 5 * 2 3 10 - 12) |#



(define (altura-arbol l)
  (if (hoja-arbol? l)
    0
  (+ 1 (altura-bosque (hijos-arbol l)))))


(define (altura-bosque b)
  (if (null? b)
    0
    (max (altura-arbol (first b)) (altura-bosque (rest b)))))


#| (define (altura-arbol-fos a) |#
#|   (foldr max 0 (map altura-bosque-fos (hijos-arbol a)))) |#

#| (define (suma-datos-ab arbol)  |#
#|   (if (vacio-arbolb? arbo) |#
#|     0 |#
#|     (+ (dato-arbolb arbol) |#
#|        (suma-datos-ab (hijo-izq-arbolb arbol))  |#
#|        (suma-datos-ab (hijo-der-arbolb arbol))))) |#

(define (to-list arbol) 
  (if (vacio-arbolb? arbol)
    '()
    (cons (dato-arbolb arbol)
       (append (to-list (hijo-izq-arbolb arbol)) 
       (to-list (hijo-der-arbolb arbol))))))

(define arbolb2
   (construye-arbolb 40 
                 (construye-arbolb 18
                               (construye-arbolb 3 arbolb-vacio arbolb-vacio)
                               (construye-arbolb 23 
                                             arbolb-vacio
                                             (construye-arbolb 29 
                                                           arbolb-vacio
                                                           arbolb-vacio)))
                 (construye-arbolb 52
                               (construye-arbolb 47 arbolb-vacio arbolb-vacio)
                               arbolb-vacio)))

#| (to-list arbolb2) ; ⇒ (40 18 3 23 29 52 47) |#

(define (cuadrado-arbolb arbol)
  (if (vacio-arbolb? arbol)
    arbolb-vacio
    (construye-arbolb (* (dato-arbolb arbol) (dato-arbolb arbol))
                    (cuadrado-arbolb (hijo-izq-arbolb arbol))
                    (cuadrado-arbolb (hijo-der-arbolb arbol)))))


#| (cuadrado-arbolb arbolb2) ; ⇒ (40 18 3 23 29 52 47) |#

