#lang racket

(require rackunit)
(require 2htdp/image)
(require graphics/turtles)

(require "lpp.rkt")

; author Nathan Rodriguez Moyses nrm69
;
; notas: el código está hecho para divertirme/entretenerme, 
; probablemente haya partes bastante mejorables

; arbol pitagórico
; debido a cómo funciona above y beside no se puede
; hacer con esta librería (se superpone)
(define (pitagoras n tr dic shape)
  (if (= n 0)
    (shape tr "outline" "green") ; leaf
    (overlay/xy; above  
      (overlay/xy ; beside/align "bottom";
        (if (key-exists? n dic) 
          (rotate 45 (get n dic))
          (rotate 45 (put n (pitagoras (- n 1) (/ tr (sqrt 2)) dic shape) dic)))
        tr  0
        (if (key-exists? n dic) 
          (rotate -45 (get n dic))
          (rotate -45 (put n (pitagoras (- n 1) (/ tr (sqrt 2)) dic shape) dic))))
       0 (+ tr (/ tr (sqrt 2)))
      (shape tr "outline" "brown"))))



; versión de la libreria 2htdp/image
; de una variante de la alfombra de sierpinski
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



(turtles #t) ; activo las tortugas
(turn 90) ; miro hacia arriba ()

; función inspirada en: https://es.wikipedia.org/wiki/Curva_del_drag%C3%B3n#[Des]plegado_del_drag%C3%B3n
;
; 
;
#| Los patrones de plegado de esta secuencia de tiras de papel, como secuencias  |#
#| de pliegues a la derecha (D) e izquierda (I), son: |#
#|     1.ª iteración: D |#
#|     2.ª iteración: D D I |#
#|     3.ª iteración: D D I D D I I |#
#|     4.ª iteración: D D I D D I I D D D I I D I I |#
#| Cada iteración se puede encontrar copiando la iteración anterior, luego una D,  |#
#| luego una segunda copia de la iteración anterior en orden inverso con las  |#
#| letras I y D intercambiadas. |#

(define (create-dragon-pattern n)
  (if (= n 1) 
    (list 90)
    (append (create-dragon-pattern (- n 1)) 
            (cons 90 (foldl cons 
                            '() 
                            (map (lambda (x) (if (= 90 x) -90 90)) 
                                          (create-dragon-pattern (- n 1))))))))

(define (dragon n size)
  (if (null? n)
    (home) ; = no hacer nada (para la recursividad)
    (begin
      (draw size)
      (turn (first n))
      (dragon (rest n) size))))



; versión del arbol pitagórico de turtle
(define (pitagoras-turtle n size)
  (if (= n 0)
    (begin 
      (draw size)
      (home))
    (begin
      (draw size)
      (turn 45)
      (split (turn -90))
      (pitagoras-turtle (- n 1) (/ size (sqrt 2))))))



; distintas versiones de sierpinski 
; basadas en la página web: 
; https://larryriddle.agnesscott.org/ifs/siertri/boxVariation.htm
; sin dicha página habría sido imposible hacer estas funciones
(define (sierpinski-turtle n size)
  (if (= n 0)
    (begin ; empieza y acaba en la esquina
      (draw size) 
      (move (- size))
      (turn -90)
      (draw (/ size 2))
      (move (- (/ size 2)))
      (turn 90))
  (begin 
    (sierpinski-turtle (- n 1) (/ size 2))
    (move (/ size 2))
    (sierpinski-turtle (- n 1) (/ size 2))
    (move (- (/ size 2)))
    (turn -90)
    (move (/ size 4)) 
    (turn 90)
    (sierpinski-turtle (- n 1) (/ size 2))
    (turn -90)
    (move (- (/ size 4)))
    (turn 90))))




(define (sierpinski-turtle-v2 n size)
  (if (= n 0)
    (begin ; empieza y acaba en la esquina
      (draw size) 
      (move (- size))
      (turn -90)
      (draw (/ size 2))
      (move (- (/ size 2)))
      (turn 90))
  (begin 
    (sierpinski-turtle-v2 (- n 1) (/ size 2))
    (move (/ size 2))
    (turn -90)
    (move (/ size 2))
    (turn 180)
    (sierpinski-turtle-v2 (- n 1) (/ size 2))
    (turn 90)
    (move (/ size 2))
    (turn 180)
    (sierpinski-turtle-v2 (- n 1) (/ size 2))
    (turn -90)
    (move (- (/ size 2)))
    (turn 90))))


(define (sierpinski-turtle-v3 n size)
  (if (= n 0)
    (begin ; empieza y acaba en la esquina
      (draw size) 
      (move (- size))
      (turn -90)
      (draw size)
      (move (- size))
      (turn 90))
  (begin
    (move (/ size 2))
    (turn -90)
    (move (/ size 2))
    (turn -90)
    (sierpinski-turtle-v3 (- n 1) (/ size 2))
    (turn -90)
    (move (/ size 2))
    (turn -90)
    (move (/ size 2))
    (turn -90)
    (sierpinski-turtle-v3 (- n 1) (/ size 2))
    (turn -90)
    (move size)
    (turn -90)
    (move (- size))
    (sierpinski-turtle-v3 (- n 1) (/ size 2))
    (move size)
    (turn -90))))


(define (sierpinski-turtle-v4 n size)
  (if (= n 0)
    (begin ; empieza y acaba en la esquina
      (draw size) 
      (move (- size))
      (turn -90)
      (draw size)
      (move (- size))
      (turn 90))
  (begin
    (sierpinski-turtle-v4 (- n 1) (/ size 2))
    (move size)
    (turn -90)
    (sierpinski-turtle-v4 (- n 1) (/ size 2))
    (turn -90)
    (move size)
    (turn -90)
    (move (- size))
    (sierpinski-turtle-v4 (- n 1) (/ size 2))
    (move size)
    (turn -90))))

(dragon (create-dragon-pattern 10) 10)

(define (create-dragon-pattern-gold n)
  (if (= n 1) 
    (list 79.87980027)
    (append (create-dragon-pattern (- n 1)) 
            (cons 79.87980027 (foldl cons 
                            '() 
                            (map (lambda (x) (if (= 79.87980027 x) -79.87980027 79.87980027)) 
                                          (create-dragon-pattern (- n 1))))))))

(define (dragon-gold n size)
  (if (null? n)
    (home) ; = no hacer nada (para la recursividad)
    (begin
      (draw (* size 0.74274))
      (turn (first n))
      (draw (* size (* 0.74274 0.74274)))
      (dragon-gold (rest n) size))))
