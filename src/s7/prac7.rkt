#lang racket

(require rackunit)
(require 2htdp/image)

(require "../s6/lpp.rkt")


(define lista-a '((a b) d (c (e) (f g) h)))
#| (check-equal? (fourth (third lista-a)) 'h) |#


(define lista-b1 '((2 (3)) (4 2) ((2) 3)))
;
;             * 
;   *         *     * 
; 2 *       4   2   * 3
;   3               2
;
; ((4 (9)) (16 4) ((4) 9))
; (cuadrado-estruct (2 (3)))
; (cuadrado-estruct ((4 2) ((2) 3)))

(define lista-b2 '((b) (c (a)) d (a)))
;
;             *
;   *         *     d  *
;   b      c   *       a
;              a
;
;

#| (define (suma-1-si-mayor-igual-que-0 x) |#
#|   (if (>= x 0) |#
#|       (+ x 1) |#
#|       x)) |#
#| (define (nivel-hoja-fos dato ld) |#
#|   (if (hoja? ld) |#
#|       (if (equal? ld dato) 0 -1) |#
#|       (suma-1-si-mayor-igual-que-0 |#
#|        (foldr max -1 (map (lambda (elem) |#
#|                            (nivel-hoja-fos dato elem)) ld))))) |#
; (map (lambda (elem) (nivel-hoja-fos 'a elem)) lista-b2) ; -> (-1 2 -1 1)



#| (define (concatena l) |#
#|   (cond  |#
#|     ((null? l) "") |#
#|     ((hoja? l) (symbol->string l)) |#
#|     (else (string-append (concatena (first l)) (concatena (rest l)))))) |#


#| (define (concatena l) |#
#|   (if (hoja? l) |#
#|       (symbol->string l ) |#
#|       (foldr string-append  "" (map concatena l)))) |#

#| (concatena '(a b (c) d)) ; ⇒ "abcd" |#
#| (concatena '(a (((b)) (c (d (e f (g))) h)) i)) ; ⇒ "abcdefghi" |#

(define (todos-positivos? l)
  (cond 
    ((null? l) #t)
    ((hoja? l) (> l 0))
    (else (and (todos-positivos? (first l)) (todos-positivos? (rest l))))))

#| (todos-positivos? '(1 (2 (3 (-3))) 4)) ; ⇒ #f |#

(define (todos-positivos-fos? l)
  (if (hoja? l)
    (> l 0)
    (andmap todos-positivos-fos? l)))

#| (todos-positivos-fos? '(1 (2 (3 (-3))) 4)) ; ⇒ #t |#


#| (define (cumplen-predicado p l) |#
#|   (cond  |#
#|     ((null? l) '()) |#
#|     ((hoja? l) (if (p l) (list l) '())) |#
#|     (else (append (cumplen-predicado p (first l)) (cumplen-predicado p (rest l)))))) |#

(define (cumplen-predicado p l)
  (if (hoja? l) 
    (if (p l) (list l) '())
    (foldr append '() (map (lambda (x) (cumplen-predicado p x)) l))))

#| (cumplen-predicado even? '(1 (2 (3 (4))) (5 6))) ; ⇒ (2 4 6) |#
#| (cumplen-predicado pair? '(((1 . 2) 3 (4 . 3) 5) 6)) ; ⇒ ((1 . 2) (4 . 3)) |#


(define (busca-mayores n l)
  (cumplen-predicado (lambda (x) (< n x)) l))


#| (busca-mayores 10 '(-1 (20 (10 12) (30 (25 (15)))))) ; ⇒ (20 12 30 25 15) |#

(define (empieza-por c l)
  (cumplen-predicado (lambda (x) (equal? c (string-ref (symbol->string x) 0))) l))

#| (empieza-por #\m '((hace (mucho tiempo)) (en) (una galaxia ((muy  muy) lejana)))) ; ⇒ (mucho muy muy) |#

(define (sustitu))


