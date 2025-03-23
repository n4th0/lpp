#lang racket

(require rackunit)
(require 2htdp/image)

(require "../s6/lpp.rkt")

; ej 1

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



; ej 2
(define (concatena l)
  (cond 
    ((null? l) "")
    ((hoja? l) (symbol->string l))
    (else (string-append (concatena (first l)) (concatena (rest l))))))


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
; ej 3

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

; ej 4
(define (sustituye-elem elem-old elem-new l)
  (cond
    ((and (hoja? l) (equal? elem-old l)) elem-new)
    ((and (hoja? l)) l)
    (else (map (lambda (x) (sustituye-elem elem-old elem-new x)) l))))

#| (sustituye-elem 'c 'h '(a b (c d (e c)) c (f (c) g))) |#
; ⇒ (a b (h d (e h)) h (f (h) g))




(define (nivel-mas-profundo l)
  (nivel-mas-profundo-aux -1 l))

(define (nivel-mas-profundo-aux n l)
  (cond 
    ((hoja? l) (cons l (+ n 1)))
    (else (foldr (lambda (x y) (if (< (cdr x) (cdr y)) y x)) (cons 0 -1) (map (lambda (x) (nivel-mas-profundo-aux (+ n 1) x)) l)))))

#| (nivel-mas-profundo '(2 (3))) ; ⇒ (3 . 2) |#
#| (nivel-mas-profundo '((2) (3 (4)((((((5))) 6)) 7)) 8)) ; ⇒ (5 . 8) |#


; ej 5

(define (mezclar l1 l2 n)
  (cond 
    ((and (hoja? l2) (< n 0)) l2)
    ((hoja? l2) l1)
    (else (map (lambda (x y ) (mezclar x y (- n 1))) l1 l2))))

(define lista1 '(((a b) ((c))) (d) e))
(define lista2 '(((1 2) ((3))) (4) 5))
#| (mezclar lista1 lista2 2) ; ⇒ (((1 2) ((3))) (d) e) |#

(define (intersecta l1 l2) ; bastante guarro
  (cond 
    ((and (hoja? l1) (hoja? l2)) (list (cons l1 l2)))
    ((hoja? l1) '())
    ((hoja? l2) '())
    ((null? l1) '())
    ((null? l2) '())
    (else (append (intersecta (first l1) (first l2)) (intersecta (rest l1) (rest l2))))))

(define lista-1 '(a (b c) (d))) 
;     * 
;   / | \ 
;  a  *  *
;    / \  \ 
;   b   c  d

(define lista-2 '((e) (f) (g)))
;     * 
;   / | \ 
;  *  *  * 
; /  /    \ 
;e  f      g
#| (intersecta lista-1 lista-2) |#
; ⇒ (((b . f)) ((d . g)))
;     *
;     | \
;     *  *
;    /    \
;  (b.f)  (d.g)



#| (intersecta '(a b) '(c d)) ; ⇒ '((a . c) (b . d)) |#
#| (intersecta '(a (b) (c)) '(d e (f))) ; ⇒ '((a . d) ((c . f))) |#


(define (intersecta-gen f l1 l2) ; bastante guarro
  (cond
    ((and (hoja? l1) (hoja? l2)) (list (cons (f l1) (f l2))))
    ((hoja? l1) '())
    ((hoja? l2) '())
    ((null? l1) '())
    ((null? l2) '())
    (else (append (intersecta-gen f (first l1) (first l2)) (intersecta-gen f (rest l1) (rest l2))))))
