#lang racket


; (define (<name> <args>)
;   <body>)
;
;   (define <name variable> <value> )
;
;    

#| (+ 20 40) |#
#| (+) ; returns 0 |#
#| (sqrt 25) |#
#| + |#
#| (define x 10) |#
#| (+ (- 4 (* 3 (/ 4 2) 4)) 3) |#
#| (if (= 4 4) "verdadero" "falso") |#
#| (cons 1 2) ; (1 2) |#
#| (car (car (cons (cons 1 2) 3))) ; 1 |#
#| (car (cons 1 2)) ; 1 |#
#| (car (cons (cons 3 4) 2)) ; (3 4) |#
#| (cdr (cons 1 2)) ; 2 |#
#| (cdr (cons (cons 3 4) 2)) ;  2 |#
#| (cons (* 2 3) (/ 4 2)) ; 6 . 2 |#
#| (cdr (cons 1 (cons 2 3))) ; (2 . 3) |#
#| (cons (+ 2 1) (if (> 2 3) "2" "3")) ; (3 . "3") |#
#| (cdr (car (cons (cons 1 2) 3))) ; 2 |#
#| (list 1 2 3 4) 	 ; (1 2 3 4) |#
#| (cons 3 '(1 2 3)) ; (3 1 2 3) |#
#| (rest (list 1 2 3 4)) ; (2 3 4) |#
#| (rest (cons #t (cons "Hola" (list 1)))) ; ("hola" 1) |#
#| (first '(1 2 3 4)) 	 ; 1 |#
#| (first (list (list 1 2) 1 2 3 4)) ; (1 2) |#
#| (first (list #t 1 "Hola")) ;  #t |#
#| (first (rest '((1 2) 1 2))) ; 1  |#
#| (first (rest (list 1 2 3 4))) ; 2 |#
#| (cons '(1 2 3) '(4 5 6)) ; (1 2 3 4 5 6) ; mirar  -> ((1 2 3) 4 5 6) |#
#| (rest (rest '(1 2 3 4))) ; (3 4) |#
#| (first (rest (list 1 2 3 4))) ; 2 |#
#| (first (rest (rest (list 1 2 3 4)))) ; 3 |#
#| (rest (rest (list 1 2 3 4))) ; (3 4) |#
#| (list (* 2 2) (+ 1 2) (/ 4 2)) ; (4 3 2) |#
#| (first (rest (rest (rest '(1 2 3 4))))) ; 4 |#
#| (list (+ 2 3) (- 3 4) (string-ref "hola" 3)) ;  5 -1 #\a |#
#| (first ( rest( rest(list 1 2 3 4 5))))  |#
#| (rest( rest( rest( rest(list 1 2 3 4 5))))) |#
#| (first( rest( rest( rest(rest( list 1 2 3 4 5)))))) |#
#| (first (rest (rest (list 1 (list 2 3) (list 4 5) 6)))) ; '(4 5) |#
#| (rest (rest '(1 (2 3) 4 5))) ; '(4 5) |#
#| (equal? "hola" "hola") ; #t |#
#| (+ (char->integer(integer->char 1200)) (char->integer #\A)) ; 1265 |#
#| (string-ref "pepe" 1) 	 ; e |#
#| (string-length (make-string 7 #\E)) ; 7 |#
#| (substring "buenos dias" 1 4) 	; "uen" |#
#| (define a 3) ; a = 3 |#
#| (define b (+ a 1)) ; b = 4 |#
#| ; (= "hola" "hola") 	; #t RunTime error  |#
#| (+ a b (* a b)) ; 12 + 3 + 4 = 19 |#
#| (string-ref (substring "buenos dias" 2 5) 1) 	; ; n |#
#| (= a b); #f |#
#| (define pi 3.14159) 	 |#
#| (if (and (> a b) (< b (* a b))) b a) ; a= 3 |#
#| pi 	 ; 3.14159 |#
#| (cond ((= a 4) 6)  |#
#| ((= b 4) (+ 6 7 a))  |#
#| (else 25)); 16 |#
#| "pi" 	 ;"pi" |#
#| (+ 2 (if (> b a) b a)) ; 6 |#
#| (+ pi (+ pi pi)) 	 ; 3*pi |#
#| (* (cond ((> a b) a) |#
#| ((< a b) b) ;  |#
#| (else -1))  |#
#| (+ a 1)); b * 4 = 16 |#
#| (+ (* pi pi) (- 2 pi pi pi pi)) ; pi**2 + 2 - (pi pi pi pi) |#
#| ((if (< a b) + -) a b) ; 7 |#


(+ (char->integer(integer->char 1200)) (char->integer #\A)) ; 1265
;
; define la distancia entre dos cons de números
;
(define (dist a b) 
  (sqrt (+ (* (- (cdr a) (cdr b)) 
              (- (cdr a) (cdr b))) 
           (* (- (car a) (car b)) 
              (- (car a) (car b))))))
;p

;
; comprueba si los cons de números componen un triángulo isósceles
;
(define (isosceles? a b c) 
  (or (and (= (dist a b) (dist a c)) 
           (not 
             (= (dist a b) (dist b c))))  
      (and (= (dist b c) (dist b a)) 
           (not (= (dist b c) (dist a c))))
      (and (= (dist c a) (dist c b)) 
           (not (= (dist c b) (dist a b))))))
;



(isosceles? (cons 0 0 ) (cons 3 0) (cons 6 0))
(isosceles? (cons 2 2 ) (cons 4 0) (cons 0 0))
(isosceles? (cons 0 0 ) (cons 0 0) (cons 0 0))
(isosceles? (cons 0 0 ) (cons 1 1) (cons 3 2))



