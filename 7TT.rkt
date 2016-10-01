#lang racket
(require rackunit)



(define (my-map func lst)    ;nur fuer funktioen mit einem Argument
  (cond[(empty? lst) empty]
       [else (cons (func (first lst))
                   (my-map func (rest lst)))]))

(define (my-foldr func base lst) ;nur fuer funktioen mit ZWEI Argumenten
  (cond [(empty? lst) base]
        [else (func (first lst) (my-foldr func base (rest lst)))]))

(define (my-foldl func base lst) ;nur fuer funktioen mit ZWEI Argumenten
  (cond [(empty? lst) base]
        [else (my-foldl func (func (first lst) base) (rest lst))]))
;.............................^----new base--------^

(define (my-filter pred lst)
  (cond[(empty? lst) empty]
       [(pred (first lst))
              (cons (first lst) (my-filter pred (rest lst)))]
       [else (my-filter pred (rest lst))]))
                      
(define (my-ormap pred lst)
  (cond[(empty? lst) #f]
       [(pred (first lst)) #t]
       [else (my-ormap pred (rest lst))]))

(define (my-andmap pred lst)
  (cond [(empty? lst) #t]
        [else (and (pred (first lst))
                   (my-andmap pred (rest lst)))]))

(define (my-build-list n f)
  (

;derive a function
(define (d/dx fun)
  (define delta (/ 1 1000000))
  (lambda (x) (/ (-
                  (fun (+ x delta))
                  (fun (- x delta))) (* 2 delta))))

(define (x2 x)
  (* x x))


(define testliste '(2 2 3 4))
(define tl2 '(1 2 3 4 5 6))

;try to write a filter function, that takes a predicate with 2 arguments, one of them given, the other from the list
(define (pred2to1 pred 1st)
  (lambda (2nd) (pred 1st 2nd)))


;Tests
(check-equal? (my-map add1 testliste) '(3 3 4 5))

(check-equal? (my-foldr + 0 testliste) 11)

(check-equal? (my-filter positive? testliste) '(2 2 3 4))
(check-equal? (my-filter negative? testliste) '())
(check-equal? (my-filter even? testliste) '(2 2 4))

(check-equal? (my-andmap even? testliste) #f)
(check-equal? (my-ormap even? testliste) #t)
(check-equal? ((lambda (num) (- num 2)) 5) 3)

(check-equal? (my-filter (pred2to1 < 2) testliste) '(3 4))







             