#lang racket
(define upper 100)

(define lower 1)

(define (start n m)
  (set! upper (max n m))
  (set! lower (min n m))
  (guess))

(define (guess)
    (quotient (+ upper lower) 2))

(define (smaller)
  (set! upper (sub1 (guess)))
  (guess))

(define (bigger)
  (set! lower (add1 (guess)))
  (guess))
