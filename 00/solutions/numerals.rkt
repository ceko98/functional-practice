#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (from-numeral n)
  (n (lambda (x) (+ x 1)) 0))

(define (to-numeral n)
  (if (= n 0)
    zero
    (succ (to-numeral (- n 1)))))

(define (plus n m)
  (lambda (f v) (n f (m f v))))

(define (mult n m)
  (lambda (f v)
    (m (lambda (x) (n f x)) v)))

(define (pred n) void)
