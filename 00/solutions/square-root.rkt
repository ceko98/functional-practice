#lang racket

(provide my-sqrt)

(define (my-sqrt x)
  (define (f a)
    (- (* a a) x))

  (define (f-pr a)
    (* 2 a))

  (define (go x x-i)
    (if (< (abs (- x (* x-i x-i))) 0.0001)
      x-i
      (go
        x
        (- x-i (/ (f x-i) (f-pr x-i)))
      )
    )
  )
  (go x x)
)
