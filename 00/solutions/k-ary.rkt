#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
  (define (help n k i acc)
    (if (< n 10)
      (+ acc (* n (expt k i)))
      (help
        (quotient n 10)
        k
        (+ i 1)
        (+ acc (* (remainder n 10) (expt k i)))
      )
    )
  )
  (help n k 0 0)
)

(define (to-k-ary n k)
  (if (< n k)
    n
    (+ (* (to-k-ary (quotient n k) k) 10) (remainder n k))
  )
)
