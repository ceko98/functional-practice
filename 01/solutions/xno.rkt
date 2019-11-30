#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; winner implementation that only detects draws right now.
; Put your own implementation here!
(define (equal p)
  (lambda (x) (equal? p x)))

(define (check-player p xss)
  (ormap (lambda (xs) (all? (equal p) xs)) xss))

(define (rows-cols-diags b)
  (append (rows b) (cols b) (diags b)))

(define (winner b)
  (define (helper xss)
    (cond 
      ((check-player "X" xss) "X")
      ((check-player "O" xss) "O")
      ((andmap (lambda (xs) (andmap id xs)) b) "D")
      (else #f)))
  (helper (rows-cols-diags b)))

; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!

(define (next-sign p) (if (equal? p "X") "O" "X"))

(define (eval-board win curr-sign)
  (cond
    ((equal? win curr-sign) 1)
    ((equal? win (next-sign curr-sign)) -1)
    ((equal? win "D") 0)
    (else #f)))

(define (get-empty b)
  (define (helper i j)
    (cond ((> i 2) '())
          ((> j 2) (helper (+ i 1) 0))
          ((not (matrix-ref b i j)) (cons (cons i j) (helper i (+ j 1))))
          (else (helper i (+ j 1)))))
  (helper 0 0))

(define (helper-max b p)
  (define (map-help b p)
    (lambda (pr) (helper-min (place b (car pr) (cdr pr) p) (next-sign p))))

  (if (winner b)
    (eval-board (winner b) p)
    (foldr max -2 (map (map-help b p) (get-empty b)))))

(define (helper-min b p)
  (define (map-help b p)
    (lambda (pr) (helper-max (place b (car pr) (cdr pr) p) (next-sign p))))

  (if (winner b)
    (eval-board (winner b) p)
    (foldr min 2 (map (map-help b p) (get-empty b)))))

(define (play b p)
  (define (foldr-help)
    (lambda (pr curr)
      (if (> (car pr) (car curr))
        pr
        curr)))
  (cdr (foldr (foldr-help) (cons -2 (cons 0 0))
    (zip-with
      cons
      (map 
        (lambda (pr)
          (helper-max b p))
        (get-empty b))
      (get-empty b)))))
