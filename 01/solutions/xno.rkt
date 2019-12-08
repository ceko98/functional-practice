#lang racket
 (require racket/trace)

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

(define (next-p p) (if (equal? p "X") "O" "X"))

(define (get-empty b)
  (define (helper i j)
    (cond 
      ((> i 2) '())
      ((> j 2) (helper (+ i 1) 0))
      ((not (matrix-ref b i j)) (cons (cons i j) (helper i (+ j 1))))
      (else (helper i (+ j 1)))))
  (helper 0 0))

(define (eval-board b me)
  (cond
    ((equal? (winner b) "D") 0)
    (me -1)
    (else 1)))

(define (helper-max b curr-p me)
  (define (help)
    (lambda (pr) (helper-min (place b (car pr) (cdr pr) curr-p) (next-p curr-p) (not me))))

  (if (winner b)
    (eval-board b me)
    (foldr max -2 (map (help) (get-empty b)))))

(define (helper-min b curr-p me)
  (define (help)
    (lambda (pr) (helper-max (place b (car pr) (cdr pr) curr-p) (next-p curr-p) (not me))))

  (if (winner b)
    (eval-board b me)
    (foldr min 2 (map (help) (get-empty b)))))

(define (play b p)
  (define (help-lambda)
    (lambda (acc val)
      (if (> (cdr val) (cdr acc))
        val
        acc)))
  (define (mapped-values)
    (map 
      (lambda (pr) (cons pr (helper-min (place b (car pr) (cdr pr) p) p #t)))
      (get-empty b)))
  (mapped-values))
  ;(car (foldr (help-lambda) '((0 . 0) . -2) (mapped-values))))
(play '(( #f #f)
        (#f #f #f)
        ("O" "X" "X")) "X")
