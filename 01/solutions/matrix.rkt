#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix
         foldl
         foldr)
; the provide "exports" these functions

(define (foldr f nv xs)
  (if (null? xs)
    nv
    (f (car xs) (foldr f nv (cdr xs)))))

(define (foldl f acc xs)
  (if (null? xs)
    acc
    (foldl f (f acc (car xs)) (cdr xs))))

; 00.
(define (all? p? xs)
  (foldl (lambda (acc v) (and v acc)) #t (map p? xs)))

; 01.
(define (any? p? xs)
  (foldl (lambda (acc v) (or v acc)) #f (map p? xs)))

; 02.
(define (concat xss)
  (foldr append '() xss))

; 03.
(define (rows xss) xss)

; 04.
(define (cols xss)
  (if (null? (car xss))
    '()
    (cons (map car xss) (cols (map cdr xss)))))

; 05.
(define (matrix-ref xss i j)
  (list-ref (list-ref xss i) j))

; 06.
(define (set xs i x)
  (if (= i 0)
    (cons x (cdr xs))
    (cons (car xs) (set (cdr xs) (- i 1) x))))

; 07.
(define (place xss i j x)
  (set xss i (set (list-ref xss i) j x)))
; 08.
(define (diag xss)
  (if (null? xss)
    '()
    (cons (car (car xss)) (diag (map cdr (cdr xss))))))
; 09.
(define (diags xss)
  (list (diag xss) (diag (reverse (cols xss)))))

; 10.
(define (map-matrix f xss)
  (map (lambda (xs) (map f xs)) xss))

; 11.
(define (filter-matrix p? xss)
  (map (lambda (xs) (filter p? xs)) xss))

; 12.
(define (zip-with f xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))))

; 13.
(define (zip-matrix xss yss)
  (map (lambda (xs ys) (zip-with cons xs ys)) xss yss))
