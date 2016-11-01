#lang planet neil/sicp

(define zero (lambda (f) (lambda (x) x)))
; n here is a Church number
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero) expansion
; (add-1 zero)
; -> (lambda (f) (lambda (x) (f (((lambda (f2) (lambda (x2) x2)) f) x))))
; -> (lambda (f) (lambda (x) (f ( (lambda (x2) x2) x))))
; -> (lambda (f) (lambda (x) (f ( x ))))
(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one) expansion
; (add-1 one)
; -> (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; -> (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; -> (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (church->int z)
  ((z (lambda (x) (+ x 1))) 0))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x))))) ; (n f) means applying f for n times
