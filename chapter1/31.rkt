#lang planet neil/sicp

(define (product a b eval next)
  (if (> a b)
      1
      (* (eval a) (product (next a) b eval next))))

(define (factorial n)
  (product 1 n identity increase))

(define (identity n) n)

(define (approximate-pi n)
  (define (numerator m)
    (* 2 (+ 1 (floor (/ m 2)))))
  (define (denominator m)
    (+ (* 2 (ceiling (/ m 2))) 1))
  (* 4 (/ (product 1 n numerator increase) (product 1 n denominator increase))))

(define (product-iter a b eval next)
  (define (internal-product-iter a result)
    (if (> a b)
        result
        (internal-product-iter (next a) (* (eval a) result))))
  (internal-product-iter a 1))
