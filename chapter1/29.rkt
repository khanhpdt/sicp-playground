#lang planet neil/sicp

(define (simpson-integral f a b n)
  (define (h)
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (eval a)
    (cond ((or (= a 0) (= a n)) (y a))
          ((even? a) (* 2 (y a)))
          (else (* 4 (y a)))))
  (* (/ (h) 3) (sum 0 n eval increase)))

(define (integral f a b dx)
  (define (next a)
    (+ a dx))
  (* dx (sum (+ a (/ dx 2)) b f next)))

(define (sum a b eval next)
  (if (> a b)
      0
      (+ (eval a) (sum (next a) b eval next))))

(define (sum-cubes a b)
  (sum a b cube increase))

(define (cube a) (* a a a))

(define (increase a) (+ a 1))
