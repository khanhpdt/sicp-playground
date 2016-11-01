#lang planet neil/sicp

(define (timed-fast-prime-test n)
  (newline)
  (display n)
  (start-fast-prime-test n (runtime)))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 50) (report-prime (- (runtime) start-time))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test? n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test? n)
  (define (try-it a n)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1))) n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

; Exercise 1.25
(define (expmod-2 base exp m)
  ; this computes the exponentation before the modulo. Because the exponentation
  ; involves very large numbers, this takes more time than the original expmod,
  ; which always keeps the modulo operands small.
  (remainder (fast-expt-iter base exp) m))

; Exercise 1.26
(define (expmod-3 base exp m)
  (cond ((= exp 0) 1)
        ; because of the two recursive calls and the applicative order, the number
        ; of steps is equal to the number of nodes in the computation tree.
        ; e.g. exp = 16, the tree is like 16 -> 8 8 -> 4 4 4 4 -> ....
        ; So instead of producing a linear-recursive process, this algorithm
        ; produces a tree-recursive one.
        ((even? exp) (remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
