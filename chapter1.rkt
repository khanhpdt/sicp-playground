#lang planet neil/sicp

; Exercise 1.2
; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(define (sum-of-squares x y z)
  (cond ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))
        ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))
        ((and (>= x z) (>= y z)) (+ (* x x) (* y y)))))

; Exercies 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.7
(define (sqrt-iter new-guess old-guess x)
  ; if the two consecutive guesses are really close, we say that
  ; they approach the correct solution
  (if (close-enough? new-guess old-guess)
      new-guess
      (sqrt-iter (improve new-guess x) new-guess x)))

(define (close-enough? new-guess old-guess)
  (< (abs (- new-guess old-guess)) (/ new-guess 1000)))

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; Exercise 1.8
(define (cube-root new-guess old-guess x)
  (if (close-enough? new-guess old-guess)
      new-guess
      (cube-root (improve-cube-root-guess new-guess x) new-guess x)))

(define (improve-cube-root-guess guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

; Exercise 1.11
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3)))))))

(define (f-iterative n)
  ; fj = f(i - j), e.g., f2 = f(i - 2)
  (define (f-iter i f1 f2 f3)
    (cond ((= i n) (+ f1 (* 2 f2) (* 3 f3)))
          (else (f-iter (+ i 1) (+ f1 (* 2 f2) (* 3 f3)) f1 f2))))
  (cond ((< n 3) n)
        (else (f-iter 3 2 1 0))))

; Exercise 1.12
; v: vertical position, h: horizontal position. these positions start from 1.
(define (pascal-triangle v h)
  (cond ((or (= v 1) (= v 2)) 1)
        ((or (= h 1) (>= h v)) 1)
        (else (+ (pascal-triangle (- v 1) (- h 1)) (pascal-triangle (- v 1) h)))))
  
; Exercise 1.16
(define (fast-expt-iter b n)
  ; loop invariant: a*(b^n)
  (define (internal-fast-expt-iter b n a)
    (cond ((= n 0) a)
          ; this is what makes this exponentation procedure fast
          ((even? n) (internal-fast-expt-iter (* b b) (/ n 2) a))
          (else (internal-fast-expt-iter b (- n 1) (* a b)))))
  (internal-fast-expt-iter b n 1))

(define (even? n) (= (remainder n 2) 0))

; Exercise 1.17
(define (fast-mult-recursive a b)
  (cond ((= a 1) b)
        ((even? a) (double (fast-mult-recursive (halve a) b)))
        (else (+ b (fast-mult-recursive (- a 1) b)))))

(define (halve n) (/ n 2))

(define (double n) (* n 2))

; Exercise 1.18
(define (fast-mult-iter a b)
  ; loop invariant: a*b + c
  (define (internal-fast-mult-iter a b c)
    (cond ((= a 1) (+ b c))
          ((even? a) (internal-fast-mult-iter (halve a) (double b) c))
          (else (internal-fast-mult-iter (- a 1) b (+ c b)))))
  (internal-fast-mult-iter a b 0))

; Exercise 1.19
(define (fast-fib n)
  (define (internal-fast-fib a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (internal-fast-fib
            a
            b
            (+ (* p p) (* q q)) ; p'
            (+ (* 2 p q) (* q q)) ; q'
            (/ count 2)))
          (else
           (internal-fast-fib
            (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- count 1)))))
  (internal-fast-fib 1 0 0 1 n))