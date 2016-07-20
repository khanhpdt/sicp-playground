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

; Exercise 1.21
(define (smallest-divisor n)
  (define (internal-smallest-divisor d)
    (cond ((> (square d) n) n) ; the divisor of n must be less than or equal to square-root of n
          ((divides? n d) d)
          (else (internal-smallest-divisor (+ d 1)))))
  (internal-smallest-divisor 2))

(define (divides? n d) (= (remainder n d) 0))

; Exercise 1.22
; l: left bound inclusive, r: right bound inclusive
(define (search-for-primes l r)
  (define (internal-search-for-primes l r)
    (if (<= l r) (timed-prime-test l))
    (if (< l r) (internal-search-for-primes (+ l 2) r)))
  ; make sure that the first number tested is an odd number
  (internal-search-for-primes (if (even? l) (+ l 1) l) r))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n) (report-prime (- (runtime) start-time))))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; Exercise 1.23
(define (smart-smallest-divisor n)
  (define (internal-smart-smallest-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          ; no need to check for multiples of 2 when we know that 2 is not the divisor
          (else (internal-smart-smallest-divisor (next-divisor test-divisor)))))
  (internal-smart-smallest-divisor 2))

(define (next-divisor d)
  (if (= d 2) 3 (+ d 2)))

(define (timed-smart-prime-test n)
  (newline)
  (display n)
  (start-smart-prime-test n (runtime)))

(define (start-smart-prime-test n start-time)
  (if (smart-prime? n) (report-prime (- (runtime) start-time))))

(define (smart-prime? n)
  (= (smart-smallest-divisor n) n))

; Exercise 1.24
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
  (cond ((= exp 1) (remainder base m))
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))