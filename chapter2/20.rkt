#lang planet neil/sicp

(define (same-parity . numbers)
  (define (same-parity-with n others)
    (cond ((null? others) nil)
          ((= (remainder n 2) (remainder (car others) 2)) (cons (car others)
                                                                (same-parity-with n (cdr others))))
          (else (same-parity-with n (cdr others)))))
  (same-parity-with (car numbers) (cdr numbers)))
