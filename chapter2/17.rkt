#lang planet neil/sicp

(define (last-pair l)
  (cond ((null? l) nil)
        ((null? (cdr l)) (cons (car l) nil))
        (else (last-pair (cdr l)))))
