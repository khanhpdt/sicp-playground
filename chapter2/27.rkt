#lang planet neil/sicp

(define (deep-reverse l)
  (cond ((null? l) nil)
        ((not (pair? l)) l)
        (else (append-list (deep-reverse (cdr l)) (cons (deep-reverse (car l)) nil)))))
