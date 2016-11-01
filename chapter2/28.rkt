#lang planet neil/sicp

(define (append x l)
  (cond ((null? x) l)
        ((not (pair? x)) (cons x l))
        (else (append-list x l))))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) tree)
        (else (append (fringe (car tree)) (fringe (cdr tree))))))
