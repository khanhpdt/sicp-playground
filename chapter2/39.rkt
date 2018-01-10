#lang racket

(require "../fold-right.rkt")
(require "../fold-left.rkt")

(define (reverse-1 sequence)
  (fold-right (lambda (x y)
                (if (null? y)
                    (list x)
                    (append y (list x))))
              null
              sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y)
               (if (null? y)
                   x
                   (cons y x)))
             null
             sequence))