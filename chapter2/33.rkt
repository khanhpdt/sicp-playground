#lang racket

(require "../accumulate.rkt")

(define (map p sequence)
  (accumulate (lambda (x y)
                (if (null? x)
                    null
                    (cons (p x) y)))
              null
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y))
              0
              seq))
