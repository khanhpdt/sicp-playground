#lang racket

(require "../accumulate.rkt")
(require "../map2.rkt")

(provide accumulate-n)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map2 car seqs))
            (accumulate-n op init (map2 cdr seqs)))))
