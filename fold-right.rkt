#lang racket

(require "./accumulate.rkt")

(provide fold-right)

(define (fold-right op initial sequence)
  (accumulate op initial sequence))
