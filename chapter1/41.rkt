#lang planet neil/sicp

(define (double-apply f)
  (lambda (x)
    (println "apply")
    (f (f x))))

; The excution flow of (((double-apply (double-apply double-apply)) increase) 5).
; (((double-apply (double-apply double-apply)) increase) 5)
; (((double-apply (lambda (x) (double-apply (double-apply x)))) increase) 5)
; (((lambda (y) ((lambda (x) (double-apply (double-apply x))) ((lambda (x) (double-apply (double-apply x))) y))) increase) 5)
; (((lambda (x) (double-apply (double-apply x))) ((lambda (x) (double-apply (double-apply x))) increase)) 5)
; (((lambda (x) (double-apply (double-apply x))) (double-apply (double-apply increase))) 5)
; ((double-apply (double-apply (double-apply (double-apply increase)))) 5) ; the function increase will be applied for 16 times
