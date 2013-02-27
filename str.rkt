#lang racket

(require srfi/13)

(provide 
 (prefix-out s: (all-defined-out)))

(define (pad str n [char #\space])
  (string-pad str n char))

(define (rpad str n [char #\space])
  (string-pad-right str n char))

(define (padder n [char #\space]) 
  (λ (str) 
    (pad str n char)))

(define cat string-append)
(define cat* string-append*)