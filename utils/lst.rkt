#lang racket

(provide (all-defined-out))

(define/contract (sum alist)
                 (-> (listof number?) number?)
  (foldl + (car alist) (cdr alist)))

(define/contract (list-set lst i val)
                 (-> list? integer? any/c list?)
  (for/list 
      ([(el n) (in-indexed lst)])
    (if (= i n) val el)))

(define (zip . lists)
  (apply map list lists))

(define (index-of lst v)
  (for/last ([(el n) (in-indexed lst)]
             #:final (equal? el v))
    n))

(define (list->values vals)
  (apply values vals))