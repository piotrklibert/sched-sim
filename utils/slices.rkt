#lang racket

(require srfi/26)

(define (correct-length? lst)
  (let ([len (length lst)])
    (and (len . > . 0)
         (len . < . 4))))

(provide/contract (vslice (->* (vector?) () #:rest (and/c (listof integer?) 
                                                          correct-length?) 
                               vector?))
                  (slice  (->* (list?)   () #:rest (and/c (listof integer?) 
                                                          correct-length?) 
                               list?)))

(define (multi-ref vec idx-list)
  (list->vector (for/list 
                    ([idx idx-list]) 
                  (vector-ref vec idx))))

(define (vslice vec . args)
  (let ([argc (length args)]
        [fstarg (car args)])
    
    (cond [(and (argc . = . 1) 
                (fstarg . < . 0))
           (let* 
               ([vlen (- (vector-length vec) 1)]
                [beg  (+ fstarg vlen)]) 
             (multi-ref vec (range beg vlen)))]
          
          [else (multi-ref vec (apply range args))])))

(define (slice lst . args) 
  (let* ([input-vec (list->vector lst)]
        [real-args (list* input-vec args)])
    (vector->list (apply vslice 
                         real-args))))