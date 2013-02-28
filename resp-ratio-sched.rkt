#lang racket

(require 
 "job.rkt"
 "utils/lst.rkt")

(provide
 choose-job)

(define (job-search number jobs)
  (define priorities (map job-priority jobs))
  (define summed     (box 0.0))
  (define denom      (sum priorities))
  
  (let/cc found
    (for 
        ([(prior job) (in-parallel priorities jobs)])
      (set-box! summed (+ (unbox summed) (/ prior denom)))
      (when [(unbox summed) . > . number]
        (found job)))
    
    #false))

(define (next-job-fifo jobs)
  (list-ref (sort (filter job-processing? jobs) > #:key job-waiting-time) 0))

(define (next-job-random jobs)
  (define good-jobs (filter job-processing? jobs))
  (list-ref good-jobs (random (length good-jobs))))

(define methods (hash 'hrrn (λ (jobs) (job-search (random) jobs))
                      'fifo next-job-fifo
                      'rand next-job-random))

(define (choose-job jobs #:method [meth 'hrrn])
  ((hash-ref methods meth) jobs))