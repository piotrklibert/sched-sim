#lang racket

(require
 "utils/lst.rkt"
 "job.rkt"
 "resp-ratio-sched.rkt")


(provide
 (contract-out 
  [run-simulation (->* ((listof job?)) (number? #:method symbol?) (listof (listof job?)))]))


(define (run-simulation initial [limit 10] #:method [meth 'hrrn])
  
  (let loop ([n 0] 
             [jobs initial]
             [accumulator '()])
  
    (define rand-job   (choose-job jobs #:method meth))
    (define next-jobs  (next-step jobs rand-job))
    
    (if (>= n limit)
        (reverse accumulator)
        (loop (add1 n) 
              next-jobs 
              (cons jobs accumulator)))))

(define (next-step jobs job)
  (define-values (head tail) (split-at jobs (index-of jobs job)))
  (let* 
      ([job (job-make-older (job-use-task job))]
       [job (if (job-processing? job) (list job) (list job))])
    (append (map job-make-older head)
            job
            (map job-make-older (cdr tail)))))

