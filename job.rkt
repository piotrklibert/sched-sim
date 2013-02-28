#lang racket

(struct job-error exn:fail ())

(struct job (waiting-time remaining-tasks)
  #:transparent)

(define 0+? exact-nonnegative-integer?)

(provide 
 
 job-error 
 make-job-error
 
 (contract-out
  [struct job ((waiting-time    0+?)
               (remaining-tasks 0+?))]
  
  ;; Job creation/functional update
  [make-job           (->* ()     (0+? 0+?) job?)]
  [job-make-older     (->* (job?) (real?)   job?)]
  [job-use-task       (-> job? job?)]
  
  ;; Accessors
  [job-remaining-time (-> job? positive?)]
  [job-priority       (-> job? number?)]
  [job-processing?    (-> job? boolean?)]))


(define (make-job-error msg cont-marks)
  (job-error msg cont-marks))

(define (make-job [waiting #f] [remaining #f])
  (let
      ([waiting   (if waiting waiting (add1 (* 1000 (random 100))))]
       [remaining (if remaining remaining (add1 (* 1000 (random 100))))])
    (job waiting remaining))) 

(define (job-make-older job [by 100])
  (define so-far (job-waiting-time job))
  (make-job (+ by so-far)
            (job-remaining-tasks job))) 

(define (job-use-task job)
  (define tasks (job-remaining-tasks job))
  (if (> tasks 0)
      (make-job (job-waiting-time job) 
                (sub1 tasks))
      job)) 

(define (job-remaining-time job)
  (* 6000 (job-remaining-tasks job))) 

(define (job-priority job)
  (with-handlers 
      ([exn:fail:contract:divide-by-zero? (λ args -1)])
    (+ 1 (/ (job-waiting-time job) 
            (job-remaining-tasks job)))))

(define (job-processing? job)
  (if (> (job-remaining-tasks job) 0)
      #true
      #false))