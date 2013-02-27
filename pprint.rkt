#lang racket

(require 
 "job.rkt"
 "table.rkt"
 "pluck.rkt")


(provide
 (contract-out 
  [format-job (-> job? string?)] 
  [format-jobs (-> (listof job?) string?)]))


(define (format-job job) 
  (define-plucked-values (list job)
    (job waiting-time remaining-time remaining-tasks priority))  
  (format-table '("")
                `("Waiting:"         . ,waiting-time)
                `("Remaining time:"  . ,remaining-time)
                `("Remaining tasks:" . ,remaining-tasks)
                `("Priority:"        . ,priority)))


(define (format-jobs jobslist)
  (define-plucked-values jobslist 
    (job waiting-time remaining-tasks priority))
  (format-table #false 
                `("Waiting time"    . ,waiting-time)
                `("Remaining tasks" . ,remaining-tasks)
                `("Priority"        . ,priority)))
