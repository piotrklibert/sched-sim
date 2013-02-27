#lang racket

(require 
 plot
 [except-in plot/utils sum]
 racket/generator
 "job.rkt"
 "simul.rkt")

(provide
 plot-simulation)

;;================================================================================

;; Test data - fixtures

(define-values
  (VOLD OLD MID NEW VNEW)
  (values 150000 99000 20000 2000 1250))

(define sim-jobs (make-parameter (list (make-job VOLD 30)
                                       (make-job NEW  15)
                                       (make-job VNEW 45)
                                       (make-job MID  23)
                                       (make-job NEW  495)
                                       (make-job VOLD  505)
                                       (make-job OLD  305))))


;;================================================================================

;; Helper syntax for displaying a plot of a simulation

(define-syntax (plot-simulation stx)
  (syntax-case stx ()
    [(_ rng sim)
     #'(let
           ([data (list->vector sim)])
         (plot (get-functions data rng)))]))

(define (run n #:method [m 'hrrn])
  (run-simulation (sim-jobs) n #:method m))

(define (make-plotter sim ref)
    (λ (x) 
      (let ([x (round x)])
        (with-handlers ([any/c (λ args (displayln args) 20)])
          (job-remaining-tasks (list-ref (sequence-ref sim x) ref))))))

(define (get-functions sim rng)
  (define first-row (sequence-ref sim 0))
  (define next-label (sequence->generator (map label-name first-row)))
  (define next-color (sequence->generator line-colors))
  
  (for/list 
      ([s (in-list first-row)]
       [n (in-range (length first-row))])
    (function (make-plotter sim n) 
              (car rng) (cdr rng) 
              #:label (next-label)
              #:color (next-color))))

(define (label-name job)
  (format "~a/~a" 
          (job-remaining-time job) 
          (job-remaining-tasks job)))

(define line-colors 
  (map ->color '(yellow green brown blue red black orange)))

;;================================================================================

(module+ main
  (parameterize ([plot-new-window? #t])
    (plot-simulation (cons 0 200) (run 201))
    (plot-simulation (cons 0 200) (run 201 #:method 'fifo))
    (plot-simulation (cons 0 200) (run 201 #:method 'rand))))