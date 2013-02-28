#lang racket

(provide
 define-plucked-values)

(begin-for-syntax 
  (define (get-accessors struct-name attr-names)
    (for/list
        ([attr attr-names])
      (string-append 
       (symbol->string struct-name) "-" (symbol->string attr)))))

(define-syntax (define-plucked-values stx)
  (syntax-case stx ()
    [(_ lst (st attr1 attr2 ...))
     (begin 
       (let* 
           ([acc (get-accessors (syntax->datum #'st) (syntax->datum #'(attr1 attr2 ...)))]
            [accessors (datum->syntax stx `(list ,@(map string->symbol acc)))])
         (with-syntax ([%acc accessors])
           #'(define-values 
               (attr1 attr2 ...) 
               (apply values (for/list ([a %acc])
                               (map a lst)))))))]))

(struct s (a b c))

(define z (list (s 6 5 4) (s 9 8 7)))

(define-plucked-values z (s a c))