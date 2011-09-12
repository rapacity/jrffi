#lang racket/base

(require syntax/stx)

;<!!!> this code is taken racket with-stx.rkt and has been slightly modified
(define counter 0)
(define (append-number s)
  (set! counter (add1 counter))
  (string->symbol (format "~a~s" s counter)))
(define (generate-temporaries* sl)
  (unless (stx-list? sl)
    (raise-type-error 
     'generate-temporaries
     "syntax pair"
     sl))
  (let ([l (stx->list sl)])
    (map (lambda (x) 
           ((make-syntax-introducer)
            (cond
              [(symbol? x)
               (datum->syntax #f (append-number x))]
              [(string? x)
               (datum->syntax #f (append-number x))]
              [(keyword? x)
               (datum->syntax #f (append-number (keyword->string x)))]
              [(identifier? x)
               (datum->syntax #f (append-number (syntax-e x)))]
              [(and (syntax? x) (keyword? (syntax-e x)))
               (datum->syntax #f (append-number (keyword->string (syntax-e x))))]
              [(and (syntax? x) (stx-list? x))
               (datum->syntax #f (generate-temporaries x))]
              ; TODO handle improper list case
              #;[(and (syntax? x) (stx-pair? x))
               (datum->syntax #f (cons (generate-temporaries (stx-car x))
                                       (generate-temporaries (stx-cdr x))))]
              [else 
               (datum->syntax #f (append-number 'temp))])))
         l)))
;</!!!>

  (define (maybe-syntax ? stx [else #`()])
    (when (syntax? ?) (set! ? (syntax-e ?)))
    (if ? #`(#,stx) else))
  
  
  (define (maybe-not-syntax ? stx [else #`()])
    (when (syntax? ?) (set! ? (syntax-e ?)))
    (if ? else #`(#,stx)))
  
  
  (define (generate-n-temporaries n)
    (generate-temporaries (build-list n values)))
  
  


(provide generate-temporaries* maybe-syntax maybe-not-syntax generate-n-temporaries)