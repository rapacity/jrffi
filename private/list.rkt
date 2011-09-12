#lang racket/base



(require racket/dict racket/function)
     

(define (partition-by proc lst)
  (for/fold ([buckets null]) ([e (in-list lst)])
    (dict-update buckets (proc e) (curry cons e) null)))



(provide (all-defined-out))
