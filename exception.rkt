#lang racket/base


(require "require.rkt"
         (java java/lang/Throwable 
               java/lang/Exception
               java/lang/Error))

(struct exn:java exn:fail:user (object) #:transparent)

(define print-java-exception-stack-trace? (make-parameter #t))

(define (default-java-throw-handler obj)
  (cond
    [(or (Exception? obj) (Error? obj))
     (raise (exn:java (Throwable-toString obj)
                      (current-continuation-marks)
                      obj))]
    [else (raise obj)]))

(provide (all-defined-out))