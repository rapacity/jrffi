#lang racket

(require rackunit "../main.rkt" "../autobind.rkt" "../jvector.rkt")


; autobinder line
(jimport java.util.Arrays)


(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "jimport" "[false, true]" (java.util.Arrays-toString x))

